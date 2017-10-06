/*
** $Id: lgc.c,v 2.38.1.1 2007/12/27 13:02:25 roberto Exp $
** Garbage Collector
** See Copyright Notice in lua.h
*/

#include <string.h>

#define lgc_c
#define LUA_CORE

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"

// 每次自动GC至少的尺寸
#define GCSTEPSIZE	1024u
#define GCSWEEPMAX	40
#define GCSWEEPCOST	10
#define GCFINALIZECOST	100

// 除了黑白色之外的位值
#define maskmarks	cast_byte(~(bitmask(BLACKBIT)|WHITEBITS))

// 首先只留下黑白色之外的位值， 然后与当前的 白色或操作，也就是设置为当前的白色
#define makewhite(g,x)	\
   ((x)->gch.marked = cast_byte(((x)->gch.marked & maskmarks) | luaC_white(g)))

// 从白色变成灰色，做法就是把白色位值去掉
#define white2gray(x)	reset2bits((x)->gch.marked, WHITE0BIT, WHITE1BIT)

// 从黑色变成灰色，做法就是把黑色位值去掉
#define black2gray(x)	resetbit((x)->gch.marked, BLACKBIT)
// 把两种白色都去掉
#define stringmark(s)	reset2bits((s)->tsv.marked, WHITE0BIT, WHITE1BIT)


#define isfinalized(u)		testbit((u)->marked, FINALIZEDBIT)
#define markfinalized(u)	l_setbit((u)->marked, FINALIZEDBIT)


#define KEYWEAK         bitmask(KEYWEAKBIT)
#define VALUEWEAK       bitmask(VALUEWEAKBIT)



#define markvalue(g,o) { checkconsistency(o); \
  if (iscollectable(o) && iswhite(gcvalue(o))) reallymarkobject(g,gcvalue(o)); }

#define markobject(g,t) { if (iswhite(obj2gco(t))) \
		reallymarkobject(g, obj2gco(t)); }

// 设置触发GC的阈值：estimate的值的某个百分比，这个百分比由gcpause参数控制
#define setthreshold(g)  (g->GCthreshold = (g->estimate/100) * g->gcpause)


static void removeentry (Node *n) {
  lua_assert(ttisnil(gval(n)));
  if (iscollectable(gkey(n)))
    setttype(gkey(n), LUA_TDEADKEY);  /* dead key; remove it */
}

// 标记一个对象为可回收对象
static void reallymarkobject (global_State *g, GCObject *o) {
  lua_assert(iswhite(o) && !isdead(g, o));
  // 首先从白色变成灰色
  white2gray(o);
  switch (o->gch.tt) {
    case LUA_TSTRING: {
      // 字符串不做处理
      return;
    }
    case LUA_TUSERDATA: {
      Table *mt = gco2u(o)->metatable;
      // gray2black表示将与之关联的obj也mark了
      gray2black(o);  /* udata are never gray */
      if (mt) markobject(g, mt);
      markobject(g, gco2u(o)->env);
      return;
    }
    case LUA_TUPVAL: {
      UpVal *uv = gco2uv(o);
      markvalue(g, uv->v);
      // 说明现在没有值与这个upval关联，可以直接置为black
      // uv->v == &uv->u.value表示这个upval是close状态
      // open状态是不会变成black的,因为open的时候关联的对象还在呢
      if (uv->v == &uv->u.value)  /* closed? */
        gray2black(o);  /* open upvalues are never black */
      return;
    }
    case LUA_TFUNCTION: {
      // 对象挂载到gray链表中, 下同
      gco2cl(o)->c.gclist = g->gray;
      g->gray = o;
      break;
    }
    case LUA_TTABLE: {
      gco2h(o)->gclist = g->gray;
      g->gray = o;
      break;
    }
    case LUA_TTHREAD: {
      gco2th(o)->gclist = g->gray;
      g->gray = o;
      break;
    }
    case LUA_TPROTO: {
      gco2p(o)->gclist = g->gray;
      g->gray = o;
      break;
    }
    default: lua_assert(0);
  }
}


static void marktmu (global_State *g) {
  GCObject *u = g->tmudata;
  if (u) {
    do {
      u = u->gch.next;
      // 先变成白色，再重新mark
      makewhite(g, u);  /* may be marked, if left from previous GC */
      reallymarkobject(g, u);
    } while (u != g->tmudata);
  }
}


/* move `dead' udata that need finalization to list `tmudata' */
// 分离udata,将有__gc的udata放到tmudata链表中
size_t luaC_separateudata (lua_State *L, int all) {
  global_State *g = G(L);
  size_t deadmem = 0;
  // udata放在mainthread的next中,这样方便找
  GCObject **p = &g->mainthread->next;
  GCObject *curr;
  while ((curr = *p) != NULL) {
    if (!(iswhite(curr) || all) || isfinalized(gco2u(curr)))
      // 已经做过finalize标记的可以无视,
      p = &curr->gch.next;  /* don't bother with them */
    else if (fasttm(L, gco2u(curr)->metatable, TM_GC) == NULL) {
      // 如果没有__gc元方法,直接标记成final状态
      markfinalized(gco2u(curr));  /* don't need finalization */
      p = &curr->gch.next;
    }
    else {  /* must call its gc method */
      // 有GC方法的都放到tmudata链表中
      deadmem += sizeudata(gco2u(curr));
      markfinalized(gco2u(curr));
      *p = curr->gch.next;
      /* link `curr' at the end of `tmudata' list */
      if (g->tmudata == NULL)  /* list is empty? */
        g->tmudata = curr->gch.next = curr;  /* creates a circular list */
      else {
        curr->gch.next = g->tmudata->gch.next;
        g->tmudata->gch.next = curr;
        g->tmudata = curr;
      }
    }
  }
  return deadmem;
}

// 遍历一个表, 返回1表示是弱表
static int traversetable (global_State *g, Table *h) {
  int i;
  int weakkey = 0;
  int weakvalue = 0;
  const TValue *mode;
  // markmeta表
  if (h->metatable)
    markobject(g, h->metatable);
  mode = gfasttm(g, h->metatable, TM_MODE);
  // 如果__mode元方法被定义
  if (mode && ttisstring(mode)) {  /* is there a weak mode? */
	  // 判断是弱键还是弱值
    weakkey = (strchr(svalue(mode), 'k') != NULL);
    weakvalue = (strchr(svalue(mode), 'v') != NULL);
    if (weakkey || weakvalue) {  /* is really weak? */
      // 如果其中之一的条件满足
      // 首先将原来的弱键/弱值标记位清除
      h->marked &= ~(KEYWEAK | VALUEWEAK);  /* clear bits */
      // 标记这次的标记位
      h->marked |= cast_byte((weakkey << KEYWEAKBIT) |
                             (weakvalue << VALUEWEAKBIT));
      // 加入weak链表中
      h->gclist = g->weak;  /* must be cleared after GC, ... */
      g->weak = obj2gco(h);  /* ... so put in the appropriate list */
    }
  }
  // 如果既是弱值也是弱键直接返回了，表中的数据都不需要进行标记
  if (weakkey && weakvalue) return 1;
  // 如果不是弱值，那么需要mark所有的数组值
  if (!weakvalue) {
    i = h->sizearray;
    // 把所有数组的值都mark
    while (i--)
      markvalue(g, &h->array[i]);
  }
  // 无论是弱值还是弱key都需要做这一步:遍历hash部分,根据是弱值/key来标记key/值
  i = sizenode(h);
  while (i--) {
    Node *n = gnode(h, i);
    lua_assert(ttype(gkey(n)) != LUA_TDEADKEY || ttisnil(gval(n)));
    // 如果值已经是nil了,直接把这个节点删除了
    if (ttisnil(gval(n)))
      removeentry(n);  /* remove empty entries */
    else {
      lua_assert(!ttisnil(gkey(n)));
      // 分别视到底是弱值还是弱键情况mark值和键
      if (!weakkey) markvalue(g, gkey(n));
      if (!weakvalue) markvalue(g, gval(n));
    }
  }
  return weakkey || weakvalue;
}


/*
** All marks are conditional because a GC may happen while the
** prototype is still being created
*/
// 标记proto
static void traverseproto (global_State *g, Proto *f) {
  int i;
  // 标记source字符串
  if (f->source) stringmark(f->source);
  // 标记常量
  for (i=0; i<f->sizek; i++)  /* mark literals */
    markvalue(g, &f->k[i]);
  for (i=0; i<f->sizeupvalues; i++) {  /* mark upvalue names */
	// 标记upval的名字字符串
    if (f->upvalues[i])
      stringmark(f->upvalues[i]);
  }
  // 标记嵌套的proto
  for (i=0; i<f->sizep; i++) {  /* mark nested protos */
    if (f->p[i])
      markobject(g, f->p[i]);
  }
  // 标记局部变量名
  for (i=0; i<f->sizelocvars; i++) {  /* mark local-variable names */
    if (f->locvars[i].varname)
      stringmark(f->locvars[i].varname);
  }
}


// 遍历一个closure, mark其中的数据为可回收
static void traverseclosure (global_State *g, Closure *cl) {
  markobject(g, cl->c.env);
  if (cl->c.isC) {
	// 这是一个C函数
    int i;
    for (i=0; i<cl->c.nupvalues; i++)  /* mark its upvalues */
      markvalue(g, &cl->c.upvalue[i]);
  }
  else {
	// 这是一个lua函数
    int i;
    lua_assert(cl->l.nupvalues == cl->l.p->nups);
    markobject(g, cl->l.p);
    for (i=0; i<cl->l.nupvalues; i++)  /* mark its upvalues */
      markobject(g, cl->l.upvals[i]);
  }
}


static void checkstacksizes (lua_State *L, StkId max) {
  int ci_used = cast_int(L->ci - L->base_ci);  /* number of `ci' in use */
  int s_used = cast_int(max - L->stack);  /* part of stack in use */
  if (L->size_ci > LUAI_MAXCALLS)  /* handling overflow? */
    return;  /* do not touch the stacks */
  if (4*ci_used < L->size_ci && 2*BASIC_CI_SIZE < L->size_ci)
    luaD_reallocCI(L, L->size_ci/2);  /* still big enough... */
  condhardstacktests(luaD_reallocCI(L, ci_used + 1));
  if (4*s_used < L->stacksize &&
      2*(BASIC_STACK_SIZE+EXTRA_STACK) < L->stacksize)
    luaD_reallocstack(L, L->stacksize/2);  /* still big enough... */
  condhardstacktests(luaD_reallocstack(L, s_used));
}


static void traversestack (global_State *g, lua_State *l) {
  StkId o, lim;
  CallInfo *ci;
  markvalue(g, gt(l));
  lim = l->top;
  for (ci = l->base_ci; ci <= l->ci; ci++) {
    lua_assert(ci->top <= l->stack_last);
    if (lim < ci->top) lim = ci->top;
  }
  for (o = l->stack; o < l->top; o++)
    markvalue(g, o);
  for (; o <= lim; o++)
    setnilvalue(o);
  checkstacksizes(l, lim);
}


/*
** traverse one gray object, turning it to black.
** Returns `quantity' traversed.
*/
// 把一个对象置黑,表示已回收.同时返回收集的内存数量
static l_mem propagatemark (global_State *g) {
  GCObject *o = g->gray;
  // 到这个函数的都是灰色的对象
  lua_assert(isgray(o));
  // 首先把对象从灰置黑,意思是所有关联对象都被扫描过了
  gray2black(o);
  switch (o->gch.tt) {
    case LUA_TTABLE: {
      Table *h = gco2h(o);
      g->gray = h->gclist;
      // 如果是弱表,变回到灰色----这又是为什么呢?
      if (traversetable(g, h))  /* table is weak? */
        black2gray(o);  /* keep it gray */
      return sizeof(Table) + sizeof(TValue) * h->sizearray +
                             sizeof(Node) * sizenode(h);
    }
    case LUA_TFUNCTION: {
      Closure *cl = gco2cl(o);
      g->gray = cl->c.gclist;
      traverseclosure(g, cl);
      return (cl->c.isC) ? sizeCclosure(cl->c.nupvalues) :
                           sizeLclosure(cl->l.nupvalues);
    }
    case LUA_TTHREAD: {
      lua_State *th = gco2th(o);
      g->gray = th->gclist;
      // 线程对象由于也是1:N的对应关系，所以也是加入到grayagain链表中
      th->gclist = g->grayagain;
      g->grayagain = o;
      black2gray(o);
      traversestack(g, th);
      return sizeof(lua_State) + sizeof(TValue) * th->stacksize +
                                 sizeof(CallInfo) * th->size_ci;
    }
    case LUA_TPROTO: {
      Proto *p = gco2p(o);
      g->gray = p->gclist;
      traverseproto(g, p);
      return sizeof(Proto) + sizeof(Instruction) * p->sizecode +
                             sizeof(Proto *) * p->sizep +
                             sizeof(TValue) * p->sizek + 
                             sizeof(int) * p->sizelineinfo +
                             sizeof(LocVar) * p->sizelocvars +
                             sizeof(TString *) * p->sizeupvalues;
    }
    default: lua_assert(0); return 0;
  }
}

// 使用一个循环遍历所有gray链表的元素,这是一个原子的行为,即不可被打断
static size_t propagateall (global_State *g) {
  size_t m = 0;
  while (g->gray) m += propagatemark(g);
  return m;
}


/*
** The next function tells whether a key or value can be cleared from
** a weak table. Non-collectable objects are never removed from weak
** tables. Strings behave as `values', so are never removed too. for
** other objects: if really collected, cannot keep them; for userdata
** being finalized, keep them in keys, but not in values
*/
static int iscleared (const TValue *o, int iskey) {
  // 非GC数据类型，返回0
  if (!iscollectable(o)) return 0;
  // 如果是字符串,走字符串特殊的mark流程
  if (ttisstring(o)) {
    stringmark(rawtsvalue(o));  /* strings are `values', so are never weak */
    return 0;
  }
  // 如果是白色，或者是udata同时不是key以及已经被mark finalize过了，都返回cleared标记为1
  return iswhite(gcvalue(o)) ||
    (ttisuserdata(o) && (!iskey && isfinalized(uvalue(o))));
}


/*
** clear collected entries from weaktables
*/
// 清理弱表
static void cleartable (GCObject *l) {
  while (l) {
    Table *h = gco2h(l);
    int i = h->sizearray;
    lua_assert(testbit(h->marked, VALUEWEAKBIT) ||
               testbit(h->marked, KEYWEAKBIT));
    if (testbit(h->marked, VALUEWEAKBIT)) {
      //如果是弱值
      while (i--) {
    	// 遍历数组元素，已经清理的就置空
        TValue *o = &h->array[i];
        if (iscleared(o, 0))  /* value was collected? */
          setnilvalue(o);  /* remove value */
      }
    }
    i = sizenode(h);
    while (i--) {
      // 遍历hash元素,这一步无论是弱值还是弱key都要处理
      Node *n = gnode(h, i);
      if (!ttisnil(gval(n)) &&  /* non-empty entry? */
          (iscleared(key2tval(n), 1) || iscleared(gval(n), 0))) {
    	// 如果不是空值， 同时key或者值已经被清理掉了

    	// 清理值
        setnilvalue(gval(n));  /* remove value ... */
        // 将该节点从hash中删除
        removeentry(n);  /* remove entry from table */
      }
    }
    l = h->gclist;
  }
}

// 根据类型删除一个object
static void freeobj (lua_State *L, GCObject *o) {
  switch (o->gch.tt) {
    case LUA_TPROTO: luaF_freeproto(L, gco2p(o)); break;
    case LUA_TFUNCTION: luaF_freeclosure(L, gco2cl(o)); break;
    case LUA_TUPVAL: luaF_freeupval(L, gco2uv(o)); break;
    case LUA_TTABLE: luaH_free(L, gco2h(o)); break;
    case LUA_TTHREAD: {
      lua_assert(gco2th(o) != L && gco2th(o) != G(L)->mainthread);
      luaE_freethread(L, gco2th(o));
      break;
    }
    case LUA_TSTRING: {
      G(L)->strt.nuse--;
      luaM_freemem(L, o, sizestring(gco2ts(o)));
      break;
    }
    case LUA_TUSERDATA: {
      luaM_freemem(L, o, sizeudata(gco2u(o)));
      break;
    }
    default: lua_assert(0);
  }
}


// 传入的count为一个很大的值MAX_LUMEM，说明让这个过程一直进行下去
#define sweepwholelist(L,p)	sweeplist(L,p,MAX_LUMEM)

// 扫描一个sweep链表的所有对象,对可以回收的回收内存,不用回收的markwhite等待下一次扫描
static GCObject **sweeplist (lua_State *L, GCObject **p, lu_mem count) {
  GCObject *curr;
  global_State *g = G(L);
  int deadmask = otherwhite(g);
  while ((curr = *p) != NULL && count-- > 0) {
    if (curr->gch.tt == LUA_TTHREAD)  /* sweep open upvalues of each thread */
      sweepwholelist(L, &gco2th(curr)->openupval);
    if ((curr->gch.marked ^ WHITEBITS) & deadmask) {  /* not dead? */
      // 这句的意思是这个对象otherwhite这一位上是0,而因为已经过了mark阶段
      // 所以此时的otherwhite,其实就是本次GC的白色
      // 所以上面这个判断的意思是,otherwhite这一位是0,也就是不是本次GC的mark阶段被mark成白色的
      // 也就是说,这个对象本次不会去回收
      lua_assert(!isdead(g, curr) || testbit(curr->gch.marked, FIXEDBIT));
      makewhite(g, curr);  /* make it white (for next cycle) */
      p = &curr->gch.next;
    }
    else {  /* must erase `curr' */
      // 好了,可以回收这个对象了
      lua_assert(isdead(g, curr) || deadmask == bitmask(SFIXEDBIT));
      *p = curr->gch.next;
      if (curr == g->rootgc)  /* is the first element of the list? */
        g->rootgc = curr->gch.next;  /* adjust first */
      freeobj(L, curr);
    }
  }
  return p;
}


static void checkSizes (lua_State *L) {
  global_State *g = G(L);
  /* check size of string hash */
  if (g->strt.nuse < cast(lu_int32, g->strt.size/4) &&
      g->strt.size > MINSTRTABSIZE*2)
    // 字符串的数量小于桶数组数量的1/4，同时还大于最低要求的hash桶数量两倍时
    // 此时桶数组就太大，有点浪费了，于是这里将桶大小减一倍
    luaS_resize(L, g->strt.size/2);  /* table is too big */
  /* check size of buffer */
  if (luaZ_sizebuffer(&g->buff) > LUA_MINBUFFER*2) {  /* buffer too big? */
    size_t newsize = luaZ_sizebuffer(&g->buff) / 2;
    luaZ_resizebuffer(L, &g->buff, newsize);
  }
}

// 对有GC方法的udata进行回收,也就是调用其注册的GC函数
static void GCTM (lua_State *L) {
  global_State *g = G(L);
  GCObject *o = g->tmudata->gch.next;  /* get first element */
  Udata *udata = rawgco2u(o);
  const TValue *tm;
  /* remove udata from `tmudata' */
  // 首先将这个udata从tmudata中删除,如果是最后一个元素了,就把tmudata链表置NULL
  if (o == g->tmudata)  /* last element? */
    g->tmudata = NULL;
  else
    g->tmudata->gch.next = udata->uv.next;
  // 把它放回mainthread的下一个对象中,
  udata->uv.next = g->mainthread->next;  /* return it to `root' list */
  g->mainthread->next = o;
  // 这里为什么要变成白色呢?也就是下一次GC才回收
  // 换言之,udata的GC方法是这一次被调用,但是udata本身是下一次才回收
  makewhite(g, o);
  tm = fasttm(L, udata->uv.metatable, TM_GC);
  if (tm != NULL) {
	// 调用GC方法
    lu_byte oldah = L->allowhook;
    lu_mem oldt = g->GCthreshold;
    L->allowhook = 0;  /* stop debug hooks during GC tag method */
    g->GCthreshold = 2*g->totalbytes;  /* avoid GC steps */
    setobj2s(L, L->top, tm);
    setuvalue(L, L->top+1, udata);
    L->top += 2;
    luaD_call(L, L->top - 2, 0);
    L->allowhook = oldah;  /* restore hooks */
    g->GCthreshold = oldt;  /* restore threshold */
  }
}


/*
** Call all GC tag methods
*/
void luaC_callGCTM (lua_State *L) {
  while (G(L)->tmudata)
    GCTM(L);
}

// 全部清除,不管什么颜色
void luaC_freeall (lua_State *L) {
  global_State *g = G(L);
  int i;
  // 两种白色都清除
  g->currentwhite = WHITEBITS | bitmask(SFIXEDBIT);  /* mask to collect all elements */
  sweepwholelist(L, &g->rootgc);
  for (i = 0; i < g->strt.size; i++)  /* free all string lists */
    sweepwholelist(L, &g->strt.hash[i]);
}


static void markmt (global_State *g) {
  int i;
  for (i=0; i<NUM_TAGS; i++)
    if (g->mt[i]) markobject(g, g->mt[i]);
}


/* mark root set */
static void markroot (lua_State *L) {
  global_State *g = G(L);
  // 首先置空这几个链表
  g->gray = NULL;
  g->grayagain = NULL;
  g->weak = NULL;
  markobject(g, g->mainthread);
  /* make global table be traversed before main stack */
  // 标记g表和reg表
  markvalue(g, gt(g->mainthread));
  markvalue(g, registry(L));
  // 标记meta表
  markmt(g);
  g->gcstate = GCSpropagate;
}

// 重新markupval的值
static void remarkupvals (global_State *g) {
  UpVal *uv;
  for (uv = g->uvhead.u.l.next; uv != &g->uvhead; uv = uv->u.l.next) {
    lua_assert(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    // 如果是灰色，说明关联的值还没有mark
    if (isgray(obj2gco(uv)))
      markvalue(g, uv->v);
  }
}

// 一个原子的过程,不可被中断
static void atomic (lua_State *L) {
  global_State *g = G(L);
  size_t udsize;  /* total size of userdata to be finalized */
  /* remark occasional upvalues of (maybe) dead threads */
  remarkupvals(g);
  /* traverse objects cautch by write barrier and by 'remarkupvals' */
  propagateall(g);
  /* remark weak tables */
  // 标记弱表
  g->gray = g->weak;
  g->weak = NULL;
  lua_assert(!iswhite(obj2gco(g->mainthread)));
  markobject(g, L);  /* mark running thread */
  markmt(g);  /* mark basic metatables (again) */
  propagateall(g);
  /* remark gray again */
  // 遍历again链表进行标记
  g->gray = g->grayagain;
  g->grayagain = NULL;
  propagateall(g);
  udsize = luaC_separateudata(L, 0);  /* separate userdata to be finalized */
  marktmu(g);  /* mark `preserved' userdata */
  udsize += propagateall(g);  /* remark, to propagate `preserveness' */
  // 一个原子的过程去mark弱表
  cleartable(g->weak);  /* remove collected objects from weak tables */
  /* flip current white */
  g->currentwhite = cast_byte(otherwhite(g));
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->gcstate = GCSsweepstring;
  g->estimate = g->totalbytes - udsize;  /* first estimate */
}

// GC状态机的单步工作
static l_mem singlestep (lua_State *L) {
  global_State *g = G(L);
  /*lua_checkmemory(L);*/
  switch (g->gcstate) {
    case GCSpause: {
      markroot(L);  /* start a new collection */
      return 0;
    }
    case GCSpropagate: {
      // 当gray链表还有元素的时候,持续mark该元素关联的值,每次mark一个对象
      if (g->gray)
        return propagatemark(g);
      else {  /* no more `gray' objects */
    	  // 再也没有灰色的对象了,来一个原子的mark过程
        atomic(L);  /* finish mark phase */
        return 0;
      }
    }
    case GCSsweepstring: {
      // 回收某个string hash table
      // 首先保存旧的总大小
      lu_mem old = g->totalbytes;
      // 对某个string的hash table进行回收
      sweepwholelist(L, &g->strt.hash[g->sweepstrgc++]);
      // 如果已经回收完了，进入下一个阶段GCSsweep
      if (g->sweepstrgc >= g->strt.size)  /* nothing more to sweep? */
        g->gcstate = GCSsweep;  /* end sweep-string phase */
      lua_assert(old >= g->totalbytes);
      // 减少估值
      g->estimate -= old - g->totalbytes;
      // 我猜想这里返回一个固定的值，而不是按照实际回收的大小返回
      // 是因为前面扫描阶段已经返回实际的值了？
      return GCSWEEPCOST;
    }
    case GCSsweep: {
      lu_mem old = g->totalbytes;
      g->sweepgc = sweeplist(L, g->sweepgc, GCSWEEPMAX);
      if (*g->sweepgc == NULL) {  /* nothing more to sweep? */
        checkSizes(L);
        g->gcstate = GCSfinalize;  /* end sweep phase */
      }
      lua_assert(old >= g->totalbytes);
      g->estimate -= old - g->totalbytes;
      // 我猜想这里返回一个固定的值，而不是按照实际回收的大小返回
      // 是因为前面扫描阶段已经返回实际的值了？
      return GCSWEEPMAX*GCSWEEPCOST;
    }
    case GCSfinalize: {
      if (g->tmudata) {
        GCTM(L);
        if (g->estimate > GCFINALIZECOST)
          g->estimate -= GCFINALIZECOST;
        return GCFINALIZECOST;
      }
      else {
        g->gcstate = GCSpause;  /* end collection */
        g->gcdept = 0;
        return 0;
      }
    }
    default: lua_assert(0); return 0;
  }
}


void luaC_step (lua_State *L) {
  global_State *g = G(L);
  // 大致估算本次回收要回收多少数据
  // 其中，gcstepmul用于控制这次回收是GCSTEPSIZE的多少百分比
  // 显然这个数据越大，在后面的singlestep函数中调用的时间就越长
  l_mem lim = (GCSTEPSIZE/100) * g->gcstepmul;
  // 为0的情况说明是无限制，所以还是需要设置一个具体的数据
  if (lim == 0)
    lim = (MAX_LUMEM-1)/2;  /* no limit */
  // 首先累加本次totalbytes和GCthreshold的差值，知道要到自动GC完毕要回收多少数据
  g->gcdept += g->totalbytes - g->GCthreshold;
  do {
    lim -= singlestep(L);
    if (g->gcstate == GCSpause)
      break;
  } while (lim > 0);
  if (g->gcstate != GCSpause) {
	 // 走到这里，说明lim不大于0，也就是本次自动GC将预估的数据大小全部回收了
    if (g->gcdept < GCSTEPSIZE)
      // 如果待GC的数据大小比GCSTEPSIZE（自动GC要求的尺寸）小，那么设置GCthreshold比totalbytes大GCSTEPSIZE
      // 也就是说，只要totalbyte多分配了GCSTEPSIZE的数量，就马上触发GC操作。
      g->GCthreshold = g->totalbytes + GCSTEPSIZE;  /* - lim/g->gcstepmul;*/
    else {
      // 为什么这里是减去GCSTEPSIZE，这一次回收不见得就是回收了GCSTEPSIZE这么多的内存啊？！
      g->gcdept -= GCSTEPSIZE;
      // 马上触发下一次GC
      g->GCthreshold = g->totalbytes;
    }
  }
  else {
	  // 走到这里，说明g->gcstate == GCSpause
    // 说明已经完成了一次完整的GC
    lua_assert(g->totalbytes >= g->estimate);
    // 重新设置GCthreshold
    setthreshold(g);
  }
}

// 完整的一次GC过程
void luaC_fullgc (lua_State *L) {
  global_State *g = G(L);
  // 重新把所有对象都mark成白色
  if (g->gcstate <= GCSpropagate) {
    /* reset sweep marks to sweep all elements (returning them to white) */
    // 注意在这里并没有改变当前白色，因此在前面标记过的数据并不会被回收
    // 所以在这里只是简单的重置了状态而已
    g->sweepstrgc = 0;
    g->sweepgc = &g->rootgc;
    /* reset other collector lists */
    g->gray = NULL;
    g->grayagain = NULL;
    g->weak = NULL;
    g->gcstate = GCSsweepstring;
  }
  lua_assert(g->gcstate != GCSpause && g->gcstate != GCSpropagate);
  /* finish any pending sweep phase */
  // 这里仅执行sweep和sweepstring两个过程，因为前面没有改变白色，
  // 所以这里只是将所有对象重新mark成白色
  while (g->gcstate != GCSfinalize) {
    lua_assert(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep);
    singlestep(L);
  }
  // 重新开始一次完整GC
  markroot(L);
  while (g->gcstate != GCSpause) {
    singlestep(L);
  }
  setthreshold(g);
}

// GC向前走一步
void luaC_barrierf (lua_State *L, GCObject *o, GCObject *v) {
  global_State *g = G(L);
  // o是黑色的，v是白色的，同时都是活着的
  lua_assert(isblack(o) && iswhite(v) && !isdead(g, v) && !isdead(g, o));
  // gcstate不等于GCSfinalize和GCSpause
  lua_assert(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
  // o的类型不是TABLE
  lua_assert(ttype(&o->gch) != LUA_TTABLE);
  /* must keep invariant? */
  if (g->gcstate == GCSpropagate)
	// 如果在mark阶段，就把要关联的值也mark起来
    reallymarkobject(g, v);  /* restore invariant */
  else  /* don't mind */
	// 否则变成白色，再有barrier操作也不会走进来了，避免多次barrier
    makewhite(g, o);  /* mark as white just to avoid other barriers */
}

// 从黑色回到灰色,意思是回退一步，加入grayagain链表（因为grayagain是需要一次原子过程去mark的对象链表）
// back仅针对table类型成员,因为table对象经常出现1个table对象存放的是N个对象也就是1:N的情况
// 如果每个table在受到其中一个对象的影响都要重新标记，那会操作的很频繁，所以就索性回退处理
// 等待后续的atomic过程才一次性处理这个重新加入grayagain链表的对象
void luaC_barrierback (lua_State *L, Table *t) {
  global_State *g = G(L);
  GCObject *o = obj2gco(t);
  lua_assert(isblack(o) && !isdead(g, o));
  lua_assert(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
  black2gray(o);  /* make table gray (again) */
  // 把这个table加入grayagain链表,意思是原子扫描
  t->gclist = g->grayagain;
  g->grayagain = o;
}

// 将某个对象指针加入GC链表中,同时置它的颜色为当前的白色
void luaC_link (lua_State *L, GCObject *o, lu_byte tt) {
  global_State *g = G(L);
  o->gch.next = g->rootgc;
  g->rootgc = o;
  o->gch.marked = luaC_white(g);
  o->gch.tt = tt;
}

// 这里需要问一下:upval和一般的对象有什么区别?为什么要单独一个函数来处理?
void luaC_linkupval (lua_State *L, UpVal *uv) {
  global_State *g = G(L);
  GCObject *o = obj2gco(uv);
  o->gch.next = g->rootgc;  /* link upvalue into `rootgc' list */
  g->rootgc = o;
  if (isgray(o)) { 
	// 如果obj是灰色的，说明与它关联的对象还没mark过
    if (g->gcstate == GCSpropagate) {
      // 如果当前在mark阶段，就对它关联的对象进行mark
      gray2black(o);  /* closed upvalues need barrier */
      luaC_barrier(L, uv, uv->v);
    }
    else {  /* sweep phase: sweep it (turning it into white) */
      // 否则就是已经过了mark阶段，这里的注释说将会sweep这个节点,但是这样是不对的
      // 因为过了GCSpropagate阶段的话,在atomic函数中已经切换了白色,也就是下一次GC时的白色.
      // 所以这个时候的切换,并不会让它在本次GC中被sweep掉
      makewhite(g, o);
      lua_assert(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
    }
  }
}

