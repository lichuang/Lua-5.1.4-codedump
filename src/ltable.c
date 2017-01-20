/*
** $Id: ltable.c,v 2.32.1.2 2007/12/28 15:32:23 roberto Exp $
** Lua tables (hash)
** See Copyright Notice in lua.h
*/


/*
** Implementation of tables (aka arrays, objects, or hash tables).
** Tables keep its elements in two parts: an array part and a hash part.
** Non-negative integer keys are all candidates to be kept in the array
** part. The actual size of the array is the largest `n' such that at
** least half the slots between 0 and n are in use.
** Hash uses a mix of chained scatter table with Brent's variation.
** A main invariant of these tables is that, if an element is not
** in its main position (i.e. the `original' position that its hash gives
** to it), then the colliding element is in its own main position.
** Hence even when the load factor reaches 100%, performance remains good.
*/

#include <math.h>
#include <string.h>

#define ltable_c
#define LUA_CORE

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "ltable.h"


/*
** max size of array part is 2^MAXBITS
*/
#if LUAI_BITSINT > 26
#define MAXBITS		26
#else
#define MAXBITS		(LUAI_BITSINT-2)
#endif

#define MAXASIZE	(1 << MAXBITS)


#define hashpow2(t,n)      (gnode(t, lmod((n), sizenode(t))))
  
#define hashstr(t,str)  hashpow2(t, (str)->tsv.hash)
#define hashboolean(t,p)        hashpow2(t, p)


/*
** for some types, it is better to avoid modulus by power of 2, as
** they tend to have many 2 factors.
*/
// 对某些类型而言,最好避免直接与2的次幂模操作取hash值,因为它们可能有很多2因子
#define hashmod(t,n)	(gnode(t, ((n) % ((sizenode(t)-1)|1))))


#define hashpointer(t,p)	hashmod(t, IntPoint(p))


/*
** number of ints inside a lua_Number
*/
#define numints		cast_int(sizeof(lua_Number)/sizeof(int))



#define dummynode		(&dummynode_)

static const Node dummynode_ = {
  {{NULL}, LUA_TNIL},  /* value */
  {{{NULL}, LUA_TNIL, NULL}}  /* key */
};


/*
** hash for lua_Numbers
*/
// 对数字进行hash
static Node *hashnum (const Table *t, lua_Number n) {
  unsigned int a[numints];
  int i;
  if (luai_numeq(n, 0))  /* avoid problems with -0 */
    return gnode(t, 0);
  memcpy(a, &n, sizeof(a));
  for (i = 1; i < numints; i++) a[0] += a[i];
  return hashmod(t, a[0]);
}



/*
** returns the `main' position of an element in a table (that is, the index
** of its hash value)
*/
// 根据key寻找在hash中的Node
static Node *mainposition (const Table *t, const TValue *key) {
  switch (ttype(key)) {
    case LUA_TNUMBER:
      return hashnum(t, nvalue(key));
    case LUA_TSTRING:
      return hashstr(t, rawtsvalue(key));
    case LUA_TBOOLEAN:
      return hashboolean(t, bvalue(key));
    case LUA_TLIGHTUSERDATA:
      return hashpointer(t, pvalue(key));
    default:
      return hashpointer(t, gcvalue(key));
  }
}


/*
** returns the index for `key' if `key' is an appropriate key to live in
** the array part of the table, -1 otherwise.
*/
// 在数组中寻找一个key, 如果找到则返回在数组中的索引, 否则返回-1
static int arrayindex (const TValue *key) {
  if (ttisnumber(key)) {
    lua_Number n = nvalue(key);
    int k;
    lua_number2int(k, n);
    if (luai_numeq(cast_num(k), n))
      return k;
  }
  return -1;  /* `key' did not match some condition */
}


/*
** returns the index of a `key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signalled by -1.
*/
// 根据key寻找索引(无论是在数字还是hash中)
static int findindex (lua_State *L, Table *t, StkId key) {
  int i;
  if (ttisnil(key)) return -1;  /* first iteration */
  // 首先在数组部分进行查找
  i = arrayindex(key);
  // 如果index在数组范围内,则直接返回数组索引
  if (0 < i && i <= t->sizearray)  /* is `key' inside array part? */
	// 返回的index需要-1是因为要跟C数组匹配上
    return i-1;  /* yes; that's the index (corrected to C) */
  else {
	// 否则查找hash部分
    Node *n = mainposition(t, key);
    do {  /* check whether `key' is somewhere in the chain */
      /* key may be dead already, but it is ok to use it in `next' */
      if (luaO_rawequalObj(key2tval(n), key) ||
            (ttype(gkey(n)) == LUA_TDEADKEY && iscollectable(key) &&
             gcvalue(gkey(n)) == gcvalue(key))) {
    	// 需要算出在hash部分中的偏移位置
        i = cast_int(n - gnode(t, 0));  /* key index in hash table */
        /* hash elements are numbered after array ones */
        // 这个偏移位置还要加上数组部分的长度,以便区分
        return i + t->sizearray;
      }
      // 没有找到的话,就继续寻找hash桶中的下一个元素
      else n = gnext(n);
    } while (n);
    luaG_runerror(L, "invalid key to " LUA_QL("next"));  /* key not found */
    return 0;  /* to avoid warnings */
  }
}

// 根据key寻找下一个不为nil的元素, 找到返回1,
int luaH_next (lua_State *L, Table *t, StkId key) {
  int i = findindex(L, t, key);  /* find original element */
  for (i++; i < t->sizearray; i++) {  /* try first array part */
    if (!ttisnil(&t->array[i])) {  /* a non-nil value? */
      // i + 1存入key中
      setnvalue(key, cast_num(i+1));
      // 将i的值复制到key + 1中(也就是i + 2)
      setobj2s(L, key+1, &t->array[i]);
      return 1;
    }
  }
  // 需要减去数组部分长度,
  // 这里居然使用的i++,不是node中的next,这不对吧????
  // 换言之,这里取到的不是在同一个hash桶上的node
  for (i -= t->sizearray; i < sizenode(t); i++) {  /* then hash part */
    if (!ttisnil(gval(gnode(t, i)))) {  /* a non-nil value? */
      setobj2s(L, key, key2tval(gnode(t, i)));
      setobj2s(L, key+1, gval(gnode(t, i)));
      return 1;
    }
  }
  return 0;  /* no more elements */
}


/*
** {=============================================================
** Rehash
** ==============================================================
*/

// 只有利用率超过50%的数组元素会进入数组,否则进去hash
static int computesizes (int nums[], int *narray) {
  int i;
  int twotoi;  /* 2^i */
  int a = 0;  /* number of elements smaller than 2^i */
  int na = 0;  /* number of elements to go to array part */
  int n = 0;  /* optimal size for array part */

  // 这个循环完毕后,最重要的na存放的是数组部分的数据数量,而n是
  // na满足这个条件:在
  for (i = 0, twotoi = 1; twotoi/2 < *narray; i++, twotoi *= 2) {
    if (nums[i] > 0) {
      // 加上当前数量
      a += nums[i];
      // 如果数量已经超过半数, 那么数组的尺寸可以设置为这个大小
      if (a > twotoi/2) {  /* more than half elements present? */
        n = twotoi;  /* optimal size (till now) */
        // 记录下当前的数组大小
        na = a;  /* all elements smaller than n will go to array part */
      }
    }
    if (a == *narray) break;  /* all elements already counted */
  }
  *narray = n;
  lua_assert(*narray/2 <= na && na <= *narray);
  return na;
}

// 传入nums数组, 计算key是不是在2^(i-1) and 2^i范围内,如果是将nums相应的部分加一
// 返回0/1的依据是它是否落在合适的数组范围内
static int countint (const TValue *key, int *nums) {
  int k = arrayindex(key);
  if (0 < k && k <= MAXASIZE) {  /* is `key' an appropriate array index? */
	// 如果k在一个合理的范围内,将相应的nums加1
    nums[ceillog2(k)]++;  /* count as such */
    return 1;
  }
  else
    return 0;
}

// 传入nums数组, 它的意义是:nums[i] = number of keys between 2^(i-1) and 2^i
static int numusearray (const Table *t, int *nums) {
  int lg;
  int ttlg;  /* 2^lg */
  int ause = 0;  /* summation of `nums' */
  int i = 1;  /* count to traverse all array keys */
  for (lg=0, ttlg=1; lg<=MAXBITS; lg++, ttlg*=2) {  /* for each slice */
    int lc = 0;  /* counter */
    int lim = ttlg;
    if (lim > t->sizearray) {
      lim = t->sizearray;  /* adjust upper limit */
      if (i > lim)
        break;  /* no more elements to count */
    }
    /* count elements in range (2^(lg-1), 2^lg] */
    for (; i <= lim; i++) {
      if (!ttisnil(&t->array[i-1]))
        lc++;
    }
    nums[lg] += lc;
    ause += lc;
  }
  return ause;
}

// 传入nums数组, 它的意义是:nums[i] = number of keys between 2^(i-1) and 2^i
// 同时计算在hash中的整数数量,将数值更新到数组大小中
static int numusehash (const Table *t, int *nums, int *pnasize) {
  int totaluse = 0;  /* total number of elements */
  int ause = 0;  /* summation of `nums' */
  int i = sizenode(t);
  while (i--) {
    Node *n = &t->node[i];
    if (!ttisnil(gval(n))) {
      ause += countint(key2tval(n), nums);
      totaluse++;
    }
  }
  *pnasize += ause;
  return totaluse;
}

// 初始化table的数组部分
static void setarrayvector (lua_State *L, Table *t, int size) {
  int i;
  // 为什么这里用的是luaM_reallocvector,而后面的setnodevector中使用的是luaM_newvector
  luaM_reallocvector(L, t->array, t->sizearray, size, TValue);
  for (i=t->sizearray; i<size; i++)
     setnilvalue(&t->array[i]);
  t->sizearray = size;
}

// 初始化table的hash数组部分
static void setnodevector (lua_State *L, Table *t, int size) {
  int lsize;
  if (size == 0) {  /* no elements to hash part? */
    t->node = cast(Node *, dummynode);  /* use common `dummynode' */
    lsize = 0;
  }
  else {
    int i;
    // 为什么这里要计算log2,因为lsizenode就是log2值
    lsize = ceillog2(size);
    // 过大了!!
    if (lsize > MAXBITS)
      luaG_runerror(L, "table overflow");
    // 算原始值
    size = twoto(lsize);
    // 以上的ceillog2和twoto操作将size转换为大于size且为2的次幂的最小的数
    // 见setarrayvector中注释
    t->node = luaM_newvector(L, size, Node);
    // 初始化每个hash成员
    for (i=0; i<size; i++) {
      Node *n = gnode(t, i);
      // 将next指针置NULL,将key/value置NIL
      gnext(n) = NULL;
      setnilvalue(gkey(n));
      setnilvalue(gval(n));
    }
  }
  // 存放尺寸和lastfree指针指向最后一个元素
  t->lsizenode = cast_byte(lsize);
  t->lastfree = gnode(t, size);  /* all positions are free */
}

// 重新分配table的数组和hash部分的大小
static void resize (lua_State *L, Table *t, int nasize, int nhsize) {
  int i;
  int oldasize = t->sizearray;
  int oldhsize = t->lsizenode;
  Node *nold = t->node;  /* save old hash ... */
  // 如果新的数组部分大于老的数组部分,那么需要扩展数组尺寸
  if (nasize > oldasize)  /* array part must grow? */
    setarrayvector(L, t, nasize);
  /* create new hash part with appropriate size */
  setnodevector(L, t, nhsize);
  // 如果新的数组部分小于老的数组部分
  if (nasize < oldasize) {  /* array part must shrink? */
    t->sizearray = nasize;
    /* re-insert elements from vanishing slice */
    // 遍历多出来的那部分
    for (i=nasize; i<oldasize; i++) {
      // 如果多出来的部分元素不为nil
      if (!ttisnil(&t->array[i]))
    	// 以i + 1为key, 将i的数据插入hash部分
        setobjt2t(L, luaH_setnum(L, t, i+1), &t->array[i]);
    }
    /* shrink array */
    // 缩减数组大小
    luaM_reallocvector(L, t->array, oldasize, nasize, TValue);
  }
  /* re-insert elements from hash part */
  // 为什么从后往前遍历插入呢?
  for (i = twoto(oldhsize) - 1; i >= 0; i--) {
    Node *old = nold+i;
    // 将原来不为nil的元素重新插入hash中
    if (!ttisnil(gval(old)))
      setobjt2t(L, luaH_set(L, t, key2tval(old)), gval(old));
  }
  // 释放旧的hash部分
  if (nold != dummynode)
    luaM_freearray(L, nold, twoto(oldhsize), Node);  /* free old array */
}

// 数组部分重新分配
void luaH_resizearray (lua_State *L, Table *t, int nasize) {
  int nsize = (t->node == dummynode) ? 0 : sizenode(t);
  resize(L, t, nasize, nsize);
}

// 对table进行重新划分hash和数组部分的大小
static void rehash (lua_State *L, Table *t, const TValue *ek) {
  int nasize, na;
  // nums中存放的是key在2^(i-1), 2^i之间的元素数量
  int nums[MAXBITS+1];  /* nums[i] = number of keys between 2^(i-1) and 2^i */
  int i;
  int totaluse;
  // 首先清空nums数组
  for (i=0; i<=MAXBITS; i++) nums[i] = 0;  /* reset counts */
  // 计算数组部分在每个范围中数据的数量,返回的nasize是数组部分数据的数量
  nasize = numusearray(t, nums);  /* count keys in array part */
  totaluse = nasize;  /* all those keys are integer keys */
  // 计算hash中key的数量
  totaluse += numusehash(t, nums, &nasize);  /* count keys in hash part */
  /* count extra key */
  // 判断新key的范围
  nasize += countint(ek, nums);
  totaluse++;
  /* compute new size for array part */
  // 计算新的数组部分的大小
  na = computesizes(nums, &nasize);
  // 重新分配table中数组和hash的大小
  /* resize the table to new computed sizes */
  resize(L, t, nasize, totaluse - na);
}



/*
** }=============================================================
*/

// 新分配table
Table *luaH_new (lua_State *L, int narray, int nhash) {
  Table *t = luaM_new(L, Table);
  luaC_link(L, obj2gco(t), LUA_TTABLE);
  t->metatable = NULL;
  t->flags = cast_byte(~0);
  /* temporary values (kept only if some malloc fails) */
  t->array = NULL;
  t->sizearray = 0;
  t->lsizenode = 0;
  t->node = cast(Node *, dummynode);
  setarrayvector(L, t, narray);
  setnodevector(L, t, nhash);
  return t;
}

// 释放table
void luaH_free (lua_State *L, Table *t) {
  if (t->node != dummynode)
    luaM_freearray(L, t->node, sizenode(t), Node);
  luaM_freearray(L, t->array, t->sizearray, TValue);
  luaM_free(L, t);
}

// 在hash中寻找一个可用位置
static Node *getfreepos (Table *t) {
  while (t->lastfree-- > t->node) {
    if (ttisnil(gkey(t->lastfree)))
      return t->lastfree;
  }
  return NULL;  /* could not find a free place */
}



/*
** inserts a new key into a hash table; first, check whether key's main 
** position is free. If not, check whether colliding node is in its main 
** position or not: if it is not, move colliding node to an empty place and 
** put new key in its main position; otherwise (colliding node is in its main 
** position), new key goes to an empty position. 
*/
// 向hash中插入一个新的key
static TValue *newkey (lua_State *L, Table *t, const TValue *key) {
  // 根据key寻找在hash中的位置
  Node *mp = mainposition(t, key);
  // 如果该位置上已经有数据了(!ttisnil(gval(mp)), 或者找不到位置(mp == dummynode)
  if (!ttisnil(gval(mp)) || mp == dummynode) {
    Node *othern;
    // 尝试着获取一个空闲位置
    Node *n = getfreepos(t);  /* get a free place */
    // 找不到空闲位置?
    if (n == NULL) {  /* cannot find a free place? */
      rehash(L, t, key);  /* grow table */
      return luaH_set(L, t, key);  /* re-insert key into grown table */
    }
    lua_assert(n != dummynode);
    othern = mainposition(t, key2tval(mp));
    if (othern != mp) {  /* is colliding node out of its main position? */
      /* yes; move colliding node into free position */
      while (gnext(othern) != mp) othern = gnext(othern);  /* find previous */
      gnext(othern) = n;  /* redo the chain with `n' in place of `mp' */
      *n = *mp;  /* copy colliding node into free pos. (mp->next also goes) */
      gnext(mp) = NULL;  /* now `mp' is free */
      setnilvalue(gval(mp));
    }
    else {  /* colliding node is in its own main position */
      /* new node will go into free position */
      gnext(n) = gnext(mp);  /* chain new position */
      gnext(mp) = n;
      mp = n;
    }
  }
  gkey(mp)->value = key->value; gkey(mp)->tt = key->tt;
  luaC_barriert(L, t, key);
  lua_assert(ttisnil(gval(mp)));
  return gval(mp);
}


/*
** search function for integers
*/
// 以数字为key的查找函数
const TValue *luaH_getnum (Table *t, int key) {
  /* (1 <= key && key <= t->sizearray) */
  // 只要比sizearray小,那么都在数组部分
  if (cast(unsigned int, key-1) < cast(unsigned int, t->sizearray))
    return &t->array[key-1];
  else {
	// 否则在hash部分中
    lua_Number nk = cast_num(key);
    Node *n = hashnum(t, nk);
    do {  /* check whether `key' is somewhere in the chain */
      if (ttisnumber(gkey(n)) && luai_numeq(nvalue(gkey(n)), nk))
        return gval(n);  /* that's it */
      else n = gnext(n);
    } while (n);
    return luaO_nilobject;
  }
}


/*
** search function for strings
*/
// 以字符串为key的查找函数
const TValue *luaH_getstr (Table *t, TString *key) {
  Node *n = hashstr(t, key);
  do {  /* check whether `key' is somewhere in the chain */
    if (ttisstring(gkey(n)) && rawtsvalue(gkey(n)) == key)
      return gval(n);  /* that's it */
    else n = gnext(n);
  } while (n);
  return luaO_nilobject;
}


/*
** main search function
*/
// 表查找的主函数,根据key类型进行区分
const TValue *luaH_get (Table *t, const TValue *key) {
  switch (ttype(key)) {
    case LUA_TNIL: return luaO_nilobject;
    case LUA_TSTRING: return luaH_getstr(t, rawtsvalue(key));
    case LUA_TNUMBER: {
      int k;
      lua_Number n = nvalue(key);
      lua_number2int(k, n);
      if (luai_numeq(cast_num(k), nvalue(key))) /* index is int? */
        return luaH_getnum(t, k);  /* use specialized version */
      /* else go through */
      // 注意前面的不成功,再走近下面的hash部分
    }
    default: {
      Node *n = mainposition(t, key);
      do {  /* check whether `key' is somewhere in the chain */
        if (luaO_rawequalObj(key2tval(n), key))
          return gval(n);  /* that's it */
        else n = gnext(n);
      } while (n);
      return luaO_nilobject;
    }
  }
}

// 除了数字之外的key的set操作
TValue *luaH_set (lua_State *L, Table *t, const TValue *key) {
  const TValue *p = luaH_get(t, key);
  t->flags = 0;
  if (p != luaO_nilobject)
	// 如果存在值, 则返回值
    return cast(TValue *, p);
  else {
    if (ttisnil(key)) luaG_runerror(L, "table index is nil");
    else if (ttisnumber(key) && luai_numisnan(nvalue(key)))
      luaG_runerror(L, "table index is NaN");
    // 否则分配一个以key为key的新值
    return newkey(L, t, key);
  }
}

// 以数字为key的set操作
TValue *luaH_setnum (lua_State *L, Table *t, int key) {
  const TValue *p = luaH_getnum(t, key);
  if (p != luaO_nilobject)
	// 如果原来有数据, 直接返回了
    return cast(TValue *, p);
  else {
	// 否则没有的话, 新创建一个出来
    TValue k;
    setnvalue(&k, cast_num(key));
    return newkey(L, t, &k);
  }
}

// 以字符串为key的set操作
TValue *luaH_setstr (lua_State *L, Table *t, TString *key) {
  const TValue *p = luaH_getstr(t, key);
  if (p != luaO_nilobject)
    return cast(TValue *, p);
  else {
    TValue k;
    setsvalue(L, &k, key);
    return newkey(L, t, &k);
  }
}


static int unbound_search (Table *t, unsigned int j) {
  unsigned int i = j;  /* i is zero or a present index */
  j++;
  /* find `i' and `j' such that i is present and j is not */
  while (!ttisnil(luaH_getnum(t, j))) {
    i = j;
    j *= 2;
    if (j > cast(unsigned int, MAX_INT)) {  /* overflow? */
      /* table was built with bad purposes: resort to linear search */
      i = 1;
      while (!ttisnil(luaH_getnum(t, i))) i++;
      return i - 1;
    }
  }
  /* now do a binary search between them */
  while (j - i > 1) {
    unsigned int m = (i+j)/2;
    if (ttisnil(luaH_getnum(t, m))) j = m;
    else i = m;
  }
  return i;
}


/*
** Try to find a boundary in table `t'. A `boundary' is an integer index
** such that t[i] is non-nil and t[i+1] is nil (and 0 if t[1] is nil).
*/
// 找到第一个"boundary"位置--它本身不为空, 而后一个元素为nil,
int luaH_getn (Table *t) {
  // 首先取数组的大小
  unsigned int j = t->sizearray;
  if (j > 0 && ttisnil(&t->array[j - 1])) {
	// 如果数组的最后一个元素为空
    /* there is a boundary in the array part: (binary) search for it */
    unsigned int i = 0;
    while (j - i > 1) {
      unsigned int m = (i+j)/2;
      if (ttisnil(&t->array[m - 1])) j = m;
      else i = m;
    }
    return i;
  }
  /* else must find a boundary in hash part */
  else if (t->node == dummynode)  /* hash part is empty? */
    return j;  /* that is easy... */
  else return unbound_search(t, j);
}



#if defined(LUA_DEBUG)

Node *luaH_mainposition (const Table *t, const TValue *key) {
  return mainposition(t, key);
}

int luaH_isdummy (Node *n) { return n == dummynode; }

#endif
