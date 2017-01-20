#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <limits.h>

#define BITS_PER_WORD (CHAR_BIT * sizeof(int))
#define I_WORD(i)     ((unsigned int)(i))/BITS_PER_WORD
#define I_BIT(i)      (1 << ((unsigned int)(i)%BITS_PER_WORD))

typedef struct NumArray {
  int size;
  unsigned int values[1];
} NumArray;

int newArray(lua_State* L)
{
  //1. 检查第一个参数是否为整型。以及该参数的值是否大于等于1.
  int n = luaL_checkint(L,1);
  int i;
  luaL_argcheck(L, n >= 1, 1, "invalid size.");
  size_t nbytes = sizeof(NumArray) + I_WORD(n - 1) * sizeof(int);
  //2. 参数表示Lua为userdata分配的字节数。同时将分配后的userdata对象压入栈中。
  NumArray* a = (NumArray*)lua_newuserdata(L,nbytes);
  a->size = n;
  for (i = 0; i < I_WORD(n - 1); ++i)
    a->values[i] = 0;
  //获取注册表变量myarray，该key的值为metatable。
  luaL_getmetatable(L,"myarray");
  //将userdata的元表设置为和myarray关联的table。同时将栈顶元素弹出。
  lua_setmetatable(L,-2);
  return 1;
}

int setArray(lua_State* L)
{
  //1. Lua传给该函数的第一个参数必须是userdata，该对象的元表也必须是注册表中和myarray关联的table。
  //否则该函数报错并终止程序。
  NumArray* a = (NumArray*)luaL_checkudata(L,1,"myarray");
  int index = luaL_checkint(L,2) - 1;
  //2. 由于任何类型的数据都可以成为布尔值，因此这里使用any只是为了确保有3个参数。
  luaL_checkany(L,3);
  luaL_argcheck(L,a != NULL,1,"'array' expected.");
  luaL_argcheck(L,0 <= index && index < a->size,2,"index out of range.");
  if (lua_toboolean(L,3))
    a->values[I_WORD(index)] |= I_BIT(index);
  else
    a->values[I_WORD(index)] &= ~I_BIT(index);
  return 0;
}

int getArray(lua_State* L)
{
  NumArray* a = (NumArray*)luaL_checkudata(L,1,"myarray");
  int index = luaL_checkint(L,2) - 1;
  luaL_argcheck(L, a != NULL, 1, "'array' expected.");
  luaL_argcheck(L, 0 <= index && index < a->size,2,"index out of range");
  lua_pushboolean(L,a->values[I_WORD(index)] & I_BIT(index));
  return 1;
}

int getSize(lua_State* L)
{
  NumArray* a = (NumArray*)luaL_checkudata(L,1,"myarray");
  luaL_argcheck(L,a != NULL,1,"'array' expected.");
  lua_pushinteger(L,a->size);
  return 1;
}

int array2string(lua_State* L)
{
  NumArray* a = (NumArray*)luaL_checkudata(L,1,"myarray");
  lua_pushfstring(L,"array(%d)",a->size);
  return 1;
}

static luaL_Reg arraylib_f [] = { 
  {"new", newArray},
  {NULL, NULL} 
}; 

static luaL_Reg arraylib_m [] = {
  {"set", setArray},
  {"get", getArray},
  {"size", getSize},
  {"__tostring", array2string}, //print(a)时Lua会调用该元方法。
  {NULL, NULL} 
};

int luaopen_testuserdata(lua_State* L)
{
  //1. 创建元表，并将该元表指定给newArray函数新创建的userdata。在Lua中userdata也是以table的身份表现的。
  //这样在调用对象函数时，可以通过验证其metatable的名称来确定参数userdata是否合法。
  luaL_newmetatable(L,"myarray");
  lua_pushvalue(L,-1);
  //2. 为了实现面对对象的调用方式，需要将元表的__index字段指向自身，同时再将arraylib_m数组中的函数注册到
  //元表中，之后基于这些注册函数的调用就可以以面向对象的形式调用了。
  //lua_setfield在执行后会将栈顶的table弹出。
  lua_setfield(L,-2,"__index");
  //将这些成员函数注册给元表，以保证Lua在寻找方法时可以定位。NULL参数表示将用栈顶的table代替第二个参数。
  luaL_register(L,NULL,arraylib_m);
  //这里只注册的工厂方法。
  luaL_register(L,"testuserdata",arraylib_f);
  return 1;
}

int main(){
  char *file = NULL;
  file = "my.lua";

  lua_State *L = lua_open();
  luaL_openlibs(L);
  luaopen_testuserdata(L);
  if (luaL_dofile(L, file)) {
    printf("error: %s\n", lua_tostring(L, -1));
  }

  return 0;
}

