#include "jim.c"

#include <tarantool/lua.h>
#include <tarantool/lualib.h>
#include <tarantool/lauxlib.h>


/**********************************************************************
 * JIM interpretor instance - 1 per Lua state
 */

static const char interp_typename[] = "tclmeta.interp";
static const char interp_instance[1];

struct Jim_Interp *interp_get(lua_State *L)
{
    void **p;
    Jim_Interp *interp;
    lua_pushlightuserdata(L, (void *)interp_instance);
    lua_gettable(L, LUA_REGISTRYINDEX);
    if (!lua_isnil(L, -1)) {
        interp = *(void **)luaL_checkudata(L, -1, interp_typename);
        lua_pop(L, 1);
        return interp;
    }
    p = lua_newuserdata(L, sizeof interp);
    luaL_getmetatable(L, interp_typename);
    lua_setmetatable(L, -2);
    interp = Jim_CreateInterp();
    if (interp != NULL) {
        *p = interp;
        lua_pushlightuserdata(L, (void *)interp_instance);
        lua_settable(L, LUA_REGISTRYINDEX);
    }
    return interp;
}

static int interp_gc(lua_State *L)
{
    Jim_Interp *interp = *(void **)luaL_checkudata(L, 1, interp_typename);
    if (interp != NULL)
        Jim_FreeInterp(interp);
    return 0;
}

static const struct luaL_reg interp_meta[] = {
    { "__gc", interp_gc },
    { NULL, NULL }
};


/**********************************************************************
 * JIM object
 */

static const char obj_typename[] = "tclmeta.obj";

static void obj_new(lua_State *L, struct Jim_Obj *obj)
{
    void **p;
    p = (void **)lua_newuserdata(L, sizeof obj);
    luaL_getmetatable(L, obj_typename);
    lua_setmetatable(L, -2);
    *p = obj;
    Jim_IncrRefCount(obj);
}

/* reports error if item at index is of incorrect type */
static Jim_Obj *obj_check(lua_State *L, int index)
{
    return *(void **)luaL_checkudata(L, index, obj_typename);
}

/* returns NULL if item at index is of incorrect type */
static Jim_Obj *obj_get(lua_State *L, int index)
{
    int is_obj;
    if (lua_type(L, index) != LUA_TUSERDATA)
        return NULL;
    lua_getmetatable(L, index);
    luaL_getmetatable(L, obj_typename);
    is_obj = lua_rawequal(L, -1, -2);
    lua_pop(L, 2);
    return is_obj ? *(void **)lua_topointer(L, index): NULL;
}

static int obj_gc(lua_State *L)
{
    Jim_Obj *obj = obj_check(L, 1);
    if (obj != NULL)
        Jim_DecrRefCount(interp_get(L), obj);
    return 0;
}

static int obj_tostring(lua_State *L)
{
    const char *s;
    int len;
    s = Jim_GetString(obj_check(L, 1), &len);
    if (s == NULL)
        return 0;
    lua_pushlstring(L, s, len);
    return 1;
}

static const struct luaL_reg obj_meta[] = {
    { "__gc", obj_gc },
    { "__tostring", obj_tostring },
    { NULL, NULL }
};


/**********************************************************************
 * exported functions
 */

/* get i-th core cmd */
static int tcl_corecmd(lua_State *L)
{
    const char *name;
    int i = 0;
    lua_newtable(L);
    while ((name = Jim_CoreCommandsTable[i].name) != NULL) {
        i++;
        lua_pushstring(L, name);
        lua_rawseti(L, -2, i);
    }
    return 1;
}

static const struct luaL_reg tclmeta[] = {
    { "tcl_corecmd", tcl_corecmd },
    { NULL, NULL }
};


/**********************************************************************
 * init
 */

int luaopen_tclmeta(lua_State *L)
{
    luaL_newmetatable(L, interp_typename);
    luaL_register(L, NULL, interp_meta);
    lua_pop(L, 1);

    luaL_newmetatable(L, obj_typename);
    luaL_register(L, NULL, obj_meta);
    lua_pop(L, 1);

    lua_newtable(L);
    luaL_register(L, NULL, tclmeta);
    return 1;
}
