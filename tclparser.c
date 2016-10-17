#define JIM_MATH_FUNCTIONS 1
#include "jim.c"

#include <tarantool/lua.h>
#include <tarantool/lualib.h>
#include <tarantool/lauxlib.h>

static const char toknames[][16] = {
    [JIM_TT_NONE] = "NONE",
    [JIM_TT_STR] = "STR",
    [JIM_TT_ESC] = "ESC",
    [JIM_TT_VAR] = "VAR",
    [JIM_TT_DICTSUGAR] = "DICTSUGAR",
    [JIM_TT_CMD] = "CMD",
    [JIM_TT_SEP] = "SEP",
    [JIM_TT_EOL] = "EOL",
    [JIM_TT_EOF] = "EOF",
    [JIM_TT_LINE] = "LINE",
    [JIM_TT_WORD] = "WORD",
    [JIM_TT_SUBEXPR_START] = "SUBEXPR_START",
    [JIM_TT_SUBEXPR_END] = "SUBEXPR_END",
    [JIM_TT_SUBEXPR_COMMA] = "SUBEXPR_COMMA",
    [JIM_TT_EXPR_INT] = "INT",
    [JIM_TT_EXPR_DOUBLE] = "DOUBLE",
    [JIM_TT_EXPR_BOOLEAN] = "BOOLEAN",
    [JIM_TT_EXPRSUGAR] = "EXPRSUGAR",
    [JIM_EXPROP_MUL] = "MUL",
    [JIM_EXPROP_DIV] = "DIV",
    [JIM_EXPROP_MOD] = "MOD",
    [JIM_EXPROP_SUB] = "SUB",
    [JIM_EXPROP_ADD] = "ADD",
    [JIM_EXPROP_LSHIFT] = "LSHIFT",
    [JIM_EXPROP_RSHIFT] = "RSHIFT",
    [JIM_EXPROP_ROTL] = "ROTL",
    [JIM_EXPROP_ROTR] = "ROTR",
    [JIM_EXPROP_LT] = "LT",
    [JIM_EXPROP_GT] = "GT",
    [JIM_EXPROP_LTE] = "LTE",
    [JIM_EXPROP_GTE] = "GTE",
    [JIM_EXPROP_NUMEQ] = "EQ",
    [JIM_EXPROP_NUMNE] = "NE",
    [JIM_EXPROP_BITAND] = "BITAND",
    [JIM_EXPROP_BITXOR] = "BITXOR",
    [JIM_EXPROP_BITOR] = "BITOR",
    [JIM_EXPROP_LOGICAND] = "AND",
    [JIM_EXPROP_LOGICAND_LEFT] = "AND_LEFT",
    [JIM_EXPROP_LOGICAND_RIGHT] = "AND_RIGHT",
    [JIM_EXPROP_LOGICOR] = "OR",
    [JIM_EXPROP_LOGICOR_LEFT] = "OR_LEFT",
    [JIM_EXPROP_LOGICOR_RIGHT] = "OR_RIGHT",
    [JIM_EXPROP_TERNARY] = "TERNARY",
    [JIM_EXPROP_TERNARY_LEFT] = "TERNARY_LEFT",
    [JIM_EXPROP_TERNARY_RIGHT] = "TERNARY_RIGHT",
    [JIM_EXPROP_COLON] = "COLON",
    [JIM_EXPROP_COLON_LEFT] = "COLON_LEFT",
    [JIM_EXPROP_COLON_RIGHT] = "COLON_RIGHT",
    [JIM_EXPROP_POW] = "POW",
    [JIM_EXPROP_STREQ] = "EQ",
    [JIM_EXPROP_STRNE] = "NE",
    [JIM_EXPROP_STRIN] = "IN",
    [JIM_EXPROP_STRNI] = "NI",
    [JIM_EXPROP_NOT] = "NOT",
    [JIM_EXPROP_BITNOT] = "BITNOT",
    [JIM_EXPROP_UNARYMINUS] = "UNARYMINUS",
    [JIM_EXPROP_UNARYPLUS] = "UNARYPLUS",
    [JIM_EXPROP_FUNC_INT] = "FNINT",
    [JIM_EXPROP_FUNC_WIDE] = "FNWIDE",
    [JIM_EXPROP_FUNC_ABS] = "FNABS",
    [JIM_EXPROP_FUNC_DOUBLE] = "FNDOUBLE",
    [JIM_EXPROP_FUNC_ROUND] = "FNROUND",
    [JIM_EXPROP_FUNC_RAND] = "FNRAND",
    [JIM_EXPROP_FUNC_SRAND] = "FNSRAND",
    [JIM_EXPROP_FUNC_SIN] = "FNSIN",
    [JIM_EXPROP_FUNC_COS] = "FNCOS",
    [JIM_EXPROP_FUNC_TAN] = "FNTAN",
    [JIM_EXPROP_FUNC_ASIN] = "FNASIN",
    [JIM_EXPROP_FUNC_ACOS] = "FNACOS",
    [JIM_EXPROP_FUNC_ATAN] = "FNATAN",
    [JIM_EXPROP_FUNC_ATAN2] = "FNATAN2",
    [JIM_EXPROP_FUNC_SINH] = "FNSINH",
    [JIM_EXPROP_FUNC_COSH] = "FNCOSH",
    [JIM_EXPROP_FUNC_TANH] = "FNTANH",
    [JIM_EXPROP_FUNC_CEIL] = "FNCEIL",
    [JIM_EXPROP_FUNC_FLOOR] = "FNFLOOR",
    [JIM_EXPROP_FUNC_EXP] = "FNEXP",
    [JIM_EXPROP_FUNC_LOG] = "FNLOG",
    [JIM_EXPROP_FUNC_LOG10] = "FNLOG10",
    [JIM_EXPROP_FUNC_SQRT] = "FNSQRT",
    [JIM_EXPROP_FUNC_POW] = "FNPOW",
    [JIM_EXPROP_FUNC_HYPOT] = "FNHYPOT",
    [JIM_EXPROP_FUNC_FMOD] = "FNFMOD",
};

static const char *tokname(int tok)
{
    return tok < 0 || (size_t)tok >= (sizeof toknames / sizeof toknames[0]) ?
        NULL : toknames[tok];
}

/**********************************************************************
 * Jim interpretor - 1 per Lua state
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
    lua_pop(L, 1);
    p = lua_newuserdata(L, sizeof interp);
    luaL_getmetatable(L, interp_typename);
    lua_setmetatable(L, -2);
    interp = Jim_CreateInterp();
    if (interp != NULL) {
#ifndef NDEBUG
        fprintf(stderr, "creating Jim interpretor\n");
#endif
        *p = interp;
        lua_pushlightuserdata(L, (void *)interp_instance);
        lua_insert(L, -2);
        lua_settable(L, LUA_REGISTRYINDEX);
        return interp;
    }
    lua_pop(L, 1);
    return NULL;
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
 * exported functions
 */

static void push_jim(lua_State *L, Jim_Obj *obj)
{
    if (obj->typePtr == &intObjType)
        lua_pushinteger(L, obj->internalRep.intValue);
    else if (obj->typePtr == &doubleObjType)
        lua_pushnumber(L, obj->internalRep.doubleValue);
    else
        lua_pushlstring(L, Jim_String(obj), Jim_Length(obj));
}

/*
 * push   true, { tokens }   |   false, "error"
 *
 * if interp == NULL, assumes the script is correct
 */
static void push_script(lua_State *L, ScriptToken *token, int len)
{
    int i, pos = 1;
    lua_createtable(L, 2*len, 0);
    for (i = 0; i < len; i++) {
        int ttype = token[i].type;
        Jim_Obj *tval = token[i].objPtr;
        if (tokname(ttype) == NULL)
            lua_pushinteger(L, ttype);
        else
            lua_pushstring(L, tokname(ttype));
        lua_rawseti(L, -2, pos++);
        if (ttype == JIM_TT_LINE) {
            lua_pushinteger(L, tval->internalRep.scriptLineValue.line);
            lua_rawseti(L, -2, pos++);
            lua_pushinteger(L, tval->internalRep.scriptLineValue.argc);
            lua_rawseti(L, -2, pos++);
        } else {
            push_jim(L, tval);
            lua_rawseti(L, -2, pos++);
        }
    }
}

/* tokenize script */
static int tcl_parse(lua_State *L)
{
    Jim_Interp *interp = interp_get(L);
    Jim_Obj *script_str; ScriptObj *script;
    size_t len;
    const char *s = lua_tolstring(L, 1, &len);
    if (s == NULL) return 0;
    script_str = Jim_NewStringObj(interp, s, len);
    /*
     * Apparently, objects are born with refcount == 0.
     * DecRef() frees an object if --refcount <= 0.
     * Typically, IncRef() followed by DecRef() is a noop, unless it was
     * the last ref!
     */
    Jim_IncrRefCount(script_str);
    script = JimGetScript(interp, script_str);
    if (JimScriptValid(interp, script) == 0) {
        lua_pushboolean(L, 0);
        push_jim(L, Jim_GetResult(interp));
    } else {
        lua_pushboolean(L, 1);
        push_script(L, script->token, script->len);
    }
    Jim_DecrRefCount(interp, script_str);
    return 2;
}

/* tokenize [subst] arguments
 *   ( -nobackslashes | -nocommands | -novariables )* arg
 */
static int tcl_parse_subst(lua_State *L)
{
    Jim_Interp *interp = interp_get(L);
    int i, n = lua_gettop(L);
    int flags = JIM_SUBST_FLAG;
    Jim_Obj *subst_str; ScriptObj *subst;
    size_t len;
    const char *s;
    for (i = 1; i < n; i++) {
        const char *flag = lua_tostring(L, i);
        if (flag == NULL) return 0;
        if (flag[0] != '-' || flag[1] != 'n' || flag[2] != 'o') {
errflag:
            lua_pushboolean(L, 0);
            lua_pushfstring(L, "unknown flag %s", flag);
            return 2;
        }
        switch (flag[3]) {
        case 'b':
            if (strcmp(flag+3, "backslashes")) goto errflag;
            flags |= JIM_SUBST_NOESC;
            break;
        case 'c':
            if (strcmp(flag+3, "commands")) goto errflag;
            flags |= JIM_SUBST_NOCMD;
            break;
        case 'v':
            if (strcmp(flag+3, "variables")) goto errflag;
            flags |= JIM_SUBST_NOVAR;
            break;
        }
    }
    /* for consistency with other parsers */
    s = lua_tolstring(L, n == 0 ? 1 : n, &len);
    if (s == NULL) return 0;
    subst_str = Jim_NewStringObj(interp, s, len);
    Jim_IncrRefCount(subst_str);
    subst = Jim_GetSubst(interp, subst_str, flags);
    if (subst == NULL) {
        lua_pushboolean(L, 0);
        push_jim(L, Jim_GetResult(interp));
    } else {
        lua_pushboolean(L, 1);
        push_script(L, subst->token, subst->len);
    }
    Jim_DecrRefCount(interp, subst_str);
    return 2;
}

/* tokenize expression */
static int tcl_parse_expr(lua_State *L)
{
    Jim_Interp *interp = interp_get(L);
    Jim_Obj *expr_str; ExprByteCode *expr;
    size_t len;
    const char *s = lua_tolstring(L, 1, &len);
    if (s == NULL) return 0;
    expr_str = Jim_NewStringObj(interp, s, len);
    Jim_IncrRefCount(expr_str);
    expr = JimGetExpression(interp, expr_str);
    if (expr == NULL) {
        lua_pushboolean(L, 0);
        push_jim(L, Jim_GetResult(interp));
    } else {
        lua_pushboolean(L, 1);
        push_script(L, expr->token, expr->len);
    }
    Jim_DecrRefCount(interp, expr_str);
    return 2;
}

/* tokenize list */
static int tcl_parse_list(lua_State *L)
{
    Jim_Interp *interp = interp_get(L);
    Jim_Obj *list;
    size_t i, n;
    size_t len;
    const char *s = lua_tolstring(L, 1, &len);
    if (s == NULL) return 0;
    list = Jim_NewStringObj(interp, s, len);
    Jim_IncrRefCount(list);
    SetListFromAny(interp, list);
    n = list->internalRep.listValue.len;
    lua_pushboolean(L, 1);
    lua_createtable(L, n, 0);
    for (i = 0; i < n; i++) {
        Jim_Obj *item = list->internalRep.listValue.ele[i];
        lua_pushlstring(L, Jim_String(item), Jim_Length(item));
        lua_rawseti(L, -2, i+1);
    }
    Jim_DecrRefCount(interp, list);
    return 2;
}

static const struct luaL_reg tclmeta[] = {
    { "parse", tcl_parse },
    { "parsesubst", tcl_parse_subst },
    { "parseexpr", tcl_parse_expr },
    { "parselist", tcl_parse_list },
    { NULL, NULL }
};

/**********************************************************************
 * init
 */

int luaopen_tclparser(lua_State *L)
{
    luaL_newmetatable(L, interp_typename);
    luaL_register(L, NULL, interp_meta);
    lua_pop(L, 1);

    lua_newtable(L);
    luaL_register(L, NULL, tclmeta);
    return 1;
}
