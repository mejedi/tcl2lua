local max, min = math.max, math.min
local format, match, sub, gsub = string.format, string.match, string.sub, string.gsub
local find, rep, gmatch, lower = string.find, string.rep, string.gmatch, string.lower
local insert, remove, concat = table.insert, table.remove, table.concat
local tclparser = require('tclparser')
local json = require('json')

local file = ... and ... or 'select3.test'
local source = io.open(file):read('*a')

-- node_type(node)        - get
-- node_type(type, node)  - set
local node_type
-- node_line(node)        - get
-- node_line(line, node)  - set
local node_line
do
    local node_types = setmetatable( {}, {__mode='k'} )
    local node_lines = setmetatable( {}, {__mode='k'} )
    node_type = function(_1, _2)
        if _2 then node_types[_2] = _1; return _2 end
        return node_types[_1]
    end
    node_line = function(_1, _2)
        if _2 then node_lines[_2] = _1; return _2 end
        return node_lines[_1]
    end
end

-----------------------------------------------------------------------

local tcl_parse, tcl_ast
local tokfunc = {}

function tokfunc.LINE(line, tokens, tokpos)
    line = tokens[tokpos+1]
    local argv, tokpos = tcl_ast(line, tokens, tokpos+3, tokens[tokpos+2])
    node_type('cmd', argv)
    if node_type(argv[1]) == 'comment' then
        assert(#argv == 1)
        return argv[1], tokpos
    end
    return argv, tokpos
end

function tokfunc.STR(line, tokens, tokpos)
    return tokens[tokpos+1], tokpos + 2
end

tokfunc.ESC = tokfunc.STR

function tokfunc.NONE(line, tokens, tokpos)
    return node_line(line, node_type('comment', { tokens[tokpos+1] })), tokpos + 2
end

function tokfunc.VAR(line, tokens, tokpos)
    return node_line(line, node_type('var', { tokens[tokpos+1] })), tokpos + 2
end
local nil_ref = node_type('var', {'nil'})

function tokfunc.WORD(line, tokens, tokpos)
    local n = tokens[tokpos+1]
    local subst, tokpos = tcl_ast(line, tokens, tokpos+2, max(n, 1))
    local copy, pos = {}, 0
    -- combine adjacent string nodes
    for i,node in ipairs(subst) do
        if type(node) == 'string' and type(copy[pos]) == 'string' then
            copy[pos] = copy[pos]..node
        else
            pos = pos + 1
            copy[pos] = node
        end
    end
    return node_type(n > 0 and 'subst' or 'subst*', copy), tokpos
end

local function do_subst(...)
    local ok, tokens = tclparser.parsesubst(...)
    if ok then
        local ast = node_type('subst', tcl_ast(nil, tokens, nil, #tokens))
        -- get rid of redundant subst-s
        if #ast == 1 and type(ast[1]) == 'string' then
            return ast[1]
        end
        return ast
    end
end

function tokfunc.CMD(line, tokens, tokpos)
    local ast = tcl_parse(tokens[tokpos+1], line)
    if #ast == 1 then
        local cmd = ast[1]
        if cmd[1] == 'subst' then
            -- handle subst here - emit the same AST as created by WORD
            ast = do_subst(select(2, unpack(cmd))) or ast
        end
    end
    return ast, tokpos + 2
end

function tokfunc.DICTSUGAR(line, tokens, tokpos)
    return node_line(line, node_type('dsugar', {tokens[tokpos+1]})), tokpos + 2
end

function tokfunc.EXPRSUGAR(line, tokens, tokpos)
    return node_line(line, node_type('esugar', {tokens[tokpos+1]})), tokpos + 2
end

-- construct ast starting from the given pos in the token stream
-- returns ast + next pos
tcl_ast = function(line, tokens, tokpos, limit)
    limit = limit or 10000000
    tokpos = tokpos or 1
    local res, i = node_line(line, {}), 1
    while tokens[tokpos] and i <= limit do
        local ttype, item = tokens[tokpos]
        local handler = tokfunc[ttype]
        if not handler then
            error(format('%s:%s: unhandled token [%q %q]',
                         file, line, ttype, tokens[tokpos+1]),
                  0)
        end
        item, tokpos = handler(line, tokens, tokpos)
        res[i] = item
        i = i + 1
    end
    return res, tokpos
end

-- parse script and construct ast
tcl_parse = function(src, firstline)
    local ok, res = tclparser.parse(src)
    if not ok then return
        node_type('unparsed', node_line(firstline or 1, {src}))
    end
    if firstline then
        for i, tok in ipairs(res) do
            if tok == 'LINE' then
                res[i+1] = firstline + res[i+1]
            end
        end
    end
    return node_type('cmds', tcl_ast(firstline, res, nil, nil))
end

-----------------------------------------------------------------------

local function indent(result)
    local indent = result.indent or ''
    result.indent = indent .. '  '
    return indent
end

local function dedent(result)
    local indent = result.indent
    if indent then result.indent = sub(indent, 3) end
end

local function insert_indent(result, v)
    local base = #result
    result[base+1] = result.indent or ''
    result[base+2] = v
end

-----------------------------------------------------------------------

local xstats = {}
local function xbump(k)
    xstats[k] = (xstats[k] or 0) + 1
end

local function safestr(str)
    local a, b
    if not match(str, '["\\\n]') then
        return format('%q', str)
    elseif not find(str, ']') then
        a, b = '[[', ']]'
    elseif not find(str, ']=') then
        a, b = '[=[', ']=]'
    elseif not find(str, ']==') then
        a, b = '[==[', ']==]'
    else
        return format('%q', str)
    end
    return a..str..b
end

local function insert_x(result, marker, ast)
    xbump(marker)
    insert(result, format("X(%d, %q, ", node_line(ast) or 0, marker))
    insert(result, safestr(json.encode(ast)))
    insert(result, ')')
end

local cmdfunc = {}
local cmdfuzz = {}

local tolua
tolua = function(result, ast)
    local nt = node_type(ast)
    if nt == 'comment' then
        local comment = ast[1]
        if match(comment, '^[#*]*$') then comment = rep('-', #comment) end
        insert(result, '--'); insert(result, comment)
    elseif nt == 'cmds' then
        for _, node in ipairs(ast) do
            insert_indent(result)
            tolua(result, node)
            insert(result, '\n')
        end
    elseif nt == 'cmd' or nt == 'rcmd' then
        if nt == 'rcmd' then insert(result,'return ') end
        local name = ast[1]
        if type(name) ~= 'string' then return end
        local handler = cmdfunc[name]
        local pattern = nil
        while not (handler and handler(result, ast)) do
            pattern, handler = next(cmdfuzz, pattern)
            if not pattern then
                xbump(ast[1])
                insert_x(result, 'X!cmd', ast)
                break
            end
            if not match(name, pattern) then handler = nil end
        end
        xbump('[total cmds]')
    else
        insert_x(result, 'X!token!'..nt, ast)
    end
end

-----------------------------------------------------------------------

local function insert_literal(result, _1, _2)
    local ast = _2 or _1
    if type(ast) == 'string' then
        insert(result, tonumber(ast) and ast or
                       (match(ast, '[\n"\\]') and safestr(ast)) or
                       format('%q', ast))
    else
        insert_x(result, _2 and _1 or 'X!literal', ast)
    end
end

-----------------------------------------------------------------------

local function checkvar(var)
    if type(var) ~= 'string' then return end
    var = gsub(var, '^::', '')
    return match(var, '^[_%a][_%w]*$') and var
end

local function translate_subst(ast)
    local template, params = {}, {}
    for _, node in ipairs(ast) do
        if type(node) == 'string' then
            insert(template, (gsub(node, '%%', '%%%%%')))
        else
            insert(template, '%s')
            insert(params, node)
        end
    end
    return concat(template, ''), params
end

local insert_expr
insert_expr = function(result, _1, _2)
    local ast = _2 or _1
    local nt = node_type(ast)
    if nt == 'cmds' and #ast == 1 then
        return tolua(result, ast[1])
    elseif nt == 'var' then
        local var = checkvar(ast[1])
        if var then
            insert(result, var)
        else
            insert_x(result, 'X!name', ast)
        end
    elseif nt == 'subst' then
        local template, params = translate_subst(ast)
        if #params == 1 and not find(template, '["\n]') then
            for i, node in ipairs(ast) do
                if i ~= 1 then insert(result, '..') end
                if type(node) == 'string' then
                    insert(result, safestr(node))
                else
                    insert_expr(result, node)
                end
            end
        else
            insert(result, 'string.format(')
            insert(result, safestr(template))
            for _, expr in ipairs(params) do
                insert(result, ', '); insert_expr(result, expr)
            end
            insert(result, ')')
        end
    else
        insert_literal(result, _2 and _1 or 'X!expr', ast)
    end
end

-----------------------------------------------------------------------

local function reindent_sql(result, sql, force_multiline)
    local lines = {}
    local pos, len = 1, #sql
    while pos <= len do
        local nl = find(sql, '\n', pos) or len
        insert(lines, sub(sql, pos, nl))
        pos = nl + 1
    end
    if force_multiline and #lines == 1 then
        local line = lines[1]
        if not find(line, '\n') then line = line ..'\n' end
        lines[1], lines[2] = '\n', line
    end
    if #lines > 1 and not find(lines[1], '[^%s\n]') then
        local pindent = indent(result)
        local indent = result.indent
        dedent(result)
        local origin
        for i = 1,#lines do
            local line = lines[i]
            local corigin = find(line, '[^%s\n]')
            if corigin then
                origin = origin or corigin
                lines[i] = indent .. sub(line, min(origin, corigin))
            else
                -- line entirely made of whitespace
                lines[i] = sub(line, find(line, '\n') or 10000000)
            end
        end
        local last = lines[#lines]
        if last == '' or find(last, '\n') then insert(lines, pindent) end
        return concat(lines, '')
    end
    return sql
end

local function insert_sql(result, sql, force_multiline)
    if type(sql) == 'string' then
        insert(result, safestr(reindent_sql(result, sql, force_multiline)))
    elseif node_type(sql) == 'subst' then
        local template, params = translate_subst(sql)
        if #params == 1 and not find(template, '\n') then
            insert_expr(result, 'X!sql', sql)
        else
            insert(result, 'string.format(')
            insert(result, safestr(reindent_sql(result, template, force_multiline)))
            for _, expr in ipairs(params) do
                insert(result, ', '); insert_expr(result, expr)
            end
            insert(result, ')')
        end
    else
        insert_expr(result, 'X!sql', sql)
    end
end

-----------------------------------------------------------------------

local function ignorecmd(result, cmd)
    insert(result, '-- ')
    insert(result, json.encode(cmd))
    return true
end
cmdfunc.unset = ignorecmd
cmdfunc.source = ignorecmd

cmdfunc['return'] = function(result, cmd)
    if #cmd <= 2 then
        insert(result, "return ")
        if #cmd == 2 then insert_expr(result, cmd[2]) end
        return true
    end
end

local function ifhlp(result, cmd)
    local i = cmd[3] == 'then' and 4 or 3
    while cmd[i] do
        indent(result)
        tolua(result, tcl_parse(cmd[i], node_line(cmd)))
        dedent(result)
        if cmd[i+1] == 'else' then
            insert_indent(result, 'else\n')
            i = i + 2
        elseif cmd[i+1] == 'elseif' then
            insert_indent(result, 'elseif ')
            insert_x(result, 'X!expr01', cmd[i+2])
            insert(result, ' then\n')
            i = i + 3
        else
            break
        end
    end
    insert_indent(result, 'end')
    return true
end

cmdfunc['if'] = function(result, cmd)
    insert(result, 'if ')
    insert_x(result, 'X!expr01', cmd[2])
    insert(result, ' then\n')
    return ifhlp(result, cmd)
end

cmdfunc['while'] = function(result, cmd)
    if #cmd == 3 and type(cmd[3]) == 'string' then
        insert(result, 'while ');
        insert_x(result, 'X!expr01', cmd[1])
        insert(result, ' do\n')
        indent(result)
        tolua(result, tcl_parse(cmd[3], node_line(cmd)))
        dedent(result)
        insert_indent(result, 'end')
        return true
    end
end

cmdfunc['switch'] = function(result, cmd)
    local i, n = 2, #cmd
    local branches, start
    -- skip options which we ignore for now
    while type(cmd[i]) == 'string' and match(cmd[i], '^-') do
        i = i + 1
    end
    -- maybe { case1 {...} ,,, caseN {...} }?
    if i == n-1 and type(cmd[n]) == 'string' then
        branches = select(2,tclparser.parselist(cmd[n]))
        start = 1
    else
        branches, start = cmd, i + 1
    end
    for j = start,#branches,2 do
        if j == start then
            insert(result, 'if ')
        else
            insert_indent(result, 'elseif ')
        end
        insert_x(result, 'X!case', {cmd[i], branches[j]})
        insert(result, ' then\n')
        indent(result)
        tolua(result, tcl_parse(branches[j+1]))
        dedent(result)
    end
    insert_indent(result, 'end')
    return true
end

cmdfunc['break'] = function(result, cmd)
    insert(result, 'break')
    return true
end

cmdfunc['for'] = function(result, cmd)
    if #cmd == 5 and type(cmd[5]) == 'string' then
        insert(result, 'for _ in ');
        insert_x(result, 'X!for', {cmd[2], cmd[3], cmd[4]})
        insert(result, ' do\n')
        indent(result)
        tolua(result, tcl_parse(cmd[5], node_line(cmd)))
        dedent(result)
        insert_indent(result, 'end')
        return true
    end
end

cmdfunc['foreach'] = function(result, cmd)
    if #cmd == 4 and type(cmd[4]) == 'string' then
        insert(result, 'for _ in ');
        insert_x(result, 'X!foreach', {cmd[2], cmd[3]})
        insert(result, ' do\n')
        indent(result)
        tolua(result, tcl_parse(cmd[4], node_line(cmd)))
        dedent(result)
        insert_indent(result, 'end')
        return true
    end
end

function cmdfunc.set(result, cmd)
    local name = cmd[2]
    -- standard preamble sets $testdir
    if name == 'testdir' then
        return ignorecmd(result, cmd)
    end 
    local var = checkvar(name)
    if #cmd <= 3 and var then
        local val = cmd[3] or nil_ref
        if node_type(cmd) == 'rcmd' or not var then
            insert(result, format('set('))
            insert_expr(result, var)
            insert(result, ', ')
            insert_expr(result, val)
            insert(result, ')')
        else
            insert(result, var .. ' = ')
            insert_expr(result, val)
        end
        return true
    end
end

-----------------------------------------------------------------------

local exprtok
do
    local function literal(def, ttype, tval, stack)
        insert(stack, tval)    
    end

    local function literalbool(def, ttype, tval, stack)
        insert(stack, tval == 1 or tval == 'on' or tval == 'true' or false)
    end

    local function unop(def, ttype, tval, stack)
        insert(stack, {def.op, remove(stack) })
    end

    local function binop(def, ttype, tval, stack)
        local rhs = remove(stack)
        local lhs = remove(stack)
        insert(stack, {def.op, lhs, rhs})
    end

    local function ternop(def, ttype, tval, stack)
        local c = remove(stack)
        local b = remove(stack)
        local a = remove(stack)
        insert(stack, {def.op, a, b, c})
    end

    local function ignore() end
    local function consume1(def, ttype, tval, stack) remove(stack) end

    exprtok = {
        ESCX    = { fn = literal },
        STR     = { fn = literal },
        INT     = { fn = literal },
        DOUBLE  = { fn = literal },
        BOOLEAN = { fn = literalbool },
        MUL     = { op = '*',  fn = binop },
        DIV     = { op = '/',  fn = binop },
        MOD     = { op = '%',  fn = binop },
        SUB     = { op = '-',  fn = binop },
        ADD     = { op = '+',  fn = binop },
        LSHIFT  = { op = '<<', fn = binop },
        RSHIFT  = { op = '>>', fn = binop },
        ROTL    = { op = '<<<', fn = binop },
        ROTR    = { op = '>>>', fn = binop },
        LT      = { op = '<',  fn = binop },
        GT      = { op = '>',  fn = binop },
        LTE     = { op = '<=', fn = binop },
        GTE     = { op = '>=', fn = binop },
        EQ      = { op = '==', fn = binop },
        NE      = { op = '!=', fn = binop },
        BITAND  = { op = '&',  fn = binop },
        BITXOR  = { op = '^',  fn = binop },
        BITOR   = { op = '|',  fn = binop },
        -- AND
        AND_LEFT      = { fn = consume1 },
        AND_RIGHT     = { op = '&&', fn = binop },
        -- OR
        OR_LEFT       = { fn = consume1 },
        OR_RIGHT      = { op = '||', fn = binop },
        -- TERNARY
        TERNARY_LEFT  = { fn = consume1 },
        TERNARY_RIGHT = { fn = consume1 },
        -- COLON
        COLON_LEFT    = { fn = ignore },
        COLON_RIGHT   = { op = '?:', fn = ternop },
        POW           = { op = '**', fn = binop },
        EQ            = { op = 'eq', fn = binop },
        NE            = { op = 'ne', fn = binop },
        IN            = { op = 'ni', fn = binop },
        NI            = { op = 'ni', fn = binop },
        NOT           = { op = '!',  fn = unop },
        BITNOT        = { op = '~',  fn = unop },
        UNARYMINUS    = { op = '-',  fn = unop },
        UNARYPLUS     = { op = '+',  fn = unop }
    }
end

local function tcl_expr_ast(tokens)
    local stack = {}
    for i = 1,#tokens,2 do
        local tok = tokens[i]
        local def = exprtok[tok]
        if not def then
            if match(tok, '^FN') then
                insert(stack, { lower(sub(tok, 3)), remove(stack) })
            else
                return nil, tok
            end
        else
            def.fn(def, tok, tokens[i+1], stack)
        end
    end
    return remove(stack)
end

function cmdfunc.expr(result, cmd)
    local merged = {}
    for i = 2,#cmd do
        local node = do_subst('-nocommands', cmd[i]) or cmd[i]
        if node_type(node) == 'subst' then
            for _, child in ipairs(node) do insert(merged, child) end
        else
            insert(merged, node)
        end
        insert(merged, ' ')
    end
    local template, params = translate_subst(merged)
    local paramids = {}
    for i=1,#params do paramids[i] = 0 end
    local e0 = format(template, unpack(paramids))
    local ok, tokens = tclparser.parseexpr(e0)
    if not ok then return end
    -- collect all ints in expression
    local ints = {}
    for i = 1,#tokens,2 do
        if tokens[i] == 'INT' then ints[tokens[i+1]] = true end
    end
    -- assign unique integer literal for each param for later matching
    local param_by_id = {}
    for i,param in ipairs(params) do
        local id = 1000
        while ints[id] do id = id + 1 end
        paramids[i] = id
        ints[id] = true
        param_by_id[id] = param
    end
    local e1 = format(template, unpack(paramids))
    local ok, tokens = tclparser.parseexpr(e1)
    assert(ok)
    for i = 1,#tokens,2 do
        -- replace placeholders with respective params; expand CMD
        local ttype, tval = tokens[i], tokens[i+1]
        if ttype == 'INT' then
            local param = param_by_id[tval]
            if param then
                tokens[i], tokens[i+1] = 'ESCX', param
                param_by_id[tval] = nil
            end
        elseif ttype == 'CMD' then
            --
        end
    end
    -- every param consumed?
    if next(param_by_id) then return end
    local ast = tcl_expr_ast(tokens)
    if ast then
        insert_x(result, "X!expr", ast)
        return true
    end
end

-----------------------------------------------------------------------

function cmdfunc.string(result, cmd)
    local subcmd = cmd[2]
    if subcmd == 'repeat' and #cmd == 4 then
        insert(result, 'string.rep(')
        insert_expr(result, cmd[3])
        insert(result, ', ')
        insert_expr(result, cmd[4])
        insert(result, ')')
        return true
    end
end

function cmdfunc.catch(result, cmd)
    if cmd[2] == 'unset' then
        return ignorecmd(result, cmd)
    end
end

local function insert_list(result, list, start)
    start = start or 1
    for i,item in ipairs(list) do
        if i > start then insert(result, ', ') end
        if i >= start then insert_expr(result, item) end
    end
end

function cmdfunc.list(result, cmd)
    insert(result, '{ ')
    insert_list(result, cmd, 2)
    insert(result, ' }')
    return true
end

function cmdfunc.lappend(result, cmd)
    local var = checkvar(cmd[2])
    if var and cmd[3] then
        insert(result, 'table.append(')
        insert(result, var)
        insert(result, ', ')
        insert_list(result, cmd, 3)
        insert(result, ')')
        return true
    end
end

local function usercmd(result, cmd)
    insert(result, cmd[1])
    insert(result, '(')
    insert_list(result, cmd, 2)
    insert(result, ')')
    return true
end

function cmdfunc.proc(result, cmd)
    if #cmd == 4 and type(cmd[4]) == 'string' then
        local line, name, params = node_line(cmd), cmd[2], cmd[3]
        
        local pos = #result
        result[pos+1] = 'function '
        result[pos+2] = '<name placeholder>'
        result[pos+3] = '('
        result[pos+4] = '' -- params placeholder
        result[pos+5] = ')\n'

        indent(result)
        local usename = checkvar(name)
        if usename then
            result[pos+2] = usename
            cmdfunc[name] = cmdfunc[name] or usercmd -- no overide
        else
            result[pos+2] = '_' .. line
            insert_x(result, "X!procname", name)
        end

        local ok, param_list = tclparser.parselist(params)
        local param_names, param_defaults = {}, {}
        if ok then
            for i, param in ipairs(param_list) do
                local ok, name_plus = tclparser.parselist(param)
                if ok and #name_plus <= 2 then
                    param_names[i] = checkvar(name_plus[1])
                    param_defaults[i] = name_plus[2]
                end
                if not param_names[i] then param_names = nil; break end
            end
        end
        if param_names then
            result[pos+4] = concat(param_names, ', ')
            for i,param in ipairs(param_names) do
                local default = param_defaults[i]
                if default then
                    insert_indent(result, param .. ' = ' .. param  .. ' or ')
                    insert_expr(result, default)
                    insert(result, '\n')
                end
            end
        else
            insert_x(result, "X!procparams", params)
            insert(result, '\n')
        end

        tolua(result, tcl_parse(cmd[4], line))
        dedent(result)
        insert_indent(result, 'end\n')
        return true
    end
end

-----------------------------------------------------------------------

function cmdfunc.ifcapable(result, cmd)
    local n = #cmd
    if (n ~= 3 and n ~= 5) or type(cmd[3]) ~= 'string' then return end
    if n == 5 and (cmd[4] ~= 'else' or type(cmd[5]) ~= 'string') then return end

    insert(result, 'if ')
    insert_x(result, 'X!capable', cmd[2])
    insert(result, ' then\n');
    ifhlp(result, cmd)
    insert(result, '\n')
    return true
end

-----------------------------------------------------------------------

local function execsql(result, cmd)
    local n = #cmd
    if n == 2 or n == 3 then
        local sql = cmd[2]
        sql = do_subst('-nocommands', sql) or sql
        local pos = #result
        result[pos+1] = cmd[1]
        result[pos+2] = '('
        insert_sql(result, sql)
        if n == 2 and #result == pos + 3 then
            result[pos+2] = ' ' -- assume paren was unnecessary
        else
            if n == 3 then
                insert(result, ", "); insert_expr(result, cmd[3])
            end
            insert(result, ')')
        end
        return true
    end
end
cmdfunc.execsql = execsql
cmdfunc.execsql2 = execsql
cmdfunc.catchsql = execsql
cmdfunc.catchsql2 = execsql

local function db(result, cmd)
    local subcmd, n = assert(cmd[2]), #cmd
    if subcmd == 'eval' then
        if n ~= 3 then return end
        insert(result, cmd[1]..'(')
        insert(result, '"eval", ')
        insert_sql(result, cmd[3])
        insert(result, ')')
        return true
    else
        return usercmd(result, cmd)
    end
end
cmdfunc.db = db
cmdfunc.db2 = db
cmdfunc.db3 = db
cmdfunc.dbat = db

cmdfunc.sqlite3 = usercmd
cmdfuzz['^sqlite3_'] = usercmd

cmdfunc.forcedelete = usercmd
cmdfunc.reset_db = usercmd
cmdfunc.finish_test = usercmd

-----------------------------------------------------------------------

local function insert_result(result, ast, label)
    insert(result, '{\n');
    indent(result); insert_indent(result)
    if type(label)=='string' then
        insert(result, format('-- <%s>\n', label)); insert_indent(result)
    end
    if type(ast) == 'string' then
        local _, list = tclparser.parselist(ast)
        insert_list(result, list)
    elseif node_type(ast) == 'cmds' and #ast == 1 and ast[1][1] == 'list' then
        insert_list(result, ast[1], 2)
    else
        insert_expr(result, ast)
    end
    insert(result, '\n');
    if type(label)=='string' then
        insert_indent(result, format('-- </%s>\n', label))
    end
    dedent(result)
    insert_indent(result, '}')
end

-- match this frequent pattern:
-- set v [catch {...} msg]
-- lappend v $msg
local function match_catch(cmd, prevcmd)
    if node_type(cmd) ~= 'cmd' or cmd[1] ~= 'lappend' or
       type(cmd[2]) ~= 'string' or node_type(cmd[3]) ~= 'var' or
       #cmd ~= 3 then
            return
    end
    if node_type(prevcmd) ~= 'cmd' or prevcmd[1] ~= 'set' or
       prevcmd[2] ~= cmd[2] or node_type(prevcmd[3]) ~= 'cmds' or
       #prevcmd[3] ~= 1 or #prevcmd ~= 3 then
            return
    end
    local catch = prevcmd[3][1]
    if node_type(catch) ~= 'cmd' or catch[1] ~= 'catch' or
       type(catch[2]) ~= 'string' or catch[3] ~= cmd[3][1] or
       #catch ~= 3 then
            return
    end
    local nested = tcl_parse(catch[2], node_line(catch))
    if node_type(nested) ~= 'cmds' or #nested ~= 1 or
       node_type(nested[1]) ~= 'cmd' then
            return
    end
    return nested[1]
end

function cmdfunc.do_test(result, cmd)
    if #cmd ~= 4 or type(cmd[3]) ~= 'string' then return false end
    
    local nested = tcl_parse(cmd[3], node_line(cmd))
    for i = #nested,1,-1 do
        local cmd = nested[i]
        if node_type(cmd) == 'cmd' then
            local protected = match_catch(cmd, nested[i-1])
            if protected and match(protected[1], '^execsql') then
                protected[1] = gsub(protected[1], '^exec', 'catch')
                cmd = protected
                nested[i] = nil
                nested[i-1] = cmd
            end
            node_type('rcmd', cmd); break
        end
    end
    if #nested == 1 then
        local nested = nested[1]
        if #nested == 2 and match(nested[1], 'sql') then
                -- emit a shorter form
                insert(result, format('do_%s_test(', nested[1]))
                insert_expr(result, cmd[2])
                insert(result, ', ')
                insert_sql(result, nested[2], 'force_multi')
                insert(result, ', ')
                insert_result(result, cmd[4], cmd[2])
                insert(result, ')\n')
                return true
        end
    end
    
    insert(result, 'do_test(');
    insert_expr(result, cmd[2])
    insert(result, ', function()\n')

    indent(result)
    tolua(result, nested)

    dedent(result)

    insert_indent(result, 'end, ')
    insert_result(result, cmd[4], cmd[2])
    insert(result, ')\n')
    return true
end

function cmdfunc.do_execsql_test(result, cmd)
    if #cmd >= 3 then
        insert(result, 'do_execsql_test(')
        insert_expr(result, cmd[2])
        insert(result, ', ')
        insert_sql(result, cmd[3], 'force_multi')
        if cmd[4] then
            insert(result, ', ')
            insert_result(result, cmd[4], cmd[2])
        end
        insert(result, ')\n')
        return true
    end
end

function cmdfunc.do_catchsql_test(result, cmd)
    if #cmd == 4 then
        insert(result, 'do_catchsql_test(')
        insert_expr(result, cmd[2])
        insert(result, ', ')
        insert_sql(result, cmd[3])
        insert(result, ', ')
        insert_result(result, cmd[4], cmd[2])
        insert(result, ')\n')
        return true
    end
end

-----------------------------------------------------------------------

local result = {}
tolua(result, tcl_parse(source))
print(concat(result, ''))

local ks = {}
for k, _ in pairs(xstats) do
    insert(ks, k)
end
table.sort(ks, function(a,b)
    return xstats[a] < xstats[b]
end)

for _, k in ipairs(ks) do
    local v = xstats[k]
    if v > 1 then
        io.stderr:write(format("%q: %d\n", k, xstats[k]))
    end
end
