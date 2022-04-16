--% Kale Ewasiuk (kalekje@gmail.com)
--% +REVDATE+
--% Copyright (C) 2021-2022 Kale Ewasiuk
--%
--% Permission is hereby granted, free of charge, to any person obtaining a copy
--% of this software and associated documentation files (the "Software"), to deal
--% in the Software without restriction, including without limitation the rights
--% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--% copies of the Software, and to permit persons to whom the Software is
--% furnished to do so, subject to the following conditions:
--%
--% The above copyright notice and this permission notice shall be included in
--% all copies or substantial portions of the Software.
--%
--% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
--% ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
--% TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--% PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT
--% SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
--% ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
--% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
--% OR OTHER DEALINGS IN THE SOFTWARE.

__SKIP_TEX__ = __SKIP_TEX__ or false --if declared true before here, it will use regular print functions
--                                       (for troubleshooting with texlua instead of actual use in lua latex)
__PL_NO_GLOBALS__ = __PL_NO_GLOBALS__ or false
__PL_EXTRAS__ = 1

-- requires penlight
local pl = _G['penlight'] or _G['pl'] -- penlight for this namespace is pl

luakeys = require'luakeys'

-- some bonus string operations, % text operator, and functional programming
pl.stringx.import()
pl.stringx.format_operator()
pl.utils.import'pl.func' -- allow placeholder expressions _1 +1 etc.

pl.COMP = require'pl.comprehension'.new() -- for comprehensions

-- http://lua-users.org/wiki/SplitJoin -- todo read me!!

pl.tex = {} -- adding a sub-module for tex related stuff

local bind = bind or pl.func.bind


function pl.hasval(x)  -- if something has value
    if (type(x) == 'function') or (type(x) == 'CFunction') or (type(x) == 'userdata') then
        return true
    elseif (x == nil) or (x == false) or (x == 0) or (x == '') or (x == {}) then
        return false
    elseif (type(x) ~= 'boolean') and (type(x) ~= 'number') and (type(x) ~= 'string') then  -- something else? maybe ths not needed
        if #x == 0 then -- one more check, probably no needed though, I was trying to cover other classes but they all tables
            return false
        else
            return true
        end
    end
    return true
end


-- Some simple and helpful LaTeX functions -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- xparse defaults
pl.tex._xTrue = '\\BooleanTrue '
pl.tex._xFalse = '\\BooleanFalse '
pl.tex._xNoValue = '-NoValue-'

--Generic LuaLaTeX utilities for print commands or environments

if not __SKIP_TEX__ then
    local function check_special_chars(s) -- todo extend to toher special chars?
        if type(s) == 'string' then
            if string.find(s, '[\n\r\t\0]') then
                pl.tex.pkgwarn('penlight', 'printing string with special (eg. newline) char, possible unexpected behaviour on string: '..s)
            end
        end
    end

    -- NOTE: usage is a bit different than default. If number is first arg, you CANT change catcode.
    --              We don't need that under normal use, use tex.print or tex.sprint if you need
    function pl.tex.prt(s, ...) -- print something, no new line after
        check_special_chars(s)
        if type(s) == 'number' then s = tostring(s) end
        tex.sprint(s, ...)     --can print lists as well, but will NOT put new line between them or anything printed
    end

    function pl.tex.prtn(s, ...) -- print with new line after, can print lists or nums. C-function not in Lua, apparantly
        s = s or ''
        check_special_chars(s)
        if type(s) == 'number' then s = tostring(s) end
        tex.print(s, ...)
    end

    pl.tex.wrt = texio.write
    pl.tex.wrtn = texio.write_nl
else
    pl.tex.prt = io.write
    pl.tex.prtn = print     --print with new line
    pl.tex.wrt = io.write
    pl.tex.wrtn = io.write_nl
end

function pl.tex.prtl(str) -- prints a literal/lines string in latex, adds new line between them
    for line in str:gmatch"[^\n]*" do  -- gets all characters up to a new line
        pl.tex.prtn(line)
    end
end

-- todo option to specify between character? one for first table, on for recursives?
function pl.tex.prtt(tab, d1, d2) -- prints a table with new line between each item
    d1 = d1 or ''
    d2 = d2 or '\\leavevmode\\\\'
    for _, t in pairs(tab) do  --
        if type(t) ~= 'table' then
            if d1 == '' then
                pl.tex.prtn(t)
            else
                pl.tex.prt(t, d1)
            end
         else
            pl.tex.prtn(d2)
            pl.tex.prtt(t,d1,d2)
        end
    end
end

function pl.tex.help_wrt(s1, s2) -- helpful printing, makes it easy to debug, s1 is object, s2 is note
    local wrt2 = wrt or texio.write_nl or print
    s2 = s2 or ''
    wrt2('\nvvvvv '..s2..'\n')
    if type(s1) == 'table' then
        wrt2(pl.pretty.write(s1))
    else
        wrt2(tostring(s1))
    end
    wrt2('\n^^^^^\n')
end

function pl.tex.prt_array2d(t)
    for _, r in ipairs(t) do
        local s = ''
        for _, v in ipairs(r) do
            s = s.. tostring(v)..', '
        end
        pl.tex.prt(s)
        pl.tex.prt('\n')
    end
end

-- -- -- -- --

function pl.tex.pkgwarn(pkg, msg1, msg2)
    pkg = pkg or ''
    msg1 = msg1 or ''
    msg2 = msg2 or ''
    tex.sprint('\\PackageWarning{'..pkg..'}{'..msg1..'}{'..msg2..'}')
end

function pl.tex.pkgerror(pkg, msg1, msg2, stop)
    pkg = pkg or ''
    msg1 = msg1 or ''
    msg2 = msg2 or ''
    stop = pl.hasval(stop)
    tex.sprint('\\PackageError{'..pkg..'}{'..msg1..'}{'..msg2..'}')
    if stop then tex.sprint('\\stop') end -- stop on the spot (say that 10 times)
end


--definition helpers -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function pl.tex.defmacro(cs, val) -- , will not work if val contains undefined tokens (so pre-define them if using..)
    val = val or ''          -- however this works for arbitrary command names (\@hello-123 etc allowed)
    token.set_macro(cs, val, 'global')
end


function pl.tex.defcmd(cs, val) -- fixes above issue, but only chars allowed in cs (and no @)
    val = val or ''
    tex.sprint('\\gdef\\'..cs..'{'..val..'}')
end

function pl.tex.defcmdAT(cs, val) -- allows @ in cs,
    --however should only be used in preamble. I avoid \makeatother because I've ran into issues with cls and sty files using it.
    val = val or ''
    tex.sprint('\\makeatletter\\gdef\\'..cs..'{'..val..'}')
end



function pl.tex.prvcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
       -- do nothing if token is defined already --pkgwarn('penlight', 'Definition '..cs..' is being overwritten')
    else
        pl.tex.defcmd(cs, val)
    end
end

function pl.tex.newcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
       pl.tex.pkgerror('penlight: newcmd',cs..' already defined')
    else
        pl.tex.defcmd(cs, val)
    end
end

function pl.tex.renewcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
        pl.tex.defcmd(cs, val)
    else
        pl.tex.pkgerror('penlight: renewcmd',cs..' not defined')
    end
end

function pl.tex.deccmd(cs, def, overwrite) -- declare a definition, placeholder throws an error if it used but not set!
    overwrite = pl.hasval(overwrite)
    local decfun
    if overwrite then decfun = pl.tex.defcmd else decfun = pl.tex.newcmd end
    if def == nil then
        decfun(cs, pkgerror('penlight', cs..' was declared and used in document, but never set'))
    else
        decfun(cs, def)
    end
end


--
-- -- todo add and improve this, options for args?
--local function defcmd_nest(cs) -- for option if you'd like your commands under  a parent ex. \csparent{var}
--    tex.print('\\gdef\\'..cs..'#1{\\csname '..var..'--#1--\\endcsname}')
--end
--
--
--local function defcmd(cs, val, nargs)
--    if (nargs == nil) or (args == 0) then
--        token.set_macro(cs, tostring(val), 'global')
--    else
--        local args = '#1'
--        tex.print('\\gdef\\'..cs..args..'{'..val..'}')
--        -- todo https://tex.stackexchange.com/questions/57551/create-a-capitalized-macro-token-using-csname
--        --    \expandafter\gdef\csname Two\endcsname#1#2{1:#1, two:#2} --todo do it like this
--    end
--end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



-- when nesting commands, this makes it helpful to not worry about brackets
pl.tex._NumBkts = 0
--prt(opencmd('textbf')..opencmd('texttt')..'bold typwriter'..close_bkt_cnt())

function pl.tex.opencmd(cmd)
    return '\\'..cmd..add_bkt_cnt()
end

function pl.tex.reset_bkt_cnt(n)
     n = n or 0
    _NumBkts = n
end

function pl.tex.add_bkt_cnt(n)
    -- add open bracket n times, returns brackets
     n = n or 1
    _NumBkts = _NumBkts + n
    return ('{'):rep(n)
end

function pl.tex.close_bkt_cnt(n)
    n = n or _NumBkts
    local s = ('}'):rep(n)
    _NumBkts = _NumBkts - n
    return s
end





-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --






-- -- -- -- math stuff
function math.mod(a, b) -- math modulo, return remainder only
    return a - (math.floor(a/b)*b)
end

function math.mod2(a) -- math modulo 2
    return math.mod(a,2)
end



-- -- -- -- string stuff
local lpeg = require"lpeg"
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V

local number = P{"number",
    number = (V"int" * V"frac"^-1 * V"exp"^-1) / tonumber,
    int = V"sign"^-1 * (R"19" * V"digits" + V"digit"),
    digits = V"digit" * V"digits" + V"digit",
    digit = R"09",
    sign = S"+-",
    frac = P"." * V"digits",
    exp = S"eE" * V"sign"^-1 * V"digits",
    }


local str_mt = getmetatable("") -- register functions with str

function str_mt.__index.gnum(s)
    return number:match(s)
end

function str_mt.__index.gextract(s, pat) --extract a pattern from string, returns both
    local s_extr = ''
    local s_rem = s
    for e in s:gmatch(pat) do
        s_extr = s_extr..e
        s_rem = s_rem:gsub(e,'')
    end
    return s_extr, s_rem
end

function str_mt.__index.gfirst(s, t) -- get the first pattern found from a table of pattern
    for _, pat in pairs(t) do
        if string.find(s, pat) then
            return pat
        end
    end
end

function str_mt.__index.appif(S, W, B, O) --append W ord to S tring if B oolean true, otherwise O ther
    --append Word to String
    if B then --if b is true
        S = S .. W
    else --consider Other word
        O = O or ''
        S = S .. O
    end
    return S
end


 function str_mt.__index.containsany(s, exp)
    if type(exp) ~= 'table' then exp = {exp} end
    for _, e in ipairs(exp) do
        if s:find(e) then return true end
    end
    return false
end

function str_mt.__index.containsanycase(s, exp)
    if type(exp) ~= 'table' then exp = {exp} end
    for _, e in ipairs(exp) do
        if s:lower():find(e:lower()) then return true end
    end
    return false
end

function str_mt.__index.totable(str)
    local t = {}
    for i = 1, #str do
        t[i] = str:sub(i, i)
    end
    return t
end


function str_mt.__index.upfirst(str)
    return str:gsub('%a', function(x) return x:upper()  end, 1)
end



-- -- -- -- function stuff

function pl.clone_function(fn)
  local dumped = string.dump(fn)
  local cloned = loadstring(dumped)
  local i = 1
  while true do
    local name = debug.getupvalue(fn, i)
    if not name then
      break
    end
    debug.upvaluejoin(cloned, i, fn, i)
    i = i + 1
  end
  return cloned
end




-- -- -- -- -- -- -- -- -- -- -- --  functions below extend the operator module

function pl.operator.strgt(a,b) return tostring(a) > tostring(b) end
function pl.operator.strlt(a,b) return tostring(a) < tostring(b) end



-- -- -- --  functions below are helpers for arrays and 2d

local function compare_elements(a, b, op, ele)
    op = op or pl.oper.gt
    ele = ele or 1
    return op(a[ele], b[ele])
end

local function comp_2ele_func(op, ele) -- make a 2 element comparison function,
    --sort with function on element nnum
    return bind(compare_elements, _1, _2, op, ele)
end





-- table stuff below


function pl.tablex.map_slice(func, T, j1, j2)
    if type(j1) == 'string' then
        return pl.array2d.map_slice(func, {T}, ','..j1)[1]
    else
        return pl.array2d.map_slice(func, {T}, 1, j1, 1, j2)[1]
    end
end

pl.array2d.map_slice1 = pl.tablex.map_slice


-- todo option for multiple filters with AND logic, like the filter files??
function pl.tablex.filterstr(t, exp, case)
    -- case = case sensitive
    case = hasval(case)
    -- apply lua patterns to a table to filter iter
    -- str or table of str's can be passed, OR logic is used if table is passed
    if case then
        return pl.tablex.filter(t, bind(string.containsany,_1,exp))
    else
        return pl.tablex.filter(t, bind(string.containsanycase,_1,exp))
    end
end


function pl.utils.filterfiles(...)
    -- f1 is a series of filtering patterns, or condition
    -- f2 is a series of filtering patters, or condition
    -- (f1_a or f2_...) and (f2 .. ) must match
    local args = table.pack(...)
    -- todo -- check where boolean is for recursive or not, set starting argument
    -- this could allow one to omit dir
    -- todo if no boolean at all, assume dir = '.' and r = false
    -- if boolean given, assume dir = '.'
    local nstart = 3
    local r = args[2]
    local dir = args[1]
    if type(args[1]) == 'boolean' then
        dir = '.'
        r =  args[1]
        nstart = 2
    elseif type(args[2]) ~= 'boolean' then
        dir = '.'
        r =  false
        nstart = 1
    end

    local files
    if r then  files = pl.dir.getallfiles(dir)
    else files = pl.dir.getfiles(dir)
    end
    for i=nstart,args.n do
        files = pl.tablex.filter(files, pl.func.compose(bind(string.containsanycase,_1, args[i]), pl.path.basename))
    end
    return  files
end




-- -- -- -- -- -- -- --  functions below extend the array2d module


function pl.array2d.map_slice(func, M, i1, j1, i2, j2) -- map a function to a slice of a Matrix
    func = pl.utils.function_arg(1, func)
    for i,j in pl.array2d.iter(M, true, i1, j1, i2, j2) do
        M[i][j] = func(M[i][j])
    end
   return M
end

pl.array2d.map_slice2 = pl.array2d.map_slice

function pl.array2d.map_cols(func, M, j1, j2) -- map function to columns of matrix
    if type(j1) == 'string' then
        return pl.array2d.map_slice(func, M, ','..j1)
    else
        j2 = j2 or -1
        return pl.array2d.map_slice(func, M, 1, j1, -1, j2)
    end
end

pl.array2d.map_columns = pl.array2d.map_cols

function pl.array2d.map_rows(func, M, i1, i2) -- map function to rows of matrix
    if type(i1) == 'string' then
        return pl.array2d.map_slice(func, M, i1)
    else
        i2 = i2 or -1
        return pl.array2d.map_slice(func, M, i1, 1, i2, -1)
    end
end


-- -- -- -- -- -- -- --

function pl.array2d.sortOP(M, op, ele) -- sort a 2d array based on operator criteria, ele is column, ie sort on which element
       M_new = {}
        for row in pl.seq.sort(M, comp_2ele_func(op, ele)) do
            M_new[#M_new+1] = row
        end
        return M_new
end

function pl.array2d.like(M1, v)
    v = v or 0
    r, c = pl.array2d.size(M1)
    return pl.array2d.new(r,c,v)
end

function pl.array2d.from_table(t) -- turns a labelled table to a 2d, label-free array
    t_new = {}
    for k, v in pairs(t) do
        if type(v) == 'table' then
            t_new_row = {k}
            for _, v_ in ipairs(v) do
                 t_new_row[#t_new_row+1] =  v_
            end
            t_new[#t_new+1] = t_new_row
        else
            t_new[#t_new+1] = {k, v}
        end
    end
    return t_new
end

function pl.array2d.toTeX(M, EL) --puts & between columns, can choose to end line with \\ if EL is true (end-line)
    EL = EL or false
    if EL then EL = '\\\\' else EL = '' end
    return pl.array2d.reduce2(_1..EL.._2, _1..'&'.._2, M)..EL
end


local function parse_numpy1d(i1, i2, iS)
    i1 = tonumber(i1)
    i2 = tonumber(i2)
    if iS == ':' then
        if i1 == nil then i1 = 1 end
        if i2 == nil then i2 = -1 end
    else
        if i1 == nil then
            i1 = 1
            i2 = -1
        else
            i2 = i1
        end
    end
    return i1, i2
end

function pl.array2d.parse_numpy2d_str(s)
    s = s:gsub('%s+', '')
    _, _, i1, iS, i2, j1, jS, j2 = string.find(s, "(%-?%d*)(:?)(%-?%d*),?(%-?%d*)(:?)(%-?%d*)")
    i1, i2 = parse_numpy1d(i1, i2, iS)
    j1, j2 = parse_numpy1d(j1, j2, jS)
    return i1, j1, i2, j2
end


local _parse_range = pl.clone_function(pl.array2d.parse_range)

function pl.array2d.parse_range(s) -- edit parse range to do numpy string if no letter passed
    pl.utils.assert_arg(1,s,'string')
    if not s:find'%a' then
        return pl.array2d.parse_numpy2d_str(s)
    end
    return _parse_range(s)
end






-- https://tex.stackexchange.com/questions/38150/in-lualatex-how-do-i-pass-the-content-of-an-environment-to-lua-verbatim
pl.tex.recordedbuf = ""
function pl.tex.readbuf(buf)
    i,j = string.find(buf, '\\end{%w+}')
     if i==nil then -- if not ending an environment
        pl.tex.recordedbuf = pl.tex.recordedbuf .. buf .. "\n"
        return ""
    else
        return nil
    end
end

function pl.tex.startrecording()
    pl.tex.recordedbuf = ""
    luatexbase.add_to_callback('process_input_buffer', pl.tex.readbuf, 'readbuf')
end

function pl.tex.stoprecording()
    luatexbase.remove_from_callback('process_input_buffer', 'readbuf')
    pl.tex.recordedbuf = pl.tex.recordedbuf:gsub("\\end{%w+}\n","")
end



__PDFmetadata__ = {}
pl.tex.add_xspace_intext = true


function pl.tex.updatePDFtable(k, v, o)
    k = k:upfirst()
    if not pl.hasval(o) and __PDFmetadata__[k] ~= nil then
        return
    end
    __PDFmetadata__[k] = v
end

pl.tex.writePDFmetadata = function(t) -- write PDF metadata to xmpdata file
  t = t or __PDFmetadata__
  local str = ''
  for k, v in pairs(t) do
    k = k:upfirst()
    str = str..'\\'..k..'{'..v..'}'..'\n'
  end
  pl.utils.writefile(tex.jobname..'.xmpdata', str)
end



function pl.tex.clear_cmds_str(s)
    return s:gsub('%s+', ' '):gsub('\\\\',' '):gsub('\\%a+',''):gsub('{',' '):gsub('}',' '):gsub('%s+',' '):strip()
end

function pl.tex.makePDFvarstr(s)
    s = s:gsub('%s*\\sep%s+','\0'):gsub('%s*\\and%s+','\0')  -- turn \and into \sep
    s = pl.tex.clear_cmds_str(s)
    s = s:gsub('\0','\\sep ')
    pl.tex.help_wrt(s,'PDF var string')
    return s
end

function pl.tex.makeInTextstr(s)
    local s, c_and = s:gsub('%s*\\and%s+','\0')
    s = pl.tex.clear_cmds_str(s)
    if pl.tex.add_xspace_intext then
        s = s..'\\xspace'
    end
    if c_and == 1 then
        s = s:gsub('\0',' and ')
    elseif c_and > 1 then
        s = s:gsub('\0',', ', c_and - 1)
        s = s:gsub('\0',', and ')
    end
    pl.tex.help_wrt(s,'in text var string')
    return s
end






if not __PL_NO_GLOBALS__ then
    __PL_EXTRAS__ = 2
    -- iterators
    kpairs = pl.utils.kpairs
    npairs = pl.utils.npairs
    --enum = utils.enum

    for k,v in pairs(pl.tablex) do  -- extend the table table to contain tablex functions
        if k == 'sort' then
            table.sortk = v
        elseif k == 'move' then
            table.xmove = v
        else
         _G['table'][k] = v
        end
    end
    table.join = table.concat -- alias

    hasval = pl.hasval
    COMP = pl.COMP

    -- shortcuts
-- http://stevedonovan.github.io/Penlight/api/libraries/pl.utils.html
    pl.writefile = pl.utils.writefile
    pl.readfile = pl.utils.readfile
    pl.readlines = pl.utils.readfile
    pl.filterfiles = pl.utils.filterfiles

    pl.a2 = pl.array2d
    pl.tbl = pl.tablex


    for k,v in pairs(pl.tex) do  -- make tex functions global
        _G[k] = v
    end

end





    --_xTrue = pl.tex._xTrue
    --_xFalse = pl.tex._xFalse
    --_xNoValue = pl.tex._xNoValue
    --
    --prt = pl.tex.prt
    --prtn = pl.tex.prtn
    --wrt = pl.tex.wrt
    --wrtn = pl.tex.wrtn
    --
    --prtl = pl.tex.prtl
    --prtt = pl.tex.prtt
    --
    --help_wrt = pl.tex.help_wrt
    --prt_array2d = pl.tex.prt_array2d
    --
    --pkgwarn = pl.tex.pkgwarn
    --pkgerror = pl.tex.pkgerror
    --
    --defcmd = pl.tex.defcmd
    --prvcmd = pl.tex.prvcmd
    --newcmd = pl.tex.newcmd
    --renewcmd = pl.tex.renewcmd
    --deccmd = pl.tex.deccmd
    --
    --_NumBkts = pl.tex._NumBkts
    --opencmd = pl.tex.opencmd
    --reset_bkt_cnt = pl.tex.reset_bkt_cnt
    --add_bkt_cnt = pl.tex.add_bkt_cnt
    --close_bkt_cnt = pl.tex.close_bkt_cnt


-- graveyard


-- luakeys parses individual keys as ipairs, this changes the list to a pure map
--function pl.luakeystomap(t)
--    local t_new = {}
--    for k, v in pairs(t) do
--        if type(k) == 'number' then
--            t_new[v] = true
--        else
--            t_new[k] = v
--        end
--    end
--    return t_new
--end
--if luakeys then -- if luakeys is already loaded
--    function luakeys.parseN(s, ...)
--        local t = luakeys.parse(s,...)
--        t = pl.luakeystomap(t)
--        return t
--    end
--end
-- might not be needed


    --local func = check_func(func)
--local function check_func(func)  -- check if a function is a PE, if so, make it a function
--    if type(func) ~= 'function' then
--        return I(func)
--    end
--    return func
--end

-- -- -- -- -- -- --
-- -- -- --  functions below extend the array2d module


--function pl.array2d.map_slice1(func, L, i1, i2) -- map a function to a slice of an array, can use PlcExpr
--    i2 = i2 or i1
--    local len = #L
--    i1 = check_index(i1, len)
--    i2 = check_index(i2, len)
--    func = check_func(func)
--    for i in pl.seq.range(i1,i2) do
--            L[i] = func(L[i])
--        end
--   return L
--end

    -- used this below when iter was not working..
    --i1, j1, i2, j2 = check_slice(M, i1, j1, i2, j2)
        --for i in pl.seq.range(i1,i2) do
    --    for j in pl.seq.range(j1,j2) do
        --end
    -- penlight may have fixed this
--local function check_index(ij, rc) -- converts array index to positive value if negative
--    if type(ij) ~= 'number' then
--        return 1
--    else
--        if ij < 0 then
--            ij = rc + ij + 1
--        elseif ij > rc then
--            ij = rc
--        elseif ij == 0 then
--            ij = 1
--        end
--        return ij
--    end
--end
--local function check_slice(M, i1, j1, i2, j2) -- ensure a slice is valid; i.e. all positive numbers
--    r, c = pl.array2d.size(M)
--    i1 = check_index(i1 or 1, r)
--    i2 = check_index(i2 or r, r)
--    j1 = check_index(j1 or 1, c)
--    j2 = check_index(j2 or c, c)
--    return i1, j1, i2, j2
--end

