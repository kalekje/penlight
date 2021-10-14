-- requires penlight
local pl = _G['penlight'] or _G['pl'] -- penlight for this namespace is pl
local bind = bind or pl.func.bind

-- some bonus string operations, % text operator, and functional programmng
pl.stringx.import()
pl.text.format_operator()
pl.utils.import('pl.func')

function help_wrt(s1, s2) -- helpful printing, makes it easy to debug, s1 is object, s2 is note
    local wrt = wrt or texio.write_nl
    local wrt = wrt or print
    s2 = s2 or ''
    wrt('\nvvvvv '..s2..'\n')
    if type(s1) == 'table' then
        wrt(pl.pretty.write(s1))
    else
        wrt(tostring(s1))
    end
    wrt('\n^^^^^\n')
end


function prt_array2d(t)
    for _, r in ipairs(t) do
        local s = ''
        for _, v in ipairs(r) do
            s = s.. tostring(v)..', '
        end
        print(s)
    end
end

-- -- -- -- --


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


-- -- -- --

local function check_index(ij, rc) -- converts array index to positive value if negative
    if type(ij) ~= 'number' then
        return 1
    else
        if ij < 0 then
            ij = rc + ij + 1
        elseif ij > rc then
            ij = rc
        end
        return ij
    end
end
local function check_slice(M, i1, j1, i2, j2) -- ensure a slice is valid; i.e. all positive numbers
    r, c = pl.array2d.size(M)
    i1 = check_index(i1, r)
    i2 = check_index(i2, r)
    j1 = check_index(j1, c)
    j2 = check_index(j2, c)
    return i1, j1, i2, j2
end

local function check_func(func)  -- check if a function is a PE, if so, make it a function
    if type(func) ~= 'function' then
        __func = I(func)
    end
    return __func
end


-- -- -- -- -- -- --
-- -- -- --  functions below extend the array2d module


local function map_slice1(func, L, i1, i2) -- map a function to a slice of an array, can use PlcExpr
    i2 = i2 or i1
    local len = #L
    i1 = check_index(i1, len)
    i2 = check_index(i2, len)
    func = check_func(func)
    for i in pl.seq.range(i1,i2) do
            L[i] = func(L[i])
        end
   return L
end

local function map_slice2(func, M, i1, j1, i2, j2) -- map a function to a slice of a Matrix
    i1, j1, i2, j2 = check_slice(M, i1, j1, i2, j2)
    --for i,j in array2d.iter(M, true, i1, j1, i2, j2) do  --todo this did not work, penlight may have fixed this
    func = check_func(func)
    for i in pl.seq.range(i1,i2) do
        for j in pl.seq.range(j1,j2) do
            M[i][j] = func(M[i][j])
        end
    end
   return M
end

local function map_columns(func, M, j1, j2) -- map function to columns of matrix
    j2 = j2 or j1
    return map_slice2(func, M, 1, j1, -1, j2)
end

local function map_rows(func, M, i1, i2) -- map function to rows of matrix
    i2 = i2 or i1
    return map_slice2(func, M, i1, 1, i2, -1)
end


-- -- -- -- -- -- -- --

function sortOP(M, op, ele) -- sort a 2d array based on operator criteria, ele is column, ie sort on which element
       M_new = {}
        for row in pl.seq.sort(M, comp_2ele_func(op, ele)) do
            M_new[#M_new+1] = row
        end
        return M_new
end

local function like(M1, v)
    v = v or 0
    r, c = pl.array2d.size(M1)
    return pl.array2d.new(r,c,v)
end

local function from_table(t) -- turns a labelled table to a 2d, label-free array
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

local function toTeX(M, EL) --puts & between columns, can choose to end line with \\ if EL is true (end-line)
    EL = EL or false
    if EL then EL = '\\\\' else EL = '' end
    return pl.array2d.reduce2(_1..EL.._2, _1..'&'.._2, M)..EL
end

-- -- -- -- -- -- --

-- add to array2d module
pl.array2d['map_columns'] = map_columns
pl.array2d['map_rows'] = map_rows
pl.array2d['map_slice2'] = map_slice2
pl.array2d['map_slice1'] = map_slice1
pl.array2d['from_table'] = from_table
pl.array2d['toTeX'] = toTeX
pl.array2d['sortOP'] = sortOP
pl.array2d['like'] = like




-- -- -- -- -- -- -- -- -- -- -- --  functions below extend the operator module

local function strgt(a,b) return tostring(a) > tostring(b) end
local function strlt(a,b) return tostring(a) < tostring(b) end
pl.operator['strgt'] = strgt
pl.operator['strlt'] = strlt





local function gextract(s, pat) --extract a pattern from string, returns both
    local s_extr = ''
    local s_rem = s
    for e in s:gmatch(pat) do
        s_extr = s_extr..e
        s_rem = s_rem:gsub(e,'')
    end
    return s_extr, s_rem
end

local function gfirst(s, t) -- get the first pattern found from a table of pattern
    for _, pat in pairs(t) do
        if string.find(s, pat) then
            return pat
        end
    end
end

local function appif(S, W, B, O) --append W ord to S tring if B oolean true, otherwise O ther
    --append Word to String
    if B then --if b is true
        S = S .. W
    else --consider Other word
        O = O or ''
        S = S .. O
    end
    return S
end

local mt = getmetatable("") -- register functions with str
mt.__index["gextract"] = gextract
mt.__index["gfirst"] = gfirst
mt.__index["app_if"] = appif


function mod(a, b) -- math modulo, return remainder only
    return a - (math.floor(a/b)*b)
end

function mod2(a) -- math modulo 2
    return mod(a,2)
end

function hasval(x)  -- if something has value
    if (x == nil) or (x == false) or (x == 0) or (x == '') then
        return false
    elseif (type(x) ~= 'number') or (type(x) ~= 'string') then
        if #x == 0 then
            return false
        else
            return true
        end
    end
    return true
end




-- testing here
____zzz__ = [[
function hasStrKey(T)
    --checks if a Table contains a string-type key
    local hasaStrKey = false
    for k, v in pairs(T) do  --look through table pairs
        if type(k) == "string" then  --if any string found, return true
            hasaStrKey = true
            break
        end
    end
    return hasaStrKey
end


function any()
    --todo go through table or list of args and return true of anything is something
end

function all()
    --todo go through table or list of args and return true of anything is something
end


    --elseif x == {} then
    --    return false

n = nil
b = false
z = 0
e = ''
t = {}

--s = 'a'
--n = 1


if is(nil) then
    print('TRUE')
else
    print('FALSE')
end

--print({}=={})
--print(0.0==0)


]]











-- Some simple and helpful LaTeX functions
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--Generic LuaLaTeX utilities for print commands or environments
-- http://lua-users.org/wiki/SplitJoin -- todo read me!!
__SKIP_TEX__ = __SKIP_TEX__ or false --if declared true before here, it will use regular print functions
--                                       (for troubleshooting with texlua)

-- xparse defaults
_xTrue = '\\BooleanTrue '
_xFalse = '\\BooleanFalse '
_xNoValue = '-NoValue-'


if not __SKIP_TEX__ then
    prt = tex.sprint     --can print lists, but will NOT put new line between them
    prtn = tex.print      --can print lists and will put new line. C-function not in Lua. think P rint R return
    wrt = texio.write
    wrtn = texio.write_nl
else
    prt = io.write
    prtn = print --print with new line
    wrt = io.write
    wrtn = io.write_nl
end

function prtl(str) -- prints a literal string to latex, adds new line between them
    for line in str:gmatch"[^\n]*" do  -- gets all characters up to a new line
        prtn(line)
    end
end



_NumBkts = 0
function reset_bkt_cnt(n)
     n = n or 0
    _NumBkts = n
end

function add_bkt_cnt(n)
     n = n or 1
    _NumBkts = _NumBkts + n
end

function close_bkt_cnt()
    local s = ('}'):rep(_NumBkts)
    reset_bkt_cnt()
    return s
end








--definition helpers -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- todo add this

local function defcmd_nest(cs) -- for option if you'd like your commands under  a parent ex. \csparent{var}
    tex.print('\\gdef\\'..cs..'#1{\\csname '..var..'--#1--\\endcsname}')
end


local function defcmd(cs, val, nargs)
    if (nargs == nil) or (args == 0) then
        token.set_macro(cs, tostring(val), 'global')
    else
        local args = '#1'
        tex.print('\\gdef\\'..cs..args..'{'..val..'}')
        -- todo https://tex.stackexchange.com/questions/57551/create-a-capitalized-macro-token-using-csname
        --    \expandafter\gdef\csname Two\endcsname#1#2{1:#1, two:#2} --todo do it like this
    end
end


local function prvcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
        tex.print('\\PackageWarning{YAMLvars}{Variable '..cs..' already defined, could not declare}{}')
    else
        defcmd(cs, val)
    end
end


local function newcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
        tex.print('\\PackageError{luadefs}{Command '..cs..' already defined}{}')
    else
        defcmd(cs, val)
    end
end

local function renewcmd(cs, val) -- provide command via lua
   if token.is_defined(cs) then
        defcmd(cs, val)
    else
        tex.print('\\PackageError{luadefs}{Command '..cs..' already defined}{}')
    end
end

local function deccmd(cs, def)
    if def == nil then
        prvcmd(cs, '\\PackageError{luadefs}{Command "'..cs..'" was declared and used but, not set}{}')
    else
        prvcmd(cs, def)
    end
end


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
