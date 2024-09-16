
---------------------------------------------------------
----------------Auto generated code block----------------
---------------------------------------------------------

do
    local searchers = package.searchers or package.loaders
    local origin_seacher = searchers[2]
    searchers[2] = function(path)
        local files =
        {
------------------------
-- Modules part begin --
------------------------

["pl.xml"] = function()
--------------------
-- Module: 'pl.xml'
--------------------
--- XML LOM Utilities.
--
-- This implements some useful things on [LOM](http://matthewwild.co.uk/projects/luaexpat/lom.html) documents, such as returned by `lxp.lom.parse`.
-- In particular, it can convert LOM back into XML text, with optional pretty-printing control.
-- It is based on stanza.lua from [Prosody](http://hg.prosody.im/trunk/file/4621c92d2368/util/stanza.lua)
--
--     > d = xml.parse "<nodes><node id='1'>alice</node></nodes>"
--     > = d
--     <nodes><node id='1'>alice</node></nodes>
--     > = xml.tostring(d,'','  ')
--     <nodes>
--        <node id='1'>alice</node>
--     </nodes>
--
-- Can be used as a lightweight one-stop-shop for simple XML processing; a simple XML parser is included
-- but the default is to use `lxp.lom` if it can be found.
-- <pre>
-- Prosody IM
-- Copyright (C) 2008-2010 Matthew Wild
-- Copyright (C) 2008-2010 Waqas Hussain--
-- classic Lua XML parser by Roberto Ierusalimschy.
-- modified to output LOM format.
-- http://lua-users.org/wiki/LuaXml
-- </pre>
-- See @{06-data.md.XML|the Guide}
--
-- Dependencies: `pl.utils`
--
-- Soft Dependencies: `lxp.lom` (fallback is to use basic Lua parser)
-- @module pl.xml

local utils = require 'pl.utils'
local split         =   utils.split
local t_insert      =  table.insert
local t_concat      =  table.concat
local t_remove      =  table.remove
local s_match       =  string.match
local tostring      =      tostring
local setmetatable  =  setmetatable
local getmetatable  =  getmetatable
local pairs         =         pairs
local ipairs        =        ipairs
local type          =          type
local next          =          next
local print         =         print
local unpack        =  utils.unpack
local s_gsub        =   string.gsub
local s_sub         =    string.sub
local s_find        =   string.find
local pcall         =         pcall
local require       =       require


utils.raise_deprecation {
  source = "Penlight " .. utils._VERSION,
  message = "the contents of module 'pl.xml' has been deprecated, please use a more specialized library instead",
  version_removed = "2.0.0",
  deprecated_after = "1.11.0",
  no_trace = true,
}



local _M = {}
local Doc = { __type = "doc" };
Doc.__index = Doc;


local function is_text(s) return type(s) == 'string' end
local function is_tag(d) return type(d) == 'table' and is_text(d.tag) end



--- create a new document node.
-- @tparam string tag the tag name
-- @tparam[opt={}] table attr attributes (table of name-value pairs)
-- @return the Node object
-- @see xml.elem
-- @usage
-- local doc = xml.new("main", { hello = "world", answer = "42" })
-- print(doc)  -->  <main hello='world' answer='42'/>
function _M.new(tag, attr)
  if type(tag) ~= "string" then
    error("expected 'tag' to be a string value, got: " .. type(tag), 2)
  end
  attr = attr or {}
  if type(attr) ~= "table" then
    error("expected 'attr' to be a table value, got: " .. type(attr), 2)
  end

  local doc = { tag = tag, attr = attr, last_add = {}};
  return setmetatable(doc, Doc);
end


--- parse an XML document. By default, this uses lxp.lom.parse, but
-- falls back to basic_parse, or if `use_basic` is truthy
-- @param text_or_filename  file or string representation
-- @param is_file whether text_or_file is a file name or not
-- @param use_basic do a basic parse
-- @return a parsed LOM document with the document metatatables set
-- @return nil, error the error can either be a file error or a parse error
function _M.parse(text_or_filename, is_file, use_basic)
  local parser,status,lom
  if use_basic then
    parser = _M.basic_parse
  else
    status,lom = pcall(require,'lxp.lom')
    if not status then
      parser = _M.basic_parse
    else
      parser = lom.parse
    end
  end

  if is_file then
    local text, err = utils.readfile(text_or_filename)
    if not text then
      return nil, err
    end
    text_or_filename = text
  end

  local doc, err = parser(text_or_filename)
  if not doc then
    return nil, err
  end

  if lom then
    _M.walk(doc, false, function(_, d)
      setmetatable(d, Doc)
    end)
  end
  return doc
end


--- Create a Node with a set of children (text or Nodes) and attributes.
-- @tparam string tag a tag name
-- @tparam table|string items either a single child (text or Node), or a table where the hash
-- part is the attributes and the list part is the children (text or Nodes).
-- @return the new Node
-- @see xml.new
-- @see xml.tags
-- @usage
-- local doc = xml.elem("top", "hello world")                -- <top>hello world</top>
-- local doc = xml.elem("main", xml.new("child"))            -- <main><child/></main>
-- local doc = xml.elem("main", { "this ", "is ", "nice" })  -- <main>this is nice</main>
-- local doc = xml.elem("main", { xml.new "this",
--                                xml.new "is",
--                                xml.new "nice" })          -- <main><this/><is/><nice/></main>
-- local doc = xml.elem("main", { hello = "world" })         -- <main hello='world'/>
-- local doc = xml.elem("main", {
--   "prefix",
--   xml.elem("child", { "this ", "is ", "nice"}),
--   "postfix",
--   attrib = "value"
-- })   -- <main attrib='value'>prefix<child>this is nice</child>postfix</main>"
function _M.elem(tag, items)
  local s = _M.new(tag)
  if is_text(items) then items = {items} end
  if is_tag(items) then
    t_insert(s,items)
  elseif type(items) == 'table' then
    for k,v in pairs(items) do
      if is_text(k) then
        s.attr[k] = v
        t_insert(s.attr,k)
      else
        s[k] = v
      end
    end
  end
  return s
end


--- given a list of names, return a number of element constructors.
-- If passing a comma-separated string, then whitespace surrounding the values
-- will be stripped.
--
-- The returned constructor functions are a shortcut to `xml.elem` where you
-- no longer provide the tag-name, but only the `items` table.
-- @tparam string|table list a list of names, or a comma-separated string.
-- @return (multiple) constructor functions; `function(items)`. For the `items`
-- parameter see `xml.elem`.
-- @see xml.elem
-- @usage
-- local new_parent, new_child = xml.tags 'mom, kid'
-- doc = new_parent {new_child 'Bob', new_child 'Annie'}
-- -- <mom><kid>Bob</kid><kid>Annie</kid></mom>
function _M.tags(list)
  local ctors = {}
  if is_text(list) then
    list = split(list:match("^%s*(.-)%s*$"),'%s*,%s*')
  end
  for i,tag in ipairs(list) do
    local function ctor(items)
      return _M.elem(tag,items)
    end
    ctors[i] = ctor
  end
  return unpack(ctors)
end


--- Adds a document Node, at current position.
-- This updates the last inserted position to the new Node.
-- @tparam string tag the tag name
-- @tparam[opt={}] table attrs attributes (table of name-value pairs)
-- @return the current node (`self`)
-- @usage
-- local doc = xml.new("main")
-- doc:addtag("penlight", { hello = "world"})
-- doc:addtag("expat")  -- added to 'penlight' since position moved
-- print(doc)  -->  <main><penlight hello='world'><expat/></penlight></main>
function Doc:addtag(tag, attrs)
  local s = _M.new(tag, attrs)
  self:add_child(s)
  t_insert(self.last_add, s)
  return self
end


--- Adds a text node, at current position.
-- @tparam string text a string
-- @return the current node (`self`)
-- @usage
-- local doc = xml.new("main")
-- doc:text("penlight")
-- doc:text("expat")
-- print(doc)  -->  <main><penlightexpat</main>
function Doc:text(text)
  self:add_child(text)
  return self
end


--- Moves current position up one level.
-- @return the current node (`self`)
function Doc:up()
  t_remove(self.last_add)
  return self
end


--- Resets current position to top level.
-- Resets to the `self` node.
-- @return the current node (`self`)
function Doc:reset()
  local last_add = self.last_add
  for i = 1,#last_add do
    last_add[i] = nil
  end
  return self
end


--- Append a child to the current Node (ignoring current position).
-- @param child a child node (either text or a document)
-- @return the current node (`self`)
-- @usage
-- local doc = xml.new("main")
-- doc:add_direct_child("dog")
-- doc:add_direct_child(xml.new("child"))
-- doc:add_direct_child("cat")
-- print(doc)  -->  <main>dog<child/>cat</main>
function Doc:add_direct_child(child)
  t_insert(self, child)
  return self
end


--- Append a child at the current position (without changing position).
-- @param child a child node (either text or a document)
-- @return the current node (`self`)
-- @usage
-- local doc = xml.new("main")
-- doc:addtag("one")
-- doc:add_child(xml.new("item1"))
-- doc:add_child(xml.new("item2"))
-- doc:add_child(xml.new("item3"))
-- print(doc)  -->  <main><one><item1/><item2/><item3/></one></main>
function Doc:add_child(child)
  (self.last_add[#self.last_add] or self):add_direct_child(child)
  return self
end


--accessing attributes: useful not to have to expose implementation (attr)
--but also can allow attr to be nil in any future optimizations


--- Set attributes of a document node.
-- Will add/overwrite values, but will not remove existing ones.
-- Operates on the Node itself, will not take position into account.
-- @tparam table t a table containing attribute/value pairs
-- @return the current node (`self`)
function Doc:set_attribs(t)
  -- TODO: keep array part in sync
  for k,v in pairs(t) do
    self.attr[k] = v
  end
  return self
end


--- Set a single attribute of a document node.
-- Operates on the Node itself, will not take position into account.
-- @param a attribute
-- @param v its value, pass in `nil` to delete the attribute
-- @return the current node (`self`)
function Doc:set_attrib(a,v)
  -- TODO: keep array part in sync
  self.attr[a] = v
  return self
end


--- Gets the attributes of a document node.
-- Operates on the Node itself, will not take position into account.
-- @return table with attributes (attribute/value pairs)
function Doc:get_attribs()
  return self.attr
end



local template_cache do
  local templ_cache = {}

  -- @param templ a template, a string being valid xml to be parsed, or a Node object
  function template_cache(templ)
    if is_text(templ) then
      if templ_cache[templ] then
        -- cache hit
        return templ_cache[templ]

      else
        -- parse and cache
        local ptempl, err = _M.parse(templ,false,true)
        if not ptempl then
          return nil, err
        end
        templ_cache[templ] = ptempl
        return ptempl
      end
    end

    if is_tag(templ) then
      return templ
    end

    return nil, "template is not a document"
  end
end


do
  local function is_data(data)
    return #data == 0 or type(data[1]) ~= 'table'
  end


  local function prepare_data(data)
    -- a hack for ensuring that $1 maps to first element of data, etc.
    -- Either this or could change the gsub call just below.
    for i,v in ipairs(data) do
      data[tostring(i)] = v
    end
  end

  --- create a substituted copy of a document,
  -- @param template may be a document or a string representation which will be parsed and cached
  -- @param data a table of name-value pairs or a list of such tables
  -- @return an XML document
  function Doc.subst(template, data)
    if type(data) ~= 'table' or not next(data) then
      return nil, "data must be a non-empty table"
    end

    if is_data(data) then
      prepare_data(data)
    end

    local templ, err = template_cache(template)
    if err then
      return nil, err
    end

    local function _subst(item)
      return _M.clone(templ, function(s)
        return s:gsub('%$(%w+)', item)
      end)
    end

    if is_data(data) then
      return _subst(data)
    end

    local list = {}
    for _, item in ipairs(data) do
      prepare_data(item)
      t_insert(list, _subst(item))
    end

    if data.tag then
      list = _M.elem(data.tag,list)
    end
    return list
  end
end


--- Return the first child with a given tag name (non-recursive).
-- @param tag the tag name
-- @return the child Node found or `nil` if not found
function Doc:child_with_name(tag)
  for _, child in ipairs(self) do
    if child.tag == tag then
      return child
    end
  end
end


do
  -- @param self document node to traverse
  -- @param tag tag-name to look for
  -- @param list array table to add the matching ones to
  -- @param recurse if truthy, recursively search the node
  local function _children_with_name(self, tag, list, recurse)
    -- TODO: protect against recursion
    for _, child in ipairs(self) do
      if type(child) == 'table' then
        if child.tag == tag then
          t_insert(list, child)
        end
        if recurse then
          _children_with_name(child, tag, list, recurse)
        end
      end
    end
  end

  --- Returns all elements in a document that have a given tag.
  -- @tparam string tag a tag name
  -- @tparam[opt=false] boolean dont_recurse optionally only return the immediate children with this tag name
  -- @return a list of elements found, list will be empty if none was found.
  function Doc:get_elements_with_name(tag, dont_recurse)
    local res = {}
    _children_with_name(self, tag, res, not dont_recurse)
    return res
  end
end



--- Iterator over all children of a document node, including text nodes.
-- This function is not recursive, so returns only direct child nodes.
-- @return iterator that returns a single Node per iteration.
function Doc:children()
  local i = 0;
  return function (a)
    i = i + 1
    return a[i];
  end, self, i;
end


--- Return the first child element of a node, if it exists.
-- This will skip text nodes.
-- @return first child Node or `nil` if there is none.
function Doc:first_childtag()
  if #self == 0 then
    return
  end
  for _, t in ipairs(self) do
    if is_tag(t) then
      return t
    end
  end
end


--- Iterator that matches tag names, and a namespace (non-recursive).
-- @tparam[opt=nil] string tag tag names to return. Returns all tags if not provided.
-- @tparam[opt=nil] string xmlns the namespace value ('xmlns' attribute) to return. If not
-- provided will match all namespaces.
-- @return iterator that returns a single Node per iteration.
function Doc:matching_tags(tag, xmlns)
  -- TODO: this doesn't make sense??? namespaces are not "xmnls", as matched below
  -- but "xmlns:name"... so should be a string-prefix match if anything...
  xmlns = xmlns or self.attr.xmlns;
  local tags = self
  local next_i = 1
  local max_i = #tags
  local node
  return function ()
      for i = next_i, max_i do
        node = tags[i];
        if (not tag or node.tag == tag) and
           (not xmlns or xmlns == node.attr.xmlns) then
          next_i = i + 1
          return node
        end
      end
    end, tags, next_i
end


--- Iterator over all child tags of a document node. This will skip over
-- text nodes.
-- @return iterator that returns a single Node per iteration.
function Doc:childtags()
  local i = 0;
  return function (a)
    local v
      repeat
        i = i + 1
        v = self[i]
        if v and type(v) == 'table' then
          return v
        end
      until not v
    end, self[1], i;
end


--- Visit child Nodes of a node and call a function, possibly modifying the document.
-- Text elements will be skipped.
-- This is not recursive, so only direct children will be passed.
-- @tparam function callback a function with signature `function(node)`, passed the node.
-- The element will be updated with the returned value, or deleted if it returns `nil`.
function Doc:maptags(callback)
  local i = 1;

  while i <= #self do
    if is_tag(self[i]) then
      local ret = callback(self[i]);
      if ret == nil then
        -- remove it
        t_remove(self, i);

      else
        -- update it
        self[i] = ret;
        i = i + 1;
      end
    else
      i = i + 1
    end
  end

  return self;
end


do
  local escape_table = {
    ["'"] = "&apos;",
    ['"'] = "&quot;",
    ["<"] = "&lt;",
    [">"] = "&gt;",
    ["&"] = "&amp;",
  }

  --- Escapes a string for safe use in xml.
  -- Handles quotes(single+double), less-than, greater-than, and ampersand.
  -- @tparam string str string value to escape
  -- @return escaped string
  -- @usage
  -- local esc = xml.xml_escape([["'<>&]])  --> "&quot;&apos;&lt;&gt;&amp;"
  function _M.xml_escape(str)
    return (s_gsub(str, "['&<>\"]", escape_table))
  end
end
local xml_escape = _M.xml_escape

do
  local escape_table = {
    quot = '"',
    apos = "'",
    lt = "<",
    gt = ">",
    amp = "&",
  }

  --- Unescapes a string from xml.
  -- Handles quotes(single+double), less-than, greater-than, and ampersand.
  -- @tparam string str string value to unescape
  -- @return unescaped string
  -- @usage
  -- local unesc = xml.xml_escape("&quot;&apos;&lt;&gt;&amp;")  --> [["'<>&]]
  function _M.xml_unescape(str)
    return (str:gsub( "&(%a+);", escape_table))
  end
end
local xml_unescape = _M.xml_unescape

-- pretty printing
-- if indent, then put each new tag on its own line
-- if attr_indent, put each new attribute on its own line
local function _dostring(t, buf, parentns, block_indent, tag_indent, attr_indent)
  local nsid = 0
  local tag = t.tag

  local lf = ""
  if tag_indent then
    lf = '\n'..block_indent
  end

  local alf = " "
  if attr_indent then
    alf = '\n'..block_indent..attr_indent
  end

  t_insert(buf, lf.."<"..tag)

  local function write_attr(k,v)
    if s_find(k, "\1", 1, true) then
      nsid = nsid + 1
      local ns, attrk = s_match(k, "^([^\1]*)\1?(.*)$")
      t_insert(buf, " xmlns:ns"..nsid.."='"..xml_escape(ns).."' ".."ns"..nsid..":"..attrk.."='"..xml_escape(v).."'")

    elseif not (k == "xmlns" and v == parentns) then
      t_insert(buf, alf..k.."='"..xml_escape(v).."'");
    end
  end

  -- it's useful for testing to have predictable attribute ordering, if available
  if #t.attr > 0 then
    -- TODO: the key-value list is leading, what if they are not in-sync
    for _,k in ipairs(t.attr) do
      write_attr(k,t.attr[k])
    end
  else
    for k, v in pairs(t.attr) do
      write_attr(k,v)
    end
  end

  local len = #t
  local has_children

  if len == 0 then
    t_insert(buf, attr_indent and '\n'..block_indent.."/>" or "/>")

  else
    t_insert(buf, ">");

    for n = 1, len do
      local child = t[n]

      if child.tag then
        has_children = true
        _dostring(child, buf, t.attr.xmlns, block_indent and block_indent..tag_indent, tag_indent, attr_indent)

      else
        -- text element
        t_insert(buf, xml_escape(child))
      end
    end

    t_insert(buf, (has_children and lf or '').."</"..tag..">");
  end
end

--- Function to pretty-print an XML document.
-- @param doc an XML document
-- @tparam[opt] string|int b_ind an initial block-indent (required when `t_ind` is set)
-- @tparam[opt] string|int t_ind an tag-indent for each level (required when `a_ind` is set)
-- @tparam[opt] string|int a_ind if given, indent each attribute pair and put on a separate line
-- @tparam[opt] string|bool xml_preface force prefacing with default or custom <?xml...>, if truthy then `&lt;?xml version='1.0'?&gt;` will be used as default.
-- @return a string representation
-- @see Doc:tostring
function _M.tostring(doc, b_ind, t_ind, a_ind, xml_preface)
  local buf = {}

  if type(b_ind) == "number" then b_ind = (" "):rep(b_ind) end
  if type(t_ind) == "number" then t_ind = (" "):rep(t_ind) end
  if type(a_ind) == "number" then a_ind = (" "):rep(a_ind) end

  if xml_preface then
    if type(xml_preface) == "string" then
      buf[1] = xml_preface
    else
      buf[1] = "<?xml version='1.0'?>"
    end
  end

  _dostring(doc, buf, nil, b_ind, t_ind, a_ind, xml_preface)

  return t_concat(buf)
end


Doc.__tostring = _M.tostring


--- Method to pretty-print an XML document.
-- Invokes `xml.tostring`.
-- @tparam[opt] string|int b_ind an initial indent (required when `t_ind` is set)
-- @tparam[opt] string|int t_ind an indent for each level (required when `a_ind` is set)
-- @tparam[opt] string|int a_ind if given, indent each attribute pair and put on a separate line
-- @tparam[opt="&lt;?xml version='1.0'?&gt;"] string xml_preface force prefacing with default or custom <?xml...>
-- @return a string representation
-- @see xml.tostring
function Doc:tostring(b_ind, t_ind, a_ind, xml_preface)
  return _M.tostring(self, b_ind, t_ind, a_ind, xml_preface)
end


--- get the full text value of an element.
-- @return a single string with all text elements concatenated
-- @usage
-- local doc = xml.new("main")
-- doc:text("one")
-- doc:add_child(xml.elem "two")
-- doc:text("three")
--
-- local t = doc:get_text()    -->  "onethree"
function Doc:get_text()
  local res = {}
  for i,el in ipairs(self) do
    if is_text(el) then t_insert(res,el) end
  end
  return t_concat(res);
end


do
  local function _copy(object, kind, parent, strsubst, lookup_table)
    if type(object) ~= "table" then
      if strsubst and is_text(object) then
        return strsubst(object, kind, parent)
      else
        return object
      end
    end

    if lookup_table[object] then
      error("recursion detected")
    end
    lookup_table[object] = true

    local new_table = {}
    lookup_table[object] = new_table

    local tag = object.tag
    new_table.tag = _copy(tag, '*TAG', parent, strsubst, lookup_table)

    if object.attr then
      local res = {}
      for attr, value in pairs(object.attr) do
        if type(attr) == "string" then
          res[attr] = _copy(value, attr, object, strsubst, lookup_table)
        end
      end
      new_table.attr = res
    end

    for index = 1, #object do
      local v = _copy(object[index], '*TEXT', object, strsubst, lookup_table)
      t_insert(new_table,v)
    end

    return setmetatable(new_table, getmetatable(object))
  end

  --- Returns a copy of a document.
  -- The `strsubst` parameter is a callback with signature `function(object, kind, parent)`.
  --
  -- Param `kind` has the following values, and parameters:
  --
  -- - `"*TAG"`: `object` is the tag-name, `parent` is the Node object. Returns the new tag name.
  --
  -- - `"*TEXT"`: `object` is the text-element, `parent` is the Node object. Returns the new text value.
  --
  -- - other strings not prefixed with `*`: `kind` is the attribute name, `object` is the
  --   attribute value, `parent` is the Node object. Returns the new attribute value.
  --
  -- @tparam Node|string doc a Node object or string (text node)
  -- @tparam[opt] function strsubst an optional function for handling string copying
  -- which could do substitution, etc.
  -- @return copy of the document
  -- @see Doc:filter
  function _M.clone(doc, strsubst)
    return _copy(doc, nil, nil, strsubst, {})
  end
end


--- Returns a copy of a document.
-- This is the method version of `xml.clone`.
-- @see xml.clone
-- @name Doc:filter
-- @tparam[opt] function strsubst an optional function for handling string copying
Doc.filter = _M.clone -- also available as method

do
  local function _compare(t1, t2, recurse_check)

    local ty1 = type(t1)
    local ty2 = type(t2)

    if ty1 ~= ty2 then
      return false, 'type mismatch'
    end

    if ty1 == 'string' then
      if t1 == t2 then
        return true
      else
        return false, 'text '..t1..' ~= text '..t2
      end
    end

    if ty1 ~= 'table' or ty2 ~= 'table' then
      return false, 'not a document'
    end

    if recurse_check[t1] then
      return false, "recursive document"
    end
    recurse_check[t1] = true

    if t1.tag ~= t2.tag then
      return false, 'tag  '..t1.tag..' ~= tag '..t2.tag
    end

    if #t1 ~= #t2 then
      return false, 'size '..#t1..' ~= size '..#t2..' for tag '..t1.tag
    end

    -- compare attributes
    for k,v in pairs(t1.attr) do
      local t2_value = t2.attr[k]
      if type(k) == "string" then
        if t2_value ~= v then return false, 'mismatch attrib' end
      else
        if t2_value ~= nil and t2_value ~= v then return false, "mismatch attrib order" end
      end
    end
    for k,v in pairs(t2.attr) do
      local t1_value = t1.attr[k]
      if type(k) == "string" then
        if t1_value ~= v then return false, 'mismatch attrib' end
      else
        if t1_value ~= nil and t1_value ~= v then return false, "mismatch attrib order" end
      end
    end

    -- compare children
    for i = 1, #t1 do
      local ok, err = _compare(t1[i], t2[i], recurse_check)
      if not ok then
        return ok, err
      end
    end
    return true
  end

  --- Compare two documents or elements.
  -- Equality is based on tag, child nodes (text and tags), attributes and order
  -- of those (order only fails if both are given, and not equal).
  -- @tparam Node|string t1 a Node object or string (text node)
  -- @tparam Node|string t2 a Node object or string (text node)
  -- @treturn boolean `true` when the Nodes are equal.
  function _M.compare(t1,t2)
    return _compare(t1, t2, {})
  end
end


--- is this value a document element?
-- @param d any value
-- @treturn boolean `true` if it is a `table` with property `tag` being a string value.
-- @name is_tag
_M.is_tag = is_tag


do
  local function _walk(doc, depth_first, operation, recurse_check)
    if not depth_first then operation(doc.tag, doc) end
    for _,d in ipairs(doc) do
      if is_tag(d) then
        assert(not recurse_check[d], "recursion detected")
        recurse_check[d] = true
        _walk(d, depth_first, operation, recurse_check)
      end
    end
    if depth_first then operation(doc.tag, doc) end
  end

  --- Calls a function recursively over Nodes in the document.
  -- Will only call on tags, it will skip text nodes.
  -- The function signature for `operation` is `function(tag_name, Node)`.
  -- @tparam Node|string doc a Node object or string (text node)
  -- @tparam boolean depth_first visit child nodes first, then the current node
  -- @tparam function operation a function which will receive the current tag name and current node.
  function _M.walk(doc, depth_first, operation)
    return _walk(doc, depth_first, operation, {})
  end
end


local html_empty_elements = { --lists all HTML empty (void) elements
    br      = true,
    img     = true,
    meta    = true,
    frame   = true,
    area    = true,
    hr      = true,
    base    = true,
    col     = true,
    link    = true,
    input   = true,
    option  = true,
    param   = true,
    isindex = true,
    embed = true,
}

--- Parse a well-formed HTML file as a string.
-- Tags are case-insensitive, DOCTYPE is ignored, and empty elements can be .. empty.
-- @param s the HTML
function _M.parsehtml(s)
    return _M.basic_parse(s,false,true)
end

--- Parse a simple XML document using a pure Lua parser based on Robero Ierusalimschy's original version.
-- @param s the XML document to be parsed.
-- @param all_text  if true, preserves all whitespace. Otherwise only text containing non-whitespace is included.
-- @param html if true, uses relaxed HTML rules for parsing
function _M.basic_parse(s, all_text, html)
    local stack = {}
    local top = {}

    local function parseargs(s)
      local arg = {}
      s:gsub("([%w:%-_]+)%s*=%s*([\"'])(.-)%2", function (w, _, a)
        if html then w = w:lower() end
        arg[w] = xml_unescape(a)
      end)
      if html then
        s:gsub("([%w:%-_]+)%s*=%s*([^\"']+)%s*", function (w, a)
          w = w:lower()
          arg[w] = xml_unescape(a)
        end)
      end
      return arg
    end

    t_insert(stack, top)
    local ni,c,label,xarg, empty, _, istart
    local i = 1
    local j
    -- we're not interested in <?xml version="1.0"?>
    _,istart = s_find(s,'^%s*<%?[^%?]+%?>%s*')
    if not istart then -- or <!DOCTYPE ...>
        _,istart = s_find(s,'^%s*<!DOCTYPE.->%s*')
    end
    if istart then i = istart+1 end
    while true do
        ni,j,c,label,xarg, empty = s_find(s, "<([%/!]?)([%w:%-_]+)(.-)(%/?)>", i)
        if not ni then break end
        if c == "!" then -- comment
            -- case where there's no space inside comment
            if not (label:match '%-%-$' and xarg == '') then
                if xarg:match '%-%-$' then -- we've grabbed it all
                    j = j - 2
                end
                -- match end of comment
                _,j = s_find(s, "-->", j, true)
            end
        else
            local text = s_sub(s, i, ni-1)
            if html then
                label = label:lower()
                if html_empty_elements[label] then empty = "/" end
            end
            if all_text or not s_find(text, "^%s*$") then
                t_insert(top, xml_unescape(text))
            end
            if empty == "/" then  -- empty element tag
                t_insert(top, setmetatable({tag=label, attr=parseargs(xarg), empty=1},Doc))
            elseif c == "" then   -- start tag
                top = setmetatable({tag=label, attr=parseargs(xarg)},Doc)
                t_insert(stack, top)   -- new level
            else  -- end tag
                local toclose = t_remove(stack)  -- remove top
                top = stack[#stack]
                if #stack < 1 then
                    error("nothing to close with "..label..':'..text)
                end
                if toclose.tag ~= label then
                    error("trying to close "..toclose.tag.." with "..label.." "..text)
                end
                t_insert(top, toclose)
            end
        end
        i = j+1
    end
    local text = s_sub(s, i)
    if all_text or  not s_find(text, "^%s*$") then
        t_insert(stack[#stack], xml_unescape(text))
    end
    if #stack > 1 then
        error("unclosed "..stack[#stack].tag)
    end
    local res = stack[1]
    return is_text(res[1]) and res[2] or res[1]
end

do
  local match do

    local function empty(attr) return not attr or not next(attr) end

    local append_capture do
      -- returns the key,value pair from a table if it has exactly one entry
      local function has_one_element(t)
          local key,value = next(t)
          if next(t,key) ~= nil then return false end
          return key,value
      end

      function append_capture(res,tbl)
          if not empty(tbl) then -- no point in capturing empty tables...
              local key
              if tbl._ then  -- if $_ was set then it is meant as the top-level key for the captured table
                  key = tbl._
                  tbl._ = nil
                  if empty(tbl) then return end
              end
              -- a table with only one pair {[0]=value} shall be reduced to that value
              local numkey,val = has_one_element(tbl)
              if numkey == 0 then tbl = val end
              if key then
                  res[key] = tbl
              else -- otherwise, we append the captured table
                  t_insert(res,tbl)
              end
          end
      end
    end

    local function make_number(pat)
        if pat:find '^%d+$' then -- $1 etc means use this as an array location
            pat = tonumber(pat)
        end
        return pat
    end

    local function capture_attrib(res,pat,value)
        pat = make_number(pat:sub(2))
        res[pat] = value
        return true
    end

    function match(d,pat,res,keep_going)
        local ret = true
        if d == nil then d = '' end --return false end
        -- attribute string matching is straight equality, except if the pattern is a $ capture,
        -- which always succeeds.
        if is_text(d) then
            if not is_text(pat) then return false end
            if _M.debug then print(d,pat) end
            if pat:find '^%$' then
                return capture_attrib(res,pat,d)
            else
                return d == pat
            end
        else
        if _M.debug then print(d.tag,pat.tag) end
            -- this is an element node. For a match to succeed, the attributes must
            -- match as well.
            -- a tagname in the pattern ending with '-' is a wildcard and matches like an attribute
            local tagpat = pat.tag:match '^(.-)%-$'
            if tagpat then
                tagpat = make_number(tagpat)
                res[tagpat] = d.tag
            end
            if d.tag == pat.tag or tagpat then

                if not empty(pat.attr) then
                    if empty(d.attr) then ret =  false
                    else
                        for prop,pval in pairs(pat.attr) do
                            local dval = d.attr[prop]
                            if not match(dval,pval,res) then ret = false;  break end
                        end
                    end
                end
                -- the pattern may have child nodes. We match partially, so that {P1,P2} shall match {X,P1,X,X,P2,..}
                if ret and #pat > 0 then
                    local i,j = 1,1
                    local function next_elem()
                        j = j + 1  -- next child element of data
                        if is_text(d[j]) then j = j + 1 end
                        return j <= #d
                    end
                    repeat
                        local p = pat[i]
                        -- repeated {{<...>}} patterns  shall match one or more elements
                        -- so e.g. {P+} will match {X,X,P,P,X,P,X,X,X}
                        if is_tag(p) and p.repeated then
                            local found
                            repeat
                                local tbl = {}
                                ret = match(d[j],p,tbl,false)
                                if ret then
                                    found = false --true
                                    append_capture(res,tbl)
                                end
                            until not next_elem() or (found and not ret)
                            i = i + 1
                        else
                            ret = match(d[j],p,res,false)
                            if ret then i = i + 1 end
                        end
                    until not next_elem() or i > #pat -- run out of elements or patterns to match
                    -- if every element in our pattern matched ok, then it's been a successful match
                    if i > #pat then return true end
                end
                if ret then return true end
            else
                ret = false
            end
            -- keep going anyway - look at the children!
            if keep_going then
                for child in d:childtags() do
                    ret = match(child,pat,res,keep_going)
                    if ret then break end
                end
            end
        end
        return ret
    end
  end

  --- does something...
  function Doc:match(pat)
      local err
      pat,err = template_cache(pat)
      if not pat then return nil, err end
      _M.walk(pat,false,function(_,d)
          if is_text(d[1]) and is_tag(d[2]) and is_text(d[3]) and
            d[1]:find '%s*{{' and d[3]:find '}}%s*' then
            t_remove(d,1)
            t_remove(d,2)
            d[1].repeated = true
          end
      end)

      local res = {}
      local ret = match(self,pat,res,true)
      return res,ret
  end
end


return _M


end,

["pl.app"] = function()
--------------------
-- Module: 'pl.app'
--------------------
--- Application support functions.
-- See @{01-introduction.md.Application_Support|the Guide}
--
-- Dependencies: `pl.utils`, `pl.path`
-- @module pl.app

local io,package,require = _G.io, _G.package, _G.require
local utils = require 'pl.utils'
local path = require 'pl.path'

local app = {}

--- return the name of the current script running.
-- The name will be the name as passed on the command line
-- @return string filename
function app.script_name()
    if _G.arg and _G.arg[0] then
        return _G.arg[0]
    end
    return utils.raise("No script name found")
end

--- prefixes the current script's path to the Lua module path.
-- Applies to both the source and the binary module paths. It makes it easy for
-- the main file of a multi-file program to access its modules in the same directory.
-- `base` allows these modules to be put in a specified subdirectory, to allow for
-- cleaner deployment and resolve potential conflicts between a script name and its
-- library directory.
--
-- Note: the path is prefixed, so it is searched first when requiring modules.
-- @string base optional base directory (absolute, or relative path).
-- @bool nofollow always use the invocation's directory, even if the invoked file is a symlink
-- @treturn string the current script's path with a trailing slash
function app.require_here (base, nofollow)
    local p = app.script_name()
    if not path.isabs(p) then
        p = path.join(path.currentdir(),p)
    end
    if not nofollow then
      local t = path.link_attrib(p)
      if t and t.mode == 'link' then
        t = t.target
        if not path.isabs(t) then
          t = path.join(path.dirname(p), t)
        end
        p = t
      end
    end
    p = path.normpath(path.dirname(p))
    if p:sub(-1,-1) ~= path.sep then
        p = p..path.sep
    end
    if base then
        if path.is_windows then
            base = base:gsub('/','\\')
        end
        if path.isabs(base) then
            p = base .. path.sep
        else
            p = p..base..path.sep
        end
    end
    local so_ext = path.is_windows and 'dll' or 'so'
    local lsep = package.path:find '^;' and '' or ';'
    local csep = package.cpath:find '^;' and '' or ';'
    package.path = ('%s?.lua;%s?%sinit.lua%s%s'):format(p,p,path.sep,lsep,package.path)
    package.cpath = ('%s?.%s%s%s'):format(p,so_ext,csep,package.cpath)
    return p
end

--- return a suitable path for files private to this application.
-- These will look like '~/.SNAME/file', with '~' as with expanduser and
-- SNAME is the name of the script without .lua extension.
-- If the directory does not exist, it will be created.
-- @string file a filename (w/out path)
-- @return a full pathname, or nil
-- @return cannot create directory error
-- @usage
-- -- when run from a script called 'testapp' (on Windows):
-- local app = require 'pl.app'
-- print(app.appfile 'test.txt')
-- -- C:\Documents and Settings\steve\.testapp\test.txt
function app.appfile(file)
    local sfullname, err = app.script_name()
    if not sfullname then return utils.raise(err) end
    local sname = path.basename(sfullname)
    local name = path.splitext(sname)
    local dir = path.join(path.expanduser('~'),'.'..name)
    if not path.isdir(dir) then
        local ret = path.mkdir(dir)
        if not ret then return utils.raise('cannot create '..dir) end
    end
    return path.join(dir,file)
end

--- return string indicating operating system.
-- @return 'Windows','OSX' or whatever uname returns (e.g. 'Linux')
function app.platform()
    if path.is_windows then
        return 'Windows'
    else
        local f = io.popen('uname')
        local res = f:read()
        if res == 'Darwin' then res = 'OSX' end
        f:close()
        return res
    end
end

--- return the full command-line used to invoke this script.
-- It will not include the scriptname itself, see `app.script_name`.
-- @return command-line
-- @return name of Lua program used
-- @usage
-- -- execute:  lua -lluacov -e 'print(_VERSION)' myscript.lua
--
-- -- myscript.lua
-- print(require("pl.app").lua())  --> "lua -lluacov -e 'print(_VERSION)'", "lua"
function app.lua()
    local args = _G.arg
    if not args then
        return utils.raise "not in a main program"
    end

    local cmd = {}
    local i = -1
    while true do
        table.insert(cmd, 1, args[i])
        if not args[i-1] then
            return utils.quote_arg(cmd), args[i]
        end
        i = i - 1
    end
end

--- parse command-line arguments into flags and parameters.
-- Understands GNU-style command-line flags; short (`-f`) and long (`--flag`).
--
-- These may be given a value with either '=' or ':' (`-k:2`,`--alpha=3.2`,`-n2`),
-- a number value can be given without a space. If the flag is marked
-- as having a value, then a space-separated value is also accepted (`-i hello`),
-- see the `flags_with_values` argument).
--
-- Multiple short args can be combined like so: ( `-abcd`).
--
-- When specifying the `flags_valid` parameter, its contents can also contain
-- aliases, to convert short/long flags to the same output name. See the
-- example below.
--
-- Note: if a flag is repeated, the last value wins.
-- @tparam {string} args an array of strings (default is the global `arg`)
-- @tab flags_with_values any flags that take values, either list or hash
-- table e.g. `{ out=true }` or `{ "out" }`.
-- @tab flags_valid (optional) flags that are valid, either list or hashtable.
-- If not given, everything
-- will be accepted(everything in `flags_with_values` will automatically be allowed)
-- @return a table of flags (flag=value pairs)
-- @return an array of parameters
-- @raise if args is nil, then the global `args` must be available!
-- @usage
-- -- Simple form:
-- local flags, params = app.parse_args(nil,
--      { "hello", "world" },  -- list of flags taking values
--      { "l", "a", "b"})      -- list of allowed flags (value ones will be added)
--
-- -- More complex example using aliases:
-- local valid = {
--     long = "l",           -- if 'l' is specified, it is reported as 'long'
--     new = { "n", "old" }, -- here both 'n' and 'old' will go into 'new'
-- }
-- local values = {
--     "value",   -- will automatically be added to the allowed set of flags
--     "new",     -- will mark 'n' and 'old' as requiring a value as well
-- }
-- local flags, params = app.parse_args(nil, values, valid)
--
-- -- command:  myapp.lua -l --old:hello --value world param1 param2
-- -- will yield:
-- flags = {
--     long = true,     -- input from 'l'
--     new = "hello",   -- input from 'old'
--     value = "world", -- allowed because it was in 'values', note: space separated!
-- }
-- params = {
--     [1] = "param1"
--     [2] = "param2"
-- }
function app.parse_args (args,flags_with_values, flags_valid)
    if not args then
        args = _G.arg
        if not args then utils.raise "Not in a main program: 'arg' not found" end
    end

    local with_values = {}
    for k,v in pairs(flags_with_values or {}) do
        if type(k) == "number" then
            k = v
        end
        with_values[k] = true
    end

    local valid
    if not flags_valid then
        -- if no allowed flags provided, we create a table that always returns
        -- the keyname, no matter what you look up
        valid = setmetatable({},{ __index = function(_, key) return key end })
    else
        valid = {}
        for k,aliases in pairs(flags_valid) do
            if type(k) == "number" then         -- array/list entry
                k = aliases
            end
            if type(aliases) == "string" then  -- single alias
                aliases = { aliases }
            end
            if type(aliases) == "table" then   -- list of aliases
                -- it's the alternate name, so add the proper mappings
                for i, alias in ipairs(aliases) do
                    valid[alias] = k
                end
            end
            valid[k] = k
        end
        do
            local new_with_values = {}  -- needed to prevent "invalid key to 'next'" error
            for k,v in pairs(with_values) do
                if not valid[k] then
                    valid[k] = k   -- add the with_value entry as a valid one
                    new_with_values[k] = true
                else
                    new_with_values[valid[k]] = true  --set, but by its alias
                end
            end
            with_values = new_with_values
        end
    end

    -- now check that all flags with values are reported as such under all
    -- of their aliases
    for k, main_alias in pairs(valid) do
        if with_values[main_alias] then
            with_values[k] = true
        end
    end

    local _args = {}
    local flags = {}
    local i = 1
    while i <= #args do
        local a = args[i]
        local v = a:match('^-(.+)')
        local is_long
        if not v then
            -- we have a parameter
            _args[#_args+1] = a
        else
            -- it's a flag
            if v:find '^-' then
                is_long = true
                v = v:sub(2)
            end
            if with_values[v] then
                if i == #args or args[i+1]:find '^-' then
                    return utils.raise ("no value for '"..v.."'")
                end
                flags[valid[v]] = args[i+1]
                i = i + 1
            else
                -- a value can also be indicated with = or :
                local var,val =  utils.splitv (v,'[=:]', false, 2)
                var = var or v
                val = val or true
                if not is_long then
                    if #var > 1 then
                        if var:find '.%d+' then -- short flag, number value
                            val = var:sub(2)
                            var = var:sub(1,1)
                        else -- multiple short flags
                            for i = 1,#var do
                                local f = var:sub(i,i)
                                if not valid[f] then
                                    return utils.raise("unknown flag '"..f.."'")
                                else
                                    f = valid[f]
                                end
                                flags[f] = true
                            end
                            val = nil -- prevents use of var as a flag below
                        end
                    else  -- single short flag (can have value, defaults to true)
                        val = val or true
                    end
                end
                if val then
                    if not valid[var] then
                        return utils.raise("unknown flag '"..var.."'")
                    else
                        var = valid[var]
                    end
                    flags[var] = val
                end
            end
        end
        i = i + 1
    end
    return flags,_args
end

return app

end,

["pl.array2d"] = function()
--------------------
-- Module: 'pl.array2d'
--------------------
--- Operations on two-dimensional arrays.
-- See @{02-arrays.md.Operations_on_two_dimensional_tables|The Guide}
--
-- The size of the arrays is determined by using the length operator `#` hence
-- the module is not `nil` safe, and the usual precautions apply.
--
-- Note: all functions taking `i1,j1,i2,j2` as arguments will normalize the
-- arguments using `default_range`.
--
-- Dependencies: `pl.utils`, `pl.tablex`, `pl.types`
-- @module pl.array2d

local tonumber,tostring,io,ipairs,string,table =
    _G.tonumber,_G.tostring,_G.io,_G.ipairs,_G.string,_G.table
local setmetatable,getmetatable = setmetatable,getmetatable

local tablex = require 'pl.tablex'
local utils = require 'pl.utils'
local types = require 'pl.types'
local imap,tmap,reduce,keys,tmap2,tset,index_by = tablex.imap,tablex.map,tablex.reduce,tablex.keys,tablex.map2,tablex.set,tablex.index_by
local remove = table.remove
local splitv,fprintf,assert_arg = utils.splitv,utils.fprintf,utils.assert_arg
local byte = string.byte
local stdout = io.stdout
local min = math.min


local array2d = {}

local function obj (int,out)
    local mt = getmetatable(int)
    if mt then
        setmetatable(out,mt)
    end
    return out
end

local function makelist (res)
    return setmetatable(res, require('pl.List'))
end

--- return the row and column size.
-- Size is calculated using the Lua length operator #, so usual precautions
-- regarding `nil` values apply.
-- @array2d a a 2d array
-- @treturn int number of rows (`#a`)
-- @treturn int number of cols (`#a[1]`)
function array2d.size (a)
    assert_arg(1,a,'table')
    return #a,#a[1]
end

do
    local function index (t,k)
        return t[k]
    end

    --- extract a column from the 2D array.
    -- @array2d a 2d array
    -- @param j column index
    -- @return 1d array
    function array2d.column (a,j)
        assert_arg(1,a,'table')
        return makelist(imap(index,a,j))
    end
end
local column = array2d.column

--- extract a row from the 2D array.
-- Added in line with `column`, for read-only purposes directly
-- accessing a[i] is more performant.
-- @array2d a 2d array
-- @param i row index
-- @return 1d array (copy of the row)
function array2d.row(a,i)
    assert_arg(1,a,'table')
    local row = a[i]
    local r = {}
    for n,v in ipairs(row) do
        r[n] = v
    end
    return makelist(r)
end

--- map a function over a 2D array
-- @func f a function of at least one argument
-- @array2d a 2d array
-- @param arg an optional extra argument to be passed to the function.
-- @return 2d array
function array2d.map (f,a,arg)
    assert_arg(2,a,'table')
    f = utils.function_arg(1,f)
    return obj(a,imap(function(row) return imap(f,row,arg) end, a))
end

--- reduce the rows using a function.
-- @func f a binary function
-- @array2d a 2d array
-- @return 1d array
-- @see pl.tablex.reduce
function array2d.reduce_rows (f,a)
    assert_arg(1,a,'table')
    return tmap(function(row) return reduce(f,row) end, a)
end

--- reduce the columns using a function.
-- @func f a binary function
-- @array2d a 2d array
-- @return 1d array
-- @see pl.tablex.reduce
function array2d.reduce_cols (f,a)
    assert_arg(1,a,'table')
    return tmap(function(c) return reduce(f,column(a,c)) end, keys(a[1]))
end

--- reduce a 2D array into a scalar, using two operations.
-- @func opc operation to reduce the final result
-- @func opr operation to reduce the rows
-- @param a 2D array
function array2d.reduce2 (opc,opr,a)
    assert_arg(3,a,'table')
    local tmp = array2d.reduce_rows(opr,a)
    return reduce(opc,tmp)
end

--- map a function over two arrays.
-- They can be both or either 2D arrays
-- @func f function of at least two arguments
-- @int ad order of first array (`1` if `a` is a list/array, `2` if it is a 2d array)
-- @int bd order of second array (`1` if `b` is a list/array, `2` if it is a 2d array)
-- @tab a 1d or 2d array
-- @tab b 1d or 2d array
-- @param arg optional extra argument to pass to function
-- @return 2D array, unless both arrays are 1D
function array2d.map2 (f,ad,bd,a,b,arg)
    assert_arg(1,a,'table')
    assert_arg(2,b,'table')
    f = utils.function_arg(1,f)
    if ad == 1 and bd == 2 then
        return imap(function(row)
            return tmap2(f,a,row,arg)
        end, b)
    elseif ad == 2 and bd == 1 then
        return imap(function(row)
            return tmap2(f,row,b,arg)
        end, a)
    elseif ad == 1 and bd == 1 then
        return tmap2(f,a,b)
    elseif ad == 2 and bd == 2 then
        return tmap2(function(rowa,rowb)
            return tmap2(f,rowa,rowb,arg)
        end, a,b)
    end
end

--- cartesian product of two 1d arrays.
-- @func f a function of 2 arguments
-- @array t1 a 1d table
-- @array t2 a 1d table
-- @return 2d table
-- @usage product('..',{1,2},{'a','b'}) == {{'1a','2a'},{'1b','2b'}}
function array2d.product (f,t1,t2)
    f = utils.function_arg(1,f)
    assert_arg(2,t1,'table')
    assert_arg(3,t2,'table')
    local res = {}
    for i,v in ipairs(t2) do
        res[i] = tmap(f,t1,v)
    end
    return res
end

--- flatten a 2D array.
-- (this goes over columns first.)
-- @array2d t 2d table
-- @return a 1d table
-- @usage flatten {{1,2},{3,4},{5,6}} == {1,2,3,4,5,6}
function array2d.flatten (t)
    local res = {}
    local k = 1
    local rows, cols = array2d.size(t)
    for r = 1, rows do
        local row = t[r]
        for c = 1, cols do
            res[k] = row[c]
            k = k + 1
        end
    end
    return makelist(res)
end

--- reshape a 2D array. Reshape the array by specifying a new nr of rows.
-- @array2d t 2d array
-- @int nrows new number of rows
-- @bool co use column-order (Fortran-style) (default false)
-- @return a new 2d array
function array2d.reshape (t,nrows,co)
    local nr,nc = array2d.size(t)
    local ncols = nr*nc / nrows
    local res = {}
    local ir,ic = 1,1
    for i = 1,nrows do
        local row = {}
        for j = 1,ncols do
            row[j] = t[ir][ic]
            if not co then
                ic = ic + 1
                if ic > nc then
                    ir = ir + 1
                    ic = 1
                end
            else
                ir = ir + 1
                if ir > nr then
                    ic = ic + 1
                    ir = 1
                end
            end
        end
        res[i] = row
    end
    return obj(t,res)
end

--- transpose a 2D array.
-- @array2d t 2d array
-- @return a new 2d array
function array2d.transpose(t)
  assert_arg(1,t,'table')
  local _, c = array2d.size(t)
  return array2d.reshape(t,c,true)
end

--- swap two rows of an array.
-- @array2d t a 2d array
-- @int i1 a row index
-- @int i2 a row index
-- @return t (same, modified 2d array)
function array2d.swap_rows (t,i1,i2)
    assert_arg(1,t,'table')
    t[i1],t[i2] = t[i2],t[i1]
    return t
end

--- swap two columns of an array.
-- @array2d t a 2d array
-- @int j1 a column index
-- @int j2 a column index
-- @return t (same, modified 2d array)
function array2d.swap_cols (t,j1,j2)
    assert_arg(1,t,'table')
    for _, row in ipairs(t) do
        row[j1],row[j2] = row[j2],row[j1]
    end
    return t
end

--- extract the specified rows.
-- @array2d t 2d array
-- @tparam {int} ridx a table of row indices
-- @return a new 2d array with the extracted rows
function array2d.extract_rows (t,ridx)
    return obj(t,index_by(t,ridx))
end

--- extract the specified columns.
-- @array2d t 2d array
-- @tparam {int} cidx a table of column indices
-- @return a new 2d array with the extracted columns
function array2d.extract_cols (t,cidx)
    assert_arg(1,t,'table')
    local res = {}
    for i = 1,#t do
        res[i] = index_by(t[i],cidx)
    end
    return obj(t,res)
end

--- remove a row from an array.
-- @function array2d.remove_row
-- @array2d t a 2d array
-- @int i a row index
array2d.remove_row = remove

--- remove a column from an array.
-- @array2d t a 2d array
-- @int j a column index
function array2d.remove_col (t,j)
    assert_arg(1,t,'table')
    for i = 1,#t do
        remove(t[i],j)
    end
end

do
    local function _parse (s)
        local r, c = s:match 'R(%d+)C(%d+)'
        if r then
            r,c = tonumber(r),tonumber(c)
            return r,c
        end
        c,r = s:match '(%a+)(%d+)'
        if c then
            local cv = 0
            for i = 1, #c do
              cv = cv * 26 + byte(c:sub(i,i)) - byte 'A' + 1
            end
            return tonumber(r), cv
        end
        error('bad cell specifier: '..s)
    end

    --- parse a spreadsheet range or cell.
    -- The range/cell can be specified either as 'A1:B2' or 'R1C1:R2C2' or for
    -- single cells as 'A1' or 'R1C1'.
    -- @string s a range (case insensitive).
    -- @treturn int start row
    -- @treturn int start col
    -- @treturn int end row (or `nil` if the range was a single cell)
    -- @treturn int end col (or `nil` if the range was a single cell)
    function array2d.parse_range (s)
        assert_arg(1,s,'string')
        s = s:upper()
        if s:find ':' then
            local start,finish = splitv(s,':')
            local i1,j1 = _parse(start)
            local i2,j2 = _parse(finish)
            return i1,j1,i2,j2
        else -- single value
            local i,j = _parse(s)
            return i,j
        end
    end
end

--- get a slice of a 2D array.
-- Same as `slice`.
-- @see slice
function array2d.range (...)
    return array2d.slice(...)
end

local default_range do
    local function norm_value(v, max)
        if not v then return v end
        if v < 0 then
            v = max + v + 1
        end
        if v < 1 then v = 1 end
        if v > max then v = max end
        return v
    end

    --- normalizes coordinates to valid positive entries and defaults.
    -- Negative indices will be counted from the end, too low, or too high
    -- will be limited by the array sizes.
    -- @array2d t a 2D array
    -- @tparam[opt=1] int|string i1 start row or spreadsheet range passed to `parse_range`
    -- @tparam[opt=1] int j1 start col
    -- @tparam[opt=N] int i2 end row
    -- @tparam[opt=M] int j2 end col
    -- @see parse_range
    -- @return i1, j1, i2, j2
    function array2d.default_range (t,i1,j1,i2,j2)
        if (type(i1) == 'string') and not (j1 or i2 or j2) then
            i1, j1, i2, j2 = array2d.parse_range(i1)
        end
        local nr, nc = array2d.size(t)
        i1 = norm_value(i1 or 1, nr)
        j1 = norm_value(j1 or 1, nc)
        i2 = norm_value(i2 or nr, nr)
        j2 = norm_value(j2 or nc, nc)
        return i1,j1,i2,j2
    end
    default_range = array2d.default_range
end

--- get a slice of a 2D array. Note that if the specified range has
-- a 1D result, the rank of the result will be 1.
-- @array2d t a 2D array
-- @tparam[opt=1] int|string i1 start row or spreadsheet range passed to `parse_range`
-- @tparam[opt=1] int j1 start col
-- @tparam[opt=N] int i2 end row
-- @tparam[opt=M] int j2 end col
-- @see parse_range
-- @return an array, 2D in general but 1D in special cases.
function array2d.slice (t,i1,j1,i2,j2)
    assert_arg(1,t,'table')
    i1,j1,i2,j2 = default_range(t,i1,j1,i2,j2)
    local res = {}
    for i = i1,i2 do
        local val
        local row = t[i]
        if j1 == j2 then
            val = row[j1]
        else
            val = {}
            for j = j1,j2 do
                val[#val+1] = row[j]
            end
        end
        res[#res+1] = val
    end
    if i1 == i2 then res = res[1] end
    return obj(t,res)
end

--- set a specified range of an array to a value.
-- @array2d t a 2D array
-- @param value the value (may be a function, called as `val(i,j)`)
-- @tparam[opt=1] int|string i1 start row or spreadsheet range passed to `parse_range`
-- @tparam[opt=1] int j1 start col
-- @tparam[opt=N] int i2 end row
-- @tparam[opt=M] int j2 end col
-- @see parse_range
-- @see tablex.set
function array2d.set (t,value,i1,j1,i2,j2)
    i1,j1,i2,j2 = default_range(t,i1,j1,i2,j2)
    local i = i1
    if types.is_callable(value) then
        local old_f = value
        value = function(j)
            return old_f(i,j)
        end
    end
    while i <= i2 do
        tset(t[i],value,j1,j2)
        i = i + 1
    end
end

--- write a 2D array to a file.
-- @array2d t a 2D array
-- @param f a file object (default stdout)
-- @string fmt a format string (default is just to use tostring)
-- @tparam[opt=1] int|string i1 start row or spreadsheet range passed to `parse_range`
-- @tparam[opt=1] int j1 start col
-- @tparam[opt=N] int i2 end row
-- @tparam[opt=M] int j2 end col
-- @see parse_range
function array2d.write (t,f,fmt,i1,j1,i2,j2)
    assert_arg(1,t,'table')
    f = f or stdout
    local rowop
    if fmt then
        rowop = function(row,j) fprintf(f,fmt,row[j]) end
    else
        rowop = function(row,j) f:write(tostring(row[j]),' ') end
    end
    local function newline()
        f:write '\n'
    end
    array2d.forall(t,rowop,newline,i1,j1,i2,j2)
end

--- perform an operation for all values in a 2D array.
-- @array2d t 2D array
-- @func row_op function to call on each value; `row_op(row,j)`
-- @func end_row_op function to call at end of each row; `end_row_op(i)`
-- @tparam[opt=1] int|string i1 start row or spreadsheet range passed to `parse_range`
-- @tparam[opt=1] int j1 start col
-- @tparam[opt=N] int i2 end row
-- @tparam[opt=M] int j2 end col
-- @see parse_range
function array2d.forall (t,row_op,end_row_op,i1,j1,i2,j2)
    assert_arg(1,t,'table')
    i1,j1,i2,j2 = default_range(t,i1,j1,i2,j2)
    for i = i1,i2 do
        local row = t[i]
        for j = j1,j2 do
            row_op(row,j)
        end
        if end_row_op then end_row_op(i) end
    end
end

---- move a block from the destination to the source.
-- @array2d dest a 2D array
-- @int di start row in dest
-- @int dj start col in dest
-- @array2d src a 2D array
-- @tparam[opt=1] int|string i1 start row or spreadsheet range passed to `parse_range`
-- @tparam[opt=1] int j1 start col
-- @tparam[opt=N] int i2 end row
-- @tparam[opt=M] int j2 end col
-- @see parse_range
function array2d.move (dest,di,dj,src,i1,j1,i2,j2)
    assert_arg(1,dest,'table')
    assert_arg(4,src,'table')
    i1,j1,i2,j2 = default_range(src,i1,j1,i2,j2)
    local nr,nc = array2d.size(dest)
    i2, j2 = min(nr,i2), min(nc,j2)
    --i1, j1 = max(1,i1), max(1,j1)
    dj = dj - 1
    for i = i1,i2 do
        local drow, srow = dest[i+di-1], src[i]
        for j = j1,j2 do
            drow[j+dj] = srow[j]
        end
    end
end

--- iterate over all elements in a 2D array, with optional indices.
-- @array2d a 2D array
-- @bool indices with indices (default false)
-- @tparam[opt=1] int|string i1 start row or spreadsheet range passed to `parse_range`
-- @tparam[opt=1] int j1 start col
-- @tparam[opt=N] int i2 end row
-- @tparam[opt=M] int j2 end col
-- @see parse_range
-- @return either `value` or `i,j,value` depending on the value of `indices`
function array2d.iter(a,indices,i1,j1,i2,j2)
    assert_arg(1,a,'table')
    i1,j1,i2,j2 = default_range(a,i1,j1,i2,j2)
    local i,j = i1,j1-1
    local row = a[i]
    return function()
        j = j + 1
        if j > j2 then
            j = j1
            i = i + 1
            row = a[i]
            if i > i2 then
                return nil
            end
        end
        if indices then
            return i,j,row[j]
        else
            return row[j]
        end
    end
end

--- iterate over all columns.
-- @array2d a a 2D array
-- @return column, column-index
function array2d.columns(a)
  assert_arg(1,a,'table')
  local n = #a[1]
  local i = 0
  return function()
      i = i + 1
      if i > n then return nil end
      return column(a,i), i
  end
end

--- iterate over all rows.
-- Returns a copy of the row, for read-only purposes directly iterating
-- is more performant; `ipairs(a)`
-- @array2d a a 2D array
-- @return row, row-index
function array2d.rows(a)
  assert_arg(1,a,'table')
  local n = #a
  local i = 0
  return function()
      i = i + 1
      if i > n then return nil end
      return array2d.row(a,i), i
  end
end

--- new array of specified dimensions
-- @int rows number of rows
-- @int cols number of cols
-- @param val initial value; if it's a function then use `val(i,j)`
-- @return new 2d array
function array2d.new(rows,cols,val)
    local res = {}
    local fun = types.is_callable(val)
    for i = 1,rows do
        local row = {}
        if fun then
            for j = 1,cols do row[j] = val(i,j) end
        else
            for j = 1,cols do row[j] = val end
        end
        res[i] = row
    end
    return res
end

return array2d

end,

["pl.class"] = function()
--------------------
-- Module: 'pl.class'
--------------------
--- Provides a reusable and convenient framework for creating classes in Lua.
-- Two possible notations:
--
--    B = class(A)
--    class.B(A)
--
-- The latter form creates a named class within the current environment. Note
-- that this implicitly brings in `pl.utils` as a dependency.
--
-- See the Guide for further @{01-introduction.md.Simplifying_Object_Oriented_Programming_in_Lua|discussion}
-- @module pl.class

local error, getmetatable, io, pairs, rawget, rawset, setmetatable, tostring, type =
    _G.error, _G.getmetatable, _G.io, _G.pairs, _G.rawget, _G.rawset, _G.setmetatable, _G.tostring, _G.type
local compat

-- this trickery is necessary to prevent the inheritance of 'super' and
-- the resulting recursive call problems.
local function call_ctor (c,obj,...)
    local init = rawget(c,'_init')
    local parent_with_init = rawget(c,'_parent_with_init')

    if parent_with_init then
        if not init then -- inheriting an init
            init = rawget(parent_with_init, '_init')
            parent_with_init = rawget(parent_with_init, '_parent_with_init')
        end
        if parent_with_init then -- super() points to one above wherever _init came from
            rawset(obj,'super',function(obj,...)
                call_ctor(parent_with_init,obj,...)
            end)
        end
    else
        -- Without this, calling super() where none exists will sometimes loop and stack overflow
        rawset(obj,'super',nil)
    end

    local res = init(obj,...)
    if parent_with_init then -- If this execution of call_ctor set a super, unset it
        rawset(obj,'super',nil)
    end

    return res
end

--- initializes an __instance__ upon creation.
-- @function class:_init
-- @param ... parameters passed to the constructor
-- @usage local Cat = class()
-- function Cat:_init(name)
--   --self:super(name)   -- call the ancestor initializer if needed
--   self.name = name
-- end
--
-- local pussycat = Cat("pussycat")
-- print(pussycat.name)  --> pussycat

--- checks whether an __instance__ is derived from some class.
-- Works the other way around as `class_of`. It has two ways of using;
-- 1) call with a class to check against, 2) call without params.
-- @function instance:is_a
-- @param some_class class to check against, or `nil` to return the class
-- @return `true` if `instance` is derived from `some_class`, or if `some_class == nil` then
-- it returns the class table of the instance
-- @usage local pussycat = Lion()  -- assuming Lion derives from Cat
-- if pussycat:is_a(Cat) then
--   -- it's true, it is a Lion, but also a Cat
-- end
--
-- if pussycat:is_a() == Lion then
--   -- It's true
-- end
local function is_a(self,klass)
    if klass == nil then
        -- no class provided, so return the class this instance is derived from
        return getmetatable(self)
    end
    local m = getmetatable(self)
    if not m then return false end --*can't be an object!
    while m do
        if m == klass then return true end
        m = rawget(m,'_base')
    end
    return false
end

--- checks whether an __instance__ is derived from some class.
-- Works the other way around as `is_a`.
-- @function some_class:class_of
-- @param some_instance instance to check against
-- @return `true` if `some_instance` is derived from `some_class`
-- @usage local pussycat = Lion()  -- assuming Lion derives from Cat
-- if Cat:class_of(pussycat) then
--   -- it's true
-- end
local function class_of(klass,obj)
    if type(klass) ~= 'table' or not rawget(klass,'is_a') then return false end
    return klass.is_a(obj,klass)
end

--- cast an object to another class.
-- It is not clever (or safe!) so use carefully.
-- @param some_instance the object to be changed
-- @function some_class:cast
local function cast (klass, obj)
    return setmetatable(obj,klass)
end


local function _class_tostring (obj)
    local mt = obj._class
    local name = rawget(mt,'_name')
    setmetatable(obj,nil)
    local str = tostring(obj)
    setmetatable(obj,mt)
    if name then str = name ..str:gsub('table','') end
    return str
end

local function tupdate(td,ts,dont_override)
    for k,v in pairs(ts) do
        if not dont_override or td[k] == nil then
            td[k] = v
        end
    end
end

local function _class(base,c_arg,c)
    -- the class `c` will be the metatable for all its objects,
    -- and they will look up their methods in it.
    local mt = {}   -- a metatable for the class to support __call and _handler
    -- can define class by passing it a plain table of methods
    local plain = type(base) == 'table' and not getmetatable(base)
    if plain then
        c = base
        base = c._base
    else
        c = c or {}
    end

    if type(base) == 'table' then
        -- our new class is a shallow copy of the base class!
        -- but be careful not to wipe out any methods we have been given at this point!
        tupdate(c,base,plain)
        c._base = base
        -- inherit the 'not found' handler, if present
        if rawget(c,'_handler') then mt.__index = c._handler end
    elseif base ~= nil then
        error("must derive from a table type",3)
    end

    c.__index = c
    setmetatable(c,mt)
    if not plain then
        if base and rawget(base,'_init') then c._parent_with_init = base end -- For super and inherited init
        c._init = nil
    end

    if base and rawget(base,'_class_init') then
        base._class_init(c,c_arg)
    end

    -- expose a ctor which can be called by <classname>(<args>)
    mt.__call = function(class_tbl,...)
        local obj
        if rawget(c,'_create') then obj = c._create(...) end
        if not obj then obj = {} end
        setmetatable(obj,c)

        if rawget(c,'_init') or rawget(c,'_parent_with_init') then -- constructor exists
            local res = call_ctor(c,obj,...)
            if res then -- _if_ a ctor returns a value, it becomes the object...
                obj = res
                setmetatable(obj,c)
            end
        end

        if base and rawget(base,'_post_init') then
            base._post_init(obj)
        end

        return obj
    end
    -- Call Class.catch to set a handler for methods/properties not found in the class!
    c.catch = function(self, handler)
        if type(self) == "function" then
            -- called using . instead of :
            handler = self
        end
        c._handler = handler
        mt.__index = handler
    end
    c.is_a = is_a
    c.class_of = class_of
    c.cast = cast
    c._class = c

    if not rawget(c,'__tostring') then
        c.__tostring = _class_tostring
    end

    return c
end

--- create a new class, derived from a given base class.
-- Supporting two class creation syntaxes:
-- either `Name = class(base)` or `class.Name(base)`.
-- The first form returns the class directly and does not set its `_name`.
-- The second form creates a variable `Name` in the current environment set
-- to the class, and also sets `_name`.
-- @function class
-- @param base optional base class
-- @param c_arg optional parameter to class constructor
-- @param c optional table to be used as class
local class
class = setmetatable({},{
    __call = function(fun,...)
        return _class(...)
    end,
    __index = function(tbl,key)
        if key == 'class' then
            io.stderr:write('require("pl.class").class is deprecated. Use require("pl.class")\n')
            return class
        end
        compat = compat or require 'pl.compat'
        local env = compat.getfenv(2)
        return function(...)
            local c = _class(...)
            c._name = key
            rawset(env,key,c)
            return c
        end
    end
})

class.properties = class()

function class.properties._class_init(klass)
    klass.__index = function(t,key)
        -- normal class lookup!
        local v = klass[key]
        if v then return v end
        -- is it a getter?
        v = rawget(klass,'get_'..key)
        if v then
            return v(t)
        end
        -- is it a field?
        return rawget(t,'_'..key)
    end
    klass.__newindex = function (t,key,value)
        -- if there's a setter, use that, otherwise directly set table
        local p = 'set_'..key
        local setter = klass[p]
        if setter then
            setter(t,value)
        else
            rawset(t,key,value)
        end
    end
end


return class


end,

["pl.compat"] = function()
--------------------
-- Module: 'pl.compat'
--------------------
----------------
--- Lua 5.1/5.2/5.3 compatibility.
-- Injects `table.pack`, `table.unpack`, and `package.searchpath` in the global
-- environment, to make sure they are available for Lua 5.1 and LuaJIT.
--
-- All other functions are exported as usual in the returned module table.
--
-- NOTE: everything in this module is also available in `pl.utils`.
-- @module pl.compat
local compat = {}

--- boolean flag this is Lua 5.1 (or LuaJIT).
-- @field lua51
compat.lua51 = _VERSION == 'Lua 5.1'

--- boolean flag this is LuaJIT.
-- @field jit
compat.jit = (tostring(assert):match('builtin') ~= nil)

--- boolean flag this is LuaJIT with 5.2 compatibility compiled in.
-- @field jit52
if compat.jit then
    -- 'goto' is a keyword when 52 compatibility is enabled in LuaJit
    compat.jit52 = not loadstring("local goto = 1")
end

--- the directory separator character for the current platform.
-- @field dir_separator
compat.dir_separator = _G.package.config:sub(1,1)

--- boolean flag this is a Windows platform.
-- @field is_windows
compat.is_windows = compat.dir_separator == '\\'

--- execute a shell command, in a compatible and platform independent way.
-- This is a compatibility function that returns the same for Lua 5.1 and
-- Lua 5.2+.
--
-- NOTE: Windows systems can use signed 32bit integer exitcodes. Posix systems
-- only use exitcodes 0-255, anything else is undefined.
--
-- NOTE2: In Lua 5.2 and 5.3 a Windows exitcode of -1 would not properly be
-- returned, this function will return it properly for all versions.
-- @param cmd a shell command
-- @return true if successful
-- @return actual return code
function compat.execute(cmd)
    local res1,res2,res3 = os.execute(cmd)
    if res2 == "No error" and res3 == 0 and compat.is_windows then
      -- os.execute bug in Lua 5.2/5.3 not reporting -1 properly on Windows
      -- this was fixed in 5.4
      res3 = -1
    end
    if compat.lua51 and not compat.jit52 then
        if compat.is_windows then
            return res1==0,res1
        else
            res1 = res1 > 255 and res1 / 256 or res1
            return res1==0,res1
        end
    else
        if compat.is_windows then
            return res3==0,res3
        else
            return not not res1,res3
        end
    end
end

----------------
-- Load Lua code as a text or binary chunk (in a Lua 5.2 compatible way).
-- @param ld code string or loader
-- @param[opt] source name of chunk for errors
-- @param[opt] mode 'b', 't' or 'bt'
-- @param[opt] env environment to load the chunk in
-- @function compat.load

---------------
-- Get environment of a function (in a Lua 5.1 compatible way).
-- Not 100% compatible, so with Lua 5.2 it may return nil for a function with no
-- global references!
-- Based on code by [Sergey Rozhenko](http://lua-users.org/lists/lua-l/2010-06/msg00313.html)
-- @param f a function or a call stack reference
-- @function compat.getfenv

---------------
-- Set environment of a function (in a Lua 5.1 compatible way).
-- @param f a function or a call stack reference
-- @param env a table that becomes the new environment of `f`
-- @function compat.setfenv

if compat.lua51 then -- define Lua 5.2 style load()
    if not compat.jit then -- but LuaJIT's load _is_ compatible
        local lua51_load = load
        function compat.load(str,src,mode,env)
            local chunk,err
            if type(str) == 'string' then
                if str:byte(1) == 27 and not (mode or 'bt'):find 'b' then
                    return nil,"attempt to load a binary chunk"
                end
                chunk,err = loadstring(str,src)
            else
                chunk,err = lua51_load(str,src)
            end
            if chunk and env then setfenv(chunk,env) end
            return chunk,err
        end
    else
        compat.load = load
    end
    compat.setfenv, compat.getfenv = setfenv, getfenv
else
    compat.load = load
    -- setfenv/getfenv replacements for Lua 5.2
    -- by Sergey Rozhenko
    -- http://lua-users.org/lists/lua-l/2010-06/msg00313.html
    -- Roberto Ierusalimschy notes that it is possible for getfenv to return nil
    -- in the case of a function with no globals:
    -- http://lua-users.org/lists/lua-l/2010-06/msg00315.html
    function compat.setfenv(f, t)
        f = (type(f) == 'function' and f or debug.getinfo(f + 1, 'f').func)
        local name
        local up = 0
        repeat
            up = up + 1
            name = debug.getupvalue(f, up)
        until name == '_ENV' or name == nil
        if name then
            debug.upvaluejoin(f, up, function() return name end, 1) -- use unique upvalue
            debug.setupvalue(f, up, t)
        end
        if f ~= 0 then return f end
    end

    function compat.getfenv(f)
        local f = f or 0
        f = (type(f) == 'function' and f or debug.getinfo(f + 1, 'f').func)
        local name, val
        local up = 0
        repeat
            up = up + 1
            name, val = debug.getupvalue(f, up)
        until name == '_ENV' or name == nil
        return val
    end
end


--- Global exported functions (for Lua 5.1 & LuaJIT)
-- @section lua52

--- pack an argument list into a table.
-- @param ... any arguments
-- @return a table with field n set to the length
-- @function table.pack
if not table.pack then
    function table.pack (...)       -- luacheck: ignore
        return {n=select('#',...); ...}
    end
end

--- unpack a table and return the elements.
--
-- NOTE: this version does NOT honor the n field, and hence it is not nil-safe.
-- See `utils.unpack` for a version that is nil-safe.
-- @param t table to unpack
-- @param[opt] i index from which to start unpacking, defaults to 1
-- @param[opt] j index of the last element to unpack, defaults to #t
-- @return multiple return values from the table
-- @function table.unpack
-- @see utils.unpack
if not table.unpack then
    table.unpack = unpack           -- luacheck: ignore
end

--- return the full path where a file name would be matched.
-- This function was introduced in Lua 5.2, so this compatibility version
-- will be injected in Lua 5.1 engines.
-- @string name file name, possibly dotted
-- @string path a path-template in the same form as package.path or package.cpath
-- @string[opt] sep template separate character to be replaced by path separator. Default: "."
-- @string[opt] rep the path separator to use, defaults to system separator. Default; "/" on Unixes, "\" on Windows.
-- @see path.package_path
-- @function package.searchpath
-- @return on success: path of the file
-- @return on failure: nil, error string listing paths tried
if not package.searchpath then
    function package.searchpath (name,path,sep,rep)    -- luacheck: ignore
        if type(name) ~= "string" then
            error(("bad argument #1 to 'searchpath' (string expected, got %s)"):format(type(path)), 2)
        end
        if type(path) ~= "string" then
            error(("bad argument #2 to 'searchpath' (string expected, got %s)"):format(type(path)), 2)
        end
        if sep ~= nil and type(sep) ~= "string" then
            error(("bad argument #3 to 'searchpath' (string expected, got %s)"):format(type(path)), 2)
        end
        if rep ~= nil and type(rep) ~= "string" then
            error(("bad argument #4 to 'searchpath' (string expected, got %s)"):format(type(path)), 2)
        end
        sep = sep or "."
        rep = rep or compat.dir_separator
        do
          local s, e = name:find(sep, nil, true)
          while s do
            name = name:sub(1, s-1) .. rep .. name:sub(e+1, -1)
            s, e = name:find(sep, s + #rep + 1, true)
          end
        end
        local tried = {}
        for m in path:gmatch('[^;]+') do
            local nm = m:gsub('?', name)
            tried[#tried+1] = nm
            local f = io.open(nm,'r')
            if f then f:close(); return nm end
        end
        return nil, "\tno file '" .. table.concat(tried, "'\n\tno file '") .. "'"
    end
end

--- Global exported functions (for Lua < 5.4)
-- @section lua54

--- raise a warning message.
-- This functions mimics the `warn` function added in Lua 5.4.
-- @function warn
-- @param ... any arguments
if not rawget(_G, "warn") then
    local enabled = false
    local function warn(arg1, ...)
        if type(arg1) == "string" and arg1:sub(1, 1) == "@" then
            -- control message
            if arg1 == "@on" then
                enabled = true
                return
            end
            if arg1 == "@off" then
                enabled = false
                return
            end
            return -- ignore unknown control messages
        end
        if enabled then
          io.stderr:write("Lua warning: ", arg1, ...)
          io.stderr:write("\n")
        end
    end
    -- use rawset to bypass OpenResty's protection of global scope
    rawset(_G, "warn", warn)
end

return compat

end,

["pl.comprehension"] = function()
--------------------
-- Module: 'pl.comprehension'
--------------------
--- List comprehensions implemented in Lua.
--
-- See the [wiki page](http://lua-users.org/wiki/ListComprehensions)
--
--    local C= require 'pl.comprehension' . new()
--
--    C ('x for x=1,10') ()
--    ==> {1,2,3,4,5,6,7,8,9,10}
--    C 'x^2 for x=1,4' ()
--    ==> {1,4,9,16}
--    C '{x,x^2} for x=1,4' ()
--    ==> {{1,1},{2,4},{3,9},{4,16}}
--    C '2*x for x' {1,2,3}
--    ==> {2,4,6}
--    dbl = C '2*x for x'
--    dbl {10,20,30}
--    ==> {20,40,60}
--    C 'x for x if x % 2 == 0' {1,2,3,4,5}
--    ==> {2,4}
--    C '{x,y} for x = 1,2 for y = 1,2' ()
--    ==> {{1,1},{1,2},{2,1},{2,2}}
--    C '{x,y} for x for y' ({1,2},{10,20})
--    ==> {{1,10},{1,20},{2,10},{2,20}}
--    assert(C 'sum(x^2 for x)' {2,3,4} == 2^2+3^2+4^2)
--
-- (c) 2008 David Manura. Licensed under the same terms as Lua (MIT license).
--
-- Dependencies: `pl.utils`, `pl.luabalanced`
--
-- See @{07-functional.md.List_Comprehensions|the Guide}
-- @module pl.comprehension

local utils = require 'pl.utils'

local status,lb = pcall(require, "pl.luabalanced")
if not status then
    lb = require 'luabalanced'
end

local math_max = math.max
local table_concat = table.concat

-- fold operations
-- http://en.wikipedia.org/wiki/Fold_(higher-order_function)
local ops = {
  list = {init=' {} ', accum=' __result[#__result+1] = (%s) '},
  table = {init=' {} ', accum=' local __k, __v = %s __result[__k] = __v '},
  sum = {init=' 0 ', accum=' __result = __result + (%s) '},
  min = {init=' nil ', accum=' local __tmp = %s ' ..
                             ' if __result then if __tmp < __result then ' ..
                             '__result = __tmp end else __result = __tmp end '},
  max = {init=' nil ', accum=' local __tmp = %s ' ..
                             ' if __result then if __tmp > __result then ' ..
                             '__result = __tmp end else __result = __tmp end '},
}


-- Parses comprehension string expr.
-- Returns output expression list <out> string, array of for types
-- ('=', 'in' or nil) <fortypes>, array of input variable name
-- strings <invarlists>, array of input variable value strings
-- <invallists>, array of predicate expression strings <preds>,
-- operation name string <opname>, and number of placeholder
-- parameters <max_param>.
--
-- The is equivalent to the mathematical set-builder notation:
--
--   <opname> { <out> | <invarlist> in <invallist> , <preds> }
--
-- @usage   "x^2 for x"                 -- array values
-- @usage  "x^2 for x=1,10,2"          -- numeric for
-- @usage  "k^v for k,v in pairs(_1)"  -- iterator for
-- @usage  "(x+y)^2 for x for y if x > y"  -- nested
--
local function parse_comprehension(expr)
  local pos = 1

  -- extract opname (if exists)
  local opname
  local tok, post = expr:match('^%s*([%a_][%w_]*)%s*%(()', pos)
  local pose = #expr + 1
  if tok then
    local tok2, posb = lb.match_bracketed(expr, post-1)
    assert(tok2, 'syntax error')
    if expr:match('^%s*$', posb) then
      opname = tok
      pose = posb - 1
      pos = post
    end
  end
  opname = opname or "list"

  -- extract out expression list
  local out; out, pos = lb.match_explist(expr, pos)
  assert(out, "syntax error: missing expression list")
  out = table_concat(out, ', ')

  -- extract "for" clauses
  local fortypes = {}
  local invarlists = {}
  local invallists = {}
  while 1 do
    local post = expr:match('^%s*for%s+()', pos)
    if not post then break end
    pos = post

    -- extract input vars
    local iv; iv, pos = lb.match_namelist(expr, pos)
    assert(#iv > 0, 'syntax error: zero variables')
    for _,ident in ipairs(iv) do
      assert(not ident:match'^__',
             "identifier " .. ident .. " may not contain __ prefix")
    end
    invarlists[#invarlists+1] = iv

    -- extract '=' or 'in' (optional)
    local fortype, post = expr:match('^(=)%s*()', pos)
    if not fortype then fortype, post = expr:match('^(in)%s+()', pos) end
    if fortype then
      pos = post
      -- extract input value range
      local il; il, pos = lb.match_explist(expr, pos)
      assert(#il > 0, 'syntax error: zero expressions')
      assert(fortype ~= '=' or #il == 2 or #il == 3,
             'syntax error: numeric for requires 2 or three expressions')
      fortypes[#invarlists] = fortype
      invallists[#invarlists] = il
    else
      fortypes[#invarlists] = false
      invallists[#invarlists] = false
    end
  end
  assert(#invarlists > 0, 'syntax error: missing "for" clause')

  -- extract "if" clauses
  local preds = {}
  while 1 do
    local post = expr:match('^%s*if%s+()', pos)
    if not post then break end
    pos = post
    local pred; pred, pos = lb.match_expression(expr, pos)
    assert(pred, 'syntax error: predicated expression not found')
    preds[#preds+1] = pred
  end

  -- extract number of parameter variables (name matching "_%d+")
  local stmp = ''; lb.gsub(expr, function(u, sin)  -- strip comments/strings
    if u == 'e' then stmp = stmp .. ' ' .. sin .. ' ' end
  end)
  local max_param = 0; stmp:gsub('[%a_][%w_]*', function(s)
    local s = s:match('^_(%d+)$')
    if s then max_param = math_max(max_param, tonumber(s)) end
  end)

  if pos ~= pose then
    assert(false, "syntax error: unrecognized " .. expr:sub(pos))
  end

  --DEBUG:
  --print('----\n', string.format("%q", expr), string.format("%q", out), opname)
  --for k,v in ipairs(invarlists) do print(k,v, invallists[k]) end
  --for k,v in ipairs(preds) do print(k,v) end

  return out, fortypes, invarlists, invallists, preds, opname, max_param
end


-- Create Lua code string representing comprehension.
-- Arguments are in the form returned by parse_comprehension.
local function code_comprehension(
    out, fortypes, invarlists, invallists, preds, opname, max_param
)
  local op = assert(ops[opname])
  local code = op.accum:gsub('%%s',  out)

  for i=#preds,1,-1 do local pred = preds[i]
    code = ' if ' .. pred .. ' then ' .. code .. ' end '
  end
  for i=#invarlists,1,-1 do
    if not fortypes[i] then
      local arrayname = '__in' .. i
      local idx = '__idx' .. i
      code =
        ' for ' .. idx .. ' = 1, #' .. arrayname .. ' do ' ..
        ' local ' .. invarlists[i][1] .. ' = ' .. arrayname .. '['..idx..'] ' ..
        code .. ' end '
    else
      code =
        ' for ' ..
        table_concat(invarlists[i], ', ') ..
        ' ' .. fortypes[i] .. ' ' ..
        table_concat(invallists[i], ', ') ..
        ' do ' .. code .. ' end '
    end
  end
  code = ' local __result = ( ' .. op.init .. ' ) ' .. code
  return code
end


-- Convert code string represented by code_comprehension
-- into Lua function.  Also must pass ninputs = #invarlists,
-- max_param, and invallists (from parse_comprehension).
-- Uses environment env.
local function wrap_comprehension(code, ninputs, max_param, invallists, env)
  assert(ninputs > 0)
  local ts = {}
  for i=1,max_param do
    ts[#ts+1] = '_' .. i
  end
  for i=1,ninputs do
    if not invallists[i] then
      local name = '__in' .. i
      ts[#ts+1] = name
    end
  end
  if #ts > 0 then
    code = ' local ' .. table_concat(ts, ', ') .. ' = ... ' .. code
  end
  code = code .. ' return __result '
  --print('DEBUG:', code)
  local f, err = utils.load(code,'tmp','t',env)
  if not f then assert(false, err .. ' with generated code ' .. code) end
  return f
end


-- Build Lua function from comprehension string.
-- Uses environment env.
local function build_comprehension(expr, env)
  local out, fortypes, invarlists, invallists, preds, opname, max_param
    = parse_comprehension(expr)
  local code = code_comprehension(
    out, fortypes, invarlists, invallists, preds, opname, max_param)
  local f = wrap_comprehension(code, #invarlists, max_param, invallists, env)
  return f
end


-- Creates new comprehension cache.
-- Any list comprehension function created are set to the environment
-- env (defaults to caller of new).
local function new(env)
  -- Note: using a single global comprehension cache would have had
  -- security implications (e.g. retrieving cached functions created
  -- in other environments).
  -- The cache lookup function could have instead been written to retrieve
  -- the caller's environment, lookup up the cache private to that
  -- environment, and then looked up the function in that cache.
  -- That would avoid the need for this <new> call to
  -- explicitly manage caches; however, that might also have an undue
  -- performance penalty.

  if not env then
    env = utils.getfenv(2)
  end

  local mt = {}
  local cache = setmetatable({}, mt)

  -- Index operator builds, caches, and returns Lua function
  -- corresponding to comprehension expression string.
  --
  -- Example: f = comprehension['x^2 for x']
  --
  function mt:__index(expr)
    local f = build_comprehension(expr, env)
    self[expr] = f  -- cache
    return f
  end

  -- Convenience syntax.
  -- Allows comprehension 'x^2 for x' instead of comprehension['x^2 for x'].
  mt.__call = mt.__index

  cache.new = new

  return cache
end


local comprehension = {}
comprehension.new = new

return comprehension

end,

["pl.config"] = function()
--------------------
-- Module: 'pl.config'
--------------------
--- Reads configuration files into a Lua table.
--  Understands INI files, classic Unix config files, and simple
-- delimited columns of values. See @{06-data.md.Reading_Configuration_Files|the Guide}
--
--    # test.config
--    # Read timeout in seconds
--    read.timeout=10
--    # Write timeout in seconds
--    write.timeout=5
--    #acceptable ports
--    ports = 1002,1003,1004
--
--    -- readconfig.lua
--    local config = require 'config'
--    local t = config.read 'test.config'
--    print(pretty.write(t))
--
--    ### output #####
--    {
--      ports = {
--        1002,
--        1003,
--        1004
--      },
--      write_timeout = 5,
--      read_timeout = 10
--    }
--
-- @module pl.config

local type,tonumber,ipairs,io, table = _G.type,_G.tonumber,_G.ipairs,_G.io,_G.table

local function split(s,re)
    local res = {}
    local t_insert = table.insert
    re = '[^'..re..']+'
    for k in s:gmatch(re) do t_insert(res,k) end
    return res
end

local function strip(s)
    return s:gsub('^%s+',''):gsub('%s+$','')
end

local function strip_quotes (s)
    return s:gsub("['\"](.*)['\"]",'%1')
end

local config = {}

--- like `io.lines`, but allows for lines to be continued with '`\\`'.
-- @param file a file-like object (anything where read() returns the next line) or a filename.
-- Defaults to standard input.
-- @return an iterator over the lines, or nil
-- @return error 'not a file-like object' or 'file is nil'
function config.lines(file)
    local f,openf,err
    local line = ''
    if type(file) == 'string' then
        f,err = io.open(file,'r')
        if not f then return nil,err end
        openf = true
    else
        f = file or io.stdin
        if not file.read then return nil, 'not a file-like object' end
    end
    if not f then return nil, 'file is nil' end
    return function()
        local l = f:read()
        while l do
            -- only for non-blank lines that don't begin with either ';' or '#'
            if l:match '%S' and not l:match '^%s*[;#]' then
                -- does the line end with '\'?
                local i = l:find '\\%s*$'
                if i then -- if so,
                    line = line..l:sub(1,i-1)
                elseif line == '' then
                    return l
                else
                    l = line..l
                    line = ''
                    return l
                end
            end
            l = f:read()
        end
        if openf then f:close() end
    end
end

--- read a configuration file into a table
-- @param file either a file-like object or a string, which must be a filename
-- @tab[opt] cnfg a configuration table that may contain these fields:
--
--  * `smart`  try to deduce what kind of config file we have (default false)
--  * `variabilize` make names into valid Lua identifiers (default true)
--  * `convert_numbers` try to convert values into numbers (default true)
--  * `trim_space` ensure that there is no starting or trailing whitespace with values (default true)
--  * `trim_quotes` remove quotes from strings (default false)
--  * `list_delim` delimiter to use when separating columns (default ',')
--  * `keysep` separator between key and value pairs (default '=')
--
-- @return a table containing items, or `nil`
-- @return error message (same as @{config.lines}
function config.read(file,cnfg)
    local auto

    local iter,err = config.lines(file)
    if not iter then return nil,err end
    local line = iter()
    cnfg = cnfg or {}
    if cnfg.smart then
        auto = true
        if line:match '^[^=]+=' then
            cnfg.keysep = '='
        elseif line:match '^[^:]+:' then
            cnfg.keysep = ':'
            cnfg.list_delim = ':'
        elseif line:match '^%S+%s+' then
            cnfg.keysep = ' '
            -- more than two columns assume that it's a space-delimited list
            -- cf /etc/fstab with /etc/ssh/ssh_config
            if line:match '^%S+%s+%S+%s+%S+' then
                cnfg.list_delim = ' '
            end
            cnfg.variabilize = false
        end
    end


    local function check_cnfg (var,def)
        local val = cnfg[var]
        if val == nil then return def else return val end
    end

    local initial_digits = '^[%d%+%-]'
    local t = {}
    local top_t = t
    local variabilize = check_cnfg ('variabilize',true)
    local list_delim = check_cnfg('list_delim',',')
    local convert_numbers = check_cnfg('convert_numbers',true)
    local convert_boolean = check_cnfg('convert_boolean',false)
    local trim_space = check_cnfg('trim_space',true)
    local trim_quotes = check_cnfg('trim_quotes',false)
    local ignore_assign = check_cnfg('ignore_assign',false)
    local keysep = check_cnfg('keysep','=')
    local keypat = keysep == ' ' and '%s+' or '%s*'..keysep..'%s*'
    if list_delim == ' ' then list_delim = '%s+' end

    local function process_name(key)
        if variabilize then
            key = key:gsub('[^%w]','_')
        end
        return key
    end

    local function process_value(value)
        if list_delim and value:find(list_delim) then
            value = split(value,list_delim)
            for i,v in ipairs(value) do
                value[i] = process_value(v)
            end
        elseif convert_numbers and value:find(initial_digits) then
            local val = tonumber(value)
            if not val and value:match ' kB$' then
                value = value:gsub(' kB','')
                val = tonumber(value)
            end
            if val then value = val end
        elseif convert_boolean and value == 'true' then
           return true
        elseif convert_boolean and value == 'false' then
           return false
        end
        if type(value) == 'string' then
           if trim_space then value = strip(value) end
           if not trim_quotes and auto and value:match '^"' then
                trim_quotes = true
            end
           if trim_quotes then value = strip_quotes(value) end
        end
        return value
    end

    while line do
        if line:find('^%[') then -- section!
            local section = process_name(line:match('%[([^%]]+)%]'))
            t = top_t
            t[section] = {}
            t = t[section]
        else
            line = line:gsub('^%s*','')
            local i1,i2 = line:find(keypat)
            if i1 and not ignore_assign then -- key,value assignment
                local key = process_name(line:sub(1,i1-1))
                local value = process_value(line:sub(i2+1))
                t[key] = value
            else -- a plain list of values...
                t[#t+1] = process_value(line)
            end
        end
        line = iter()
    end
    return top_t
end

return config

end,

["pl.data"] = function()
--------------------
-- Module: 'pl.data'
--------------------
--- Reading and querying simple tabular data.
--
--    data.read 'test.txt'
--    ==> {{10,20},{2,5},{40,50},fieldnames={'x','y'},delim=','}
--
-- Provides a way of creating basic SQL-like queries.
--
--    require 'pl'
--    local d = data.read('xyz.txt')
--    local q = d:select('x,y,z where x > 3 and z < 2 sort by y')
--    for x,y,z in q do
--        print(x,y,z)
--    end
--
-- See @{06-data.md.Reading_Columnar_Data|the Guide}
--
-- Dependencies: `pl.utils`, `pl.array2d` (fallback methods)
-- @module pl.data

local utils = require 'pl.utils'
local _DEBUG = rawget(_G,'_DEBUG')

local patterns,function_arg,usplit,array_tostring = utils.patterns,utils.function_arg,utils.split,utils.array_tostring
local append,concat = table.insert,table.concat
local gsub = string.gsub
local io = io
local _G,print,type,tonumber,ipairs,setmetatable = _G,print,type,tonumber,ipairs,setmetatable


local data = {}

local parse_select

local function rstrip(s)
    return (s:gsub('%s+$',''))
end

local function strip (s)
    return (rstrip(s):gsub('^%s*',''))
end

-- This gives `l` the standard List metatable,
-- pulling in the List module.
local function makelist(l)
    return setmetatable(l, require('pl.List'))
end

local function map(fun,t)
    local res = {}
    for i = 1,#t do
        res[i] = fun(t[i])
    end
    return res
end

local function split(line,delim,csv,n)
    local massage
    -- CSV fields may be double-quoted and may contain commas!
    if csv and line:match '"' then
        line = line:gsub('"([^"]+)"',function(str)
            local s,cnt = str:gsub(',','\001')
            if cnt > 0 then massage = true end
            return s
        end)
        if massage then
            massage = function(s) return (s:gsub('\001',',')) end
        end
    end
    local res = (usplit(line,delim,false,n))
    if csv then
        -- restore CSV commas-in-fields
        if massage then res = map(massage,res) end
        -- in CSV mode trailiing commas are significant!
        if line:match ',$' then append(res,'') end
    end
    return makelist(res)
end

local function find(t,v)
    for i = 1,#t do
        if v == t[i] then return i end
    end
end

local DataMT = {
    column_by_name = function(self,name)
        if type(name) == 'number' then
            name = '$'..name
        end
        local arr = {}
        for res in data.query(self,name) do
            append(arr,res)
        end
        return makelist(arr)
    end,

    copy_select = function(self,condn)
        condn = parse_select(condn,self)
        local iter = data.query(self,condn)
        local res = {}
        local row = makelist{iter()}
        while #row > 0 do
            append(res,row)
            row = makelist{iter()}
        end
        res.delim = self.delim
        return data.new(res,split(condn.fields,','))
    end,

    column_names = function(self)
        return self.fieldnames
    end,
}

local array2d

DataMT.__index = function(self,name)
    local f = DataMT[name]
    if f then return f end
    if not array2d then
        array2d = require 'pl.array2d'
    end
    return array2d[name]
end

--- return a particular column as a list of values (method).
-- @param name either name of column, or numerical index.
-- @function Data.column_by_name

--- return a query iterator on this data (method).
-- @string condn the query expression
-- @function Data.select
-- @see data.query

--- return a row iterator on this data (method).
-- @string condn the query expression
-- @function Data.select_row

--- return a new data object based on this query (method).
-- @string condn the query expression
-- @function Data.copy_select

--- return the field names of this data object (method).
-- @function Data.column_names

--- write out a row (method).
-- @param f file-like object
-- @function Data.write_row

--- write data out to file (method).
-- @param f file-like object
-- @function Data.write


-- [guessing delimiter] We check for comma, tab and spaces in that order.
-- [issue] any other delimiters to be checked?
local delims = {',', '\t', ' ', ';'}

local function guess_delim (line)
    if line=='' then return ' ' end
    for _,delim in ipairs(delims) do
        if line:find(delim) then
            return delim == ' ' and '%s+' or delim
        end
    end
    return ' '
end

-- [file parameter] If it's a string, we try open as a filename. If nil, then
-- either stdin or stdout depending on the mode. Otherwise, check if this is
-- a file-like object (implements read or write depending)
local function open_file (f,mode)
    local opened, err
    local reading = mode == 'r'
    if type(f) == 'string' then
        if f == 'stdin'  then
            f = io.stdin
        elseif f == 'stdout'  then
            f = io.stdout
        else
            f,err = io.open(f,mode)
            if not f then return nil,err end
            opened = true
        end
    end
    if f and ((reading and not f.read) or (not reading and not f.write)) then
        return nil, "not a file-like object"
    end
    return f,nil,opened
end

--- read a delimited file in a Lua table.
-- By default, attempts to treat first line as separated list of fieldnames.
-- @param file a filename or a file-like object
-- @tab cnfg parsing options
-- @string cnfg.delim a string pattern to split fields
-- @array cnfg.fieldnames (i.e. don't read from first line)
-- @bool cnfg.no_convert (default is to try conversion on first data line)
-- @tab cnfg.convert table of custom conversion functions with column keys
-- @int cnfg.numfields indices of columns known to be numbers
-- @bool cnfg.last_field_collect only split as many fields as fieldnames.
-- @int cnfg.thousands_dot thousands separator in Excel CSV is '.'
-- @bool cnfg.csv fields may be double-quoted and contain commas;
-- Also, empty fields are considered to be equivalent to zero.
-- @return `data` object, or `nil`
-- @return error message. May be a file error, 'not a file-like object'
-- or a conversion error
function data.read(file,cnfg)
    local count,line
    local D = {}
    if not cnfg then cnfg = {} end
    local f,err,opened = open_file(file,'r')
    if not f then return nil, err end
    local thousands_dot = cnfg.thousands_dot
    local csv = cnfg.csv
    if csv then cnfg.delim = ',' end

    -- note that using dot as the thousands separator (@thousands_dot)
    -- requires a special conversion function! For CSV, _empty fields_ are
    -- considered to default to numerical zeroes.
    local tonumber = tonumber
    local function try_number(x)
        if thousands_dot then x = x:gsub('%.(...)','%1') end
        if csv and x == '' then x = '0' end
        local v = tonumber(x)
        if v == nil then return nil,"not a number" end
        return v
    end

    count = 1
    line = f:read()
    if not line then return nil, "empty file" end

    -- first question: what is the delimiter?
    D.delim = cnfg.delim and cnfg.delim or guess_delim(line)
    local delim = D.delim

    local conversion
    local numfields = {}
    local function append_conversion (idx,conv)
        conversion = conversion or {}
        append(numfields,idx)
        append(conversion,conv)
    end
    if cnfg.numfields then
        for _,n in ipairs(cnfg.numfields) do append_conversion(n,try_number) end
    end

    -- some space-delimited data starts with a space.  This should not be a column,
    -- although it certainly would be for comma-separated, etc.
    local stripper
    if delim == '%s+' and line:find(delim) == 1 then
        stripper = function(s)  return s:gsub('^%s+','') end
        line = stripper(line)
    end
    -- first line will usually be field names. Unless fieldnames are specified,
    -- we check if it contains purely numerical values for the case of reading
    -- plain data files.
    if not cnfg.fieldnames then
        local fields,nums
        fields = split(line,delim,csv)
        if not cnfg.convert then
            nums = map(tonumber,fields)
            if #nums == #fields then -- they're ALL numbers!
                append(D,nums) -- add the first converted row
                -- and specify conversions for subsequent rows
                for i = 1,#nums do append_conversion(i,try_number) end
            else -- we'll try to check numbers just now..
                nums = nil
            end
        else -- [explicit column conversions] (any deduced number conversions will be added)
            for idx,conv in pairs(cnfg.convert) do append_conversion(idx,conv) end
        end
        if nums == nil then
            cnfg.fieldnames = fields
        end
        line = f:read()
        count = count + 1
        if stripper then line = stripper(line) end
    elseif type(cnfg.fieldnames) == 'string' then
        cnfg.fieldnames = split(cnfg.fieldnames,delim,csv)
    end
    local nfields
    -- at this point, the column headers have been read in. If the first
    -- row consisted of numbers, it has already been added to the dataset.
    if cnfg.fieldnames then
        D.fieldnames = cnfg.fieldnames
        -- [collecting end field] If @last_field_collect then we'll
        -- only split as many fields as there are fieldnames
        if cnfg.last_field_collect then
            nfields = #D.fieldnames
        end
        -- [implicit column conversion] unless @no_convert, we need the numerical field indices
        -- of the first data row. These can also be specified explicitly by @numfields.
        if not cnfg.no_convert then
            local fields = split(line,D.delim,csv,nfields)
            for i = 1,#fields do
                if not find(numfields,i) and try_number(fields[i]) then
                    append_conversion(i,try_number)
                end
            end
        end
    end
    -- keep going until finished
    while line do
        if not line:find ('^%s*$') then -- [blank lines] ignore them!
            if stripper then line = stripper(line) end
            local fields = split(line,delim,csv,nfields)
            if conversion then -- there were field conversions...
                for k = 1,#numfields do
                    local i,conv = numfields[k],conversion[k]
                    local val,err = conv(fields[i])
                    if val == nil then
                        return nil, err..": "..fields[i].." at line "..count
                    else
                        fields[i] = val
                    end
                end
            end
            append(D,fields)
        end
        line = f:read()
        count = count + 1
    end
    if opened then f:close() end
    if delim == '%s+' then D.delim = ' ' end
    if not D.fieldnames then D.fieldnames = {} end
    return data.new(D)
end

local function write_row (data,f,row,delim)
    data.temp = array_tostring(row,data.temp)
    f:write(concat(data.temp,delim),'\n')
end

function DataMT:write_row(f,row)
    write_row(self,f,row,self.delim)
end

--- write 2D data to a file.
-- Does not assume that the data has actually been
-- generated with `new` or `read`.
-- @param data 2D array
-- @param file filename or file-like object
-- @tparam[opt] {string} fieldnames list of fields (optional)
-- @string[opt='\t'] delim delimiter (default tab)
-- @return true or nil, error
function data.write (data,file,fieldnames,delim)
    local f,err,opened = open_file(file,'w')
    if not f then return nil, err end
    if not fieldnames then
        fieldnames = data.fieldnames
    end
    delim = delim or '\t'
    if fieldnames and #fieldnames > 0 then
        f:write(concat(fieldnames,delim),'\n')
    end
    for i = 1,#data do
        write_row(data,f,data[i],delim)
    end
    if opened then f:close() end
    return true
end


function DataMT:write(file)
    data.write(self,file,self.fieldnames,self.delim)
end

local function massage_fieldnames (fields,copy)
    -- fieldnames must be valid Lua identifiers; ignore any surrounding padding
    -- but keep the original fieldnames...
    for i = 1,#fields do
        local f = strip(fields[i])
        copy[i] = f
        fields[i] = f:gsub('%W','_')
    end
end

--- create a new dataset from a table of rows.
-- Can specify the fieldnames, else the table must have a field called
-- 'fieldnames', which is either a string of delimiter-separated names,
-- or a table of names. <br>
-- If the table does not have a field called 'delim', then an attempt will be
-- made to guess it from the fieldnames string, defaults otherwise to tab.
-- @param d the table.
-- @tparam[opt] {string} fieldnames optional fieldnames
-- @return the table.
function data.new (d,fieldnames)
    d.fieldnames = d.fieldnames or fieldnames or ''
    if not d.delim and type(d.fieldnames) == 'string' then
        d.delim = guess_delim(d.fieldnames)
        d.fieldnames = split(d.fieldnames,d.delim)
    end
    d.fieldnames = makelist(d.fieldnames)
    d.original_fieldnames = {}
    massage_fieldnames(d.fieldnames,d.original_fieldnames)
    setmetatable(d,DataMT)
    -- a query with just the fieldname will return a sequence
    -- of values, which seq.copy turns into a table.
    return d
end

local sorted_query = [[
return function (t)
    local i = 0
    local v
    local ls = {}
    for i,v in ipairs(t) do
        if CONDITION then
            ls[#ls+1] = v
        end
    end
    table.sort(ls,function(v1,v2)
        return SORT_EXPR
    end)
    local n = #ls
    return function()
        i = i + 1
        v = ls[i]
        if i > n then return end
        return FIELDLIST
    end
end
]]

-- question: is this optimized case actually worth the extra code?
local simple_query = [[
return function (t)
    local n = #t
    local i = 0
    local v
    return function()
        repeat
            i = i + 1
            v = t[i]
        until i > n or CONDITION
        if i > n then return end
        return FIELDLIST
    end
end
]]

local function is_string (s)
    return type(s) == 'string'
end

local field_error

local function fieldnames_as_string (data)
    return concat(data.fieldnames,',')
end

local function massage_fields(data,f)
    local idx
    if f:find '^%d+$' then
        idx = tonumber(f)
    else
        idx = find(data.fieldnames,f)
    end
    if idx then
        return 'v['..idx..']'
    else
        field_error = f..' not found in '..fieldnames_as_string(data)
        return f
    end
end


local function process_select (data,parms)
    --- preparing fields ----
    field_error = nil
    local fields = parms.fields
    local numfields = fields:find '%$'  or #data.fieldnames == 0
    if fields:find '^%s*%*%s*' then
        if not numfields then
            fields = fieldnames_as_string(data)
        else
            local ncol = #data[1]
            fields = {}
            for i = 1,ncol do append(fields,'$'..i) end
            fields = concat(fields,',')
        end
    end
    local idpat = patterns.IDEN
    if numfields then
        idpat = '%$(%d+)'
    else
        -- massage field names to replace non-identifier chars
        fields = rstrip(fields):gsub('[^,%w]','_')
    end
    local massage_fields = utils.bind1(massage_fields,data)
    local ret = gsub(fields,idpat,massage_fields)
    if field_error then return nil,field_error end
    parms.fields = fields
    parms.proc_fields = ret
    parms.where = parms.where or  'true'
    if is_string(parms.where) then
        parms.where = gsub(parms.where,idpat,massage_fields)
        field_error = nil
    end
    return true
end


parse_select = function(s,data)
    local endp
    local parms = {}
    local w1,w2 = s:find('where ')
    local s1,s2 = s:find('sort by ')
    if w1 then -- where clause!
        endp = (s1 or 0)-1
        parms.where = s:sub(w2+1,endp)
    end
    if s1 then -- sort by clause (must be last!)
        parms.sort_by = s:sub(s2+1)
    end
    endp = (w1 or s1 or 0)-1
    parms.fields = s:sub(1,endp)
    local status,err = process_select(data,parms)
    if not status then return nil,err
    else return parms end
end

--- create a query iterator from a select string.
-- Select string has this format: <br>
-- FIELDLIST [ where LUA-CONDN [ sort by FIELD] ]<br>
-- FIELDLIST is a comma-separated list of valid fields, or '*'. <br> <br>
-- The condition can also be a table, with fields 'fields' (comma-sep string or
-- table), 'sort_by' (string) and 'where' (Lua expression string or function)
-- @param data table produced by read
-- @param condn select string or table
-- @param context a list of tables to be searched when resolving functions
-- @param return_row if true, wrap the results in a row table
-- @return an iterator over the specified fields, or nil
-- @return an error message
function data.query(data,condn,context,return_row)
    local err
    if is_string(condn) then
        condn,err = parse_select(condn,data)
        if not condn then return nil,err end
    elseif type(condn) == 'table' then
        if type(condn.fields) == 'table' then
            condn.fields = concat(condn.fields,',')
        end
        if not condn.proc_fields then
            local status,err = process_select(data,condn)
            if not status then return nil,err end
        end
    else
        return nil, "condition must be a string or a table"
    end
    local query
    if condn.sort_by then -- use sorted_query
        query = sorted_query
    else
        query = simple_query
    end
    local fields = condn.proc_fields or condn.fields
    if return_row then
        fields = '{'..fields..'}'
    end
    query = query:gsub('FIELDLIST',fields)
    if is_string(condn.where) then
        query = query:gsub('CONDITION',condn.where)
        condn.where = nil
    else
       query = query:gsub('CONDITION','_condn(v)')
       condn.where = function_arg(0,condn.where,'condition.where must be callable')
    end
    if condn.sort_by then
        local expr,sort_var,sort_dir
        local sort_by = condn.sort_by
        local i1,i2 = sort_by:find('%s+')
        if i1 then
            sort_var,sort_dir = sort_by:sub(1,i1-1),sort_by:sub(i2+1)
        else
            sort_var = sort_by
            sort_dir = 'asc'
        end
        if sort_var:match '^%$' then sort_var = sort_var:sub(2) end
        sort_var = massage_fields(data,sort_var)
        if field_error then return nil,field_error end
        if sort_dir == 'asc' then
            sort_dir = '<'
        else
            sort_dir = '>'
        end
        expr = ('%s %s %s'):format(sort_var:gsub('v','v1'),sort_dir,sort_var:gsub('v','v2'))
        query = query:gsub('SORT_EXPR',expr)
    end
    if condn.where then
        query = 'return function(_condn) '..query..' end'
    end
    if _DEBUG then print(query) end

    local fn,err = utils.load(query,'tmp')
    if not fn then return nil,err end
    fn = fn() -- get the function
    if condn.where then
        fn = fn(condn.where)
    end
    local qfun = fn(data)
    if context then
        -- [specifying context for condition] @context is a list of tables which are
        -- 'injected'into the condition's custom context
        append(context,_G)
        local lookup = {}
        utils.setfenv(qfun,lookup)
        setmetatable(lookup,{
            __index = function(tbl,key)
               -- _G.print(tbl,key)
                for k,t in ipairs(context) do
                    if t[key] then return t[key] end
                end
            end
        })
    end
    return qfun
end


DataMT.select = data.query
DataMT.select_row = function(d,condn,context)
    return data.query(d,condn,context,true)
end

--- Filter input using a query.
-- @string Q a query string
-- @param infile filename or file-like object
-- @param outfile filename or file-like object
-- @bool dont_fail true if you want to return an error, not just fail
function data.filter (Q,infile,outfile,dont_fail)
    local d = data.read(infile or 'stdin')
    local out = open_file(outfile or 'stdout')
    local iter,err = d:select(Q)
    local delim = d.delim
    if not iter then
        err = 'error: '..err
        if dont_fail then
            return nil,err
        else
            utils.quit(1,err)
        end
    end
    while true do
        local res = {iter()}
        if #res == 0 then break end
        out:write(concat(res,delim),'\n')
    end
end

return data


end,

["pl.Date"] = function()
--------------------
-- Module: 'pl.Date'
--------------------
--- Date and Date Format classes.
-- See  @{05-dates.md|the Guide}.
--
-- NOTE: the date module is deprecated! see
-- https://github.com/lunarmodules/Penlight/issues/285
--
-- Dependencies: `pl.class`, `pl.stringx`, `pl.utils`
-- @classmod pl.Date
-- @pragma nostrip

local class = require 'pl.class'
local os_time, os_date = os.time, os.date
local stringx = require 'pl.stringx'
local utils = require 'pl.utils'
local assert_arg,assert_string = utils.assert_arg,utils.assert_string


utils.raise_deprecation {
  source = "Penlight " .. utils._VERSION,
  message = "the 'Date' module is deprecated, see https://github.com/lunarmodules/Penlight/issues/285",
  version_removed = "2.0.0",
  version_deprecated = "1.9.2",
}


local Date = class()
Date.Format = class()

--- Date constructor.
-- @param t this can be either
--
--   * `nil` or empty - use current date and time
--   * number - seconds since epoch (as returned by `os.time`). Resulting time is UTC
--   * `Date` - make a copy of this date
--   * table - table containing year, month, etc as for `os.time`. You may leave out year, month or day,
-- in which case current values will be used.
--   * year (will be followed by month, day etc)
--
-- @param ...  true if  Universal Coordinated Time, or two to five numbers: month,day,hour,min,sec
-- @function Date
function Date:_init(t,...)
    local time
    local nargs = select('#',...)
    if nargs > 2 then
        local extra = {...}
        local year = t
        t = {
            year = year,
            month = extra[1],
            day = extra[2],
            hour = extra[3],
            min = extra[4],
            sec = extra[5]
        }
    end
    if nargs == 1 then
        self.utc = select(1,...) == true
    end
    if t == nil or t == 'utc' then
        time = os_time()
        self.utc = t == 'utc'
    elseif type(t) == 'number' then
        time = t
        if self.utc == nil then self.utc = true end
    elseif type(t) == 'table' then
        if getmetatable(t) == Date then -- copy ctor
            time = t.time
            self.utc = t.utc
        else
            if not (t.year and t.month) then
                local lt = os_date('*t')
                if not t.year and not t.month and not t.day then
                    t.year = lt.year
                    t.month = lt.month
                    t.day = lt.day
                else
                    t.year = t.year or lt.year
                    t.month = t.month or (t.day and lt.month or 1)
                    t.day = t.day or 1
                end
            end
            t.day = t.day or 1
            time = os_time(t)
        end
    else
        error("bad type for Date constructor: "..type(t),2)
    end
    self:set(time)
end

--- set the current time of this Date object.
-- @int t seconds since epoch
function Date:set(t)
    self.time = t
    if self.utc then
        self.tab = os_date('!*t',t)
    else
        self.tab = os_date('*t',t)
    end
end

--- get the time zone offset from UTC.
-- @int ts seconds ahead of UTC
function Date.tzone (ts)
    if ts == nil then
        ts = os_time()
    elseif type(ts) == "table" then
        if getmetatable(ts) == Date then
            ts = ts.time
        else
            ts = Date(ts).time
        end
    end
    local utc = os_date('!*t',ts)
    local lcl = os_date('*t',ts)
    lcl.isdst = false
    return os.difftime(os_time(lcl), os_time(utc))
end

--- convert this date to UTC.
function Date:toUTC ()
    local ndate = Date(self)
    if not self.utc then
        ndate.utc = true
        ndate:set(ndate.time)
    end
    return ndate
end

--- convert this UTC date to local.
function Date:toLocal ()
    local ndate = Date(self)
    if self.utc then
        ndate.utc = false
        ndate:set(ndate.time)
--~         ndate:add { sec = Date.tzone(self) }
    end
    return ndate
end

--- set the year.
-- @int y Four-digit year
-- @class function
-- @name Date:year

--- set the month.
-- @int m month
-- @class function
-- @name Date:month

--- set the day.
-- @int d day
-- @class function
-- @name Date:day

--- set the hour.
-- @int h hour
-- @class function
-- @name Date:hour

--- set the minutes.
-- @int min minutes
-- @class function
-- @name Date:min

--- set the seconds.
-- @int sec seconds
-- @class function
-- @name Date:sec

--- set the day of year.
-- @class function
-- @int yday day of year
-- @name Date:yday

--- get the year.
-- @int y Four-digit year
-- @class function
-- @name Date:year

--- get the month.
-- @class function
-- @name Date:month

--- get the day.
-- @class function
-- @name Date:day

--- get the hour.
-- @class function
-- @name Date:hour

--- get the minutes.
-- @class function
-- @name Date:min

--- get the seconds.
-- @class function
-- @name Date:sec

--- get the day of year.
-- @class function
-- @name Date:yday


for _,c in ipairs{'year','month','day','hour','min','sec','yday'} do
    Date[c] = function(self,val)
        if val then
            assert_arg(1,val,"number")
            self.tab[c] = val
            self:set(os_time(self.tab))
            return self
        else
            return self.tab[c]
        end
    end
end

--- name of day of week.
-- @bool full abbreviated if true, full otherwise.
-- @ret string name
function Date:weekday_name(full)
    return os_date(full and '%A' or '%a',self.time)
end

--- name of month.
-- @int full abbreviated if true, full otherwise.
-- @ret string name
function Date:month_name(full)
    return os_date(full and '%B' or '%b',self.time)
end

--- is this day on a weekend?.
function Date:is_weekend()
    return self.tab.wday == 1 or self.tab.wday == 7
end

--- add to a date object.
-- @param t a table containing one of the following keys and a value:
-- one of `year`,`month`,`day`,`hour`,`min`,`sec`
-- @return this date
function Date:add(t)
    local old_dst = self.tab.isdst
    local key,val = next(t)
    self.tab[key] = self.tab[key] + val
    self:set(os_time(self.tab))
    if old_dst ~= self.tab.isdst then
        self.tab.hour = self.tab.hour - (old_dst and 1 or -1)
        self:set(os_time(self.tab))
    end
    return self
end

--- last day of the month.
-- @return int day
function Date:last_day()
    local d = 28
    local m = self.tab.month
    while self.tab.month == m do
        d = d + 1
        self:add{day=1}
    end
    self:add{day=-1}
    return self
end

--- difference between two Date objects.
-- @tparam Date other Date object
-- @treturn Date.Interval object
function Date:diff(other)
    local dt = self.time - other.time
    if dt < 0 then error("date difference is negative!",2) end
    return Date.Interval(dt)
end

--- long numerical ISO data format version of this date.
function Date:__tostring()
    local fmt = '%Y-%m-%dT%H:%M:%S'
    if self.utc then
        fmt = "!"..fmt
    end
    local t = os_date(fmt,self.time)
    if self.utc then
        return  t .. 'Z'
    else
        local offs = self:tzone()
        if offs == 0 then
            return t .. 'Z'
        end
        local sign = offs > 0 and '+' or '-'
        local h = math.ceil(offs/3600)
        local m = (offs % 3600)/60
        if m == 0 then
            return t .. ('%s%02d'):format(sign,h)
        else
            return t .. ('%s%02d:%02d'):format(sign,h,m)
        end
    end
end

--- equality between Date objects.
function Date:__eq(other)
    return self.time == other.time
end

--- ordering between Date objects.
function Date:__lt(other)
    return self.time < other.time
end

--- difference between Date objects.
-- @function Date:__sub
Date.__sub = Date.diff

--- add a date and an interval.
-- @param other either a `Date.Interval` object or a table such as
-- passed to `Date:add`
function Date:__add(other)
    local nd = Date(self)
    if Date.Interval:class_of(other) then
        other = {sec=other.time}
    end
    nd:add(other)
    return nd
end

Date.Interval = class(Date)

---- Date.Interval constructor
-- @int t an interval in seconds
-- @function Date.Interval
function Date.Interval:_init(t)
    self:set(t)
end

function Date.Interval:set(t)
    self.time = t
    self.tab = os_date('!*t',self.time)
end

local function ess(n)
    if n > 1 then return 's '
    else return ' '
    end
end

--- If it's an interval then the format is '2 hours 29 sec' etc.
function Date.Interval:__tostring()
    local t, res = self.tab, ''
    local y,m,d = t.year - 1970, t.month - 1, t.day - 1
    if y > 0 then res = res .. y .. ' year'..ess(y) end
    if m > 0 then res = res .. m .. ' month'..ess(m) end
    if d > 0 then res = res .. d .. ' day'..ess(d) end
    if y == 0 and m == 0 then
        local h = t.hour
        if h > 0 then res = res .. h .. ' hour'..ess(h) end
        if t.min > 0 then res = res .. t.min .. ' min ' end
        if t.sec > 0 then res = res .. t.sec .. ' sec ' end
    end
    if res == '' then res = 'zero' end
    return res
end

------------ Date.Format class: parsing and renderinig dates ------------

-- short field names, explicit os.date names, and a mask for allowed field repeats
local formats = {
    d = {'day',{true,true}},
    y = {'year',{false,true,false,true}},
    m = {'month',{true,true}},
    H = {'hour',{true,true}},
    M = {'min',{true,true}},
    S = {'sec',{true,true}},
}

--- Date.Format constructor.
-- @string fmt. A string where the following fields are significant:
--
--   * d day (either d or dd)
--   * y year (either yy or yyy)
--   * m month (either m or mm)
--   * H hour (either H or HH)
--   * M minute (either M or MM)
--   * S second (either S or SS)
--
-- Alternatively, if fmt is nil then this returns a flexible date parser
-- that tries various date/time schemes in turn:
--
--  * [ISO 8601](http://en.wikipedia.org/wiki/ISO_8601), like `2010-05-10 12:35:23Z` or `2008-10-03T14:30+02`
--  * times like 15:30 or 8.05pm  (assumed to be today's date)
--  * dates like 28/10/02 (European order!) or 5 Feb 2012
--  * month name like march or Mar (case-insensitive, first 3 letters); here the
-- day will be 1 and the year this current year
--
-- A date in format 3 can be optionally followed by a time in format 2.
-- Please see test-date.lua in the tests folder for more examples.
-- @usage df = Date.Format("yyyy-mm-dd HH:MM:SS")
-- @class function
-- @name Date.Format
function Date.Format:_init(fmt)
    if not fmt then
        self.fmt = '%Y-%m-%d %H:%M:%S'
        self.outf = self.fmt
        self.plain = true
        return
    end
    local append = table.insert
    local D,PLUS,OPENP,CLOSEP = '\001','\002','\003','\004'
    local vars,used = {},{}
    local patt,outf = {},{}
    local i = 1
    while i < #fmt do
        local ch = fmt:sub(i,i)
        local df = formats[ch]
        if df then
            if used[ch] then error("field appeared twice: "..ch,4) end
            used[ch] = true
            -- this field may be repeated
            local _,inext = fmt:find(ch..'+',i+1)
            local cnt = not _ and 1 or inext-i+1
            if not df[2][cnt] then error("wrong number of fields: "..ch,4) end
            -- single chars mean 'accept more than one digit'
            local p = cnt==1 and (D..PLUS) or (D):rep(cnt)
            append(patt,OPENP..p..CLOSEP)
            append(vars,ch)
            if ch == 'y' then
                append(outf,cnt==2 and '%y' or '%Y')
            else
                append(outf,'%'..ch)
            end
            i = i + cnt
        else
            append(patt,ch)
            append(outf,ch)
            i = i + 1
        end
    end
    -- escape any magic characters
    fmt = utils.escape(table.concat(patt))
   -- fmt = table.concat(patt):gsub('[%-%.%+%[%]%(%)%$%^%%%?%*]','%%%1')
    -- replace markers with their magic equivalents
    fmt = fmt:gsub(D,'%%d'):gsub(PLUS,'+'):gsub(OPENP,'('):gsub(CLOSEP,')')
    self.fmt = fmt
    self.outf = table.concat(outf)
    self.vars = vars
end

local parse_date

--- parse a string into a Date object.
-- @string str a date string
-- @return date object
function Date.Format:parse(str)
    assert_string(1,str)
    if self.plain then
        return parse_date(str,self.us)
    end
    local res = {str:match(self.fmt)}
    if #res==0 then return nil, 'cannot parse '..str end
    local tab = {}
    for i,v in ipairs(self.vars) do
        local name = formats[v][1] -- e.g. 'y' becomes 'year'
        tab[name] = tonumber(res[i])
    end
    -- os.date() requires these fields; if not present, we assume
    -- that the time set is for the current day.
    if not (tab.year and tab.month and tab.day) then
        local today = Date()
        tab.year = tab.year or today:year()
        tab.month = tab.month or today:month()
        tab.day = tab.day or today:day()
    end
    local Y = tab.year
    if Y < 100 then -- classic Y2K pivot
        tab.year = Y + (Y < 35 and 2000 or 1999)
    elseif not Y then
        tab.year = 1970
    end
    return Date(tab)
end

--- convert a Date object into a string.
-- @param d a date object, or a time value as returned by @{os.time}
-- @return string
function Date.Format:tostring(d)
    local tm
    local fmt = self.outf
    if type(d) == 'number' then
        tm = d
    else
        tm = d.time
        if d.utc then
            fmt = '!'..fmt
        end
    end
    return os_date(fmt,tm)
end

--- force US order in dates like 9/11/2001
function Date.Format:US_order(yesno)
    self.us = yesno
end

--local months = {jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12}
local months
local parse_date_unsafe
local function create_months()
    local ld, day1 = parse_date_unsafe '2000-12-31', {day=1}
    months = {}
    for i = 1,12 do
        ld = ld:last_day()
        ld:add(day1)
        local mon = ld:month_name():lower()
        months [mon] = i
    end
end

--[[
Allowed patterns:
- [day] [monthname] [year] [time]
- [day]/[month][/year] [time]

]]

local function looks_like_a_month(w)
    return w:match '^%a+,*$' ~= nil
end
local is_number = stringx.isdigit
local function tonum(s,l1,l2,kind)
    kind = kind or ''
    local n = tonumber(s)
    if not n then error(("%snot a number: '%s'"):format(kind,s)) end
    if n < l1 or n > l2 then
        error(("%s out of range: %s is not between %d and %d"):format(kind,s,l1,l2))
    end
    return n
end

local function  parse_iso_end(p,ns,sec)
    -- may be fractional part of seconds
    local _,nfrac,secfrac = p:find('^%.%d+',ns+1)
    if secfrac then
        sec = sec .. secfrac
        p = p:sub(nfrac+1)
    else
        p = p:sub(ns+1)
    end
    -- ISO 8601 dates may end in Z (for UTC) or [+-][isotime]
    -- (we're working with the date as lower case, hence 'z')
    if p:match 'z$' then -- we're UTC!
        return  sec, {h=0,m=0}
    end
    p = p:gsub(':','') -- turn 00:30 to 0030
    local _,_,sign,offs = p:find('^([%+%-])(%d+)')
    if not sign then return sec, nil end -- not UTC

    if #offs == 2 then offs = offs .. '00' end -- 01 to 0100
    local tz = { h = tonumber(offs:sub(1,2)), m = tonumber(offs:sub(3,4)) }
    if sign == '-' then tz.h = -tz.h; tz.m = -tz.m end
    return sec, tz
end

function parse_date_unsafe (s,US)
    s = s:gsub('T',' ') -- ISO 8601
    local parts = stringx.split(s:lower())
    local i,p = 1,parts[1]
    local function nextp() i = i + 1; p = parts[i] end
    local year,min,hour,sec,apm
    local tz
    local _,nxt,day, month = p:find '^(%d+)/(%d+)'
    if day then
        -- swop for US case
        if US then
            day, month = month, day
        end
        _,_,year = p:find('^/(%d+)',nxt+1)
        nextp()
    else -- ISO
        year,month,day = p:match('^(%d+)%-(%d+)%-(%d+)')
        if year then
            nextp()
        end
    end
    if p and not year and is_number(p) then -- has to be date
        if #p < 4 then
            day = p
            nextp()
        else -- unless it looks like a 24-hour time
            year = true
        end
    end
    if p and looks_like_a_month(p) then -- date followed by month
        p = p:sub(1,3)
        if not months then
            create_months()
        end
        local mon = months[p]
        if mon then
            month = mon
        else error("not a month: " .. p) end
        nextp()
    end
    if p and not year and is_number(p) then
        year = p
        nextp()
    end

    if p then -- time is hh:mm[:ss], hhmm[ss] or H.M[am|pm]
        _,nxt,hour,min = p:find '^(%d+):(%d+)'
        local ns
        if nxt then -- are there seconds?
            _,ns,sec = p:find ('^:(%d+)',nxt+1)
            --if ns then
                sec,tz = parse_iso_end(p,ns or nxt,sec)
            --end
        else -- might be h.m
            _,ns,hour,min = p:find '^(%d+)%.(%d+)'
            if ns then
                apm = p:match '[ap]m$'
            else  -- or hhmm[ss]
                local hourmin
                _,nxt,hourmin = p:find ('^(%d+)')
                if nxt then
                   hour = hourmin:sub(1,2)
                   min = hourmin:sub(3,4)
                   sec = hourmin:sub(5,6)
                   if #sec == 0 then sec = nil end
                   sec,tz = parse_iso_end(p,nxt,sec)
                end
            end
        end
    end
    local today
    if year == true then year = nil end
    if not (year and month and day) then
        today = Date()
    end
    day = day and tonum(day,1,31,'day') or (month and 1 or today:day())
    month = month and tonum(month,1,12,'month') or today:month()
    year = year and tonumber(year) or today:year()
    if year < 100 then -- two-digit year pivot around year < 2035
        year = year + (year < 35 and 2000 or 1900)
    end
    hour = hour and tonum(hour,0,apm and 12 or 24,'hour') or 12
    if apm == 'pm' then
        hour = hour + 12
    end
    min = min and tonum(min,0,59) or 0
    sec = sec and tonum(sec,0,60) or 0  --60 used to indicate leap second
    local res = Date {year = year, month = month, day = day, hour = hour, min = min, sec = sec}
    if tz then -- ISO 8601 UTC time
        local corrected = false
        if tz.h ~= 0 then res:add {hour = -tz.h}; corrected = true end
        if tz.m ~= 0 then res:add {min = -tz.m}; corrected = true end
        res.utc = true
        -- we're in UTC, so let's go local...
        if corrected then
            res = res:toLocal()
        end-- we're UTC!
    end
    return res
end

function parse_date (s)
    local ok, d = pcall(parse_date_unsafe,s)
    if not ok then -- error
        d = d:gsub('.-:%d+: ','')
        return nil, d
    else
        return d
    end
end

return Date


end,

["pl.dir"] = function()
--------------------
-- Module: 'pl.dir'
--------------------
--- Listing files in directories and creating/removing directory paths.
--
-- Dependencies: `pl.utils`, `pl.path`
--
-- Soft Dependencies: `alien`, `ffi` (either are used on Windows for copying/moving files)
-- @module pl.dir

local utils = require 'pl.utils'
local path = require 'pl.path'
local is_windows = path.is_windows
local ldir = path.dir
local mkdir = path.mkdir
local rmdir = path.rmdir
local sub = string.sub
local os,pcall,ipairs,pairs,require,setmetatable = os,pcall,ipairs,pairs,require,setmetatable
local remove = os.remove
local append = table.insert
local assert_arg,assert_string,raise = utils.assert_arg,utils.assert_string,utils.raise

local exists, isdir = path.exists, path.isdir
local sep = path.sep

local dir = {}

local function makelist(l)
    return setmetatable(l, require('pl.List'))
end

local function assert_dir (n,val)
    assert_arg(n,val,'string',path.isdir,'not a directory',4)
end

local function filemask(mask)
    mask = utils.escape(path.normcase(mask))
    return '^'..mask:gsub('%%%*','.*'):gsub('%%%?','.')..'$'
end

--- Test whether a file name matches a shell pattern.
-- Both parameters are case-normalized if operating system is
-- case-insensitive.
-- @string filename A file name.
-- @string pattern A shell pattern. The only special characters are
-- `'*'` and `'?'`: `'*'` matches any sequence of characters and
-- `'?'` matches any single character.
-- @treturn bool
-- @raise dir and mask must be strings
function dir.fnmatch(filename,pattern)
    assert_string(1,filename)
    assert_string(2,pattern)
    return path.normcase(filename):find(filemask(pattern)) ~= nil
end

--- Return a list of all file names within an array which match a pattern.
-- @tab filenames An array containing file names.
-- @string pattern A shell pattern (see `fnmatch`).
-- @treturn List(string) List of matching file names.
-- @raise dir and mask must be strings
function dir.filter(filenames,pattern)
    assert_arg(1,filenames,'table')
    assert_string(2,pattern)
    local res = {}
    local mask = filemask(pattern)
    for i,f in ipairs(filenames) do
        if path.normcase(f):find(mask) then append(res,f) end
    end
    return makelist(res)
end

local function _listfiles(dirname,filemode,match)
    local res = {}
    local check = utils.choose(filemode,path.isfile,path.isdir)
    if not dirname then dirname = '.' end
    for f in ldir(dirname) do
        if f ~= '.' and f ~= '..' then
            local p = path.join(dirname,f)
            if check(p) and (not match or match(f)) then
                append(res,p)
            end
        end
    end
    return makelist(res)
end

--- return a list of all files in a directory which match a shell pattern.
-- @string[opt='.'] dirname A directory.
-- @string[opt] mask A shell pattern (see `fnmatch`). If not given, all files are returned.
-- @treturn {string} list of files
-- @raise dirname and mask must be strings
function dir.getfiles(dirname,mask)
    dirname = dirname or '.'
    assert_dir(1,dirname)
    if mask then assert_string(2,mask) end
    local match
    if mask then
        mask = filemask(mask)
        match = function(f)
            return path.normcase(f):find(mask)
        end
    end
    return _listfiles(dirname,true,match)
end

--- return a list of all subdirectories of the directory.
-- @string[opt='.'] dirname A directory.
-- @treturn {string} a list of directories
-- @raise dir must be a valid directory
function dir.getdirectories(dirname)
    dirname = dirname or '.'
    assert_dir(1,dirname)
    return _listfiles(dirname,false)
end

local alien,ffi,ffi_checked,CopyFile,MoveFile,GetLastError,win32_errors,cmd_tmpfile

local function execute_command(cmd,parms)
   if not cmd_tmpfile then cmd_tmpfile = path.tmpname () end
   local err = path.is_windows and ' > ' or ' 2> '
    cmd = cmd..' '..parms..err..utils.quote_arg(cmd_tmpfile)
    local ret = utils.execute(cmd)
    if not ret then
        local err = (utils.readfile(cmd_tmpfile):gsub('\n(.*)',''))
        remove(cmd_tmpfile)
        return false,err
    else
        remove(cmd_tmpfile)
        return true
    end
end

local function find_ffi_copyfile ()
    if not ffi_checked then
        ffi_checked = true
        local res
        res,alien = pcall(require,'alien')
        if not res then
            alien = nil
            res, ffi = pcall(require,'ffi')
        end
        if not res then
            ffi = nil
            return
        end
    else
        return
    end
    if alien then
        -- register the Win32 CopyFile and MoveFile functions
        local kernel = alien.load('kernel32.dll')
        CopyFile = kernel.CopyFileA
        CopyFile:types{'string','string','int',ret='int',abi='stdcall'}
        MoveFile = kernel.MoveFileA
        MoveFile:types{'string','string',ret='int',abi='stdcall'}
        GetLastError = kernel.GetLastError
        GetLastError:types{ret ='int', abi='stdcall'}
    elseif ffi then
        ffi.cdef [[
            int CopyFileA(const char *src, const char *dest, int iovr);
            int MoveFileA(const char *src, const char *dest);
            int GetLastError();
        ]]
        CopyFile = ffi.C.CopyFileA
        MoveFile = ffi.C.MoveFileA
        GetLastError = ffi.C.GetLastError
    end
    win32_errors = {
        ERROR_FILE_NOT_FOUND    =         2,
        ERROR_PATH_NOT_FOUND    =         3,
        ERROR_ACCESS_DENIED    =          5,
        ERROR_WRITE_PROTECT    =          19,
        ERROR_BAD_UNIT         =          20,
        ERROR_NOT_READY        =          21,
        ERROR_WRITE_FAULT      =          29,
        ERROR_READ_FAULT       =          30,
        ERROR_SHARING_VIOLATION =         32,
        ERROR_LOCK_VIOLATION    =         33,
        ERROR_HANDLE_DISK_FULL  =         39,
        ERROR_BAD_NETPATH       =         53,
        ERROR_NETWORK_BUSY      =         54,
        ERROR_DEV_NOT_EXIST     =         55,
        ERROR_FILE_EXISTS       =         80,
        ERROR_OPEN_FAILED       =         110,
        ERROR_INVALID_NAME      =         123,
        ERROR_BAD_PATHNAME      =         161,
        ERROR_ALREADY_EXISTS    =         183,
    }
end

local function two_arguments (f1,f2)
    return utils.quote_arg(f1)..' '..utils.quote_arg(f2)
end

local function file_op (is_copy,src,dest,flag)
    if flag == 1 and path.exists(dest) then
        return false,"cannot overwrite destination"
    end
    if is_windows then
        -- if we haven't tried to load Alien/LuaJIT FFI before, then do so
        find_ffi_copyfile()
        -- fallback if there's no Alien, just use DOS commands *shudder*
        -- 'rename' involves a copy and then deleting the source.
        if not CopyFile then
            if path.is_windows then
                src = src:gsub("/","\\")
                dest = dest:gsub("/","\\")
            end
            local res, err = execute_command('copy',two_arguments(src,dest))
            if not res then return false,err end
            if not is_copy then
                return execute_command('del',utils.quote_arg(src))
            end
            return true
        else
            if path.isdir(dest) then
                dest = path.join(dest,path.basename(src))
            end
            local ret
            if is_copy then ret = CopyFile(src,dest,flag)
            else ret = MoveFile(src,dest) end
            if ret == 0 then
                local err = GetLastError()
                for name,value in pairs(win32_errors) do
                    if value == err then return false,name end
                end
                return false,"Error #"..err
            else return true
            end
        end
    else -- for Unix, just use cp for now
        return execute_command(is_copy and 'cp' or 'mv',
            two_arguments(src,dest))
    end
end

--- copy a file.
-- @string src source file
-- @string dest destination file or directory
-- @bool flag true if you want to force the copy (default)
-- @treturn bool operation succeeded
-- @raise src and dest must be strings
function dir.copyfile (src,dest,flag)
    assert_string(1,src)
    assert_string(2,dest)
    flag = flag==nil or flag
    return file_op(true,src,dest,flag and 0 or 1)
end

--- move a file.
-- @string src source file
-- @string dest destination file or directory
-- @treturn bool operation succeeded
-- @raise src and dest must be strings
function dir.movefile (src,dest)
    assert_string(1,src)
    assert_string(2,dest)
    return file_op(false,src,dest,0)
end

local function _dirfiles(dirname,attrib)
    local dirs = {}
    local files = {}
    for f in ldir(dirname) do
        if f ~= '.' and f ~= '..' then
            local p = path.join(dirname,f)
            local mode = attrib(p,'mode')
            if mode=='directory' then
                append(dirs,f)
            else
                append(files,f)
            end
        end
    end
    return makelist(dirs), makelist(files)
end


--- return an iterator which walks through a directory tree starting at root.
-- The iterator returns (root,dirs,files)
-- Note that dirs and files are lists of names (i.e. you must say path.join(root,d)
-- to get the actual full path)
-- If bottom_up is false (or not present), then the entries at the current level are returned
-- before we go deeper. This means that you can modify the returned list of directories before
-- continuing.
-- This is a clone of os.walk from the Python libraries.
-- @string root A starting directory
-- @bool bottom_up False if we start listing entries immediately.
-- @bool follow_links follow symbolic links
-- @return an iterator returning root,dirs,files
-- @raise root must be a directory
function dir.walk(root,bottom_up,follow_links)
    assert_dir(1,root)
    local attrib
    if path.is_windows or not follow_links then
        attrib = path.attrib
    else
        attrib = path.link_attrib
    end

    local to_scan = { root }
    local to_return = {}
    local iter = function()
        while #to_scan > 0 do
            local current_root = table.remove(to_scan)
            local dirs,files = _dirfiles(current_root, attrib)
            for _, d in ipairs(dirs) do
                table.insert(to_scan, current_root..path.sep..d)
            end
            if not bottom_up then
                return current_root, dirs, files
            else
                table.insert(to_return, { current_root, dirs, files })
            end
        end
        if #to_return > 0 then
            return utils.unpack(table.remove(to_return))
        end
    end

    return iter
end

--- remove a whole directory tree.
-- Symlinks in the tree will be deleted without following them.
-- @string fullpath A directory path (must be an actual directory, not a symlink)
-- @return true or nil
-- @return error if failed
-- @raise fullpath must be a string
function dir.rmtree(fullpath)
    assert_dir(1,fullpath)
    if path.islink(fullpath) then return false,'will not follow symlink' end
    for root,dirs,files in dir.walk(fullpath,true) do
        if path.islink(root) then
            -- sub dir is a link, remove link, do not follow
            if is_windows then
                -- Windows requires using "rmdir". Deleting the link like a file
                -- will instead delete all files from the target directory!!
                local res, err = rmdir(root)
                if not res then return nil,err .. ": " .. root end
            else
                local res, err = remove(root)
                if not res then return nil,err .. ": " .. root end
            end
        else
            for i,f in ipairs(files) do
                local res, err = remove(path.join(root,f))
                if not res then return nil,err .. ": " .. path.join(root,f) end
            end
            local res, err = rmdir(root)
            if not res then return nil,err .. ": " .. root end
        end
    end
    return true
end


do
  local dirpat
  if path.is_windows then
      dirpat = '(.+)\\[^\\]+$'
  else
      dirpat = '(.+)/[^/]+$'
  end

  local _makepath
  function _makepath(p)
      -- windows root drive case
      if p:find '^%a:[\\]*$' then
          return true
      end
      if not path.isdir(p) then
          local subp = p:match(dirpat)
          if subp then
            local ok, err = _makepath(subp)
            if not ok then return nil, err end
          end
          return mkdir(p)
      else
          return true
      end
  end

  --- create a directory path.
  -- This will create subdirectories as necessary!
  -- @string p A directory path
  -- @return true on success, nil + errormsg on failure
  -- @raise failure to create
  function dir.makepath (p)
      assert_string(1,p)
      if path.is_windows then
          p = p:gsub("/", "\\")
      end
      return _makepath(path.abspath(p))
  end
end

--- clone a directory tree. Will always try to create a new directory structure
-- if necessary.
-- @string path1 the base path of the source tree
-- @string path2 the new base path for the destination
-- @func file_fun an optional function to apply on all files
-- @bool verbose an optional boolean to control the verbosity of the output.
--  It can also be a logging function that behaves like print()
-- @return true, or nil
-- @return error message, or list of failed directory creations
-- @return list of failed file operations
-- @raise path1 and path2 must be strings
-- @usage clonetree('.','../backup',copyfile)
function dir.clonetree (path1,path2,file_fun,verbose)
    assert_string(1,path1)
    assert_string(2,path2)
    if verbose == true then verbose = print end
    local abspath,normcase,isdir,join = path.abspath,path.normcase,path.isdir,path.join
    local faildirs,failfiles = {},{}
    if not isdir(path1) then return raise 'source is not a valid directory' end
    path1 = abspath(normcase(path1))
    path2 = abspath(normcase(path2))
    if verbose then verbose('normalized:',path1,path2) end
    -- particularly NB that the new path isn't fully contained in the old path
    if path1 == path2 then return raise "paths are the same" end
    local _,i2 = path2:find(path1,1,true)
    if i2 == #path1 and path2:sub(i2+1,i2+1) == path.sep then
        return raise 'destination is a subdirectory of the source'
    end
    local cp = path.common_prefix (path1,path2)
    local idx = #cp
    if idx == 0 then -- no common path, but watch out for Windows paths!
        if path1:sub(2,2) == ':' then idx = 3 end
    end
    for root,dirs,files in dir.walk(path1) do
        local opath = path2..root:sub(idx)
        if verbose then verbose('paths:',opath,root) end
        if not isdir(opath) then
            local ret = dir.makepath(opath)
            if not ret then append(faildirs,opath) end
            if verbose then verbose('creating:',opath,ret) end
        end
        if file_fun then
            for i,f in ipairs(files) do
                local p1 = join(root,f)
                local p2 = join(opath,f)
                local ret = file_fun(p1,p2)
                if not ret then append(failfiles,p2) end
                if verbose then
                    verbose('files:',p1,p2,ret)
                end
            end
        end
    end
    return true,faildirs,failfiles
end


-- each entry of the stack is an array with three items:
-- 1. the name of the directory
-- 2. the lfs iterator function
-- 3. the lfs iterator userdata
local function treeiter(iterstack)
    local diriter = iterstack[#iterstack]
    if not diriter then
      return -- done
    end

    local dirname = diriter[1]
    local entry = diriter[2](diriter[3])
    if not entry then
      table.remove(iterstack)
      return treeiter(iterstack) -- tail-call to try next
    end

    if entry ~= "." and entry ~= ".." then
        entry = dirname .. sep .. entry
        if exists(entry) then  -- Just in case a symlink is broken.
            local is_dir = isdir(entry)
            if is_dir then
                table.insert(iterstack, { entry, ldir(entry) })
            end
            return entry, is_dir
        end
    end

    return treeiter(iterstack) -- tail-call to try next
end


--- return an iterator over all entries in a directory tree
-- @string d a directory
-- @return an iterator giving pathname and mode (true for dir, false otherwise)
-- @raise d must be a non-empty string
function dir.dirtree( d )
    assert( d and d ~= "", "directory parameter is missing or empty" )

    local last = sub ( d, -1 )
    if last == sep or last == '/' then
        d = sub( d, 1, -2 )
    end

    local iterstack = { {d, ldir(d)} }

    return treeiter, iterstack
end


--- Recursively returns all the file starting at 'path'. It can optionally take a shell pattern and
-- only returns files that match 'shell_pattern'. If a pattern is given it will do a case insensitive search.
-- @string[opt='.'] start_path  A directory.
-- @string[opt='*'] shell_pattern A shell pattern (see `fnmatch`).
-- @treturn List(string) containing all the files found recursively starting at 'path' and filtered by 'shell_pattern'.
-- @raise start_path must be a directory
function dir.getallfiles( start_path, shell_pattern )
    start_path = start_path or '.'
    assert_dir(1,start_path)
    shell_pattern = shell_pattern or "*"

    local files = {}
    local normcase = path.normcase
    for filename, mode in dir.dirtree( start_path ) do
        if not mode then
            local mask = filemask( shell_pattern )
            if normcase(filename):find( mask ) then
                files[#files + 1] = filename
            end
        end
    end

    return makelist(files)
end

return dir

end,

["pl.file"] = function()
--------------------
-- Module: 'pl.file'
--------------------
--- File manipulation functions: reading, writing, moving and copying.
--
-- This module wraps a number of functions from other modules into a
-- file related module for convenience.
--
-- Dependencies: `pl.utils`, `pl.dir`, `pl.path`
-- @module pl.file
local os = os
local utils = require 'pl.utils'
local dir = require 'pl.dir'
local path = require 'pl.path'

local file = {}

--- return the contents of a file as a string.
-- This function is a copy of `utils.readfile`.
-- @function file.read
file.read = utils.readfile

--- write a string to a file.
-- This function is a copy of `utils.writefile`.
-- @function file.write
file.write = utils.writefile

--- copy a file.
-- This function is a copy of `dir.copyfile`.
-- @function file.copy
file.copy = dir.copyfile

--- move a file.
-- This function is a copy of `dir.movefile`.
-- @function file.move
file.move = dir.movefile

--- Return the time of last access as the number of seconds since the epoch.
-- This function is a copy of `path.getatime`.
-- @function file.access_time
file.access_time = path.getatime

---Return when the file was created.
-- This function is a copy of `path.getctime`.
-- @function file.creation_time
file.creation_time = path.getctime

--- Return the time of last modification.
-- This function is a copy of `path.getmtime`.
-- @function file.modified_time
file.modified_time = path.getmtime

--- Delete a file.
-- This function is a copy of `os.remove`.
-- @function file.delete
file.delete = os.remove

return file

end,

["pl.func"] = function()
--------------------
-- Module: 'pl.func'
--------------------
--- Functional helpers like composition, binding and placeholder expressions.
-- Placeholder expressions are useful for short anonymous functions, and were
-- inspired by the Boost Lambda library.
--
--    > utils.import 'pl.func'
--    > ls = List{10,20,30}
--    > = ls:map(_1+1)
--    {11,21,31}
--
-- They can also be used to _bind_ particular arguments of a function.
--
--    > p = bind(print,'start>',_0)
--    > p(10,20,30)
--    > start>   10   20  30
--
-- See @{07-functional.md.Creating_Functions_from_Functions|the Guide}
--
-- Dependencies: `pl.utils`, `pl.tablex`
-- @module pl.func
local type,setmetatable,getmetatable,rawset = type,setmetatable,getmetatable,rawset
local concat,append = table.concat,table.insert
local tostring = tostring
local utils = require 'pl.utils'
local pairs,rawget,unpack,pack = pairs,rawget,utils.unpack,utils.pack
local tablex = require 'pl.tablex'
local map = tablex.map
local _DEBUG = rawget(_G,'_DEBUG')
local assert_arg = utils.assert_arg

local func = {}

-- metatable for Placeholder Expressions (PE)
local _PEMT = {}

local function P (t)
    setmetatable(t,_PEMT)
    return t
end

func.PE = P

local function isPE (obj)
    return getmetatable(obj) == _PEMT
end

func.isPE = isPE

-- construct a placeholder variable (e.g _1 and _2)
local function PH (idx)
    return P {op='X',repr='_'..idx, index=idx}
end

-- construct a constant placeholder variable (e.g _C1 and _C2)
local function CPH (idx)
    return P {op='X',repr='_C'..idx, index=idx}
end

func._1,func._2,func._3,func._4,func._5 = PH(1),PH(2),PH(3),PH(4),PH(5)
func._0 = P{op='X',repr='...',index=0}

function func.Var (name)
    local ls = utils.split(name,'[%s,]+')
    local res = {}
    for i = 1, #ls do
        append(res,P{op='X',repr=ls[i],index=0})
    end
    return unpack(res)
end

function func._ (value)
    return P{op='X',repr=value,index='wrap'}
end

local repr

func.Nil = func.Var 'nil'

function _PEMT.__index(obj,key)
    return P{op='[]',obj,key}
end

function _PEMT.__call(fun,...)
    return P{op='()',fun,...}
end

function _PEMT.__tostring (e)
    return repr(e)
end

function _PEMT.__unm(arg)
    return P{op='unm',arg}
end

function func.Not (arg)
    return P{op='not',arg}
end

function func.Len (arg)
    return P{op='#',arg}
end


local function binreg(context,t)
    for name,op in pairs(t) do
        rawset(context,name,function(x,y)
            return P{op=op,x,y}
        end)
    end
end

local function import_name (name,fun,context)
    rawset(context,name,function(...)
        return P{op='()',fun,...}
    end)
end

local imported_functions = {}

local function is_global_table (n)
    return type(_G[n]) == 'table'
end

--- wrap a table of functions. This makes them available for use in
-- placeholder expressions.
-- @string tname a table name
-- @tab context context to put results, defaults to environment of caller
function func.import(tname,context)
    assert_arg(1,tname,'string',is_global_table,'arg# 1: not a name of a global table')
    local t = _G[tname]
    context = context or _G
    for name,fun in pairs(t) do
        import_name(name,fun,context)
        imported_functions[fun] = name
    end
end

--- register a function for use in placeholder expressions.
-- @func fun a function
-- @string[opt] name an optional name
-- @return a placeholder functiond
function func.register (fun,name)
    assert_arg(1,fun,'function')
    if name then
        assert_arg(2,name,'string')
        imported_functions[fun] = name
    end
    return function(...)
        return P{op='()',fun,...}
    end
end

function func.lookup_imported_name (fun)
    return imported_functions[fun]
end

local function _arg(...) return ... end

function func.Args (...)
    return P{op='()',_arg,...}
end

-- binary operators with their precedences (see Lua manual)
-- precedences might be incremented by one before use depending on
-- left- or right-associativity, space them out
local binary_operators = {
    ['or'] = 0,
    ['and'] = 2,
    ['=='] = 4, ['~='] = 4, ['<'] = 4, ['>'] = 4,  ['<='] = 4,   ['>='] = 4,
    ['..'] = 6,
    ['+'] = 8, ['-'] = 8,
    ['*'] = 10, ['/'] = 10, ['%'] = 10,
    ['^'] = 14
}

-- unary operators with their precedences
local unary_operators = {
    ['not'] = 12, ['#'] = 12, ['unm'] = 12
}

-- comparisons (as prefix functions)
binreg (func,{And='and',Or='or',Eq='==',Lt='<',Gt='>',Le='<=',Ge='>='})

-- standard binary operators (as metamethods)
binreg (_PEMT,{__add='+',__sub='-',__mul='*',__div='/',__mod='%',__pow='^',__concat='..'})

binreg (_PEMT,{__eq='=='})

--- all elements of a table except the first.
-- @tab ls a list-like table.
function func.tail (ls)
    assert_arg(1,ls,'table')
    local res = {}
    for i = 2,#ls do
        append(res,ls[i])
    end
    return res
end

--- create a string representation of a placeholder expression.
-- @param e a placeholder expression
-- @param lastpred not used
function repr (e,lastpred)
    local tail = func.tail
    if isPE(e) then
        local pred = binary_operators[e.op] or unary_operators[e.op]
        if pred then
            -- binary or unary operator
            local s
            if binary_operators[e.op] then
                local left_pred = pred
                local right_pred = pred
                if e.op == '..' or e.op == '^' then
                    left_pred = left_pred + 1
                else
                    right_pred = right_pred + 1
                end
                local left_arg = repr(e[1], left_pred)
                local right_arg = repr(e[2], right_pred)
                s = left_arg..' '..e.op..' '..right_arg
            else
                local op = e.op == 'unm' and '-' or e.op
                s = op..' '..repr(e[1], pred)
            end
            if lastpred and lastpred > pred then
                s = '('..s..')'
            end
            return s
        else -- either postfix, or a placeholder
            local ls = map(repr,e)
            if e.op == '[]' then
                return ls[1]..'['..ls[2]..']'
            elseif e.op == '()' then
                local fn
                if ls[1] ~= nil then -- was _args, undeclared!
                    fn = ls[1]
                else
                    fn = ''
                end
                return fn..'('..concat(tail(ls),',')..')'
            else
                return e.repr
            end
        end
    elseif type(e) == 'string' then
        return '"'..e..'"'
    elseif type(e) == 'function' then
        local name = func.lookup_imported_name(e)
        if name then return name else return tostring(e) end
    else
        return tostring(e) --should not really get here!
    end
end
func.repr = repr

-- collect all the non-PE values in this PE into vlist, and replace each occurrence
-- with a constant PH (_C1, etc). Return the maximum placeholder index found.
local collect_values
function collect_values (e,vlist)
    if isPE(e) then
        if e.op ~= 'X' then
            local m = 0
            for i = 1,#e do
                local subx = e[i]
                local pe = isPE(subx)
                if pe then
                    if subx.op == 'X' and subx.index == 'wrap' then
                        subx = subx.repr
                        pe = false
                    else
                        m = math.max(m,collect_values(subx,vlist))
                    end
                end
                if not pe then
                    append(vlist,subx)
                    e[i] = CPH(#vlist)
                end
            end
            return m
        else -- was a placeholder, it has an index...
            return e.index
        end
    else -- plain value has no placeholder dependence
        return 0
    end
end
func.collect_values = collect_values

--- instantiate a PE into an actual function. First we find the largest placeholder used,
-- e.g. _2; from this a list of the formal parameters can be build. Then we collect and replace
-- any non-PE values from the PE, and build up a constant binding list.
-- Finally, the expression can be compiled, and e.__PE_function is set.
-- @param e a placeholder expression
-- @return a function
function func.instantiate (e)
    local consts,values,parms = {},{},{}
    local rep, err, fun
    local n = func.collect_values(e,values)
    for i = 1,#values do
        append(consts,'_C'..i)
        if _DEBUG then print(i,values[i]) end
    end
    for i =1,n do
        append(parms,'_'..i)
    end
    consts = concat(consts,',')
    parms = concat(parms,',')
    rep = repr(e)
    local fstr = ('return function(%s) return function(%s) return %s end end'):format(consts,parms,rep)
    if _DEBUG then print(fstr) end
    fun,err = utils.load(fstr,'fun')
    if not fun then return nil,err end
    fun = fun()  -- get wrapper
    fun = fun(unpack(values)) -- call wrapper (values could be empty)
    e.__PE_function = fun
    return fun
end

--- instantiate a PE unless it has already been done.
-- @param e a placeholder expression
-- @return the function
function func.I(e)
    if rawget(e,'__PE_function')  then
        return e.__PE_function
    else return func.instantiate(e)
    end
end

utils.add_function_factory(_PEMT,func.I)

--- bind the first parameter of the function to a value.
-- @function func.bind1
-- @func fn a function of one or more arguments
-- @param p a value
-- @return a function of one less argument
-- @usage (bind1(math.max,10))(20) == math.max(10,20)
func.bind1 = utils.bind1
func.curry = func.bind1

--- create a function which chains multiple functions.
-- @func f a function of at least one argument
-- @func g a function of at least one argument
-- @param ... additional functions to compose
-- @return a function
-- @usage printf = compose(io.write, string.format)
-- @usage printf = compose(io.write, string.lower, string.format)
function func.compose (...)
    local args = pack(...)
    return tablex.reduce(function(f, g)
      return function(...)
        return f(g(...))
      end
    end, args)
end

--- bind the arguments of a function to given values.
-- `bind(fn,v,_2)` is equivalent to `bind1(fn,v)`.
-- @func fn a function of at least one argument
-- @param ... values or placeholder variables
-- @return a function
-- @usage (bind(f,_1,a))(b) == f(a,b)
-- @usage (bind(f,_2,_1))(a,b) == f(b,a)
function func.bind(fn,...)
    local args = pack(...)
    local holders,parms,bvalues,values = {},{},{'fn'},{}
    local nv,maxplace,varargs = 1,0,false
    for i = 1,args.n do
        local a = args[i]
        if isPE(a) and a.op == 'X' then
            append(holders,a.repr)
            maxplace = math.max(maxplace,a.index)
            if a.index == 0 then varargs = true end
        else
            local v = '_v'..nv
            append(bvalues,v)
            append(holders,v)
            append(values,a)
            nv = nv + 1
        end
    end
    for np = 1,maxplace do
        append(parms,'_'..np)
    end
    if varargs then append(parms,'...') end
    bvalues = concat(bvalues,',')
    parms = concat(parms,',')
    holders = concat(holders,',')
    local fstr = ([[
return function (%s)
    return function(%s) return fn(%s) end
end
]]):format(bvalues,parms,holders)
    if _DEBUG then print(fstr) end
    local res = utils.load(fstr)
    res = res()
    return res(fn,unpack(values))
end

return func



end,

["pl.import_into"] = function()
--------------------
-- Module: 'pl.import_into'
--------------------
--------------
-- PL loader, for loading all PL libraries, only on demand.
-- Whenever a module is implicitly accessed, the table will have the module automatically injected.
-- (e.g. `_ENV.tablex`)
-- then that module is dynamically loaded. The submodules are all brought into
-- the table that is provided as the argument, or returned in a new table.
-- If a table is provided, that table's metatable is clobbered, but the values are not.
-- This module returns a single function, which is passed the environment.
-- If this is `true`, then return a 'shadow table' as the module
-- See @{01-introduction.md.To_Inject_or_not_to_Inject_|the Guide}

-- @module pl.import_into

return function(env)
    local mod
    if env == true then
        mod = {}
        env = {}
    end
    local env = env or {}

    local modules = {
        utils = true,path=true,dir=true,tablex=true,stringio=true,sip=true,
        input=true,seq=true,lexer=true,stringx=true,
        config=true,pretty=true,data=true,func=true,text=true,
        operator=true,lapp=true,array2d=true,
        comprehension=true,xml=true,types=true,
        test = true, app = true, file = true, class = true,
        luabalanced = true, permute = true, template = true,
        url = true, compat = true,
        -- classes --
        List = true, Map = true, Set = true,
        OrderedMap = true, MultiMap = true, Date = true,
    }
    rawset(env,'utils',require 'pl.utils')

    for name,klass in pairs(env.utils.stdmt) do
        klass.__index = function(t,key)
            return require ('pl.'..name)[key]
        end;
    end

    -- ensure that we play nice with libraries that also attach a metatable
    -- to the global table; always forward to a custom __index if we don't
    -- match

    local _hook,_prev_index
    local gmt = {}
    local prevenvmt = getmetatable(env)
    if prevenvmt then
        _prev_index = prevenvmt.__index
        if prevenvmt.__newindex then
            gmt.__newindex = prevenvmt.__newindex
        end
    end

    function gmt.hook(handler)
        _hook = handler
    end

    function gmt.__index(t,name)
        local found = modules[name]
        -- either true, or the name of the module containing this class.
        -- either way, we load the required module and make it globally available.
        if found then
            -- e..g pretty.dump causes pl.pretty to become available as 'pretty'
            rawset(env,name,require('pl.'..name))
            return env[name]
        else
            local res
            if _hook then
                res = _hook(t,name)
                if res then return res end
            end
            if _prev_index then
                return _prev_index(t,name)
            end
        end
    end

    if mod then
        function gmt.__newindex(t,name,value)
            mod[name] = value
            rawset(t,name,value)
        end
    end

    setmetatable(env,gmt)

    return env,mod or env
end

end,

["pl.init"] = function()
--------------------
-- Module: 'pl.init'
--------------------
--------------
-- Entry point for loading all PL libraries only on demand, into the global space.
-- Requiring 'pl' means that whenever a module is implicitly accessed
-- (e.g. `utils.split`)
-- then that module is dynamically loaded. The submodules are all brought into
-- the global space.
--Updated to use @{pl.import_into}
-- @module pl
require'pl.import_into'(_G)

if rawget(_G,'PENLIGHT_STRICT') then require 'pl.strict' end

end,

["pl.input"] = function()
--------------------
-- Module: 'pl.input'
--------------------
--- Iterators for extracting words or numbers from an input source.
--
--    require 'pl'
--    local total,n = seq.sum(input.numbers())
--    print('average',total/n)
--
-- _source_ is defined as a string or a file-like object (i.e. has a read() method which returns the next line)
--
-- See @{06-data.md.Reading_Unstructured_Text_Data|here}
--
-- Dependencies: `pl.utils`
-- @module pl.input
local strfind = string.find
local strsub = string.sub
local strmatch = string.match
local utils = require 'pl.utils'
local unpack = utils.unpack
local pairs,type,tonumber = pairs,type,tonumber
local patterns = utils.patterns
local io = io

local input = {}

--- create an iterator over all tokens.
-- based on allwords from PiL, 7.1
-- @func getter any function that returns a line of text
-- @string pattern
-- @string[opt] fn  Optionally can pass a function to process each token as it's found.
-- @return an iterator
function input.alltokens (getter,pattern,fn)
    local line = getter()  -- current line
    local pos = 1           -- current position in the line
    return function ()      -- iterator function
        while line do         -- repeat while there are lines
          local s, e = strfind(line, pattern, pos)
          if s then           -- found a word?
            pos = e + 1       -- next position is after this token
            local res = strsub(line, s, e)     -- return the token
            if fn then res = fn(res) end
            return res
          else
            line = getter()  -- token not found; try next line
            pos = 1           -- restart from first position
          end
        end
        return nil            -- no more lines: end of traversal
   end
end
local alltokens = input.alltokens

-- question: shd this _split_ a string containing line feeds?

--- create a function which grabs the next value from a source. If the source is a string, then the getter
-- will return the string and thereafter return nil. If not specified then the source is assumed to be stdin.
-- @param f a string or a file-like object (i.e. has a read() method which returns the next line)
-- @return a getter function
function input.create_getter(f)
    if f then
        if type(f) == 'string' then
            local ls = utils.split(f,'\n')
            local i,n = 0,#ls
            return function()
                i = i + 1
                if i > n then return nil end
                return ls[i]
            end
        else
            -- anything that supports the read() method!
            if not f.read then error('not a file-like object') end
            return function() return f:read() end
        end
    else
        return io.read  -- i.e. just read from stdin
    end
end

--- generate a sequence of numbers from a source.
-- @param f A source
-- @return An iterator
function input.numbers(f)
    return alltokens(input.create_getter(f),
        '('..patterns.FLOAT..')',tonumber)
end

--- generate a sequence of words from a source.
-- @param f A source
-- @return An iterator
function input.words(f)
    return alltokens(input.create_getter(f),"%w+")
end

local function apply_tonumber (no_fail,...)
    local args = {...}
    for i = 1,#args do
        local n = tonumber(args[i])
        if  n == nil then
            if not no_fail then return nil,args[i] end
        else
            args[i] = n
        end
    end
    return args
end

--- parse an input source into fields.
-- By default, will fail if it cannot convert a field to a number.
-- @param ids a list of field indices, or a maximum field index
-- @string delim delimiter to parse fields (default space)
-- @param f a source @see create_getter
-- @tab opts option table, `{no_fail=true}`
-- @return an iterator with the field values
-- @usage for x,y in fields {2,3} do print(x,y) end -- 2nd and 3rd fields from stdin
function input.fields (ids,delim,f,opts)
  local sep
  local s
  local getter = input.create_getter(f)
  local no_fail = opts and opts.no_fail
  local no_convert = opts and opts.no_convert
  if not delim or delim == ' ' then
      delim = '%s'
      sep = '%s+'
      s = '%s*'
  else
      sep = delim
      s = ''
  end
  local max_id = 0
  if type(ids) == 'table' then
    for i,id in pairs(ids) do
      if id > max_id then max_id = id end
    end
  else
    max_id = ids
    ids = {}
    for i = 1,max_id do ids[#ids+1] = i end
  end
  local pat = '[^'..delim..']*'
  local k = 1
  for i = 1,max_id do
    if ids[k] == i then
      k = k + 1
      s = s..'('..pat..')'
    else
      s = s..pat
    end
    if i < max_id then
      s = s..sep
    end
  end
  local linecount = 1
  return function()
    local line,results,err
    repeat
        line = getter()
        linecount = linecount + 1
        if not line then return nil end
        if no_convert then
            results = {strmatch(line,s)}
        else
            results,err = apply_tonumber(no_fail,strmatch(line,s))
            if not results then
                utils.quit("line "..(linecount-1)..": cannot convert '"..err.."' to number")
            end
        end
    until #results > 0
    return unpack(results)
  end
end

return input


end,

["pl.lapp"] = function()
--------------------
-- Module: 'pl.lapp'
--------------------
--- Simple command-line parsing using human-readable specification.
-- Supports GNU-style parameters.
--
--      lapp = require 'pl.lapp'
--      local args = lapp [[
--      Does some calculations
--        -o,--offset (default 0.0)  Offset to add to scaled number
--        -s,--scale  (number)  Scaling factor
--        <number> (number) Number to be scaled
--      ]]
--
--      print(args.offset + args.scale * args.number)
--
-- Lines beginning with `'-'` are flags; there may be a short and a long name;
-- lines beginning with `'<var>'` are arguments.  Anything in parens after
-- the flag/argument is either a default, a type name or a range constraint.
--
-- See @{08-additional.md.Command_line_Programs_with_Lapp|the Guide}
--
-- Dependencies: `pl.sip`
-- @module pl.lapp

local status,sip = pcall(require,'pl.sip')
if not status then
    sip = require 'sip'
end
local match = sip.match_at_start
local append,tinsert = table.insert,table.insert

sip.custom_pattern('X','(%a[%w_%-]*)')

local function lines(s) return s:gmatch('([^\n]*)\n') end
local function lstrip(str)  return str:gsub('^%s+','')  end
local function strip(str)  return lstrip(str):gsub('%s+$','') end
local function at(s,k)  return s:sub(k,k) end

local lapp = {}

local open_files,parms,aliases,parmlist,usage,script

lapp.callback = false -- keep Strict happy

local filetypes = {
    stdin = {io.stdin,'file-in'}, stdout = {io.stdout,'file-out'},
    stderr = {io.stderr,'file-out'}
}

--- controls whether to dump usage on error.
-- Defaults to true
lapp.show_usage_error = true

--- quit this script immediately.
-- @string msg optional message
-- @bool no_usage suppress 'usage' display
function lapp.quit(msg,no_usage)
    if no_usage == 'throw' then
        error(msg)
    end
    if msg then
        io.stderr:write(msg..'\n\n')
    end
    if not no_usage then
        io.stderr:write(usage)
    end
    os.exit(1)
end

--- print an error to stderr and quit.
-- @string msg a message
-- @bool no_usage suppress 'usage' display
function lapp.error(msg,no_usage)
    if not lapp.show_usage_error then
        no_usage = true
    elseif lapp.show_usage_error == 'throw' then
        no_usage = 'throw'
    end
    lapp.quit(script..': '..msg,no_usage)
end

--- open a file.
-- This will quit on error, and keep a list of file objects for later cleanup.
-- @string file filename
-- @string[opt] opt same as second parameter of `io.open`
function lapp.open (file,opt)
    local val,err = io.open(file,opt)
    if not val then lapp.error(err,true) end
    append(open_files,val)
    return val
end

--- quit if the condition is false.
-- @bool condn a condition
-- @string msg message text
function lapp.assert(condn,msg)
    if not condn then
        lapp.error(msg)
    end
end

local function range_check(x,min,max,parm)
    lapp.assert(min <= x and max >= x,parm..' out of range')
end

local function xtonumber(s)
    local val = tonumber(s)
    if not val then lapp.error("unable to convert to number: "..s) end
    return val
end

local types = {}

local builtin_types = {string=true,number=true,['file-in']='file',['file-out']='file',boolean=true}

local function convert_parameter(ps,val)
    if ps.converter then
        val = ps.converter(val)
    end
    if ps.type == 'number' then
        val = xtonumber(val)
    elseif builtin_types[ps.type] == 'file' then
        val = lapp.open(val,(ps.type == 'file-in' and 'r') or 'w' )
    elseif ps.type == 'boolean' then
        return val
    end
    if ps.constraint then
        ps.constraint(val)
    end
    return val
end

--- add a new type to Lapp. These appear in parens after the value like
-- a range constraint, e.g. '<ival> (integer) Process PID'
-- @string name name of type
-- @param converter either a function to convert values, or a Lua type name.
-- @func[opt] constraint optional function to verify values, should use lapp.error
-- if failed.
function lapp.add_type (name,converter,constraint)
    types[name] = {converter=converter,constraint=constraint}
end

local function force_short(short)
    lapp.assert(#short==1,short..": short parameters should be one character")
end

-- deducing type of variable from default value;
local function process_default (sval,vtype)
    local val, success
    if not vtype or vtype == 'number' then
        val = tonumber(sval)
    end
    if val then -- we have a number!
        return val,'number'
    elseif filetypes[sval] then
        local ft = filetypes[sval]
        return ft[1],ft[2]
    else
        if sval == 'true' and not vtype then
            return true, 'boolean'
        end
        if sval:match '^["\']' then sval = sval:sub(2,-2) end

        local ps = types[vtype] or {}
        ps.type = vtype

        local show_usage_error = lapp.show_usage_error
        lapp.show_usage_error = "throw"
        success, val = pcall(convert_parameter, ps, sval)
        lapp.show_usage_error = show_usage_error
        if success then
          return val, vtype or 'string'
        end

        return sval,vtype or 'string'
    end
end

--- process a Lapp options string.
-- Usually called as `lapp()`.
-- @string str the options text
-- @tparam {string} args a table of arguments (default is `_G.arg`)
-- @return a table with parameter-value pairs
function lapp.process_options_string(str,args)
    local results = {}
    local varargs
    local arg = args or _G.arg
    open_files = {}
    parms = {}
    aliases = {}
    parmlist = {}

    local function check_varargs(s)
        local res,cnt = s:gsub('^%.%.%.%s*','')
        return res, (cnt > 0)
    end

    local function set_result(ps,parm,val)
        parm = type(parm) == "string" and parm:gsub("%W", "_") or parm -- so foo-bar becomes foo_bar in Lua
        if not ps.varargs then
            results[parm] = val
        else
            if not results[parm] then
                results[parm] = { val }
            else
                append(results[parm],val)
            end
        end
    end

    usage = str

    for _,a in ipairs(arg) do
      if a == "-h" or a == "--help" then
        return lapp.quit()
      end
    end


    for line in lines(str) do
        local res = {}
        local optparm,defval,vtype,constraint,rest
        line = lstrip(line)
        local function check(str)
            return match(str,line,res)
        end

        -- flags: either '-<short>', '-<short>,--<long>' or '--<long>'
        if check '-$v{short}, --$o{long} $' or check '-$v{short} $' or check '--$o{long} $' then
            if res.long then
                optparm = res.long:gsub('[^%w%-]','_')  -- I'm not sure the $o pattern will let anything else through?
                if #res.rest == 1 then optparm = optparm .. res.rest end
                if res.short then aliases[res.short] = optparm  end
            else
                optparm = res.short
            end
            if res.short and not lapp.slack then force_short(res.short) end
            res.rest, varargs = check_varargs(res.rest)
        elseif check '$<{name} $'  then -- is it <parameter_name>?
            -- so <input file...> becomes input_file ...
            optparm,rest = res.name:match '([^%.]+)(.*)'
            -- follow lua legal variable names
            optparm = optparm:sub(1,1):gsub('%A','_') .. optparm:sub(2):gsub('%W', '_')
            varargs = rest == '...'
            append(parmlist,optparm)
        end
        -- this is not a pure doc line and specifies the flag/parameter type
        if res.rest then
            line = res.rest
            res = {}
            local optional
            local defval_str
            -- do we have ([optional] [<type>] [default <val>])?
            if match('$({def} $',line,res) or match('$({def}',line,res) then
                local typespec = strip(res.def)
                local ftype, rest = typespec:match('^(%S+)(.*)$')
                rest = strip(rest)
                if ftype == 'optional' then
                    ftype, rest = rest:match('^(%S+)(.*)$')
                    rest = strip(rest)
                    optional = true
                end
                local default
                if ftype == 'default' then
                    default = true
                    if rest == '' then lapp.error("value must follow default") end
                else -- a type specification
                    if match('$f{min}..$f{max}',ftype,res) then
                        -- a numerical range like 1..10
                        local min,max = res.min,res.max
                        vtype = 'number'
                        constraint = function(x)
                            range_check(x,min,max,optparm)
                        end
                    elseif not ftype:match '|' then -- plain type
                        vtype = ftype
                    else
                        -- 'enum' type is a string which must belong to
                        -- one of several distinct values
                        local enums = ftype
                        local enump = '|' .. enums .. '|'
                        vtype = 'string'
                        constraint = function(s)
                            lapp.assert(enump:find('|'..s..'|', 1, true),
                              "value '"..s.."' not in "..enums
                            )
                        end
                    end
                end
                res.rest = rest
                typespec = res.rest
                -- optional 'default value' clause. Type is inferred as
                -- 'string' or 'number' if there's no explicit type
                if default or match('default $r{rest}',typespec,res) then
                    defval_str = res.rest
                    defval,vtype = process_default(res.rest,vtype)
                end
            else -- must be a plain flag, no extra parameter required
                defval = false
                vtype = 'boolean'
            end
            local ps = {
                type = vtype,
                defval = defval,
                defval_str = defval_str,
                required = defval == nil and not optional,
                comment = res.rest or optparm,
                constraint = constraint,
                varargs = varargs
            }
            varargs = nil
            if types[vtype] then
                local converter = types[vtype].converter
                if type(converter) == 'string' then
                    ps.type = converter
                else
                    ps.converter = converter
                end
                ps.constraint = types[vtype].constraint
            elseif not builtin_types[vtype] and vtype then
                lapp.error(vtype.." is unknown type")
            end
            parms[optparm] = ps
        end
    end
    -- cool, we have our parms, let's parse the command line args
    local iparm = 1
    local iextra = 1
    local i = 1
    local parm,ps,val
    local end_of_flags = false

    local function check_parm (parm)
        local eqi = parm:find '[=:]'
        if eqi then
            tinsert(arg,i+1,parm:sub(eqi+1))
            parm = parm:sub(1,eqi-1)
        end
        return parm,eqi
    end

    local function is_flag (parm)
        return parms[aliases[parm] or parm]
    end

    while i <= #arg do
        local theArg = arg[i]
        local res = {}
        -- after '--' we don't parse args and they end up in
        -- the array part of the result (args[1] etc)
        if theArg == '--' then
            end_of_flags = true
            iparm = #parmlist + 1
            i = i + 1
            theArg = arg[i]
            if not theArg then
                break
            end
        end
        -- look for a flag, -<short flags> or --<long flag>
        if not end_of_flags and (match('--$S{long}',theArg,res) or match('-$S{short}',theArg,res)) then
            if res.long then -- long option
                parm = check_parm(res.long)
            elseif #res.short == 1 or is_flag(res.short) then
                parm = res.short
            else
                local parmstr,eq = check_parm(res.short)
                if not eq then
                    parm = at(parmstr,1)
                    local flag = is_flag(parm)
                    if flag and flag.type ~= 'boolean' then
                    --if isdigit(at(parmstr,2)) then
                        -- a short option followed by a digit is an exception (for AW;))
                        -- push ahead into the arg array
                        tinsert(arg,i+1,parmstr:sub(2))
                    else
                        -- push multiple flags into the arg array!
                        for k = 2,#parmstr do
                            tinsert(arg,i+k-1,'-'..at(parmstr,k))
                        end
                    end
                else
                    parm = parmstr
                end
            end
            if aliases[parm] then parm = aliases[parm] end
            if not parms[parm] and (parm == 'h' or parm == 'help') then
                lapp.quit()
            end
        else -- a parameter
            parm = parmlist[iparm]
            if not parm then
               -- extra unnamed parameters are indexed starting at 1
               parm = iextra
               ps = { type = 'string' }
               parms[parm] = ps
               iextra = iextra + 1
            else
                ps = parms[parm]
            end
            if not ps.varargs then
                iparm = iparm + 1
            end
            val = theArg
        end
        ps = parms[parm]
        if not ps then lapp.error("unrecognized parameter: "..parm) end
        if ps.type ~= 'boolean' then -- we need a value! This should follow
            if not val then
                i = i + 1
                val = arg[i]
                theArg = val
            end
            lapp.assert(val,parm.." was expecting a value")
        else -- toggle boolean flags (usually false -> true)
            val = not ps.defval
        end
        ps.used = true
        val = convert_parameter(ps,val)
        set_result(ps,parm,val)
        if builtin_types[ps.type] == 'file' then
            set_result(ps,parm..'_name',theArg)
        end
        if lapp.callback then
            lapp.callback(parm,theArg,res)
        end
        i = i + 1
        val = nil
    end
    -- check unused parms, set defaults and check if any required parameters were missed
    for parm,ps in pairs(parms) do
        if not ps.used then
            if ps.required then lapp.error("missing required parameter: "..parm) end
            set_result(ps,parm,ps.defval)
            if builtin_types[ps.type] == "file" then
                set_result(ps, parm .. "_name", ps.defval_str)
            end
        end
    end
    return results
end

if arg then
    script = arg[0]
    script = script or rawget(_G,"LAPP_SCRIPT") or "unknown"
    -- strip dir and extension to get current script name
    script = script:gsub('.+[\\/]',''):gsub('%.%a+$','')
else
    script = "inter"
end


setmetatable(lapp, {
    __call = function(tbl,str,args) return lapp.process_options_string(str,args) end,
})


return lapp



end,

["pl.lexer"] = function()
--------------------
-- Module: 'pl.lexer'
--------------------
--- Lexical scanner for creating a sequence of tokens from text.
-- `lexer.scan(s)` returns an iterator over all tokens found in the
-- string `s`. This iterator returns two values, a token type string
-- (such as 'string' for quoted string, 'iden' for identifier) and the value of the
-- token.
--
-- Versions specialized for Lua and C are available; these also handle block comments
-- and classify keywords as 'keyword' tokens. For example:
--
--    > s = 'for i=1,n do'
--    > for t,v in lexer.lua(s)  do print(t,v) end
--    keyword for
--    iden    i
--    =       =
--    number  1
--    ,       ,
--    iden    n
--    keyword do
--
-- See the Guide for further @{06-data.md.Lexical_Scanning|discussion}
-- @module pl.lexer

local strfind = string.find
local strsub = string.sub
local append = table.insert


local function assert_arg(idx,val,tp)
    if type(val) ~= tp then
        error("argument "..idx.." must be "..tp, 2)
    end
end

local lexer = {}

local NUMBER1  = '^[%+%-]?%d+%.?%d*[eE][%+%-]?%d+'
local NUMBER1a = '^[%+%-]?%d*%.%d+[eE][%+%-]?%d+'
local NUMBER2  = '^[%+%-]?%d+%.?%d*'
local NUMBER2a = '^[%+%-]?%d*%.%d+'
local NUMBER3  = '^0x[%da-fA-F]+'
local NUMBER4  = '^%d+%.?%d*[eE][%+%-]?%d+'
local NUMBER4a = '^%d*%.%d+[eE][%+%-]?%d+'
local NUMBER5  = '^%d+%.?%d*'
local NUMBER5a = '^%d*%.%d+'
local IDEN = '^[%a_][%w_]*'
local WSPACE = '^%s+'
local STRING1 = "^(['\"])%1" -- empty string
local STRING2 = [[^(['"])(\*)%2%1]]
local STRING3 = [[^(['"]).-[^\](\*)%2%1]]
local CHAR1 = "^''"
local CHAR2 = [[^'(\*)%1']]
local CHAR3 = [[^'.-[^\](\*)%1']]
local PREPRO = '^#.-[^\\]\n'

local plain_matches,lua_matches,cpp_matches,lua_keyword,cpp_keyword

local function tdump(tok)
    return tok,tok
end

local function ndump(tok,options)
    if options and options.number then
        tok = tonumber(tok)
    end
    return "number",tok
end

-- regular strings, single or double quotes; usually we want them
-- without the quotes
local function sdump(tok,options)
    if options and options.string then
        tok = tok:sub(2,-2)
    end
    return "string",tok
end

-- long Lua strings need extra work to get rid of the quotes
local function sdump_l(tok,options,findres)
    if options and options.string then
        local quotelen = 3
        if findres[3] then
            quotelen = quotelen + findres[3]:len()
        end
        tok = tok:sub(quotelen, -quotelen)
        if tok:sub(1, 1) == "\n" then
            tok = tok:sub(2)
        end
    end
    return "string",tok
end

local function chdump(tok,options)
    if options and options.string then
        tok = tok:sub(2,-2)
    end
    return "char",tok
end

local function cdump(tok)
    return "comment",tok
end

local function wsdump (tok)
    return "space",tok
end

local function pdump (tok)
    return "prepro",tok
end

local function plain_vdump(tok)
    return "iden",tok
end

local function lua_vdump(tok)
    if lua_keyword[tok] then
        return "keyword",tok
    else
        return "iden",tok
    end
end

local function cpp_vdump(tok)
    if cpp_keyword[tok] then
        return "keyword",tok
    else
        return "iden",tok
    end
end

--- create a plain token iterator from a string or file-like object.
-- @tparam string|file s a string or a file-like object with `:read()` method returning lines.
-- @tab matches an optional match table - array of token descriptions.
-- A token is described by a `{pattern, action}` pair, where `pattern` should match
-- token body and `action` is a function called when a token of described type is found.
-- @tab[opt] filter a table of token types to exclude, by default `{space=true}`
-- @tab[opt] options a table of options; by default, `{number=true,string=true}`,
-- which means convert numbers and strip string quotes.
function lexer.scan(s,matches,filter,options)
    local file = type(s) ~= 'string' and s
    filter = filter or {space=true}
    options = options or {number=true,string=true}
    if filter then
        if filter.space then filter[wsdump] = true end
        if filter.comments then
            filter[cdump] = true
        end
    end
    if not matches then
        if not plain_matches then
            plain_matches = {
                {WSPACE,wsdump},
                {NUMBER3,ndump},
                {IDEN,plain_vdump},
                {NUMBER1,ndump},
                {NUMBER1a,ndump},
                {NUMBER2,ndump},
                {NUMBER2a,ndump},
                {STRING1,sdump},
                {STRING2,sdump},
                {STRING3,sdump},
                {'^.',tdump}
            }
        end
        matches = plain_matches
    end

    local line_nr = 0
    local next_line = file and file:read()
    local sz = file and 0 or #s
    local idx = 1

    local tlist_i
    local tlist

    local first_hit = true

    local function iter(res)
        local tp = type(res)

        if tlist then -- returning the inserted token list
            local cur = tlist[tlist_i]
            if cur then
                tlist_i = tlist_i + 1
                return cur[1], cur[2]
            else
                tlist = nil
            end
        end

        if tp == 'string' then -- search up to some special pattern
            local i1,i2 = strfind(s,res,idx)
            if i1 then
                local tok = strsub(s,i1,i2)
                idx = i2 + 1
                return '', tok
            else
                idx = sz + 1
                return '', ''
            end

        elseif tp == 'table' then -- insert a token list
            tlist_i = 1
            tlist = res
            return '', ''

        elseif tp ~= 'nil' then -- return position
            return line_nr, idx

        else -- look for next token
            if first_hit then
                if not file then line_nr = 1 end
                first_hit = false
            end

            if idx > sz then
                if file then
                    if not next_line then
                      return -- past the end of file, done
                    end
                    s = next_line
                    line_nr = line_nr + 1
                    next_line = file:read()
                    if next_line then
                        s = s .. '\n'
                    end
                    idx, sz = 1, #s
                else
                    return -- past the end of input, done
                end
            end

            for _,m in ipairs(matches) do
                local pat = m[1]
                local fun = m[2]
                local findres = {strfind(s,pat,idx)}
                local i1, i2 = findres[1], findres[2]
                if i1 then
                    local tok = strsub(s,i1,i2)
                    idx = i2 + 1
                    local ret1, ret2
                    if not (filter and filter[fun]) then
                        lexer.finished = idx > sz
                        ret1, ret2 = fun(tok, options, findres)
                    end
                    if not file and tok:find("\n") then
                        -- Update line number.
                        local _, newlines = tok:gsub("\n", {})
                        line_nr = line_nr + newlines
                    end
                    if ret1 then
                        return ret1, ret2 -- found a match
                    else
                        return iter() -- tail-call to try again
                    end
                end
            end
        end
    end

    return iter
end

local function isstring (s)
    return type(s) == 'string'
end

--- insert tokens into a stream.
-- @param tok a token stream
-- @param a1 a string is the type, a table is a token list and
-- a function is assumed to be a token-like iterator (returns type & value)
-- @string a2 a string is the value
function lexer.insert (tok,a1,a2)
    if not a1 then return end
    local ts
    if isstring(a1) and isstring(a2) then
        ts = {{a1,a2}}
    elseif type(a1) == 'function' then
        ts = {}
        for t,v in a1() do
            append(ts,{t,v})
        end
    else
        ts = a1
    end
    tok(ts)
end

--- get everything in a stream upto a newline.
-- @param tok a token stream
-- @return a string
function lexer.getline (tok)
    local _,v = tok('.-\n')
    return v
end

--- get current line number.
-- @param tok a token stream
-- @return the line number.
-- if the input source is a file-like object,
-- also return the column.
function lexer.lineno (tok)
    return tok(0)
end

--- get the rest of the stream.
-- @param tok a token stream
-- @return a string
function lexer.getrest (tok)
    local _,v = tok('.+')
    return v
end

--- get the Lua keywords as a set-like table.
-- So `res["and"]` etc would be `true`.
-- @return a table
function lexer.get_keywords ()
    if not lua_keyword then
        lua_keyword = {
            ["and"] = true, ["break"] = true,  ["do"] = true,
            ["else"] = true, ["elseif"] = true, ["end"] = true,
            ["false"] = true, ["for"] = true, ["function"] = true,
            ["if"] = true, ["in"] = true,  ["local"] = true, ["nil"] = true,
            ["not"] = true, ["or"] = true, ["repeat"] = true,
            ["return"] = true, ["then"] = true, ["true"] = true,
            ["until"] = true,  ["while"] = true
        }
    end
    return lua_keyword
end

--- create a Lua token iterator from a string or file-like object.
-- Will return the token type and value.
-- @string s the string
-- @tab[opt] filter a table of token types to exclude, by default `{space=true,comments=true}`
-- @tab[opt] options a table of options; by default, `{number=true,string=true}`,
-- which means convert numbers and strip string quotes.
function lexer.lua(s,filter,options)
    filter = filter or {space=true,comments=true}
    lexer.get_keywords()
    if not lua_matches then
        lua_matches = {
            {WSPACE,wsdump},
            {NUMBER3,ndump},
            {IDEN,lua_vdump},
            {NUMBER4,ndump},
            {NUMBER4a,ndump},
            {NUMBER5,ndump},
            {NUMBER5a,ndump},
            {STRING1,sdump},
            {STRING2,sdump},
            {STRING3,sdump},
            {'^%-%-%[(=*)%[.-%]%1%]',cdump},
            {'^%-%-.-\n',cdump},
            {'^%[(=*)%[.-%]%1%]',sdump_l},
            {'^==',tdump},
            {'^~=',tdump},
            {'^<=',tdump},
            {'^>=',tdump},
            {'^%.%.%.',tdump},
            {'^%.%.',tdump},
            {'^.',tdump}
        }
    end
    return lexer.scan(s,lua_matches,filter,options)
end

--- create a C/C++ token iterator from a string or file-like object.
-- Will return the token type type and value.
-- @string s the string
-- @tab[opt] filter a table of token types to exclude, by default `{space=true,comments=true}`
-- @tab[opt] options a table of options; by default, `{number=true,string=true}`,
-- which means convert numbers and strip string quotes.
function lexer.cpp(s,filter,options)
    filter = filter or {space=true,comments=true}
    if not cpp_keyword then
        cpp_keyword = {
            ["class"] = true, ["break"] = true,  ["do"] = true, ["sizeof"] = true,
            ["else"] = true, ["continue"] = true, ["struct"] = true,
            ["false"] = true, ["for"] = true, ["public"] = true, ["void"] = true,
            ["private"] = true, ["protected"] = true, ["goto"] = true,
            ["if"] = true, ["static"] = true,  ["const"] = true, ["typedef"] = true,
            ["enum"] = true, ["char"] = true, ["int"] = true, ["bool"] = true,
            ["long"] = true, ["float"] = true, ["true"] = true, ["delete"] = true,
            ["double"] = true,  ["while"] = true, ["new"] = true,
            ["namespace"] = true, ["try"] = true, ["catch"] = true,
            ["switch"] = true, ["case"] = true, ["extern"] = true,
            ["return"] = true,["default"] = true,['unsigned']  = true,['signed'] = true,
            ["union"] =  true, ["volatile"] = true, ["register"] = true,["short"] = true,
        }
    end
    if not cpp_matches then
        cpp_matches = {
            {WSPACE,wsdump},
            {PREPRO,pdump},
            {NUMBER3,ndump},
            {IDEN,cpp_vdump},
            {NUMBER4,ndump},
            {NUMBER4a,ndump},
            {NUMBER5,ndump},
            {NUMBER5a,ndump},
            {CHAR1,chdump},
            {CHAR2,chdump},
            {CHAR3,chdump},
            {STRING1,sdump},
            {STRING2,sdump},
            {STRING3,sdump},
            {'^//.-\n',cdump},
            {'^/%*.-%*/',cdump},
            {'^==',tdump},
            {'^!=',tdump},
            {'^<=',tdump},
            {'^>=',tdump},
            {'^->',tdump},
            {'^&&',tdump},
            {'^||',tdump},
            {'^%+%+',tdump},
            {'^%-%-',tdump},
            {'^%+=',tdump},
            {'^%-=',tdump},
            {'^%*=',tdump},
            {'^/=',tdump},
            {'^|=',tdump},
            {'^%^=',tdump},
            {'^::',tdump},
            {'^.',tdump}
        }
    end
    return lexer.scan(s,cpp_matches,filter,options)
end

--- get a list of parameters separated by a delimiter from a stream.
-- @param tok the token stream
-- @string[opt=')'] endtoken end of list. Can be '\n'
-- @string[opt=','] delim separator
-- @return a list of token lists.
function lexer.get_separated_list(tok,endtoken,delim)
    endtoken = endtoken or ')'
    delim = delim or ','
    local parm_values = {}
    local level = 1 -- used to count ( and )
    local tl = {}
    local function tappend (tl,t,val)
        val = val or t
        append(tl,{t,val})
    end
    local is_end
    if endtoken == '\n' then
        is_end = function(t,val)
            return t == 'space' and val:find '\n'
        end
    else
        is_end = function (t)
            return t == endtoken
        end
    end
    local token,value
    while true do
        token,value=tok()
        if not token then return nil,'EOS' end -- end of stream is an error!
        if is_end(token,value) and level == 1 then
            append(parm_values,tl)
            break
        elseif token == '(' then
            level = level + 1
            tappend(tl,'(')
        elseif token == ')' then
            level = level - 1
            if level == 0 then -- finished with parm list
                append(parm_values,tl)
                break
            else
                tappend(tl,')')
            end
        elseif token == delim and level == 1 then
            append(parm_values,tl) -- a new parm
            tl = {}
        else
            tappend(tl,token,value)
        end
    end
    return parm_values,{token,value}
end

--- get the next non-space token from the stream.
-- @param tok the token stream.
function lexer.skipws (tok)
    local t,v = tok()
    while t == 'space' do
        t,v = tok()
    end
    return t,v
end

local skipws = lexer.skipws

--- get the next token, which must be of the expected type.
-- Throws an error if this type does not match!
-- @param tok the token stream
-- @string expected_type the token type
-- @bool no_skip_ws whether we should skip whitespace
function lexer.expecting (tok,expected_type,no_skip_ws)
    assert_arg(1,tok,'function')
    assert_arg(2,expected_type,'string')
    local t,v
    if no_skip_ws then
        t,v = tok()
    else
        t,v = skipws(tok)
    end
    if t ~= expected_type then error ("expecting "..expected_type,2) end
    return v
end

return lexer

end,

["pl.List"] = function()
--------------------
-- Module: 'pl.List'
--------------------
--- Python-style list class.
--
-- **Please Note**: methods that change the list will return the list.
-- This is to allow for method chaining, but please note that `ls = ls:sort()`
-- does not mean that a new copy of the list is made. In-place (mutable) methods
-- are marked as returning 'the list' in this documentation.
--
-- See the Guide for further @{02-arrays.md.Python_style_Lists|discussion}
--
-- See <a href="http://www.python.org/doc/current/tut/tut.html">http://www.python.org/doc/current/tut/tut.html</a>, section 5.1
--
-- **Note**: The comments before some of the functions are from the Python docs
-- and contain Python code.
--
-- Written for Lua version Nick Trout 4.0; Redone for Lua 5.1, Steve Donovan.
--
-- Dependencies: `pl.utils`, `pl.tablex`, `pl.class`
-- @classmod pl.List
-- @pragma nostrip

local tinsert,tremove,concat,tsort = table.insert,table.remove,table.concat,table.sort
local setmetatable, getmetatable,type,tostring,string = setmetatable,getmetatable,type,tostring,string
local tablex = require 'pl.tablex'
local filter,imap,imap2,reduce,transform,tremovevalues = tablex.filter,tablex.imap,tablex.imap2,tablex.reduce,tablex.transform,tablex.removevalues
local tsub = tablex.sub
local utils = require 'pl.utils'
local class = require 'pl.class'

local array_tostring,split,assert_arg,function_arg = utils.array_tostring,utils.split,utils.assert_arg,utils.function_arg
local normalize_slice = tablex._normalize_slice

-- metatable for our list and map objects has already been defined..
local Multimap = utils.stdmt.MultiMap
local List = utils.stdmt.List

local iter

class(nil,nil,List)

-- we want the result to be _covariant_, i.e. t must have type of obj if possible
local function makelist (t,obj)
    local klass = List
    if obj then
        klass = getmetatable(obj)
    end
    return setmetatable(t,klass)
end

local function simple_table(t)
    return type(t) == 'table' and not getmetatable(t) and #t > 0
end

function List._create (src)
    if simple_table(src) then return src end
end

function List:_init (src)
    if self == src then return end -- existing table used as self!
    if src then
        for v in iter(src) do
            tinsert(self,v)
        end
    end
end

--- Create a new list. Can optionally pass a table;
-- passing another instance of List will cause a copy to be created;
-- this will return a plain table with an appropriate metatable.
-- we pass anything which isn't a simple table to iterate() to work out
-- an appropriate iterator
-- @see List.iterate
-- @param[opt] t An optional list-like table
-- @return a new List
-- @usage ls = List();  ls = List {1,2,3,4}
-- @function List.new

List.new = List

--- Make a copy of an existing list.
-- The difference from a plain 'copy constructor' is that this returns
-- the actual List subtype.
function List:clone()
    local ls = makelist({},self)
    ls:extend(self)
    return ls
end

--- Add an item to the end of the list.
-- @param i An item
-- @return the list
function List:append(i)
    tinsert(self,i)
    return self
end

List.push = tinsert

--- Extend the list by appending all the items in the given list.
-- equivalent to 'a[len(a):] = L'.
-- @tparam List L Another List
-- @return the list
function List:extend(L)
    assert_arg(1,L,'table')
    for i = 1,#L do tinsert(self,L[i]) end
    return self
end

--- Insert an item at a given position. i is the index of the
-- element before which to insert.
-- @int i index of element before whichh to insert
-- @param x A data item
-- @return the list
function List:insert(i, x)
    assert_arg(1,i,'number')
    tinsert(self,i,x)
    return self
end

--- Insert an item at the beginning of the list.
-- @param x a data item
-- @return the list
function List:put (x)
    return self:insert(1,x)
end

--- Remove an element given its index.
-- (equivalent of Python's del s[i])
-- @int i the index
-- @return the list
function List:remove (i)
    assert_arg(1,i,'number')
    tremove(self,i)
    return self
end

--- Remove the first item from the list whose value is given.
-- (This is called 'remove' in Python; renamed to avoid confusion
-- with table.remove)
-- Return nil if there is no such item.
-- @param x A data value
-- @return the list
function List:remove_value(x)
    for i=1,#self do
        if self[i]==x then tremove(self,i) return self end
    end
    return self
 end

--- Remove the item at the given position in the list, and return it.
-- If no index is specified, a:pop() returns the last item in the list.
-- The item is also removed from the list.
-- @int[opt] i An index
-- @return the item
function List:pop(i)
    if not i then i = #self end
    assert_arg(1,i,'number')
    return tremove(self,i)
end

List.get = List.pop

--- Return the index in the list of the first item whose value is given.
-- Return nil if there is no such item.
-- @function List:index
-- @param x A data value
-- @int[opt=1] idx where to start search
-- @return the index, or nil if not found.

local tfind = tablex.find
List.index = tfind

--- Does this list contain the value?
-- @param x A data value
-- @return true or false
function List:contains(x)
    return tfind(self,x) and true or false
end

--- Return the number of times value appears in the list.
-- @param x A data value
-- @return number of times x appears
function List:count(x)
    local cnt=0
    for i=1,#self do
        if self[i]==x then cnt=cnt+1 end
    end
    return cnt
end

--- Sort the items of the list, in place.
-- @func[opt='<'] cmp an optional comparison function
-- @return the list
function List:sort(cmp)
    if cmp then cmp = function_arg(1,cmp) end
    tsort(self,cmp)
    return self
end

--- Return a sorted copy of this list.
-- @func[opt='<'] cmp an optional comparison function
-- @return a new list
function List:sorted(cmp)
    return List(self):sort(cmp)
end

--- Reverse the elements of the list, in place.
-- @return the list
function List:reverse()
    local t = self
    local n = #t
    for i = 1,n/2 do
        t[i],t[n] = t[n],t[i]
        n = n - 1
    end
    return self
end

--- Return the minimum and the maximum value of the list.
-- @return minimum value
-- @return maximum value
function List:minmax()
    local vmin,vmax = 1e70,-1e70
    for i = 1,#self do
        local v = self[i]
        if v < vmin then vmin = v end
        if v > vmax then vmax = v end
    end
    return vmin,vmax
end

--- Emulate list slicing.  like  'list[first:last]' in Python.
-- If first or last are negative then they are relative to the end of the list
-- eg. slice(-2) gives last 2 entries in a list, and
-- slice(-4,-2) gives from -4th to -2nd
-- @param first An index
-- @param last An index
-- @return a new List
function List:slice(first,last)
    return tsub(self,first,last)
end

--- Empty the list.
-- @return the list
function List:clear()
    for i=1,#self do tremove(self) end
    return self
end

local eps = 1.0e-10

--- Emulate Python's range(x) function.
-- Include it in List table for tidiness
-- @int start A number
-- @int[opt] finish A number greater than start; if absent,
-- then start is 1 and finish is start
-- @int[opt=1] incr an increment (may be less than 1)
-- @return a List from start .. finish
-- @usage List.range(0,3) == List{0,1,2,3}
-- @usage List.range(4) = List{1,2,3,4}
-- @usage List.range(5,1,-1) == List{5,4,3,2,1}
function List.range(start,finish,incr)
    if not finish then
        finish = start
        start = 1
    end
    if incr then
    assert_arg(3,incr,'number')
    if math.ceil(incr) ~= incr then finish = finish + eps end
    else
        incr = 1
    end
    assert_arg(1,start,'number')
    assert_arg(2,finish,'number')
    local t = List()
    for i=start,finish,incr do tinsert(t,i) end
    return t
end

--- list:len() is the same as #list.
function List:len()
    return #self
end

-- Extended operations --

--- Remove a subrange of elements.
-- equivalent to 'del s[i1:i2]' in Python.
-- @int i1 start of range
-- @int i2 end of range
-- @return the list
function List:chop(i1,i2)
    return tremovevalues(self,i1,i2)
end

--- Insert a sublist into a list
-- equivalent to 's[idx:idx] = list' in Python
-- @int idx index
-- @tparam List list list to insert
-- @return the list
-- @usage  l = List{10,20}; l:splice(2,{21,22});  assert(l == List{10,21,22,20})
function List:splice(idx,list)
    assert_arg(1,idx,'number')
    idx = idx - 1
    local i = 1
    for v in iter(list) do
        tinsert(self,i+idx,v)
        i = i + 1
    end
    return self
end

--- General slice assignment s[i1:i2] = seq.
-- @int i1  start index
-- @int i2  end index
-- @tparam List seq a list
-- @return the list
function List:slice_assign(i1,i2,seq)
    assert_arg(1,i1,'number')
    assert_arg(1,i2,'number')
    i1,i2 = normalize_slice(self,i1,i2)
    if i2 >= i1 then self:chop(i1,i2) end
    self:splice(i1,seq)
    return self
end

--- Concatenation operator.
-- @within metamethods
-- @tparam List L another List
-- @return a new list consisting of the list with the elements of the new list appended
function List:__concat(L)
    assert_arg(1,L,'table')
    local ls = self:clone()
    ls:extend(L)
    return ls
end

--- Equality operator ==.  True iff all elements of two lists are equal.
-- @within metamethods
-- @tparam List L another List
-- @return true or false
function List:__eq(L)
    if #self ~= #L then return false end
    for i = 1,#self do
        if self[i] ~= L[i] then return false end
    end
    return true
end

--- Join the elements of a list using a delimiter.
-- This method uses tostring on all elements.
-- @string[opt=''] delim a delimiter string, can be empty.
-- @return a string
function List:join (delim)
    delim = delim or ''
    assert_arg(1,delim,'string')
    return concat(array_tostring(self),delim)
end

--- Join a list of strings. <br>
-- Uses `table.concat` directly.
-- @function List:concat
-- @string[opt=''] delim a delimiter
-- @return a string
List.concat = concat

local function tostring_q(val)
    local s = tostring(val)
    if type(val) == 'string' then
        s = '"'..s..'"'
    end
    return s
end

--- How our list should be rendered as a string. Uses join().
-- @within metamethods
-- @see List:join
function List:__tostring()
    return '{'..self:join(',',tostring_q)..'}'
end

--- Call the function on each element of the list.
-- @func fun a function or callable object
-- @param ... optional values to pass to function
function List:foreach (fun,...)
    fun = function_arg(1,fun)
    for i = 1,#self do
        fun(self[i],...)
    end
end

local function lookup_fun (obj,name)
    local f = obj[name]
    if not f then error(type(obj).." does not have method "..name,3) end
    return f
end

--- Call the named method on each element of the list.
-- @string name the method name
-- @param ... optional values to pass to function
function List:foreachm (name,...)
    for i = 1,#self do
        local obj = self[i]
        local f = lookup_fun(obj,name)
        f(obj,...)
    end
end

--- Create a list of all elements which match a function.
-- @func fun a boolean function
-- @param[opt] arg optional argument to be passed as second argument of the predicate
-- @return a new filtered list.
function List:filter (fun,arg)
    return makelist(filter(self,fun,arg),self)
end

--- Split a string using a delimiter.
-- @string s the string
-- @string[opt] delim the delimiter (default spaces)
-- @return a List of strings
-- @see pl.utils.split
function List.split (s,delim)
    assert_arg(1,s,'string')
    return makelist(split(s,delim))
end

--- Apply a function to all elements.
-- Any extra arguments will be passed to the function.
-- @func fun a function of at least one argument
-- @param ... arbitrary extra arguments.
-- @return a new list: {f(x) for x in self}
-- @usage List{'one','two'}:map(string.upper) == {'ONE','TWO'}
-- @see pl.tablex.imap
function List:map (fun,...)
    return makelist(imap(fun,self,...),self)
end

--- Apply a function to all elements, in-place.
-- Any extra arguments are passed to the function.
-- @func fun A function that takes at least one argument
-- @param ... arbitrary extra arguments.
-- @return the list.
function List:transform (fun,...)
    transform(fun,self,...)
    return self
end

--- Apply a function to elements of two lists.
-- Any extra arguments will be passed to the function
-- @func fun a function of at least two arguments
-- @tparam List ls another list
-- @param ... arbitrary extra arguments.
-- @return a new list: {f(x,y) for x in self, for x in arg1}
-- @see pl.tablex.imap2
function List:map2 (fun,ls,...)
    return makelist(imap2(fun,self,ls,...),self)
end

--- apply a named method to all elements.
-- Any extra arguments will be passed to the method.
-- @string name name of method
-- @param ... extra arguments
-- @return a new list of the results
-- @see pl.seq.mapmethod
function List:mapm (name,...)
    local res = {}
    for i = 1,#self do
      local val = self[i]
      local fn = lookup_fun(val,name)
      res[i] = fn(val,...)
    end
    return makelist(res,self)
end

local function composite_call (method,f)
    return function(self,...)
        return self[method](self,f,...)
    end
end

function List.default_map_with(T)
    return function(self,name)
        local m
        if T then
            local f = lookup_fun(T,name)
            m = composite_call('map',f)
        else
            m = composite_call('mapn',name)
        end
        getmetatable(self)[name] = m -- and cache..
        return m
    end
end

List.default_map = List.default_map_with

--- 'reduce' a list using a binary function.
-- @func fun a function of two arguments
-- @return result of the function
-- @see pl.tablex.reduce
function List:reduce (fun)
    return reduce(fun,self)
end

--- Partition a list using a classifier function.
-- The function may return nil, but this will be converted to the string key '<nil>'.
-- @func fun a function of at least one argument
-- @param ... will also be passed to the function
-- @treturn MultiMap a table where the keys are the returned values, and the values are Lists
-- of values where the function returned that key.
-- @see pl.MultiMap
function List:partition (fun,...)
    fun = function_arg(1,fun)
    local res = {}
    for i = 1,#self do
        local val = self[i]
        local klass = fun(val,...)
        if klass == nil then klass = '<nil>' end
        if not res[klass] then res[klass] = List() end
        res[klass]:append(val)
    end
    return setmetatable(res,Multimap)
end

--- return an iterator over all values.
function List:iter ()
    return iter(self)
end

--- Create an iterator over a sequence.
-- This captures the Python concept of 'sequence'.
-- For tables, iterates over all values with integer indices.
-- @param seq a sequence; a string (over characters), a table, a file object (over lines) or an iterator function
-- @usage for x in iterate {1,10,22,55} do io.write(x,',') end ==> 1,10,22,55
-- @usage for ch in iterate 'help' do do io.write(ch,' ') end ==> h e l p
function List.iterate(seq)
    if type(seq) == 'string' then
        local idx = 0
        local n = #seq
        local sub = string.sub
        return function ()
            idx = idx + 1
            if idx > n then return nil
            else
                return sub(seq,idx,idx)
            end
        end
    elseif type(seq) == 'table' then
        local idx = 0
        local n = #seq
        return function()
            idx = idx + 1
            if idx > n then return nil
            else
                return seq[idx]
            end
        end
    elseif type(seq) == 'function' then
        return seq
    elseif type(seq) == 'userdata' and io.type(seq) == 'file' then
        return seq:lines()
    end
end
iter = List.iterate

return List


end,

["pl.luabalanced"] = function()
--------------------
-- Module: 'pl.luabalanced'
--------------------
--- Extract delimited Lua sequences from strings.
-- Inspired by Damian Conway's Text::Balanced in Perl. <br/>
-- <ul>
--   <li>[1] <a href="http://lua-users.org/wiki/LuaBalanced">Lua Wiki Page</a></li>
--   <li>[2] http://search.cpan.org/dist/Text-Balanced/lib/Text/Balanced.pm</li>
-- </ul> <br/>
-- <pre class=example>
-- local lb = require "pl.luabalanced"
-- --Extract Lua expression starting at position 4.
--  print(lb.match_expression("if x^2 + x > 5 then print(x) end", 4))
--  --> x^2 + x > 5     16
-- --Extract Lua string starting at (default) position 1.
-- print(lb.match_string([["test\"123" .. "more"]]))
-- --> "test\"123"     12
-- </pre>
-- (c) 2008, David Manura, Licensed under the same terms as Lua (MIT license).
-- @class module
-- @name pl.luabalanced

local M = {}

local assert = assert

-- map opening brace <-> closing brace.
local ends = { ['('] = ')', ['{'] = '}', ['['] = ']' }
local begins = {}; for k,v in pairs(ends) do begins[v] = k end


-- Match Lua string in string <s> starting at position <pos>.
-- Returns <string>, <posnew>, where <string> is the matched
-- string (or nil on no match) and <posnew> is the character
-- following the match (or <pos> on no match).
-- Supports all Lua string syntax: "...", '...', [[...]], [=[...]=], etc.
local function match_string(s, pos)
  pos = pos or 1
  local posa = pos
  local c = s:sub(pos,pos)
  if c == '"' or c == "'" then
    pos = pos + 1
    while 1 do
      pos = assert(s:find("[" .. c .. "\\]", pos), 'syntax error')
      if s:sub(pos,pos) == c then
        local part = s:sub(posa, pos)
        return part, pos + 1
      else
        pos = pos + 2
      end
    end
  else
    local sc = s:match("^%[(=*)%[", pos)
    if sc then
      local _; _, pos = s:find("%]" .. sc .. "%]", pos)
      assert(pos)
      local part = s:sub(posa, pos)
      return part, pos + 1
    else
      return nil, pos
    end
  end
end
M.match_string = match_string


-- Match bracketed Lua expression, e.g. "(...)", "{...}", "[...]", "[[...]]",
-- [=[...]=], etc.
-- Function interface is similar to match_string.
local function match_bracketed(s, pos)
  pos = pos or 1
  local posa = pos
  local ca = s:sub(pos,pos)
  if not ends[ca] then
    return nil, pos
  end
  local stack = {}
  while 1 do
    pos = s:find('[%(%{%[%)%}%]\"\']', pos)
    assert(pos, 'syntax error: unbalanced')
    local c = s:sub(pos,pos)
    if c == '"' or c == "'" then
      local part; part, pos = match_string(s, pos)
      assert(part)
    elseif ends[c] then -- open
      local mid, posb
      if c == '[' then mid, posb = s:match('^%[(=*)%[()', pos) end
      if mid then
        pos = s:match('%]' .. mid .. '%]()', posb)
        assert(pos, 'syntax error: long string not terminated')
        if #stack == 0 then
          local part = s:sub(posa, pos-1)
          return part, pos
        end
      else
        stack[#stack+1] = c
        pos = pos + 1
      end
    else -- close
      assert(stack[#stack] == assert(begins[c]), 'syntax error: unbalanced')
      stack[#stack] = nil
      if #stack == 0 then
        local part = s:sub(posa, pos)
        return part, pos+1
      end
      pos = pos + 1
    end
  end
end
M.match_bracketed = match_bracketed


-- Match Lua comment, e.g. "--...\n", "--[[...]]", "--[=[...]=]", etc.
-- Function interface is similar to match_string.
local function match_comment(s, pos)
  pos = pos or 1
  if s:sub(pos, pos+1) ~= '--' then
    return nil, pos
  end
  pos = pos + 2
  local partt, post = match_string(s, pos)
  if partt then
    return '--' .. partt, post
  end
  local part; part, pos = s:match('^([^\n]*\n?)()', pos)
  return '--' .. part, pos
end


-- Match Lua expression, e.g. "a + b * c[e]".
-- Function interface is similar to match_string.
local wordop = {['and']=true, ['or']=true, ['not']=true}
local is_compare = {['>']=true, ['<']=true, ['~']=true}
local function match_expression(s, pos)
  pos = pos or 1
  local _
  local posa = pos
  local lastident
  local poscs, posce
  while pos do
    local c = s:sub(pos,pos)
    if c == '"' or c == "'" or c == '[' and s:find('^[=%[]', pos+1) then
      local part; part, pos = match_string(s, pos)
      assert(part, 'syntax error')
    elseif c == '-' and s:sub(pos+1,pos+1) == '-' then
      -- note: handle adjacent comments in loop to properly support
      -- backtracing (poscs/posce).
      poscs = pos
      while s:sub(pos,pos+1) == '--' do
        local part; part, pos = match_comment(s, pos)
        assert(part)
        pos = s:match('^%s*()', pos)
        posce = pos
      end
    elseif c == '(' or c == '{' or c == '[' then
      _, pos = match_bracketed(s, pos)
    elseif c == '=' and s:sub(pos+1,pos+1) == '=' then
      pos = pos + 2  -- skip over two-char op containing '='
    elseif c == '=' and is_compare[s:sub(pos-1,pos-1)] then
      pos = pos + 1  -- skip over two-char op containing '='
    elseif c:match'^[%)%}%];,=]' then
      local part = s:sub(posa, pos-1)
      return part, pos
    elseif c:match'^[%w_]' then
      local newident,newpos = s:match('^([%w_]+)()', pos)
      if pos ~= posa and not wordop[newident] then -- non-first ident
        local pose = ((posce == pos) and poscs or pos) - 1
        while s:match('^%s', pose) do pose = pose - 1 end
        local ce = s:sub(pose,pose)
        if ce:match'[%)%}\'\"%]]' or
           ce:match'[%w_]' and not wordop[lastident]
        then
          local part = s:sub(posa, pos-1)
          return part, pos
        end
      end
      lastident, pos = newident, newpos
    else
      pos = pos + 1
    end
    pos = s:find('[%(%{%[%)%}%]\"\';,=%w_%-]', pos)
  end
  local part = s:sub(posa, #s)
  return part, #s+1
end
M.match_expression = match_expression


-- Match name list (zero or more names).  E.g. "a,b,c"
-- Function interface is similar to match_string,
-- but returns array as match.
local function match_namelist(s, pos)
  pos = pos or 1
  local list = {}
  while 1 do
    local c = #list == 0 and '^' or '^%s*,%s*'
    local item, post = s:match(c .. '([%a_][%w_]*)%s*()', pos)
    if item then pos = post else break end
    list[#list+1] = item
  end
  return list, pos
end
M.match_namelist = match_namelist


-- Match expression list (zero or more expressions).  E.g. "a+b,b*c".
-- Function interface is similar to match_string,
-- but returns array as match.
local function match_explist(s, pos)
  pos = pos or 1
  local list = {}
  while 1 do
    if #list ~= 0 then
      local post = s:match('^%s*,%s*()', pos)
      if post then pos = post else break end
    end
    local item; item, pos = match_expression(s, pos)
    assert(item, 'syntax error')
    list[#list+1] = item
  end
  return list, pos
end
M.match_explist = match_explist


-- Replace snippets of code in Lua code string <s>
-- using replacement function f(u,sin) --> sout.
-- <u> is the type of snippet ('c' = comment, 's' = string,
-- 'e' = any other code).
-- Snippet is replaced with <sout> (unless <sout> is nil or false, in
-- which case the original snippet is kept)
-- This is somewhat analogous to string.gsub .
local function gsub(s, f)
  local pos = 1
  local posa = 1
  local sret = ''
  while 1 do
    pos = s:find('[%-\'\"%[]', pos)
    if not pos then break end
    if s:match('^%-%-', pos) then
      local exp = s:sub(posa, pos-1)
      if #exp > 0 then sret = sret .. (f('e', exp) or exp) end
      local comment; comment, pos = match_comment(s, pos)
      sret = sret .. (f('c', assert(comment)) or comment)
      posa = pos
    else
      local posb = s:find('^[\'\"%[]', pos)
      local str
      if posb then str, pos = match_string(s, posb) end
      if str then
        local exp = s:sub(posa, posb-1)
        if #exp > 0 then sret = sret .. (f('e', exp) or exp) end
        sret = sret .. (f('s', str) or str)
        posa = pos
      else
        pos = pos + 1
      end
    end
  end
  local exp = s:sub(posa)
  if #exp > 0 then sret = sret .. (f('e', exp) or exp) end
  return sret
end
M.gsub = gsub


return M

end,

["pl.Map"] = function()
--------------------
-- Module: 'pl.Map'
--------------------
--- A Map class.
--
--    > Map = require 'pl.Map'
--    > m = Map{one=1,two=2}
--    > m:update {three=3,four=4,two=20}
--    > = m == M{one=1,two=20,three=3,four=4}
--    true
--
-- Dependencies: `pl.utils`, `pl.class`, `pl.tablex`, `pl.pretty`
-- @classmod pl.Map

local tablex = require 'pl.tablex'
local utils = require 'pl.utils'
local stdmt = utils.stdmt
local deepcompare = tablex.deepcompare

local pretty_write = require 'pl.pretty' . write
local Map = stdmt.Map
local Set = stdmt.Set

local class = require 'pl.class'

-- the Map class ---------------------
class(nil,nil,Map)

function Map:_init (t)
    local mt = getmetatable(t)
    if mt == Set or mt == Map then
        self:update(t)
    else
        return t -- otherwise assumed to be a map-like table
    end
end


local function makelist(t)
    return setmetatable(t, require('pl.List'))
end

--- return a List of all keys.
-- @class function
-- @name Map:keys
Map.keys = tablex.keys

--- return a List of all values.
-- @class function
-- @name Map:values
Map.values = tablex.values

--- return an iterator over all key-value pairs.
function Map:iter ()
    return pairs(self)
end

--- return a List of all key-value pairs, sorted by the keys in ascending order.
function Map:items()
    local ls = makelist(tablex.pairmap (function (k,v) return makelist {k,v} end, self))
    ls:sort(function(t1,t2) return t1[1] < t2[1] end)
    return ls
end

--- set a value in the map if it doesn't exist yet.
-- @param key the key
-- @param default value to set
-- @return the value stored in the map (existing value, or the new value)
function Map:setdefault(key, default)
    local val = self[key]
    if val ~= nil then
        return val
    end
    self:set(key,default)
   return default
end

--- size of map.
-- note: this is a relatively expensive operation!
-- @class function
-- @name Map:len
Map.len = tablex.size

--- put a value into the map.
-- This will remove the key if the value is `nil`
-- @param key the key
-- @param val the value
function Map:set (key,val)
    self[key] = val
end

--- get a value from the map.
-- @param key the key
-- @return the value, or nil if not found.
function Map:get (key)
    return rawget(self,key)
end

local index_by = tablex.index_by

--- get a list of values indexed by a list of keys.
-- @param keys a list-like table of keys
-- @return a new list
function Map:getvalues (keys)
    return makelist(index_by(self,keys))
end

--- update the map using key/value pairs from another table.
-- @tab table
-- @function Map:update
Map.update = tablex.update

--- equality between maps.
-- @within metamethods
-- @tparam Map m another map.
function Map:__eq (m)
    -- note we explicitly ask deepcompare _not_ to use __eq!
    return deepcompare(self,m,true)
end

--- string representation of a map.
-- @within metamethods
function Map:__tostring ()
    return pretty_write(self,'')
end

return Map

end,

["pl.MultiMap"] = function()
--------------------
-- Module: 'pl.MultiMap'
--------------------
--- MultiMap, a Map which has multiple values per key.
--
-- Dependencies: `pl.utils`, `pl.class`, `pl.List`, `pl.Map`
-- @classmod pl.MultiMap

local utils = require 'pl.utils'
local class = require 'pl.class'
local List = require 'pl.List'
local Map = require 'pl.Map'

-- MultiMap is a standard MT
local MultiMap = utils.stdmt.MultiMap

class(Map,nil,MultiMap)
MultiMap._name = 'MultiMap'

function MultiMap:_init (t)
    if not t then return end
    self:update(t)
end

--- update a MultiMap using a table.
-- @param t either a Multimap or a map-like table.
-- @return the map
function MultiMap:update (t)
    utils.assert_arg(1,t,'table')
    if Map:class_of(t) then
        for k,v in pairs(t) do
            self[k] = List()
            self[k]:append(v)
        end
    else
        for k,v in pairs(t) do
            self[k] = List(v)
        end
    end
end

--- add a new value to a key.  Setting a nil value removes the key.
-- @param key the key
-- @param val the value
-- @return the map
function MultiMap:set (key,val)
    if val == nil then
        self[key] = nil
    else
        if not self[key] then
            self[key] = List()
        end
        self[key]:append(val)
    end
end

return MultiMap

end,

["pl.operator"] = function()
--------------------
-- Module: 'pl.operator'
--------------------
--- Lua operators available as functions.
--
-- (similar to the Python module of the same name)
--
-- There is a module field `optable` which maps the operator strings
-- onto these functions, e.g. `operator.optable['()']==operator.call`
--
-- Operator strings like '>' and '{}' can be passed to most Penlight functions
-- expecting a function argument.
--
-- @module pl.operator

local strfind = string.find

local operator = {}

--- apply function to some arguments **()**
-- @param fn a function or callable object
-- @param ... arguments
function operator.call(fn,...)
    return fn(...)
end

--- get the indexed value from a table **[]**
-- @param t a table or any indexable object
-- @param k the key
function  operator.index(t,k)
    return t[k]
end

--- returns true if arguments are equal **==**
-- @param a value
-- @param b value
function  operator.eq(a,b)
    return a==b
end

--- returns true if arguments are not equal **~=**
 -- @param a value
-- @param b value
function  operator.neq(a,b)
    return a~=b
end

--- returns true if a is less than b **<**
-- @param a value
-- @param b value
function  operator.lt(a,b)
    return a < b
end

--- returns true if a is less or equal to b **<=**
-- @param a value
-- @param b value
function  operator.le(a,b)
    return a <= b
end

--- returns true if a is greater than b **>**
-- @param a value
-- @param b value
function  operator.gt(a,b)
    return a > b
end

--- returns true if a is greater or equal to b **>=**
-- @param a value
-- @param b value
function  operator.ge(a,b)
    return a >= b
end

--- returns length of string or table **#**
-- @param a a string or a table
function  operator.len(a)
    return #a
end

--- add two values **+**
-- @param a value
-- @param b value
function  operator.add(a,b)
    return a+b
end

--- subtract b from a **-**
-- @param a value
-- @param b value
function  operator.sub(a,b)
    return a-b
end

--- multiply two values __*__
-- @param a value
-- @param b value
function  operator.mul(a,b)
    return a*b
end

--- divide first value by second **/**
-- @param a value
-- @param b value
function  operator.div(a,b)
    return a/b
end

--- raise first to the power of second **^**
-- @param a value
-- @param b value
function  operator.pow(a,b)
    return a^b
end

--- modulo; remainder of a divided by b **%**
-- @param a value
-- @param b value
function  operator.mod(a,b)
    return a%b
end

--- concatenate two values (either strings or `__concat` defined) **..**
-- @param a value
-- @param b value
function  operator.concat(a,b)
    return a..b
end

--- return the negative of a value **-**
-- @param a value
function  operator.unm(a)
    return -a
end

--- false if value evaluates as true **not**
-- @param a value
function  operator.lnot(a)
    return not a
end

--- true if both values evaluate as true **and**
-- @param a value
-- @param b value
function  operator.land(a,b)
    return a and b
end

--- true if either value evaluate as true **or**
-- @param a value
-- @param b value
function  operator.lor(a,b)
    return a or b
end

--- make a table from the arguments **{}**
-- @param ... non-nil arguments
-- @return a table
function  operator.table (...)
    return {...}
end

--- match two strings **~**.
-- uses @{string.find}
function  operator.match (a,b)
    return strfind(a,b)~=nil
end

--- the null operation.
-- @param ... arguments
-- @return the arguments
function  operator.nop (...)
    return ...
end

---- Map from operator symbol to function.
-- Most of these map directly from operators;
-- But note these extras
--
--  * __'()'__  `call`
--  * __'[]'__  `index`
--  * __'{}'__ `table`
--  * __'~'__   `match`
--
-- @table optable
-- @field operator
 operator.optable = {
    ['+']=operator.add,
    ['-']=operator.sub,
    ['*']=operator.mul,
    ['/']=operator.div,
    ['%']=operator.mod,
    ['^']=operator.pow,
    ['..']=operator.concat,
    ['()']=operator.call,
    ['[]']=operator.index,
    ['<']=operator.lt,
    ['<=']=operator.le,
    ['>']=operator.gt,
    ['>=']=operator.ge,
    ['==']=operator.eq,
    ['~=']=operator.neq,
    ['#']=operator.len,
    ['and']=operator.land,
    ['or']=operator.lor,
    ['{}']=operator.table,
    ['~']=operator.match,
    ['']=operator.nop,
}

return operator

end,

["pl.OrderedMap"] = function()
--------------------
-- Module: 'pl.OrderedMap'
--------------------
--- OrderedMap, a map which preserves ordering.
--
-- Derived from `pl.Map`.
--
-- Dependencies: `pl.utils`, `pl.tablex`, `pl.class`, `pl.List`, `pl.Map`
-- @classmod pl.OrderedMap

local tablex = require 'pl.tablex'
local utils = require 'pl.utils'
local List = require 'pl.List'
local index_by,tsort,concat = tablex.index_by,table.sort,table.concat

local class = require 'pl.class'
local Map = require 'pl.Map'

local OrderedMap = class(Map)
OrderedMap._name = 'OrderedMap'

local rawset = rawset

--- construct an OrderedMap.
-- Will throw an error if the argument is bad.
-- @param t optional initialization table, same as for @{OrderedMap:update}
function OrderedMap:_init (t)
    rawset(self,'_keys',List())
    if t then
        local map,err = self:update(t)
        if not map then error(err,2) end
    end
end

local assert_arg,raise = utils.assert_arg,utils.raise

--- update an OrderedMap using a table.
-- If the table is itself an OrderedMap, then its entries will be appended.
-- if it s a table of the form `{{key1=val1},{key2=val2},...}` these will be appended.
--
-- Otherwise, it is assumed to be a map-like table, and order of extra entries is arbitrary.
-- @tab t a table.
-- @return the map, or nil in case of error
-- @return the error message
function OrderedMap:update (t)
    assert_arg(1,t,'table')
    if OrderedMap:class_of(t) then
       for k,v in t:iter() do
           self:set(k,v)
       end
    elseif #t > 0 then -- an array must contain {key=val} tables
       if type(t[1]) == 'table' then
           for _,pair in ipairs(t) do
               local key,value = next(pair)
               if not key then return raise 'empty pair initialization table' end
               self:set(key,value)
           end
       else
           return raise 'cannot use an array to initialize an OrderedMap'
       end
    else
       for k,v in pairs(t) do
           self:set(k,v)
       end
    end
   return self
end

--- set the key's value.   This key will be appended at the end of the map.
--
-- If the value is nil, then the key is removed.
-- @param key the key
-- @param val the value
-- @return the map
function OrderedMap:set (key,val)
    if rawget(self, key) == nil and val ~= nil then -- new key
        self._keys:append(key) -- we keep in order
        rawset(self,key,val)  -- don't want to provoke __newindex!
    else -- existing key-value pair
        if val == nil then
            self._keys:remove_value(key)
            rawset(self,key,nil)
        else
            self[key] = val
        end
    end
    return self
end

OrderedMap.__newindex = OrderedMap.set

--- insert a key/value pair before a given position.
-- Note: if the map already contains the key, then this effectively
-- moves the item to the new position by first removing at the old position.
-- Has no effect if the key does not exist and val is nil
-- @int pos a position starting at 1
-- @param key the key
-- @param val the value; if nil use the old value
function OrderedMap:insert (pos,key,val)
    local oldval = self[key]
    val = val or oldval
    if oldval then
        self._keys:remove_value(key)
    end
    if val then
        self._keys:insert(pos,key)
        rawset(self,key,val)
    end
    return self
end

--- return the keys in order.
-- (Not a copy!)
-- @return List
function OrderedMap:keys ()
    return self._keys
end

--- return the values in order.
-- this is relatively expensive.
-- @return List
function OrderedMap:values ()
    return List(index_by(self,self._keys))
end

--- sort the keys.
-- @func cmp a comparison function as for @{table.sort}
-- @return the map
function OrderedMap:sort (cmp)
    tsort(self._keys,cmp)
    return self
end

--- iterate over key-value pairs in order.
function OrderedMap:iter ()
    local i = 0
    local keys = self._keys
    local idx
    return function()
        i = i + 1
        if i > #keys then return nil end
        idx = keys[i]
        return idx,self[idx]
    end
end

--- iterate over an ordered map (5.2).
-- @within metamethods
-- @function OrderedMap:__pairs
OrderedMap.__pairs = OrderedMap.iter

--- string representation of an ordered map.
-- @within metamethods
function OrderedMap:__tostring ()
    local res = {}
    for i,v in ipairs(self._keys) do
        local val = self[v]
        local vs = tostring(val)
        if type(val) ~= 'number' then
            vs = '"'..vs..'"'
        end
        res[i] = tostring(v)..'='..vs
    end
    return '{'..concat(res,',')..'}'
end

return OrderedMap




end,

["pl.path"] = function()
--------------------
-- Module: 'pl.path'
--------------------
--- Path manipulation and file queries.
--
-- This is modelled after Python's os.path library (10.1); see @{04-paths.md|the Guide}.
--
-- NOTE: the functions assume the paths being dealt with to originate
-- from the OS the application is running on. Windows drive letters are not
-- to be used when running on a Unix system for example. The one exception
-- is Windows paths to allow both forward and backward slashes (since Lua
-- also accepts those)
--
-- Dependencies: `pl.utils`, `lfs`
-- @module pl.path

-- imports and locals
local _G = _G
local sub = string.sub
local getenv = os.getenv
local tmpnam = os.tmpname
local package = package
local append, concat, remove = table.insert, table.concat, table.remove
local utils = require 'pl.utils'
local assert_string,raise = utils.assert_string,utils.raise

local res,lfs = _G.pcall(_G.require,'lfs')
if not res then
    error("pl.path requires LuaFileSystem")
end

local attrib = lfs.attributes
local currentdir = lfs.currentdir
local link_attrib = lfs.symlinkattributes

local path = {}

local function err_func(name, param, err, code)
  local ret = ("%s failed"):format(tostring(name))
  if param ~= nil then
    ret = ret .. (" for '%s'"):format(tostring(param))
  end
  ret = ret .. (": %s"):format(tostring(err))
  if code ~= nil then
    ret = ret .. (" (code %s)"):format(tostring(code))
  end
  return ret
end

--- Lua iterator over the entries of a given directory.
-- Implicit link to [`luafilesystem.dir`](https://lunarmodules.github.io/luafilesystem/manual.html#dir)
-- @function dir
path.dir = lfs.dir

--- Creates a directory.
-- Implicit link to [`luafilesystem.mkdir`](https://lunarmodules.github.io/luafilesystem/manual.html#mkdir)
-- @function mkdir
path.mkdir = function(d)
  local ok, err, code = lfs.mkdir(d)
  if not ok then
    return ok, err_func("mkdir", d, err, code), code
  end
  return ok, err, code
end

--- Removes a directory.
-- Implicit link to [`luafilesystem.rmdir`](https://lunarmodules.github.io/luafilesystem/manual.html#rmdir)
-- @function rmdir
path.rmdir = function(d)
  local ok, err, code = lfs.rmdir(d)
  if not ok then
    return ok, err_func("rmdir", d, err, code), code
  end
  return ok, err, code
end

--- Gets attributes.
-- Implicit link to [`luafilesystem.attributes`](https://lunarmodules.github.io/luafilesystem/manual.html#attributes)
-- @function attrib
path.attrib = function(d, r)
  local ok, err, code = attrib(d, r)
  if not ok then
    return ok, err_func("attrib", d, err, code), code
  end
  return ok, err, code
end

--- Get the working directory.
-- Implicit link to [`luafilesystem.currentdir`](https://lunarmodules.github.io/luafilesystem/manual.html#currentdir)
-- @function currentdir
path.currentdir = function()
  local ok, err, code = currentdir()
  if not ok then
    return ok, err_func("currentdir", nil, err, code), code
  end
  return ok, err, code
end

--- Gets symlink attributes.
-- Implicit link to [`luafilesystem.symlinkattributes`](https://lunarmodules.github.io/luafilesystem/manual.html#symlinkattributes)
-- @function link_attrib
path.link_attrib = function(d, r)
  local ok, err, code = link_attrib(d, r)
  if not ok then
    return ok, err_func("link_attrib", d, err, code), code
  end
  return ok, err, code
end

--- Changes the working directory.
-- On Windows, if a drive is specified, it also changes the current drive. If
-- only specifying the drive, it will only switch drive, but not modify the path.
-- Implicit link to [`luafilesystem.chdir`](https://lunarmodules.github.io/luafilesystem/manual.html#chdir)
-- @function chdir
path.chdir = function(d)
  local ok, err, code = lfs.chdir(d)
  if not ok then
    return ok, err_func("chdir", d, err, code), code
  end
  return ok, err, code
end

--- is this a directory?
-- @string P A file path
function path.isdir(P)
    assert_string(1,P)
    return attrib(P,'mode') == 'directory'
end

--- is this a file?
-- @string P A file path
function path.isfile(P)
    assert_string(1,P)
    return attrib(P,'mode') == 'file'
end

-- is this a symbolic link?
-- @string P A file path
function path.islink(P)
    assert_string(1,P)
    if link_attrib then
        return link_attrib(P,'mode')=='link'
    else
        return false
    end
end

--- return size of a file.
-- @string P A file path
function path.getsize(P)
    assert_string(1,P)
    return attrib(P,'size')
end

--- does a path exist?
-- @string P A file path
-- @return the file path if it exists (either as file, directory, socket, etc), false otherwise
function path.exists(P)
    assert_string(1,P)
    return attrib(P,'mode') ~= nil and P
end

--- Return the time of last access as the number of seconds since the epoch.
-- @string P A file path
function path.getatime(P)
    assert_string(1,P)
    return attrib(P,'access')
end

--- Return the time of last modification as the number of seconds since the epoch.
-- @string P A file path
function path.getmtime(P)
    assert_string(1,P)
    return attrib(P,'modification')
end

---Return the system's ctime as the number of seconds since the epoch.
-- @string P A file path
function path.getctime(P)
    assert_string(1,P)
    return path.attrib(P,'change')
end


local function at(s,i)
    return sub(s,i,i)
end

path.is_windows = utils.is_windows

local sep, other_sep, seps
-- constant sep is the directory separator for this platform.
-- constant dirsep is the separator in the PATH environment variable
if path.is_windows then
    path.sep = '\\'; other_sep = '/'
    path.dirsep = ';'
    seps = { ['/'] = true, ['\\'] = true }
else
    path.sep = '/'
    path.dirsep = ':'
    seps = { ['/'] = true }
end
sep = path.sep

--- are we running Windows?
-- @class field
-- @name path.is_windows

--- path separator for this platform.
-- @class field
-- @name path.sep

--- separator for PATH for this platform
-- @class field
-- @name path.dirsep

--- given a path, return the directory part and a file part.
-- if there's no directory part, the first value will be empty
-- @string P A file path
-- @return directory part
-- @return file part
-- @usage
-- local dir, file = path.splitpath("some/dir/myfile.txt")
-- assert(dir == "some/dir")
-- assert(file == "myfile.txt")
--
-- local dir, file = path.splitpath("some/dir/")
-- assert(dir == "some/dir")
-- assert(file == "")
--
-- local dir, file = path.splitpath("some_dir")
-- assert(dir == "")
-- assert(file == "some_dir")
function path.splitpath(P)
    assert_string(1,P)
    local i = #P
    local ch = at(P,i)
    while i > 0 and ch ~= sep and ch ~= other_sep do
        i = i - 1
        ch = at(P,i)
    end
    if i == 0 then
        return '',P
    else
        return sub(P,1,i-1), sub(P,i+1)
    end
end

--- return an absolute path.
-- @string P A file path
-- @string[opt] pwd optional start path to use (default is current dir)
function path.abspath(P,pwd)
    assert_string(1,P)
    local use_pwd = pwd ~= nil
    if use_pwd then assert_string(2,pwd) end
    if not use_pwd and not currentdir() then return P end
    P = P:gsub('[\\/]$','')
    pwd = pwd or currentdir()
    if not path.isabs(P) then
        P = path.join(pwd,P)
    elseif path.is_windows and not use_pwd and at(P,2) ~= ':' and at(P,2) ~= '\\' then
        P = pwd:sub(1,2)..P -- attach current drive to path like '\\fred.txt'
    end
    return path.normpath(P)
end

--- given a path, return the root part and the extension part.
-- if there's no extension part, the second value will be empty
-- @string P A file path
-- @treturn string root part (everything upto the "."", maybe empty)
-- @treturn string extension part (including the ".", maybe empty)
-- @usage
-- local file_path, ext = path.splitext("/bonzo/dog_stuff/cat.txt")
-- assert(file_path == "/bonzo/dog_stuff/cat")
-- assert(ext == ".txt")
--
-- local file_path, ext = path.splitext("")
-- assert(file_path == "")
-- assert(ext == "")
function path.splitext(P)
    assert_string(1,P)
    local i = #P
    local ch = at(P,i)
    while i > 0 and ch ~= '.' do
        if seps[ch] then
            return P,''
        end
        i = i - 1
        ch = at(P,i)
    end
    if i == 0 then
        return P,''
    else
        return sub(P,1,i-1),sub(P,i)
    end
end

--- return the directory part of a path
-- @string P A file path
-- @treturn string everything before the last dir-separator
-- @see splitpath
-- @usage
-- path.dirname("/some/path/file.txt")   -- "/some/path"
-- path.dirname("file.txt")              -- "" (empty string)
function path.dirname(P)
    assert_string(1,P)
    local p1 = path.splitpath(P)
    return p1
end

--- return the file part of a path
-- @string P A file path
-- @treturn string
-- @see splitpath
-- @usage
-- path.basename("/some/path/file.txt")  -- "file.txt"
-- path.basename("/some/path/file/")     -- "" (empty string)
function path.basename(P)
    assert_string(1,P)
    local _,p2 = path.splitpath(P)
    return p2
end

--- get the extension part of a path.
-- @string P A file path
-- @treturn string
-- @see splitext
-- @usage
-- path.extension("/some/path/file.txt") -- ".txt"
-- path.extension("/some/path/file_txt") -- "" (empty string)
function path.extension(P)
    assert_string(1,P)
    local _,p2 = path.splitext(P)
    return p2
end

--- is this an absolute path?
-- @string P A file path
-- @usage
-- path.isabs("hello/path")    -- false
-- path.isabs("/hello/path")   -- true
-- -- Windows;
-- path.isabs("hello\path")    -- false
-- path.isabs("\hello\path")   -- true
-- path.isabs("C:\hello\path") -- true
-- path.isabs("C:hello\path")  -- false
function path.isabs(P)
    assert_string(1,P)
    if path.is_windows and at(P,2) == ":" then
        return seps[at(P,3)] ~= nil
    end
    return seps[at(P,1)] ~= nil
end

--- return the path resulting from combining the individual paths.
-- if the second (or later) path is absolute, we return the last absolute path (joined with any non-absolute paths following).
-- empty elements (except the last) will be ignored.
-- @string p1 A file path
-- @string p2 A file path
-- @string ... more file paths
-- @treturn string the combined path
-- @usage
-- path.join("/first","second","third")   -- "/first/second/third"
-- path.join("first","second/third")      -- "first/second/third"
-- path.join("/first","/second","third")  -- "/second/third"
function path.join(p1,p2,...)
    assert_string(1,p1)
    assert_string(2,p2)
    if select('#',...) > 0 then
        local p = path.join(p1,p2)
        local args = {...}
        for i = 1,#args do
            assert_string(i,args[i])
            p = path.join(p,args[i])
        end
        return p
    end
    if path.isabs(p2) then return p2 end
    local endc = at(p1,#p1)
    if endc ~= path.sep and endc ~= other_sep and endc ~= "" then
        p1 = p1..path.sep
    end
    return p1..p2
end

--- normalize the case of a pathname. On Unix, this returns the path unchanged,
-- for Windows it converts;
--
-- * the path to lowercase
-- * forward slashes to backward slashes
-- @string P A file path
-- @usage path.normcase("/Some/Path/File.txt")
-- -- Windows: "\some\path\file.txt"
-- -- Others : "/Some/Path/File.txt"
function path.normcase(P)
    assert_string(1,P)
    if path.is_windows then
        return P:gsub('/','\\'):lower()
    else
        return P
    end
end

--- normalize a path name.
-- `A//B`, `A/./B`, and `A/foo/../B` all become `A/B`.
--
-- An empty path results in '.'.
-- @string P a file path
function path.normpath(P)
    assert_string(1,P)
    -- Split path into anchor and relative path.
    local anchor = ''
    if path.is_windows then
        if P:match '^\\\\' then -- UNC
            anchor = '\\\\'
            P = P:sub(3)
        elseif seps[at(P, 1)] then
            anchor = '\\'
            P = P:sub(2)
        elseif at(P, 2) == ':' then
            anchor = P:sub(1, 2)
            P = P:sub(3)
            if seps[at(P, 1)] then
                anchor = anchor..'\\'
                P = P:sub(2)
            end
        end
        P = P:gsub('/','\\')
    else
        -- According to POSIX, in path start '//' and '/' are distinct,
        -- but '///+' is equivalent to '/'.
        if P:match '^//' and at(P, 3) ~= '/' then
            anchor = '//'
            P = P:sub(3)
        elseif at(P, 1) == '/' then
            anchor = '/'
            P = P:match '^/*(.*)$'
        end
    end
    local parts = {}
    for part in P:gmatch('[^'..sep..']+') do
        if part == '..' then
            if #parts ~= 0 and parts[#parts] ~= '..' then
                remove(parts)
            else
                append(parts, part)
            end
        elseif part ~= '.' then
            append(parts, part)
        end
    end
    P = anchor..concat(parts, sep)
    if P == '' then P = '.' end
    return P
end

--- relative path from current directory or optional start point
-- @string P a path
-- @string[opt] start optional start point (default current directory)
function path.relpath (P,start)
    assert_string(1,P)
    if start then assert_string(2,start) end
    local split,min,append = utils.split, math.min, table.insert
    P = path.abspath(P,start)
    start = start or currentdir()
    local compare
    if path.is_windows then
        P = P:gsub("/","\\")
        start = start:gsub("/","\\")
        compare = function(v) return v:lower() end
    else
        compare = function(v) return v end
    end
    local startl, Pl = split(start,sep), split(P,sep)
    local n = min(#startl,#Pl)
    if path.is_windows and n > 0 and at(Pl[1],2) == ':' and Pl[1] ~= startl[1] then
        return P
    end
    local k = n+1 -- default value if this loop doesn't bail out!
    for i = 1,n do
        if compare(startl[i]) ~= compare(Pl[i]) then
            k = i
            break
        end
    end
    local rell = {}
    for i = 1, #startl-k+1 do rell[i] = '..' end
    if k <= #Pl then
        for i = k,#Pl do append(rell,Pl[i]) end
    end
    return table.concat(rell,sep)
end


--- Replace a starting '~' with the user's home directory.
-- In windows, if HOME isn't set, then USERPROFILE is used in preference to
-- HOMEDRIVE HOMEPATH. This is guaranteed to be writeable on all versions of Windows.
-- @string P A file path
-- @treturn[1] string The file path with the `~` prefix substituted, or the input path if it had no prefix.
-- @treturn[2] nil
-- @treturn[2] string Error message if the environment variables were unavailable.
function path.expanduser(P)
    assert_string(1,P)
    if P:sub(1,1) ~= '~' then
        return P
    end

    local home = getenv('HOME')
    if (not home) and (not path.is_windows) then
        -- no more options to try on Nix
        return nil, "failed to expand '~' (HOME not set)"
    end

    if (not home) then
        -- try alternatives on Windows
        home = getenv 'USERPROFILE'
        if not home then
            local hd = getenv 'HOMEDRIVE'
            local hp = getenv 'HOMEPATH'
            if not (hd and hp) then
              return nil, "failed to expand '~' (HOME, USERPROFILE, and HOMEDRIVE and/or HOMEPATH not set)"
            end
            home = hd..hp
        end
    end

    return home..sub(P,2)
end


---Return a suitable full path to a new temporary file name.
-- unlike os.tmpname(), it always gives you a writeable path (uses TEMP environment variable on Windows)
function path.tmpname ()
    local res = tmpnam()
    -- On Windows if Lua is compiled using MSVC14 os.tmpname
    -- already returns an absolute path within TEMP env variable directory,
    -- no need to prepend it.
    if path.is_windows and not res:find(':') then
        res = getenv('TEMP')..res
    end
    return res
end

--- return the largest common prefix path of two paths.
-- @string path1 a file path
-- @string path2 a file path
-- @return the common prefix (Windows: separators will be normalized, casing will be original)
function path.common_prefix (path1,path2)
    assert_string(1,path1)
    assert_string(2,path2)
    -- get them in order!
    if #path1 > #path2 then path2,path1 = path1,path2 end
    local compare
    if path.is_windows then
        path1 = path1:gsub("/", "\\")
        path2 = path2:gsub("/", "\\")
        compare = function(v) return v:lower() end
    else
        compare = function(v) return v end
    end
    for i = 1,#path1 do
        if compare(at(path1,i)) ~= compare(at(path2,i)) then
            local cp = path1:sub(1,i-1)
            if at(path1,i-1) ~= sep then
                cp = path.dirname(cp)
            end
            return cp
        end
    end
    if at(path2,#path1+1) ~= sep then path1 = path.dirname(path1) end
    return path1
    --return ''
end

--- return the full path where a particular Lua module would be found.
-- Both package.path and package.cpath is searched, so the result may
-- either be a Lua file or a shared library.
-- @string mod name of the module
-- @return on success: path of module, lua or binary
-- @return on error: nil, error string listing paths tried
function path.package_path(mod)
    assert_string(1,mod)
    local res, err1, err2
    res, err1 = package.searchpath(mod,package.path)
    if res then return res,true end
    res, err2 = package.searchpath(mod,package.cpath)
    if res then return res,false end
    return raise ('cannot find module on path\n' .. err1 .. "\n" .. err2)
end


---- finis -----
return path

end,

["pl.permute"] = function()
--------------------
-- Module: 'pl.permute'
--------------------
--- Permutation operations.
--
-- Dependencies: `pl.utils`, `pl.tablex`
-- @module pl.permute
local tablex = require 'pl.tablex'
local utils = require 'pl.utils'
local copy = tablex.deepcopy
local append = table.insert
local assert_arg = utils.assert_arg


local permute = {}


--- an iterator over all order-permutations of the elements of a list.
-- Please note that the same list is returned each time, so do not keep references!
-- @param a list-like table
-- @return an iterator which provides the next permutation as a list
function permute.order_iter(a)
    assert_arg(1,a,'table')

    local t = #a
    local stack = { 1 }
    local function iter()
        local h = #stack
        local n = t - h + 1

        local i = stack[h]
        if i > t then
            return
        end

        if n == 0 then
            table.remove(stack)
            h = h - 1

            stack[h] = stack[h] + 1
            return a

        elseif i <= n then

            -- put i-th element as the last one
            a[n], a[i] = a[i], a[n]

            -- generate all permutations of the other elements
            table.insert(stack, 1)

        else

            table.remove(stack)
            h = h - 1

            n = n + 1
            i = stack[h]

            -- restore i-th element
            a[n], a[i] = a[i], a[n]

            stack[h] = stack[h] + 1
        end
        return iter() -- tail-call
    end

    return iter
end


--- construct a table containing all the order-permutations of a list.
-- @param a list-like table
-- @return a table of tables
-- @usage permute.order_table {1,2,3} --> {{2,3,1},{3,2,1},{3,1,2},{1,3,2},{2,1,3},{1,2,3}}
function permute.order_table (a)
    assert_arg(1,a,'table')
    local res = {}
    for t in permute.iter(a) do
        append(res,copy(t))
    end
    return res
end



--- an iterator over all permutations of the elements of the given lists.
-- @param ... list-like tables, they are nil-safe if a length-field `n` is provided (see `utils.pack`)
-- @return an iterator which provides the next permutation as return values in the same order as the provided lists, preceded by an index
-- @usage
-- local strs = utils.pack("one", nil, "three")  -- adds an 'n' field for nil-safety
-- local bools = utils.pack(true, false)
-- local iter = permute.list_iter(strs, bools)
--
-- print(iter())    --> 1, one, true
-- print(iter())    --> 2, nil, true
-- print(iter())    --> 3, three, true
-- print(iter())    --> 4, one, false
-- print(iter())    --> 5, nil, false
-- print(iter())    --> 6, three, false
function permute.list_iter(...)
  local elements = {...}
  local pointers = {}
  local sizes = {}
  local size = #elements
  for i, list in ipairs(elements) do
    assert_arg(i,list,'table')
    pointers[i] = 1
    sizes[i] = list.n or #list
  end
  local count = 0

  return function()
    if pointers[size] > sizes[size] then return end -- we're done
    count = count + 1
    local r = { n = #elements }
    local cascade_up = true
    for i = 1, size do
      r[i] = elements[i][pointers[i]]
      if cascade_up then
        pointers[i] = pointers[i] + 1
        if pointers[i] <= sizes[i] then
          -- this list is not done yet, stop cascade
          cascade_up = false
        else
          -- this list is done
          if i ~= size then
            -- reset pointer
            pointers[i] = 1
          end
        end
      end
    end
    return count, utils.unpack(r)
  end
end



--- construct a table containing all the permutations of a set of lists.
-- @param ... list-like tables, they are nil-safe if a length-field `n` is provided
-- @return a list of lists, the sub-lists have an 'n' field for nil-safety
-- @usage
-- local strs = utils.pack("one", nil, "three")  -- adds an 'n' field for nil-safety
-- local bools = utils.pack(true, false)
-- local results = permute.list_table(strs, bools)
-- -- results = {
-- --   { "one, true, n = 2 }
-- --   { nil, true, n = 2 },
-- --   { "three, true, n = 2 },
-- --   { "one, false, n = 2 },
-- --   { nil, false, n = 2 },
-- --   { "three", false, n = 2 },
-- -- }
function permute.list_table(...)
  local iter = permute.list_iter(...)
  local results = {}
  local i = 1
  while true do
    local values = utils.pack(iter())
    if values[1] == nil then return results end
    for i = 1, values.n do values[i] = values[i+1] end
    values.n = values.n - 1
    results[i] = values
    i = i + 1
  end
end


-- backward compat, to be deprecated

--- deprecated.
-- @param ...
-- @see permute.order_iter
function permute.iter(...)
  utils.raise_deprecation {
    source = "Penlight " .. utils._VERSION,
    message = "function 'iter' was renamed to 'order_iter'",
    version_removed = "2.0.0",
    deprecated_after = "1.9.2",
  }

  return permute.order_iter(...)
end

--- deprecated.
-- @param ...
-- @see permute.order_iter
function permute.table(...)
  utils.raise_deprecation {
    source = "Penlight " .. utils._VERSION,
    message = "function 'table' was renamed to 'order_table'",
    version_removed = "2.0.0",
    deprecated_after = "1.9.2",
  }

  return permute.order_table(...)
end

return permute

end,

["pl.pretty"] = function()
--------------------
-- Module: 'pl.pretty'
--------------------
--- Pretty-printing Lua tables.
-- Also provides a sandboxed Lua table reader and
-- a function to present large numbers in human-friendly format.
--
-- Dependencies: `pl.utils`, `pl.lexer`, `pl.stringx`, `debug`
-- @module pl.pretty

local append = table.insert
local concat = table.concat
local mfloor, mhuge = math.floor, math.huge
local mtype = math.type
local utils = require 'pl.utils'
local lexer = require 'pl.lexer'
local debug = require 'debug'
local quote_string = require'pl.stringx'.quote_string
local assert_arg = utils.assert_arg

local original_tostring = tostring

-- Calculate min and max integer supported by lua_Number
-- Assumptions:
-- 1. max_int = 2 ^ n - 1
-- 2. min_int = -max_int
-- 3. if n > max_int versions with integer support will have
-- integer overflow and versions without integers will lose least significant bit
-- Note: if lua_Integer is smaller than lua_Number mantissa string.format('%d')
-- can throw runtime error
local max_int, min_int
local next_cand = 1
while  next_cand > 0 and next_cand % 2 == 1 do
  max_int = next_cand
  min_int = -next_cand
  next_cand = next_cand * 2 + 1
end

local function is_integer(value)
  if _VERSION == "Lua 5.3" or _VERSION == "Lua 5.4" then
    return mtype(value) == "integer"
  end
  if value < min_int or value > max_int then
    return false
  end
  return math.floor(value) == value
end

local function is_float(value)
  if _VERSION == "Lua 5.3" or _VERSION == "Lua 5.4" then
    return mtype(value) == "float"
  end
  if value < min_int or value > max_int then
    return true
  end
  return mfloor(value) == value
end

-- Patch tostring to format numbers with better precision
-- and to produce cross-platform results for
-- infinite values and NaN.
local function tostring(value)
    if type(value) ~= "number" then
        return original_tostring(value)
    elseif value ~= value then
        return "NaN"
    elseif value == mhuge then
        return "Inf"
    elseif value == -mhuge then
        return "-Inf"
    elseif is_integer(value) then
        return ("%d"):format(value)
    else
        local res = ("%.14g"):format(value)
        if is_float(value) and not res:find("%.") then
            -- Number is internally a float but looks like an integer.
            -- Insert ".0" after first run of digits.
            res = res:gsub("%d+", "%0.0", 1)
        end
        return res
    end
end

local pretty = {}

local function save_global_env()
    local env = {}
    env.hook, env.mask, env.count = debug.gethook()

    -- env.hook is "external hook" if is a C hook function
    if env.hook~="external hook" then
        debug.sethook()
    end

    env.string_mt = getmetatable("")
    debug.setmetatable("", nil)
    return env
end

local function restore_global_env(env)
    if env then
        debug.setmetatable("", env.string_mt)
        if env.hook~="external hook" then
            debug.sethook(env.hook, env.mask, env.count)
        end
    end
end

--- Read a string representation of a Lua table.
-- This function loads and runs the string as Lua code, but bails out
-- if it contains a function definition.
-- Loaded string is executed in an empty environment.
-- @string s string to read in `{...}` format, possibly with some whitespace
-- before or after the curly braces. A single line comment may be present
-- at the beginning.
-- @return a table in case of success.
-- If loading the string failed, return `nil` and error message.
-- If executing loaded string failed, return `nil` and the error it raised.
function pretty.read(s)
    assert_arg(1,s,'string')
    if s:find '^%s*%-%-' then -- may start with a comment..
        s = s:gsub('%-%-.-\n','')
    end
    if not s:find '^%s*{' then return nil,"not a Lua table" end
    if s:find '[^\'"%w_]function[^\'"%w_]' then
        local tok = lexer.lua(s)
        for t,v in tok do
            if t == 'keyword' and v == 'function' then
                return nil,"cannot have functions in table definition"
            end
        end
    end
    s = 'return '..s
    local chunk,err = utils.load(s,'tbl','t',{})
    if not chunk then return nil,err end
    local global_env = save_global_env()
    local ok,ret = pcall(chunk)
    restore_global_env(global_env)
    if ok then return ret
    else
        return nil,ret
    end
end

--- Read a Lua chunk.
-- @string s Lua code.
-- @tab[opt] env environment used to run the code, empty by default.
-- @bool[opt] paranoid abort loading if any looping constructs a found in the code
-- and disable string methods.
-- @return the environment in case of success or `nil` and syntax or runtime error
-- if something went wrong.
function pretty.load (s, env, paranoid)
    env = env or {}
    if paranoid then
        local tok = lexer.lua(s)
        for t,v in tok do
            if t == 'keyword'
                and (v == 'for' or v == 'repeat' or v == 'function' or v == 'goto')
            then
                return nil,"looping not allowed"
            end
        end
    end
    local chunk,err = utils.load(s,'tbl','t',env)
    if not chunk then return nil,err end
    local global_env = paranoid and save_global_env()
    local ok,err = pcall(chunk)
    restore_global_env(global_env)
    if not ok then return nil,err end
    return env
end

local function quote_if_necessary (v)
    if not v then return ''
    else
        --AAS
        if v:find ' ' then v = quote_string(v) end
    end
    return v
end

local keywords

local function is_identifier (s)
    return type(s) == 'string' and s:find('^[%a_][%w_]*$') and not keywords[s]
end

local function quote (s)
    if type(s) == 'table' then
        return pretty.write(s,'')
    else
        --AAS
        return quote_string(s)-- ('%q'):format(tostring(s))
    end
end

local function index (numkey,key)
    --AAS
    if not numkey then
        key = quote(key)
         key = key:find("^%[") and (" " .. key .. " ") or key
    end
    return '['..key..']'
end


--- Create a string representation of a Lua table.
-- This function never fails, but may complain by returning an
-- extra value. Normally puts out one item per line, using
-- the provided indent; set the second parameter to an empty string
-- if you want output on one line.
--
-- *NOTE:* this is NOT a serialization function, not a full blown
-- debug function. Checkout out respectively the
-- [serpent](https://github.com/pkulchenko/serpent)
-- or [inspect](https://github.com/kikito/inspect.lua)
-- Lua modules for that if you need them.
-- @tab tbl Table to serialize to a string.
-- @string[opt] space The indent to use.
-- Defaults to two spaces; pass an empty string for no indentation.
-- @bool[opt] not_clever Pass `true` for plain output, e.g `{['key']=1}`.
-- Defaults to `false`.
-- @return a string
-- @return an optional error message
function pretty.write (tbl,space,not_clever)
    if type(tbl) ~= 'table' then
        local res = tostring(tbl)
        if type(tbl) == 'string' then return quote(tbl) end
        return res, 'not a table'
    end
    if not keywords then
        keywords = lexer.get_keywords()
    end
    local set = ' = '
    if space == '' then set = '=' end
    space = space or '  '
    local lines = {}
    local line = ''
    local tables = {}


    local function put(s)
        if #s > 0 then
            line = line..s
        end
    end

    local function putln (s)
        if #line > 0 then
            line = line..s
            append(lines,line)
            line = ''
        else
            append(lines,s)
        end
    end

    local function eat_last_comma ()
        local n = #lines
        local lastch = lines[n]:sub(-1,-1)
        if lastch == ',' then
            lines[n] = lines[n]:sub(1,-2)
        end
    end


    -- safe versions for iterators since 5.3+ honors metamethods that can throw
    -- errors
    local ipairs = function(t)
        local i = 0
        local ok, v
        local getter = function() return t[i] end
        return function()
                i = i + 1
                ok, v = pcall(getter)
                if v == nil or not ok then return end
                return i, t[i]
            end
    end
    local pairs = function(t)
        local k, v, ok
        local getter = function() return next(t, k) end
        return function()
                ok, k, v = pcall(getter)
                if not ok then return end
                return k, v
            end
    end

    local writeit
    writeit = function (t,oldindent,indent)
        local tp = type(t)
        if tp ~= 'string' and  tp ~= 'table' then
            putln(quote_if_necessary(tostring(t))..',')
        elseif tp == 'string' then
            -- if t:find('\n') then
            --     putln('[[\n'..t..']],')
            -- else
            --     putln(quote(t)..',')
            -- end
            --AAS
            putln(quote_string(t) ..",")
        elseif tp == 'table' then
            if tables[t] then
                putln('<cycle>,')
                return
            end
            tables[t] = true
            local newindent = indent..space
            putln('{')
            local used = {}
            if not not_clever then
                for i,val in ipairs(t) do
                    put(indent)
                    writeit(val,indent,newindent)
                    used[i] = true
                end
            end
            local ordered_keys = {}
            for k,v in pairs(t) do
               if type(k) ~= 'number' then
                  ordered_keys[#ordered_keys + 1] = k
               end
            end
            table.sort(ordered_keys, function (a, b)
                if type(a) == type(b) then
                    return tostring(a) < tostring(b)
                else
                    return type(a) < type(b)
                end
            end)
            local function write_entry (key, val)
                local tkey = type(key)
                local numkey = tkey == 'number'
                if not_clever then
                    key = tostring(key)
                    put(indent..index(numkey,key)..set)
                    writeit(val,indent,newindent)
                else
                    if not numkey or not used[key] then -- non-array indices
                        if tkey ~= 'string' then
                            key = tostring(key)
                        end
                        if numkey or not is_identifier(key) then
                            key = index(numkey,key)
                        end
                        put(indent..key..set)
                        writeit(val,indent,newindent)
                    end
                end
            end
            for i = 1, #ordered_keys do
                local key = ordered_keys[i]
                local val = t[key]
                write_entry(key, val)
            end
            for key,val in pairs(t) do
               if type(key) == 'number' then
                  write_entry(key, val)
               end
            end
            tables[t] = nil
            eat_last_comma()
            putln(oldindent..'},')
        else
            putln(tostring(t)..',')
        end
    end
    writeit(tbl,'',space)
    eat_last_comma()
    return concat(lines,#space > 0 and '\n' or '')
end

--- Dump a Lua table out to a file or stdout.
-- @tab t The table to write to a file or stdout.
-- @string[opt] filename File name to write too. Defaults to writing
-- to stdout.
function pretty.dump (t, filename)
    if not filename then
        print(pretty.write(t))
        return true
    else
        return utils.writefile(filename, pretty.write(t))
    end
end

--- Dump a series of arguments to stdout for debug purposes.
-- This function is attached to the module table `__call` method, to make it
-- extra easy to access. So the full:
--
--     print(require("pl.pretty").write({...}))
--
-- Can be shortened to:
--
--     require"pl.pretty" (...)
--
-- Any `nil` entries will be printed as `"<nil>"` to make them explicit.
-- @param ... the parameters to dump to stdout.
-- @usage
-- -- example debug output
-- require"pl.pretty" ("hello", nil, "world", { bye = "world", true} )
--
-- -- output:
-- {
--   ["arg 1"] = "hello",
--   ["arg 2"] = "<nil>",
--   ["arg 3"] = "world",
--   ["arg 4"] = {
--     true,
--     bye = "world"
--   }
-- }
function pretty.debug(...)
    local n = select("#", ...)
    local t = { ... }
    for i = 1, n do
        local value = t[i]
        if value == nil then
            value = "<nil>"
        end
        t[i] = nil
        t["arg " .. i] = value
    end

    print(pretty.write(t))
    return true
end


local memp,nump = {'B','KiB','MiB','GiB'},{'','K','M','B'}

local function comma (val)
    local thou = math.floor(val/1000)
    if thou > 0 then return comma(thou)..','.. tostring(val % 1000)
    else return tostring(val) end
end

--- Format large numbers nicely for human consumption.
-- @number num a number.
-- @string[opt] kind one of `'M'` (memory in `KiB`, `MiB`, etc.),
-- `'N'` (postfixes are `'K'`, `'M'` and `'B'`),
-- or `'T'` (use commas as thousands separator), `'N'` by default.
-- @int[opt] prec number of digits to use for `'M'` and `'N'`, `1` by default.
function pretty.number (num,kind,prec)
    local fmt = '%.'..(prec or 1)..'f%s'
    if kind == 'T' then
        return comma(num)
    else
        local postfixes, fact
        if kind == 'M' then
            fact = 1024
            postfixes = memp
        else
            fact = 1000
            postfixes = nump
        end
        local div = fact
        local k = 1
        while num >= div and k <= #postfixes do
            div = div * fact
            k = k + 1
        end
        div = div / fact
        if k > #postfixes then k = k - 1; div = div/fact end
        if k > 1 then
            return fmt:format(num/div,postfixes[k] or 'duh')
        else
            return num..postfixes[1]
        end
    end
end

return setmetatable(pretty, {
    __call = function(self, ...)
        return self.debug(...)
    end
})

end,

["pl.seq"] = function()
--------------------
-- Module: 'pl.seq'
--------------------
--- Manipulating iterators as sequences.
-- See @{07-functional.md.Sequences|The Guide}
--
-- Dependencies: `pl.utils`, `pl.types`, `debug`
-- @module pl.seq

local next,assert,pairs,tonumber,type,setmetatable = next,assert,pairs,tonumber,type,setmetatable
local strfind,format = string.find,string.format
local mrandom = math.random
local tsort,tappend = table.sort,table.insert
local io = io
local utils = require 'pl.utils'
local callable = require 'pl.types'.is_callable
local function_arg = utils.function_arg
local assert_arg = utils.assert_arg
local debug = require 'debug'

local seq = {}

-- given a number, return a function(y) which returns true if y > x
-- @param x a number
function seq.greater_than(x)
  return function(v)
    return tonumber(v) > x
  end
end

-- given a number, returns a function(y) which returns true if y < x
-- @param x a number
function seq.less_than(x)
  return function(v)
    return tonumber(v) < x
  end
end

-- given any value, return a function(y) which returns true if y == x
-- @param x a value
function seq.equal_to(x)
  if type(x) == "number" then
    return function(v)
      return tonumber(v) == x
    end
  else
    return function(v)
      return v == x
    end
  end
end

--- given a string, return a function(y) which matches y against the string.
-- @param s a string
function seq.matching(s)
  return function(v)
     return strfind(v,s)
  end
end

local nexti

--- sequence adaptor for a table.   Note that if any generic function is
-- passed a table, it will automatically use seq.list()
-- @param t a list-like table
-- @usage sum(list(t)) is the sum of all elements of t
-- @usage for x in list(t) do...end
function seq.list(t)
  assert_arg(1,t,'table')
  if not nexti then
    nexti = ipairs{}
  end
  local key,value = 0
  return function()
    key,value = nexti(t,key)
    return value
  end
end

--- return the keys of the table.
-- @param t an arbitrary table
-- @return iterator over keys
function seq.keys(t)
  assert_arg(1,t,'table')
  local key
  return function()
    key = next(t,key)
    return key
  end
end

local list = seq.list
local function default_iter(iter)
  if type(iter) == 'table' then return list(iter)
  else return iter end
end

seq.iter = default_iter

--- create an iterator over a numerical range. Like the standard Python function xrange.
-- @param start a number
-- @param finish a number greater than start
function seq.range(start,finish)
  local i = start - 1
  return function()
      i = i + 1
      if i > finish then return nil
      else return i end
  end
end

-- count the number of elements in the sequence which satisfy the predicate
-- @param iter a sequence
-- @param condn a predicate function (must return either true or false)
-- @param optional argument to be passed to predicate as second argument.
-- @return count
function seq.count(iter,condn,arg)
  local i = 0
  seq.foreach(iter,function(val)
        if condn(val,arg) then i = i + 1 end
  end)
  return i
end

--- return the minimum and the maximum value of the sequence.
-- @param iter a sequence
-- @return minimum value
-- @return maximum value
function seq.minmax(iter)
  local vmin,vmax = 1e70,-1e70
  for v in default_iter(iter) do
    v = tonumber(v)
    if v < vmin then vmin = v end
    if v > vmax then vmax = v end
  end
  return vmin,vmax
end

--- return the sum and element count of the sequence.
-- @param iter a sequence
-- @param fn an optional function to apply to the values
function seq.sum(iter,fn)
  local s = 0
  local i = 0
  for v in default_iter(iter) do
    if fn then v = fn(v) end
    s = s + v
    i = i + 1
  end
  return s,i
end

--- create a table from the sequence. (This will make the result a List.)
-- @param iter a sequence
-- @return a List
-- @usage copy(list(ls)) is equal to ls
-- @usage copy(list {1,2,3}) == List{1,2,3}
function seq.copy(iter)
    local res,k = {},1
    for v in default_iter(iter) do
        res[k] = v
        k = k + 1
    end
    setmetatable(res, require('pl.List'))
    return res
end

--- create a table of pairs from the double-valued sequence.
-- @param iter a double-valued sequence
-- @param i1 used to capture extra iterator values
-- @param i2 as with pairs & ipairs
-- @usage copy2(ipairs{10,20,30}) == {{1,10},{2,20},{3,30}}
-- @return a list-like table
function seq.copy2 (iter,i1,i2)
    local res,k = {},1
    for v1,v2 in iter,i1,i2 do
        res[k] = {v1,v2}
        k = k + 1
    end
    return res
end

--- create a table of 'tuples' from a multi-valued sequence.
-- A generalization of copy2 above
-- @param iter a multiple-valued sequence
-- @return a list-like table
function seq.copy_tuples (iter)
    iter = default_iter(iter)
    local res = {}
    local row = {iter()}
    while #row > 0 do
        tappend(res,row)
        row = {iter()}
    end
    return res
end

--- return an iterator of random numbers.
-- @param n the length of the sequence
-- @param l same as the first optional argument to math.random
-- @param u same as the second optional argument to math.random
-- @return a sequence
function seq.random(n,l,u)
  local rand
  assert(type(n) == 'number')
  if u then
     rand = function() return mrandom(l,u) end
  elseif l then
     rand = function() return mrandom(l) end
  else
     rand = mrandom
  end

  return function()
     if n == 0 then return nil
     else
       n = n - 1
       return rand()
     end
  end
end

--- return an iterator to the sorted elements of a sequence.
-- @param iter a sequence
-- @param comp an optional comparison function (comp(x,y) is true if x < y)
function seq.sort(iter,comp)
    local t = seq.copy(iter)
    tsort(t,comp)
    return list(t)
end

--- return an iterator which returns elements of two sequences.
-- @param iter1 a sequence
-- @param iter2 a sequence
-- @usage for x,y in seq.zip(ls1,ls2) do....end
function seq.zip(iter1,iter2)
    iter1 = default_iter(iter1)
    iter2 = default_iter(iter2)
    return function()
        return iter1(),iter2()
    end
end

--- Makes a table where the key/values are the values and value counts of the sequence.
-- This version works with 'hashable' values like strings and numbers.
-- `pl.tablex.count_map` is more general.
-- @param iter a sequence
-- @return a map-like table
-- @return a table
-- @see pl.tablex.count_map
function seq.count_map(iter)
    local t = {}
    local v
    for s in default_iter(iter) do
        v = t[s]
        if v then t[s] = v + 1
        else t[s] = 1 end
    end
    return setmetatable(t, require('pl.Map'))
end

-- given a sequence, return all the unique values in that sequence.
-- @param iter a sequence
-- @param returns_table true if we return a table, not a sequence
-- @return a sequence or a table; defaults to a sequence.
function seq.unique(iter,returns_table)
    local t = seq.count_map(iter)
    local res,k = {},1
    for key in pairs(t) do res[k] = key; k = k + 1 end
    table.sort(res)
    if returns_table then
        return res
    else
        return list(res)
    end
end

--- print out a sequence iter with a separator.
-- @param iter a sequence
-- @param sep the separator (default space)
-- @param nfields maximum number of values per line (default 7)
-- @param fmt optional format function for each value
function seq.printall(iter,sep,nfields,fmt)
  local write = io.write
  if not sep then sep = ' ' end
  if not nfields then
      if sep == '\n' then nfields = 1e30
      else nfields = 7 end
  end
  if fmt then
    local fstr = fmt
    fmt = function(v) return format(fstr,v) end
  end
  local k = 1
  for v in default_iter(iter) do
     if fmt then v = fmt(v) end
     if k < nfields then
       write(v,sep)
       k = k + 1
    else
       write(v,'\n')
       k = 1
    end
  end
  write '\n'
end

-- return an iterator running over every element of two sequences (concatenation).
-- @param iter1 a sequence
-- @param iter2 a sequence
function seq.splice(iter1,iter2)
  iter1 = default_iter(iter1)
  iter2 = default_iter(iter2)
  local iter = iter1
  return function()
    local ret = iter()
    if ret == nil then
      if iter == iter1 then
        iter = iter2
        return iter()
      else return nil end
   else
       return  ret
   end
 end
end

--- return a sequence where every element of a sequence has been transformed
-- by a function. If you don't supply an argument, then the function will
-- receive both values of a double-valued sequence, otherwise behaves rather like
-- tablex.map.
-- @param fn a function to apply to elements; may take two arguments
-- @param iter a sequence of one or two values
-- @param arg optional argument to pass to function.
function seq.map(fn,iter,arg)
    fn = function_arg(1,fn)
    iter = default_iter(iter)
    return function()
        local v1,v2 = iter()
        if v1 == nil then return nil end
        return fn(v1,arg or v2) or false
    end
end

--- filter a sequence using a predicate function.
-- @param iter a sequence of one or two values
-- @param pred a boolean function; may take two arguments
-- @param arg optional argument to pass to function.
function seq.filter (iter,pred,arg)
    pred = function_arg(2,pred)
    return function ()
        local v1,v2
        while true do
            v1,v2 = iter()
            if v1 == nil then return nil end
            if pred(v1,arg or v2) then return v1,v2 end
        end
    end
end

--- 'reduce' a sequence using a binary function.
-- @func fn a function of two arguments
-- @param iter a sequence
-- @param initval optional initial value
-- @usage seq.reduce(operator.add,seq.list{1,2,3,4}) == 10
-- @usage seq.reduce('-',{1,2,3,4,5}) == -13
function seq.reduce (fn,iter,initval)
   fn = function_arg(1,fn)
   iter = default_iter(iter)
   local val = initval or iter()
   if val == nil then return nil end
   for v in iter do
       val = fn(val,v)
   end
   return val
end

--- take the first n values from the sequence.
-- @param iter a sequence of one or two values
-- @param n number of items to take
-- @return a sequence of at most n items
function seq.take (iter,n)
    iter = default_iter(iter)
    return function()
        if n < 1 then return end
        local val1,val2 = iter()
        if not val1 then return end
        n = n - 1
        return val1,val2
    end
end

--- skip the first n values of a sequence
-- @param iter a sequence of one or more values
-- @param n number of items to skip
function seq.skip (iter,n)
    n = n or 1
    for i = 1,n do
        if iter() == nil then return list{} end
    end
    return iter
end

--- a sequence with a sequence count and the original value.
-- enum(copy(ls)) is a roundabout way of saying ipairs(ls).
-- @param iter a single or double valued sequence
-- @return sequence of (i,v), i = 1..n and v is from iter.
function seq.enum (iter)
    local i = 0
    iter = default_iter(iter)
    return function  ()
        local val1,val2 = iter()
        if not val1 then return end
        i = i + 1
        return i,val1,val2
    end
end

--- map using a named method over a sequence.
-- @param iter a sequence
-- @param name the method name
-- @param arg1 optional first extra argument
-- @param arg2 optional second extra argument
function seq.mapmethod (iter,name,arg1,arg2)
    iter = default_iter(iter)
    return function()
        local val = iter()
        if not val then return end
        local fn = val[name]
        if not fn then error(type(val).." does not have method "..name) end
        return fn(val,arg1,arg2)
    end
end

--- a sequence of (last,current) values from another sequence.
--  This will return S(i-1),S(i) if given S(i)
-- @param iter a sequence
function seq.last (iter)
    iter = default_iter(iter)
    local val, l = iter(), nil
    if val == nil then return list{} end
    return function ()
        val,l = iter(),val
        if val == nil then return nil end
        return val,l
    end
end

--- call the function on each element of the sequence.
-- @param iter a sequence with up to 3 values
-- @param fn a function
function seq.foreach(iter,fn)
    fn = function_arg(2,fn)
    for i1,i2,i3 in default_iter(iter) do fn(i1,i2,i3) end
end

---------------------- Sequence Adapters ---------------------

local SMT

local function SW (iter,...)
    if callable(iter) then
        return setmetatable({iter=iter},SMT)
    else
        return iter,...
    end
end


-- can't directly look these up in seq because of the wrong argument order...
local map,reduce,mapmethod = seq.map, seq.reduce, seq.mapmethod
local overrides = {
    map = function(self,fun,arg)
        return map(fun,self,arg)
    end,
    reduce = function(self,fun,initval)
        return reduce(fun,self,initval)
    end
}

SMT = {
    __index = function (tbl,key)
        local fn = overrides[key] or seq[key]
        if fn then
            return function(sw,...) return SW(fn(sw.iter,...)) end
        else
            return function(sw,...) return SW(mapmethod(sw.iter,key,...)) end
        end
    end,
    __call = function (sw)
        return sw.iter()
    end,
}

setmetatable(seq,{
    __call = function(tbl,iter,extra)
        if not callable(iter) then
            if type(iter) == 'table' then iter = seq.list(iter)
            else return iter
            end
        end
        if extra then
            return setmetatable({iter=function()
                return iter(extra)
            end},SMT)
        else
            return setmetatable({iter=iter},SMT)
        end
    end
})

--- create a wrapped iterator over all lines in the file.
-- @param f either a filename, file-like object, or 'STDIN' (for standard input)
-- @param ... for Lua 5.2 only, optional format specifiers, as in `io.read`.
-- @return a sequence wrapper
function seq.lines (f,...)
    local iter,obj
    if f == 'STDIN' then
        f = io.stdin
    elseif type(f) == 'string' then
        iter,obj = io.lines(f,...)
    elseif not f.read then
        error("Pass either a string or a file-like object",2)
    end
    if not iter then
        iter,obj = f:lines(...)
    end
    if obj then -- LuaJIT version returns a function operating on a file
        local lines,file = iter,obj
        iter = function() return lines(file) end
    end
    return SW(iter)
end

function seq.import ()
    debug.setmetatable(function() end,{
        __index = function(tbl,key)
            local s = overrides[key] or seq[key]
            if s then return s
            else
                return function(s,...) return seq.mapmethod(s,key,...) end
            end
        end
    })
end

return seq

end,

["pl.Set"] = function()
--------------------
-- Module: 'pl.Set'
--------------------
--- A Set class.
--
--     > Set = require 'pl.Set'
--     > = Set{'one','two'} == Set{'two','one'}
--     true
--     > fruit = Set{'apple','banana','orange'}
--     > = fruit['banana']
--     true
--     > = fruit['hazelnut']
--     nil
--     > colours = Set{'red','orange','green','blue'}
--     > = fruit,colours
--     [apple,orange,banana]   [blue,green,orange,red]
--     > = fruit+colours
--     [blue,green,apple,red,orange,banana]
--     [orange]
--     > more_fruits = fruit + 'apricot'
--     > = fruit*colours
--    > =  more_fruits, fruit
--    [banana,apricot,apple,orange]	[banana,apple,orange]
--
-- Dependencies: `pl.utils`, `pl.tablex`, `pl.class`, `pl.Map`, (`pl.List` if __tostring is used)
-- @classmod pl.Set

local tablex = require 'pl.tablex'
local utils = require 'pl.utils'
local array_tostring, concat = utils.array_tostring, table.concat
local merge,difference = tablex.merge,tablex.difference
local Map = require 'pl.Map'
local class = require 'pl.class'
local stdmt = utils.stdmt
local Set = stdmt.Set

-- the Set class --------------------
class(Map,nil,Set)

-- note: Set has _no_ methods!
Set.__index = nil

local function makeset (t)
    return setmetatable(t,Set)
end

--- create a set. <br>
-- @param t may be a Set, Map or list-like table.
-- @class function
-- @name Set
function Set:_init (t)
    t = t or {}
    local mt = getmetatable(t)
    if mt == Set or mt == Map then
        for k in pairs(t) do self[k] = true end
    else
        for _,v in ipairs(t) do self[v] = true end
    end
end

--- string representation of a set.
-- @within metamethods
function Set:__tostring ()
    return '['..concat(array_tostring(Set.values(self)),',')..']'
end

--- get a list of the values in a set.
-- @param self a Set
-- @function Set.values
-- @return a list
Set.values = Map.keys

--- map a function over the values of a set.
-- @param self a Set
-- @param fn a function
-- @param ... extra arguments to pass to the function.
-- @return a new set
function Set.map (self,fn,...)
    fn = utils.function_arg(1,fn)
    local res = {}
    for k in pairs(self) do
        res[fn(k,...)] = true
    end
    return makeset(res)
end

--- union of two sets (also +).
-- @param self a Set
-- @param set another set
-- @return a new set
function Set.union (self,set)
    return merge(self,set,true)
end

--- modifies '+' operator to allow addition of non-Set elements
--- Preserves +/- semantics - does not modify first argument.
local function setadd(self,other)
    local mt = getmetatable(other)
    if mt == Set or mt == Map then
        return Set.union(self,other)
    else
        local new = Set(self)
        new[other] = true
        return new
    end
end

--- union of sets.
-- @within metamethods
-- @function Set.__add

Set.__add = setadd

--- intersection of two sets (also *).
-- @param self a Set
-- @param set another set
-- @return a new set
-- @usage
-- > s = Set{10,20,30}
-- > t = Set{20,30,40}
-- > = t
-- [20,30,40]
-- > = Set.intersection(s,t)
-- [30,20]
-- > = s*t
-- [30,20]

function Set.intersection (self,set)
    return merge(self,set,false)
end

--- intersection of sets.
-- @within metamethods
-- @function Set.__mul
Set.__mul = Set.intersection

--- new set with elements in the set that are not in the other (also -).
-- @param self a Set
-- @param set another set
-- @return a new set
function Set.difference (self,set)
    return difference(self,set,false)
end

--- modifies "-" operator to remove non-Set values from set.
--- Preserves +/- semantics - does not modify first argument.
local function setminus (self,other)
    local mt = getmetatable(other)
    if mt == Set or mt == Map then
        return Set.difference(self,other)
    else
        local new = Set(self)
        new[other] = nil
        return new
    end
end

--- difference of sets.
-- @within metamethods
-- @function Set.__sub
Set.__sub = setminus

-- a new set with elements in _either_ the set _or_ other but not both (also ^).
-- @param self a Set
-- @param set another set
-- @return a new set
function Set.symmetric_difference (self,set)
    return difference(self,set,true)
end

--- symmetric difference of sets.
-- @within metamethods
-- @function Set.__pow
Set.__pow = Set.symmetric_difference

--- is the first set a subset of the second (also <)?.
-- @param self a Set
-- @param set another set
-- @return true or false
function Set.issubset (self,set)
    for k in pairs(self) do
        if not set[k] then return false end
    end
    return true
end

--- first set subset of second?
-- @within metamethods
-- @function Set.__lt
Set.__lt = Set.issubset

--- is the set empty?.
-- @param self a Set
-- @return true or false
function Set.isempty (self)
    return next(self) == nil
end

--- are the sets disjoint? (no elements in common).
-- Uses naive definition, i.e. that intersection is empty
-- @param s1 a Set
-- @param s2 another set
-- @return true or false
function Set.isdisjoint (s1,s2)
    return Set.isempty(Set.intersection(s1,s2))
end

--- size of this set (also # for 5.2).
-- @param s a Set
-- @return size
-- @function Set.len
Set.len = tablex.size

--- cardinality of set (5.2).
-- @within metamethods
-- @function Set.__len
Set.__len = Set.len

--- equality between sets.
-- @within metamethods
function Set.__eq (s1,s2)
    return Set.issubset(s1,s2) and Set.issubset(s2,s1)
end

return Set

end,

["pl.sip"] = function()
--------------------
-- Module: 'pl.sip'
--------------------
--- Simple Input Patterns (SIP).
-- SIP patterns start with '$', then a
-- one-letter type, and then an optional variable in curly braces.
--
--    sip.match('$v=$q','name="dolly"',res)
--    ==> res=={'name','dolly'}
--    sip.match('($q{first},$q{second})','("john","smith")',res)
--    ==> res=={second='smith',first='john'}
--
-- Type names:
--
--    v     identifier
--    i     integer
--    f     floating-point
--    q     quoted string
--    ([{<  match up to closing bracket
--
-- See @{08-additional.md.Simple_Input_Patterns|the Guide}
--
-- @module pl.sip

local loadstring = rawget(_G,'loadstring') or load
local unpack = rawget(_G,'unpack') or rawget(table,'unpack')

local append,concat = table.insert,table.concat
local ipairs,type = ipairs,type
local io,_G = io,_G
local print,rawget = print,rawget

local patterns = {
    FLOAT = '[%+%-%d]%d*%.?%d*[eE]?[%+%-]?%d*',
    INTEGER = '[+%-%d]%d*',
    IDEN = '[%a_][%w_]*',
    OPTION = '[%a_][%w_%-]*',
}

local function assert_arg(idx,val,tp)
    if type(val) ~= tp then
        error("argument "..idx.." must be "..tp, 2)
    end
end

local sip = {}

local brackets = {['<'] = '>', ['('] = ')', ['{'] = '}', ['['] = ']' }
local stdclasses = {a=1,c=0,d=1,l=1,p=0,u=1,w=1,x=1,s=0}

local function group(s)
    return '('..s..')'
end

-- escape all magic characters except $, which has special meaning
-- Also, un-escape any characters after $, so $( and $[ passes through as is.
local function escape (spec)
    return (spec:gsub('[%-%.%+%[%]%(%)%^%%%?%*]','%%%0'):gsub('%$%%(%S)','$%1'))
end

-- Most spaces within patterns can match zero or more spaces.
-- Spaces between alphanumeric characters or underscores or between
-- patterns that can match these characters, however, must match at least
-- one space. Otherwise '$v $v' would match 'abcd' as {'abc', 'd'}.
-- This function replaces continuous spaces within a pattern with either
-- '%s*' or '%s+' according to this rule. The pattern has already
-- been stripped of pattern names by now.
local function compress_spaces(patt)
    return (patt:gsub("()%s+()", function(i1, i2)
        local before = patt:sub(i1 - 2, i1 - 1)
        if before:match('%$[vifadxlu]') or before:match('^[^%$]?[%w_]$') then
            local after = patt:sub(i2, i2 + 1)
            if after:match('%$[vifadxlu]') or after:match('^[%w_]') then
                return '%s+'
            end
        end
        return '%s*'
    end))
end

local pattern_map = {
  v = group(patterns.IDEN),
  i = group(patterns.INTEGER),
  f = group(patterns.FLOAT),
  o = group(patterns.OPTION),
  r = '(%S.*)',
  p = '([%a]?[:]?[\\/%.%w_]+)'
}

function sip.custom_pattern(flag,patt)
    pattern_map[flag] = patt
end

--- convert a SIP pattern into the equivalent Lua string pattern.
-- @param spec a SIP pattern
-- @param options a table; only the <code>at_start</code> field is
-- currently meaningful and ensures that the pattern is anchored
-- at the start of the string.
-- @return a Lua string pattern.
function sip.create_pattern (spec,options)
    assert_arg(1,spec,'string')
    local fieldnames,fieldtypes = {},{}

    if type(spec) == 'string' then
        spec = escape(spec)
    else
        local res = {}
        for i,s in ipairs(spec) do
            res[i] = escape(s)
        end
        spec = concat(res,'.-')
    end

    local kount = 1

    local function addfield (name,type)
        name = name or kount
        append(fieldnames,name)
        fieldtypes[name] = type
        kount = kount + 1
    end

    local named_vars = spec:find('{%a+}')

    if options and options.at_start then
        spec = '^'..spec
    end
    if spec:sub(-1,-1) == '$' then
        spec = spec:sub(1,-2)..'$r'
        if named_vars then spec = spec..'{rest}' end
    end

    local names

    if named_vars then
        names = {}
        spec = spec:gsub('{(%a+)}',function(name)
            append(names,name)
            return ''
        end)
    end
    spec = compress_spaces(spec)

    local k = 1
    local err
    local r = (spec:gsub('%$%S',function(s)
        local type,name
        type = s:sub(2,2)
        if names then name = names[k]; k=k+1 end
        -- this kludge is necessary because %q generates two matches, and
        -- we want to ignore the first. Not a problem for named captures.
        if not names and type == 'q' then
            addfield(nil,'Q')
        else
            addfield(name,type)
        end
        local res
        if pattern_map[type] then
            res = pattern_map[type]
        elseif type == 'q' then
            -- some Lua pattern matching voodoo; we want to match '...' as
            -- well as "...", and can use the fact that %n will match a
            -- previous capture. Adding the extra field above comes from needing
            -- to accommodate the extra spurious match (which is either ' or ")
            addfield(name,type)
            res = '(["\'])(.-)%'..(kount-2)
        else
            local endbracket = brackets[type]
            if endbracket then
                res = '(%b'..type..endbracket..')'
            elseif stdclasses[type] or stdclasses[type:lower()] then
                res = '(%'..type..'+)'
            else
                err = "unknown format type or character class"
            end
        end
        return res
    end))

    if err then
        return nil,err
    else
        return r,fieldnames,fieldtypes
    end
end


local function tnumber (s)
    return s == 'd' or s == 'i' or s == 'f'
end

function sip.create_spec_fun(spec,options)
    local fieldtypes,fieldnames
    local ls = {}
    spec,fieldnames,fieldtypes = sip.create_pattern(spec,options)
    if not spec then return spec,fieldnames end
    local named_vars = type(fieldnames[1]) == 'string'
    for i = 1,#fieldnames do
        append(ls,'mm'..i)
    end
    ls[1] = ls[1] or "mm1" -- behave correctly if there are no patterns
    local fun = ('return (function(s,res)\n\tlocal %s = s:match(%q)\n'):format(concat(ls,','),spec)
    fun = fun..'\tif not mm1 then return false end\n'
    local k=1
    for i,f in ipairs(fieldnames) do
        if f ~= '_' then
            local var = 'mm'..i
            if tnumber(fieldtypes[f]) then
                var = 'tonumber('..var..')'
            elseif brackets[fieldtypes[f]] then
                var = var..':sub(2,-2)'
            end
            if named_vars then
                fun = ('%s\tres.%s = %s\n'):format(fun,f,var)
            else
                if fieldtypes[f] ~= 'Q' then -- we skip the string-delim capture
                    fun = ('%s\tres[%d] = %s\n'):format(fun,k,var)
                    k = k + 1
                end
            end
        end
    end
    return fun..'\treturn true\nend)\n', named_vars
end

--- convert a SIP pattern into a matching function.
-- The returned function takes two arguments, the line and an empty table.
-- If the line matched the pattern, then this function returns true
-- and the table is filled with field-value pairs.
-- @param spec a SIP pattern
-- @param options optional table; {at_start=true} ensures that the pattern
-- is anchored at the start of the string.
-- @return a function if successful, or nil,error
function sip.compile(spec,options)
    assert_arg(1,spec,'string')
    local fun,names = sip.create_spec_fun(spec,options)
    if not fun then return nil,names end
    if rawget(_G,'_DEBUG') then print(fun) end
    local chunk,err = loadstring(fun,'tmp')
    if err then return nil,err end
    return chunk(),names
end

local cache = {}

--- match a SIP pattern against a string.
-- @param spec a SIP pattern
-- @param line a string
-- @param res a table to receive values
-- @param options (optional) option table
-- @return true or false
function sip.match (spec,line,res,options)
    assert_arg(1,spec,'string')
    assert_arg(2,line,'string')
    assert_arg(3,res,'table')
    if not cache[spec] then
        cache[spec] = sip.compile(spec,options)
    end
    return cache[spec](line,res)
end

--- match a SIP pattern against the start of a string.
-- @param spec a SIP pattern
-- @param line a string
-- @param res a table to receive values
-- @return true or false
function sip.match_at_start (spec,line,res)
    return sip.match(spec,line,res,{at_start=true})
end

--- given a pattern and a file object, return an iterator over the results
-- @param spec a SIP pattern
-- @param f a file-like object.
function sip.fields (spec,f)
    assert_arg(1,spec,'string')
    if not f then return nil,"no file object" end
    local fun,err = sip.compile(spec)
    if not fun then return nil,err end
    local res = {}
    return function()
        while true do
            local line = f:read()
            if not line then return end
            if fun(line,res) then
                local values = res
                res = {}
                return unpack(values)
            end
        end
    end
end

local read_patterns = {}

--- register a match which will be used in the read function.
-- @string spec a SIP pattern
-- @func fun a function to be called with the results of the match
-- @see read
function sip.pattern (spec,fun)
    assert_arg(1,spec,'string')
    local pat,named = sip.compile(spec)
    append(read_patterns,{pat=pat,named=named,callback=fun})
end

--- enter a loop which applies all registered matches to the input file.
-- @param f a file-like object
-- @array matches optional list of `{spec,fun}` pairs, as for `pattern` above.
function sip.read (f,matches)
    local owned,err
    if not f then return nil,"no file object" end
    if type(f) == 'string' then
        f,err = io.open(f)
        if not f then return nil,err end
        owned = true
    end
    if matches then
        for _,p in ipairs(matches) do
            sip.pattern(p[1],p[2])
        end
    end
    local res = {}
    for line in f:lines() do
        for _,item in ipairs(read_patterns) do
            if item.pat(line,res) then
                if item.callback then
                    if item.named then
                        item.callback(res)
                    else
                        item.callback(unpack(res))
                    end
                end
                res = {}
                break
            end
        end
    end
    if owned then f:close() end
end

return sip

end,

["pl.strict"] = function()
--------------------
-- Module: 'pl.strict'
--------------------
--- Checks uses of undeclared global variables.
-- All global variables must be 'declared' through a regular assignment
-- (even assigning `nil` will do) in a main chunk before being used
-- anywhere or assigned to inside a function. Existing metatables `__newindex` and `__index`
-- metamethods are respected.
--
-- You can set any table to have strict behaviour using `strict.module`. Creating a new
-- module with `strict.closed_module` makes the module immune to monkey-patching, if
-- you don't wish to encourage monkey business.
--
-- If the global `PENLIGHT_NO_GLOBAL_STRICT` is defined, then this module won't make the
-- global environment strict - if you just want to explicitly set table strictness.
--
-- @module pl.strict

require 'debug' -- for Lua 5.2
local getinfo, error, rawset, rawget = debug.getinfo, error, rawset, rawget
local strict = {}

local function what ()
    local d = getinfo(3, "S")
    return d and d.what or "C"
end

--- make an existing table strict.
-- @string[opt] name name of table
-- @tab[opt] mod the table to protect - if `nil` then we'll return a new table
-- @tab[opt] predeclared - table of variables that are to be considered predeclared.
-- @return the given table, or a new table
-- @usage
-- local M = { hello = "world" }
-- strict.module ("Awesome_Module", M, {
--   Lua = true,  -- defines allowed keys
-- })
--
-- assert(M.hello == "world")
-- assert(M.Lua == nil)       -- access allowed, but has no value yet
-- M.Lua = "Rocks"
-- assert(M.Lua == "Rocks")
-- M.not_allowed = "bad boy"  -- throws an error
function strict.module (name,mod,predeclared)
    local mt, old_newindex, old_index, old_index_type, global
    if predeclared then
        global = predeclared.__global
    end
    if type(mod) == 'table' then
        mt = getmetatable(mod)
        if mt and rawget(mt,'__declared') then return end -- already patched...
    else
        mod = {}
    end
    if mt == nil then
        mt = {}
        setmetatable(mod, mt)
    else
        old_newindex = mt.__newindex
        old_index = mt.__index
        old_index_type = type(old_index)
    end
    mt.__declared = predeclared or {}
    mt.__newindex = function(t, n, v)
        if old_newindex then
            old_newindex(t, n, v)
            if rawget(t,n)~=nil then return end
        end
        if not mt.__declared[n] then
            if global then
                local w = what()
                if w ~= "main" and w ~= "C" then
                    error("assign to undeclared global '"..n.."'", 2)
                end
            end
            mt.__declared[n] = true
        end
        rawset(t, n, v)
    end
    mt.__index = function(t,n)
        if not mt.__declared[n] and what() ~= "C" then
            if old_index then
                if old_index_type == "table" then
                    local fallback = old_index[n]
                    if fallback ~= nil then
                        return fallback
                    end
                else
                    local res = old_index(t, n)
                    if res ~= nil then
                        return res
                    end
                end
            end
            local msg = "variable '"..n.."' is not declared"
            if name then
                msg = msg .. " in '"..tostring(name).."'"
            end
            error(msg, 2)
        end
        return rawget(t, n)
    end
    return mod
end

--- make all tables in a table strict.
-- So `strict.make_all_strict(_G)` prevents monkey-patching
-- of any global table
-- @tab T the table containing the tables to protect. Table `T` itself will NOT be protected.
function strict.make_all_strict (T)
    for k,v in pairs(T) do
        if type(v) == 'table' and v ~= T then
            strict.module(k,v)
        end
    end
end

--- make a new module table which is closed to further changes.
-- @tab mod module table
-- @string name module name
function strict.closed_module (mod,name)
    -- No clue to what this is useful for? see tests
    -- Deprecate this and remove???
    local M = {}
    mod = mod or {}
    local mt = getmetatable(mod)
    if not mt then
        mt = {}
        setmetatable(mod,mt)
    end
    mt.__newindex = function(t,k,v)
        M[k] = v
    end
    return strict.module(name,M)
end

if not rawget(_G,'PENLIGHT_NO_GLOBAL_STRICT') then
    strict.module(nil,_G,{_PROMPT=true,_PROMPT2=true,__global=true})
end

return strict

end,

["pl.stringio"] = function()
--------------------
-- Module: 'pl.stringio'
--------------------
--- Reading and writing strings using file-like objects. <br>
--
--    f = stringio.open(text)
--    l1 = f:read()  -- read first line
--    n,m = f:read ('*n','*n') -- read two numbers
--    for line in f:lines() do print(line) end -- iterate over all lines
--    f = stringio.create()
--    f:write('hello')
--    f:write('dolly')
--    assert(f:value(),'hellodolly')
--
-- See  @{03-strings.md.File_style_I_O_on_Strings|the Guide}.
-- @module pl.stringio

local unpack = rawget(_G,'unpack') or rawget(table,'unpack')
local tonumber = tonumber
local concat,append = table.concat,table.insert

local stringio = {}

-- Writer class
local SW = {}
SW.__index = SW

local function xwrite(self,...)
    local args = {...} --arguments may not be nil!
    for i = 1, #args do
        append(self.tbl,args[i])
    end
end

function SW:write(arg1,arg2,...)
    if arg2 then
        xwrite(self,arg1,arg2,...)
    else
        append(self.tbl,arg1)
    end
end

function SW:writef(fmt,...)
    self:write(fmt:format(...))
end

function SW:value()
    return concat(self.tbl)
end

function SW:__tostring()
    return self:value()
end

function SW:close() -- for compatibility only
end

function SW:seek()
end

-- Reader class
local SR = {}
SR.__index = SR

function SR:_read(fmt)
    local i,str = self.i,self.str
    local sz = #str
    if i > sz then return nil end
    local res
    if fmt == '*l' or fmt == '*L' then
        local idx = str:find('\n',i) or (sz+1)
        res = str:sub(i,fmt == '*l' and idx-1 or idx)
        self.i = idx+1
    elseif fmt == '*a' then
        res = str:sub(i)
        self.i = sz
    elseif fmt == '*n' then
        local _,i2,idx
        _,idx = str:find ('%s*%d+',i)
        _,i2 = str:find ('^%.%d+',idx+1)
        if i2 then idx = i2 end
        _,i2 = str:find ('^[eE][%+%-]*%d+',idx+1)
        if i2 then idx = i2 end
        local val = str:sub(i,idx)
        res = tonumber(val)
        self.i = idx+1
    elseif type(fmt) == 'number' then
        res = str:sub(i,i+fmt-1)
        self.i = i + fmt
    else
        error("bad read format",2)
    end
    return res
end

function SR:read(...)
    if select('#',...) == 0 then
        return self:_read('*l')
    else
        local res, fmts = {},{...}
        for i = 1, #fmts do
            res[i] = self:_read(fmts[i])
        end
        return unpack(res)
    end
end

function SR:seek(whence,offset)
    local base
    whence = whence or 'cur'
    offset = offset or 0
    if whence == 'set' then
        base = 1
    elseif whence == 'cur' then
        base = self.i
    elseif whence == 'end' then
        base = #self.str
    end
    self.i = base + offset
    return self.i
end

function SR:lines(...)
    local n, args = select('#',...)
    if n > 0 then
        args = {...}
    end
    return function()
        if n == 0 then
            return self:_read '*l'
        else
            return self:read(unpack(args))
        end
    end
end

function SR:close() -- for compatibility only
end

--- create a file-like object which can be used to construct a string.
-- The resulting object has an extra `value()` method for
-- retrieving the string value.  Implements `file:write`, `file:seek`, `file:lines`,
-- plus an extra `writef` method which works like `utils.printf`.
-- @usage f = create(); f:write('hello, dolly\n'); print(f:value())
function stringio.create()
    return setmetatable({tbl={}},SW)
end

--- create a file-like object for reading from a given string.
-- Implements `file:read`.
-- @string s The input string.
-- @usage fs = open '20 10'; x,y = f:read ('*n','*n'); assert(x == 20 and y == 10)
function stringio.open(s)
    return setmetatable({str=s,i=1},SR)
end

function stringio.lines(s,...)
    return stringio.open(s):lines(...)
end

return stringio

end,

["pl.stringx"] = function()
--------------------
-- Module: 'pl.stringx'
--------------------
--- Python-style extended string library.
--
-- see 3.6.1 of the Python reference.
-- If you want to make these available as string methods, then say
-- `stringx.import()` to bring them into the standard `string` table.
--
-- See @{03-strings.md|the Guide}
--
-- Dependencies: `pl.utils`, `pl.types`
-- @module pl.stringx
local utils = require 'pl.utils'
local is_callable = require 'pl.types'.is_callable
local string = string
local find = string.find
local type,setmetatable,ipairs = type,setmetatable,ipairs
local error = error
local gsub = string.gsub
local rep = string.rep
local sub = string.sub
local reverse = string.reverse
local concat = table.concat
local append = table.insert
local remove = table.remove
local escape = utils.escape
local ceil, max = math.ceil, math.max
local assert_arg,usplit = utils.assert_arg,utils.split
local lstrip
local unpack = utils.unpack
local pack = utils.pack

local function assert_string (n,s)
    assert_arg(n,s,'string')
end

local function non_empty(s)
    return #s > 0
end

local function assert_nonempty_string(n,s)
    assert_arg(n,s,'string',non_empty,'must be a non-empty string')
end

local function makelist(l)
    return setmetatable(l, require('pl.List'))
end

local stringx = {}

------------------
-- String Predicates
-- @section predicates

--- does s only contain alphabetic characters?
-- @string s a string
function stringx.isalpha(s)
    assert_string(1,s)
    return find(s,'^%a+$') == 1
end

--- does s only contain digits?
-- @string s a string
function stringx.isdigit(s)
    assert_string(1,s)
    return find(s,'^%d+$') == 1
end

--- does s only contain alphanumeric characters?
-- @string s a string
function stringx.isalnum(s)
    assert_string(1,s)
    return find(s,'^%w+$') == 1
end

--- does s only contain whitespace?
-- Matches on pattern '%s' so matches space, newline, tabs, etc.
-- @string s a string
function stringx.isspace(s)
    assert_string(1,s)
    return find(s,'^%s+$') == 1
end

--- does s only contain lower case characters?
-- @string s a string
function stringx.islower(s)
    assert_string(1,s)
    return find(s,'^[%l%s]+$') == 1
end

--- does s only contain upper case characters?
-- @string s a string
function stringx.isupper(s)
    assert_string(1,s)
    return find(s,'^[%u%s]+$') == 1
end

local function raw_startswith(s, prefix)
    return find(s,prefix,1,true) == 1
end

local function raw_endswith(s, suffix)
    return #s >= #suffix and find(s, suffix, #s-#suffix+1, true) and true or false
end

local function test_affixes(s, affixes, fn)
    if type(affixes) == 'string' then
        return fn(s,affixes)
    elseif type(affixes) == 'table' then
        for _,affix in ipairs(affixes) do
            if fn(s,affix) then return true end
        end
        return false
    else
        error(("argument #2 expected a 'string' or a 'table', got a '%s'"):format(type(affixes)))
    end
end

--- does s start with prefix or one of prefixes?
-- @string s a string
-- @param prefix a string or an array of strings
function stringx.startswith(s,prefix)
    assert_string(1,s)
    return test_affixes(s,prefix,raw_startswith)
end

--- does s end with suffix or one of suffixes?
-- @string s a string
-- @param suffix a string or an array of strings
function stringx.endswith(s,suffix)
    assert_string(1,s)
    return test_affixes(s,suffix,raw_endswith)
end

--- Strings and Lists
-- @section lists

--- concatenate the strings using this string as a delimiter.
-- Note that the arguments are reversed from `string.concat`.
-- @string s the string
-- @param seq a table of strings or numbers
-- @usage stringx.join(' ', {1,2,3}) == '1 2 3'
function stringx.join(s,seq)
    assert_string(1,s)
    return concat(seq,s)
end

--- Split a string into a list of lines.
-- `"\r"`, `"\n"`, and `"\r\n"` are considered line ends.
-- They are not included in the lines unless `keepends` is passed.
-- Terminal line end does not produce an extra line.
-- Splitting an empty string results in an empty list.
-- @string s the string.
-- @bool[opt] keep_ends include line ends.
-- @return List of lines
function stringx.splitlines(s, keep_ends)
    assert_string(1, s)
    local res = {}
    local pos = 1
    while true do
        local line_end_pos = find(s, '[\r\n]', pos)
        if not line_end_pos then
            break
        end

        local line_end = sub(s, line_end_pos, line_end_pos)
        if line_end == '\r' and sub(s, line_end_pos + 1, line_end_pos + 1) == '\n' then
            line_end = '\r\n'
        end

        local line = sub(s, pos, line_end_pos - 1)
        if keep_ends then
            line = line .. line_end
        end
        append(res, line)

        pos = line_end_pos + #line_end
    end

    if pos <= #s then
        append(res, sub(s, pos))
    end
    return makelist(res)
end

--- split a string into a list of strings using a delimiter.
-- @function split
-- @string s the string
-- @string[opt] re a delimiter (defaults to whitespace)
-- @int[opt] n maximum number of results
-- @return List
-- @usage #(stringx.split('one two')) == 2
-- @usage stringx.split('one,two,three', ',') == List{'one','two','three'}
-- @usage stringx.split('one,two,three', ',', 2) == List{'one','two,three'}
function stringx.split(s,re,n)
    assert_string(1,s)
    local plain = true
    if not re then -- default spaces
        s = lstrip(s)
        plain = false
    end
    local res = usplit(s,re,plain,n)
    if re and re ~= '' and
       find(s,re,-#re,true) and
       (n or math.huge) > #res then
        res[#res+1] = ""
    end
    return makelist(res)
end

--- replace all tabs in s with tabsize spaces. If not specified, tabsize defaults to 8.
-- Tab stops will be honored.
-- @string s the string
-- @int tabsize[opt=8] number of spaces to expand each tab
-- @return expanded string
-- @usage stringx.expandtabs('\tone,two,three', 4)   == '    one,two,three'
-- @usage stringx.expandtabs('  \tone,two,three', 4) == '    one,two,three'
function stringx.expandtabs(s,tabsize)
  assert_string(1,s)
  tabsize = tabsize or 8
  return (s:gsub("([^\t\r\n]*)\t", function(before_tab)
      if tabsize == 0 then
        return before_tab
      else
        return before_tab .. (" "):rep(tabsize - #before_tab % tabsize)
      end
    end))
end

--- Finding and Replacing
-- @section find

local function _find_all(s,sub,first,last,allow_overlap)
    first = first or 1
    last = last or #s
    if sub == '' then return last+1,last-first+1 end
    local i1,i2 = find(s,sub,first,true)
    local res
    local k = 0
    while i1 do
        if last and i2 > last then break end
        res = i1
        k = k + 1
        if allow_overlap then
            i1,i2 = find(s,sub,i1+1,true)
        else
            i1,i2 = find(s,sub,i2+1,true)
        end
    end
    return res,k
end

--- find index of first instance of sub in s from the left.
-- @string s the string
-- @string sub substring
-- @int[opt] first first index
-- @int[opt] last last index
-- @return start index, or nil if not found
function stringx.lfind(s,sub,first,last)
    assert_string(1,s)
    assert_string(2,sub)
    local i1, i2 = find(s,sub,first,true)

    if i1 and (not last or i2 <= last) then
        return i1
    else
        return nil
    end
end

--- find index of first instance of sub in s from the right.
-- @string s the string
-- @string sub substring
-- @int[opt] first first index
-- @int[opt] last last index
-- @return start index, or nil if not found
function stringx.rfind(s,sub,first,last)
    assert_string(1,s)
    assert_string(2,sub)
    return (_find_all(s,sub,first,last,true))
end

--- replace up to n instances of old by new in the string s.
-- If n is not present, replace all instances.
-- @string s the string
-- @string old the target substring
-- @string new the substitution
-- @int[opt] n optional maximum number of substitutions
-- @return result string
function stringx.replace(s,old,new,n)
    assert_string(1,s)
    assert_string(2,old)
    assert_string(3,new)
    return (gsub(s,escape(old),new:gsub('%%','%%%%'),n))
end

--- count all instances of substring in string.
-- @string s the string
-- @string sub substring
-- @bool[opt] allow_overlap allow matches to overlap
-- @usage
-- assert(stringx.count('banana', 'ana') == 1)
-- assert(stringx.count('banana', 'ana', true) == 2)
function stringx.count(s,sub,allow_overlap)
    assert_string(1,s)
    local _,k = _find_all(s,sub,1,false,allow_overlap)
    return k
end

--- Stripping and Justifying
-- @section strip

local function _just(s,w,ch,left,right)
    local n = #s
    if w > n then
        if not ch then ch = ' ' end
        local f1,f2
        if left and right then
            local rn = ceil((w-n)/2)
            local ln = w - n - rn
            f1 = rep(ch,ln)
            f2 = rep(ch,rn)
        elseif right then
            f1 = rep(ch,w-n)
            f2 = ''
        else
            f2 = rep(ch,w-n)
            f1 = ''
        end
        return f1..s..f2
    else
        return s
    end
end

--- left-justify s with width w.
-- @string s the string
-- @int w width of justification
-- @string[opt=' '] ch padding character
-- @usage stringx.ljust('hello', 10, '*') == '*****hello'
function stringx.ljust(s,w,ch)
    assert_string(1,s)
    assert_arg(2,w,'number')
    return _just(s,w,ch,true,false)
end

--- right-justify s with width w.
-- @string s the string
-- @int w width of justification
-- @string[opt=' '] ch padding character
-- @usage stringx.rjust('hello', 10, '*') == 'hello*****'
function stringx.rjust(s,w,ch)
    assert_string(1,s)
    assert_arg(2,w,'number')
    return _just(s,w,ch,false,true)
end

--- center-justify s with width w.
-- @string s the string
-- @int w width of justification
-- @string[opt=' '] ch padding character
-- @usage stringx.center('hello', 10, '*') == '**hello***'
function stringx.center(s,w,ch)
    assert_string(1,s)
    assert_arg(2,w,'number')
    return _just(s,w,ch,true,true)
end

local function _strip(s,left,right,chrs)
    if not chrs then
        chrs = '%s'
    else
        chrs = '['..escape(chrs)..']'
    end
    local f = 1
    local t
    if left then
        local i1,i2 = find(s,'^'..chrs..'*')
        if i2 >= i1 then
            f = i2+1
        end
    end
    if right then
        if #s < 200 then
            local i1,i2 = find(s,chrs..'*$',f)
            if i2 >= i1 then
                t = i1-1
            end
        else
            local rs = reverse(s)
            local i1,i2 = find(rs, '^'..chrs..'*')
            if i2 >= i1 then
                t = -i2-1
            end
        end
    end
    return sub(s,f,t)
end

--- trim any characters on the left of s.
-- @string s the string
-- @string[opt='%s'] chrs default any whitespace character,
-- but can be a string of characters to be trimmed
function stringx.lstrip(s,chrs)
    assert_string(1,s)
    return _strip(s,true,false,chrs)
end
lstrip = stringx.lstrip

--- trim any characters on the right of s.
-- @string s the string
-- @string[opt='%s'] chrs default any whitespace character,
-- but can be a string of characters to be trimmed
function stringx.rstrip(s,chrs)
    assert_string(1,s)
    return _strip(s,false,true,chrs)
end

--- trim any characters on both left and right of s.
-- @string s the string
-- @string[opt='%s'] chrs default any whitespace character,
-- but can be a string of characters to be trimmed
-- @usage stringx.strip('  --== Hello ==--  ', "- =")  --> 'Hello'
function stringx.strip(s,chrs)
    assert_string(1,s)
    return _strip(s,true,true,chrs)
end

--- Partitioning Strings
-- @section partitioning

--- split a string using a pattern. Note that at least one value will be returned!
-- @string s the string
-- @string[opt='%s'] re a Lua string pattern (defaults to whitespace)
-- @return the parts of the string
-- @usage  a,b = line:splitv('=')
-- @see utils.splitv
function stringx.splitv(s,re)
    assert_string(1,s)
    return utils.splitv(s,re)
end

-- The partition functions split a string using a delimiter into three parts:
-- the part before, the delimiter itself, and the part afterwards
local function _partition(p,delim,fn)
    local i1,i2 = fn(p,delim)
    if not i1 or i1 == -1 then
        return p,'',''
    else
        if not i2 then i2 = i1 end
        return sub(p,1,i1-1),sub(p,i1,i2),sub(p,i2+1)
    end
end

--- partition the string using first occurrence of a delimiter
-- @string s the string
-- @string ch delimiter (match as plain string, no patterns)
-- @return part before ch
-- @return ch
-- @return part after ch
-- @usage {stringx.partition('a,b,c', ','))} == {'a', ',', 'b,c'}
-- @usage {stringx.partition('abc', 'x'))} == {'abc', '', ''}
function stringx.partition(s,ch)
    assert_string(1,s)
    assert_nonempty_string(2,ch)
    return _partition(s,ch,stringx.lfind)
end

--- partition the string p using last occurrence of a delimiter
-- @string s the string
-- @string ch delimiter (match as plain string, no patterns)
-- @return part before ch
-- @return ch
-- @return part after ch
-- @usage {stringx.rpartition('a,b,c', ','))} == {'a,b', ',', 'c'}
-- @usage {stringx.rpartition('abc', 'x'))} == {'', '', 'abc'}
function stringx.rpartition(s,ch)
    assert_string(1,s)
    assert_nonempty_string(2,ch)
    local a,b,c = _partition(s,ch,stringx.rfind)
    if a == s then -- no match found
        return c,b,a
    end
    return a,b,c
end

--- return the 'character' at the index.
-- @string s the string
-- @int idx an index (can be negative)
-- @return a substring of length 1 if successful, empty string otherwise.
function stringx.at(s,idx)
    assert_string(1,s)
    assert_arg(2,idx,'number')
    return sub(s,idx,idx)
end


--- Text handling
-- @section text


--- indent a multiline string.
-- @tparam string s the (multiline) string
-- @tparam integer n the size of the indent
-- @tparam[opt=' '] string ch the character to use when indenting
-- @return indented string
function stringx.indent (s,n,ch)
  assert_arg(1,s,'string')
  assert_arg(2,n,'number')
  local lines = usplit(s ,'\n')
  local prefix = string.rep(ch or ' ',n)
  for i, line in ipairs(lines) do
    lines[i] = prefix..line
  end
  return concat(lines,'\n')..'\n'
end


--- dedent a multiline string by removing any initial indent.
-- useful when working with [[..]] strings.
-- Empty lines are ignored.
-- @tparam string s the (multiline) string
-- @return a string with initial indent zero.
-- @usage
-- local s = dedent [[
--          One
--
--        Two
--
--      Three
-- ]]
-- assert(s == [[
--     One
--
--   Two
--
-- Three
-- ]])
function stringx.dedent (s)
  assert_arg(1,s,'string')
  local lst = usplit(s,'\n')
  if #lst>0 then
    local ind_size = math.huge
    for i, line in ipairs(lst) do
      local i1, i2 = lst[i]:find('^%s*[^%s]')
      if i1 and i2 < ind_size then
        ind_size = i2
      end
    end
    for i, line in ipairs(lst) do
      lst[i] = lst[i]:sub(ind_size, -1)
    end
  end
  return concat(lst,'\n')..'\n'
end



do
  local buildline = function(words, size, breaklong)
    -- if overflow is set, a word longer than size, will overflow the size
    -- otherwise it will be chopped in line-length pieces
    local line = {}
    if #words[1] > size then
      -- word longer than line
      if not breaklong then
        line[1] = words[1]
        remove(words, 1)
      else
        line[1] = words[1]:sub(1, size)
        words[1] = words[1]:sub(size + 1, -1)
      end
    else
      local len = 0
      while words[1] and (len + #words[1] <= size) or
            (len == 0 and #words[1] == size) do
        if words[1] ~= "" then
          line[#line+1] = words[1]
          len = len + #words[1] + 1
        end
        remove(words, 1)
      end
    end
    return stringx.strip(concat(line, " ")), words
  end

  --- format a paragraph into lines so that they fit into a line width.
  -- It will not break long words by default, so lines can be over the length
  -- to that extent.
  -- @tparam string s the string to format
  -- @tparam[opt=70] integer width the margin width
  -- @tparam[opt=false] boolean breaklong if truthy, words longer than the width given will be forced split.
  -- @return a list of lines (List object), use `fill` to return a string instead of a `List`.
  -- @see pl.List
  -- @see fill
  stringx.wrap = function(s, width, breaklong)
    s = s:gsub('\n',' ') -- remove line breaks
    s = stringx.strip(s) -- remove leading/trailing whitespace
    if s == "" then
      return { "" }
    end
    width = width or 70
    local out = {}
    local words = usplit(s, "%s")
    while words[1] do
      out[#out+1], words = buildline(words, width, breaklong)
    end
    return makelist(out)
  end
end

--- format a paragraph so that it fits into a line width.
-- @tparam string s the string to format
-- @tparam[opt=70] integer width the margin width
-- @tparam[opt=false] boolean breaklong if truthy, words longer than the width given will be forced split.
-- @return a string, use `wrap` to return a list of lines instead of a string.
-- @see wrap
function stringx.fill (s,width,breaklong)
  return concat(stringx.wrap(s,width,breaklong),'\n') .. '\n'
end

--- Template
-- @section Template


local function _substitute(s,tbl,safe)
  local subst
  if is_callable(tbl) then
    subst = tbl
  else
    function subst(f)
      local s = tbl[f]
      if not s then
        if safe then
          return f
        else
          error("not present in table "..f)
        end
      else
        return s
      end
    end
  end
  local res = gsub(s,'%${([%w_]+)}',subst)
  return (gsub(res,'%$([%w_]+)',subst))
end



local Template = {}
stringx.Template = Template
Template.__index = Template
setmetatable(Template, {
  __call = function(obj,tmpl)
    return Template.new(tmpl)
  end
})

--- Creates a new Template class.
-- This is a shortcut to `Template.new(tmpl)`.
-- @tparam string tmpl the template string
-- @function Template
-- @treturn Template
function Template.new(tmpl)
  assert_arg(1,tmpl,'string')
  local res = {}
  res.tmpl = tmpl
  setmetatable(res,Template)
  return res
end

--- substitute values into a template, throwing an error.
-- This will throw an error if no name is found.
-- @tparam table tbl a table of name-value pairs.
-- @return string with place holders substituted
function Template:substitute(tbl)
  assert_arg(1,tbl,'table')
  return _substitute(self.tmpl,tbl,false)
end

--- substitute values into a template.
-- This version just passes unknown names through.
-- @tparam table tbl a table of name-value pairs.
-- @return string with place holders substituted
function Template:safe_substitute(tbl)
  assert_arg(1,tbl,'table')
  return _substitute(self.tmpl,tbl,true)
end

--- substitute values into a template, preserving indentation. <br>
-- If the value is a multiline string _or_ a template, it will insert
-- the lines at the correct indentation. <br>
-- Furthermore, if a template, then that template will be substituted
-- using the same table.
-- @tparam table tbl a table of name-value pairs.
-- @return string with place holders substituted
function Template:indent_substitute(tbl)
  assert_arg(1,tbl,'table')
  if not self.strings then
    self.strings = usplit(self.tmpl,'\n')
  end

  -- the idea is to substitute line by line, grabbing any spaces as
  -- well as the $var. If the value to be substituted contains newlines,
  -- then we split that into lines and adjust the indent before inserting.
  local function subst(line)
    return line:gsub('(%s*)%$([%w_]+)',function(sp,f)
      local subtmpl
      local s = tbl[f]
      if not s then error("not present in table "..f) end
      if getmetatable(s) == Template then
        subtmpl = s
        s = s.tmpl
      else
        s = tostring(s)
      end
      if s:find '\n' then
        local lines = usplit(s, '\n')
        for i, line in ipairs(lines) do
          lines[i] = sp..line
        end
        s = concat(lines, '\n') .. '\n'
      end
      if subtmpl then
        return _substitute(s, tbl)
      else
        return s
      end
    end)
  end

  local lines = {}
  for i, line in ipairs(self.strings) do
    lines[i] = subst(line)
  end
  return concat(lines,'\n')..'\n'
end



--- Miscellaneous
-- @section misc

--- return an iterator over all lines in a string
-- @string s the string
-- @return an iterator
-- @usage
-- local line_no = 1
-- for line in stringx.lines(some_text) do
--   print(line_no, line)
--   line_no = line_no + 1
-- end
function stringx.lines(s)
    assert_string(1,s)
    if not s:find '\n$' then s = s..'\n' end
    return s:gmatch('([^\n]*)\n')
end

--- initial word letters uppercase ('title case').
-- Here 'words' mean chunks of non-space characters.
-- @string s the string
-- @return a string with each word's first letter uppercase
-- @usage stringx.title("hello world") == "Hello World")
function stringx.title(s)
    assert_string(1,s)
    return (s:gsub('(%S)(%S*)',function(f,r)
        return f:upper()..r:lower()
    end))
end

stringx.capitalize = stringx.title

do
  local ellipsis = '...'
  local n_ellipsis = #ellipsis

  --- Return a shortened version of a string.
  -- Fits string within w characters. Removed characters are marked with ellipsis.
  -- @string s the string
  -- @int w the maximum size allowed
  -- @bool tail true if we want to show the end of the string (head otherwise)
  -- @usage ('1234567890'):shorten(8) == '12345...'
  -- @usage ('1234567890'):shorten(8, true) == '...67890'
  -- @usage ('1234567890'):shorten(20) == '1234567890'
  function stringx.shorten(s,w,tail)
      assert_string(1,s)
      if #s > w then
          if w < n_ellipsis then return ellipsis:sub(1,w) end
          if tail then
              local i = #s - w + 1 + n_ellipsis
              return ellipsis .. s:sub(i)
          else
              return s:sub(1,w-n_ellipsis) .. ellipsis
          end
      end
      return s
  end
end


do
  -- Utility function that finds any patterns that match a long string's an open or close.
  -- Note that having this function use the least number of equal signs that is possible is a harder algorithm to come up with.
  -- Right now, it simply returns the greatest number of them found.
  -- @param s The string
  -- @return 'nil' if not found. If found, the maximum number of equal signs found within all matches.
  local function has_lquote(s)
      local lstring_pat = '([%[%]])(=*)%1'
      local equals, new_equals, _
      local finish = 1
      repeat
          _, finish, _, new_equals = s:find(lstring_pat, finish)
          if new_equals then
              equals = max(equals or 0, #new_equals)
          end
      until not new_equals

      return equals
  end

  --- Quote the given string and preserve any control or escape characters, such that reloading the string in Lua returns the same result.
  -- @param s The string to be quoted.
  -- @return The quoted string.
  function stringx.quote_string(s)
      assert_string(1,s)
      -- Find out if there are any embedded long-quote sequences that may cause issues.
      -- This is important when strings are embedded within strings, like when serializing.
      -- Append a closing bracket to catch unfinished long-quote sequences at the end of the string.
      local equal_signs = has_lquote(s .. "]")

      -- Note that strings containing "\r" can't be quoted using long brackets
      -- as Lua lexer converts all newlines to "\n" within long strings.
      if (s:find("\n") or equal_signs) and not s:find("\r") then
          -- If there is an embedded sequence that matches a long quote, then
          -- find the one with the maximum number of = signs and add one to that number.
          equal_signs = ("="):rep((equal_signs or -1) + 1)
          -- Long strings strip out leading newline. We want to retain that, when quoting.
          if s:find("^\n") then s = "\n" .. s end
          local lbracket, rbracket =
              "[" .. equal_signs .. "[",
              "]" .. equal_signs .. "]"
          s = lbracket .. s .. rbracket
      else
          -- Escape funny stuff. Lua 5.1 does not handle "\r" correctly.
          s = ("%q"):format(s):gsub("\r", "\\r")
      end
      return s
  end
end


--- Python-style formatting operator.
-- Calling `text.format_operator()` overloads the % operator for strings to give
-- Python/Ruby style formatted output.
-- This is extended to also do template-like substitution for map-like data.
--
-- Note this goes further than the original, and will allow these cases:
--
-- 1. a single value
-- 2. a list of values
-- 3. a map of var=value pairs
-- 4. a function, as in gsub
--
-- For the second two cases, it uses $-variable substitution.
--
-- When called, this function will monkey-patch the global `string` metatable by
-- adding a `__mod` method.
--
-- See <a href="http://lua-users.org/wiki/StringInterpolation">the lua-users wiki</a>
--
-- @usage
-- require 'pl.text'.format_operator()
-- local out1 = '%s = %5.3f' % {'PI',math.pi}                   --> 'PI = 3.142'
-- local out2 = '$name = $value' % {name='dog',value='Pluto'}   --> 'dog = Pluto'
function stringx.format_operator()

  local format = string.format

  -- a more forgiving version of string.format, which applies
  -- tostring() to any value with a %s format.
  local function formatx (fmt,...)
    local args = pack(...)
    local i = 1
    for p in fmt:gmatch('%%.') do
      if p == '%s' and type(args[i]) ~= 'string' then
        args[i] = tostring(args[i])
      end
      i = i + 1
    end
    return format(fmt,unpack(args))
  end

  local function basic_subst(s,t)
    return (s:gsub('%$([%w_]+)',t))
  end

  getmetatable("").__mod = function(a, b)
    if b == nil then
      return a
    elseif type(b) == "table" and getmetatable(b) == nil then
      if #b == 0 then -- assume a map-like table
        return _substitute(a,b,true)
      else
        return formatx(a,unpack(b))
      end
    elseif type(b) == 'function' then
      return basic_subst(a,b)
    else
      return formatx(a,b)
    end
  end
end

--- import the stringx functions into the global string (meta)table
function stringx.import()
    utils.import(stringx,string)
end

return stringx

end,

["pl.tablex"] = function()
--------------------
-- Module: 'pl.tablex'
--------------------
--- Extended operations on Lua tables.
--
-- See @{02-arrays.md.Useful_Operations_on_Tables|the Guide}
--
-- Dependencies: `pl.utils`, `pl.types`
-- @module pl.tablex
local utils = require ('pl.utils')
local types = require ('pl.types')
local getmetatable,setmetatable,require = getmetatable,setmetatable,require
local tsort,append,remove = table.sort,table.insert,table.remove
local min = math.min
local pairs,type,unpack,select,tostring = pairs,type,utils.unpack,select,tostring
local function_arg = utils.function_arg
local assert_arg = utils.assert_arg

local tablex = {}

-- generally, functions that make copies of tables try to preserve the metatable.
-- However, when the source has no obvious type, then we attach appropriate metatables
-- like List, Map, etc to the result.
local function setmeta (res,tbl,pl_class)
    local mt = getmetatable(tbl) or pl_class and require('pl.' .. pl_class)
    return mt and setmetatable(res, mt) or res
end

local function makelist(l)
    return setmetatable(l, require('pl.List'))
end

local function makemap(m)
    return setmetatable(m, require('pl.Map'))
end

local function complain (idx,msg)
    error(('argument %d is not %s'):format(idx,msg),3)
end

local function assert_arg_indexable (idx,val)
    if not types.is_indexable(val) then
        complain(idx,"indexable")
    end
end

local function assert_arg_iterable (idx,val)
    if not types.is_iterable(val) then
        complain(idx,"iterable")
    end
end

local function assert_arg_writeable (idx,val)
    if not types.is_writeable(val) then
        complain(idx,"writeable")
    end
end

--- copy a table into another, in-place.
-- @within Copying
-- @tab t1 destination table
-- @tab t2 source (actually any iterable object)
-- @return first table
function tablex.update (t1,t2)
    assert_arg_writeable(1,t1)
    assert_arg_iterable(2,t2)
    for k,v in pairs(t2) do
        t1[k] = v
    end
    return t1
end

--- total number of elements in this table.
-- Note that this is distinct from `#t`, which is the number
-- of values in the array part; this value will always
-- be greater or equal. The difference gives the size of
-- the hash part, for practical purposes. Works for any
-- object with a __pairs metamethod.
-- @tab t a table
-- @return the size
function tablex.size (t)
    assert_arg_iterable(1,t)
    local i = 0
    for k in pairs(t) do i = i + 1 end
    return i
end

--- make a shallow copy of a table
-- @within Copying
-- @tab t an iterable source
-- @return new table
function tablex.copy (t)
    assert_arg_iterable(1,t)
    local res = {}
    for k,v in pairs(t) do
        res[k] = v
    end
    return res
end

local function cycle_aware_copy(t, cache)
    if type(t) ~= 'table' then return t end
    if cache[t] then return cache[t] end
    assert_arg_iterable(1,t)
    local res = {}
    cache[t] = res
    local mt = getmetatable(t)
    for k,v in pairs(t) do
        k = cycle_aware_copy(k, cache)
        v = cycle_aware_copy(v, cache)
        res[k] = v
    end
    setmetatable(res,mt)
    return res
end

--- make a deep copy of a table, recursively copying all the keys and fields.
-- This supports cycles in tables; cycles will be reproduced in the copy.
-- This will also set the copied table's metatable to that of the original.
-- @within Copying
-- @tab t A table
-- @return new table
function tablex.deepcopy(t)
    return cycle_aware_copy(t,{})
end

local abs = math.abs

local function cycle_aware_compare(t1,t2,ignore_mt,eps,cache)
    if cache[t1] and cache[t1][t2] then return true end
    local ty1 = type(t1)
    local ty2 = type(t2)
    if ty1 ~= ty2 then return false end
    -- non-table types can be directly compared
    if ty1 ~= 'table' then
        if ty1 == 'number' and eps then return abs(t1-t2) < eps end
        return t1 == t2
    end
    -- as well as tables which have the metamethod __eq
    local mt = getmetatable(t1)
    if not ignore_mt and mt and mt.__eq then return t1 == t2 end
    for k1 in pairs(t1) do
        if t2[k1]==nil then return false end
    end
    for k2 in pairs(t2) do
        if t1[k2]==nil then return false end
    end
    cache[t1] = cache[t1] or {}
    cache[t1][t2] = true
    for k1,v1 in pairs(t1) do
        local v2 = t2[k1]
        if not cycle_aware_compare(v1,v2,ignore_mt,eps,cache) then return false end
    end
    return true
end

--- compare two values.
-- if they are tables, then compare their keys and fields recursively.
-- @within Comparing
-- @param t1 A value
-- @param t2 A value
-- @bool[opt] ignore_mt if true, ignore __eq metamethod (default false)
-- @number[opt] eps if defined, then used for any number comparisons
-- @return true or false
function tablex.deepcompare(t1,t2,ignore_mt,eps)
    return cycle_aware_compare(t1,t2,ignore_mt,eps,{})
end

--- compare two arrays using a predicate.
-- @within Comparing
-- @array t1 an array
-- @array t2 an array
-- @func cmp A comparison function; `bool = cmp(t1_value, t2_value)`
-- @return true or false
-- @usage
-- assert(tablex.compare({ 1, 2, 3 }, { 1, 2, 3 }, "=="))
--
-- assert(tablex.compare(
--    {1,2,3, hello = "world"},  -- fields are not compared!
--    {1,2,3}, function(v1, v2) return v1 == v2 end)
function tablex.compare (t1,t2,cmp)
    assert_arg_indexable(1,t1)
    assert_arg_indexable(2,t2)
    if #t1 ~= #t2 then return false end
    cmp = function_arg(3,cmp)
    for k = 1,#t1 do
        if not cmp(t1[k],t2[k]) then return false end
    end
    return true
end

--- compare two list-like tables using an optional predicate, without regard for element order.
-- @within Comparing
-- @array t1 a list-like table
-- @array t2 a list-like table
-- @param cmp A comparison function (may be nil)
function tablex.compare_no_order (t1,t2,cmp)
    assert_arg_indexable(1,t1)
    assert_arg_indexable(2,t2)
    if cmp then cmp = function_arg(3,cmp) end
    if #t1 ~= #t2 then return false end
    local visited = {}
    for i = 1,#t1 do
        local val = t1[i]
        local gotcha
        for j = 1,#t2 do
            if not visited[j] then
                local match
                if cmp then match = cmp(val,t2[j]) else match = val == t2[j] end
                if match then
                    gotcha = j
                    break
                end
            end
        end
        if not gotcha then return false end
        visited[gotcha] = true
    end
    return true
end


--- return the index of a value in a list.
-- Like string.find, there is an optional index to start searching,
-- which can be negative.
-- @within Finding
-- @array t A list-like table
-- @param val A value
-- @int idx index to start; -1 means last element,etc (default 1)
-- @return index of value or nil if not found
-- @usage find({10,20,30},20) == 2
-- @usage find({'a','b','a','c'},'a',2) == 3
function tablex.find(t,val,idx)
    assert_arg_indexable(1,t)
    idx = idx or 1
    if idx < 0 then idx = #t + idx + 1 end
    for i = idx,#t do
        if t[i] == val then return i end
    end
    return nil
end

--- return the index of a value in a list, searching from the end.
-- Like string.find, there is an optional index to start searching,
-- which can be negative.
-- @within Finding
-- @array t A list-like table
-- @param val A value
-- @param idx index to start; -1 means last element,etc (default `#t`)
-- @return index of value or nil if not found
-- @usage rfind({10,10,10},10) == 3
function tablex.rfind(t,val,idx)
    assert_arg_indexable(1,t)
    idx = idx or #t
    if idx < 0 then idx = #t + idx + 1 end
    for i = idx,1,-1 do
        if t[i] == val then return i end
    end
    return nil
end


--- return the index (or key) of a value in a table using a comparison function.
--
-- *NOTE*: the 2nd return value of this function, the value returned
-- by the comparison function, has a limitation that it cannot be `false`.
-- Because if it is, then it indicates the comparison failed, and the
-- function will continue the search. See examples.
-- @within Finding
-- @tab t A table
-- @func cmp A comparison function
-- @param arg an optional second argument to the function
-- @return index of value, or nil if not found
-- @return value returned by comparison function (cannot be `false`!)
-- @usage
-- -- using an operator
-- local lst = { "Rudolph", true, false, 15 }
-- local idx, cmp_result = tablex.rfind(lst, "==", "Rudolph")
-- assert(idx == 1)
-- assert(cmp_result == true)
--
-- local idx, cmp_result = tablex.rfind(lst, "==", false)
-- assert(idx == 3)
-- assert(cmp_result == true)       -- looking up 'false' works!
--
-- -- using a function returning the value looked up
-- local cmp = function(v1, v2) return v1 == v2 and v2 end
-- local idx, cmp_result = tablex.rfind(lst, cmp, "Rudolph")
-- assert(idx == 1)
-- assert(cmp_result == "Rudolph")  -- the value is returned
--
-- -- NOTE: this fails, since 'false' cannot be returned!
-- local idx, cmp_result = tablex.rfind(lst, cmp, false)
-- assert(idx == nil)               -- looking up 'false' failed!
-- assert(cmp_result == nil)
function tablex.find_if(t,cmp,arg)
    assert_arg_iterable(1,t)
    cmp = function_arg(2,cmp)
    for k,v in pairs(t) do
        local c = cmp(v,arg)
        if c then return k,c end
    end
    return nil
end

--- return a list of all values in a table indexed by another list.
-- @tab tbl a table
-- @array idx an index table (a list of keys)
-- @return a list-like table
-- @usage index_by({10,20,30,40},{2,4}) == {20,40}
-- @usage index_by({one=1,two=2,three=3},{'one','three'}) == {1,3}
function tablex.index_by(tbl,idx)
    assert_arg_indexable(1,tbl)
    assert_arg_indexable(2,idx)
    local res = {}
    for i = 1,#idx do
        res[i] = tbl[idx[i]]
    end
    return setmeta(res,tbl,'List')
end

--- apply a function to all values of a table.
-- This returns a table of the results.
-- Any extra arguments are passed to the function.
-- @within MappingAndFiltering
-- @func fun A function that takes at least one argument
-- @tab t A table
-- @param ... optional arguments
-- @usage map(function(v) return v*v end, {10,20,30,fred=2}) is {100,400,900,fred=4}
function tablex.map(fun,t,...)
    assert_arg_iterable(1,t)
    fun = function_arg(1,fun)
    local res = {}
    for k,v in pairs(t) do
        res[k] = fun(v,...)
    end
    return setmeta(res,t)
end

--- apply a function to all values of a list.
-- This returns a table of the results.
-- Any extra arguments are passed to the function.
-- @within MappingAndFiltering
-- @func fun A function that takes at least one argument
-- @array t a table (applies to array part)
-- @param ... optional arguments
-- @return a list-like table
-- @usage imap(function(v) return v*v end, {10,20,30,fred=2}) is {100,400,900}
function tablex.imap(fun,t,...)
    assert_arg_indexable(1,t)
    fun = function_arg(1,fun)
    local res = {}
    for i = 1,#t do
        res[i] = fun(t[i],...) or false
    end
    return setmeta(res,t,'List')
end

--- apply a named method to values from a table.
-- @within MappingAndFiltering
-- @string name the method name
-- @array t a list-like table
-- @param ... any extra arguments to the method
-- @return a `List` with the results of the method (1st result only)
-- @usage
-- local Car = {}
-- Car.__index = Car
-- function Car.new(car)
--   return setmetatable(car or {}, Car)
-- end
-- Car.speed = 0
-- function Car:faster(increase)
--   self.speed = self.speed + increase
--   return self.speed
-- end
--
-- local ferrari = Car.new{ name = "Ferrari" }
-- local lamborghini = Car.new{ name = "Lamborghini", speed = 50 }
-- local cars = { ferrari, lamborghini }
--
-- assert(ferrari.speed == 0)
-- assert(lamborghini.speed == 50)
-- tablex.map_named_method("faster", cars, 10)
-- assert(ferrari.speed == 10)
-- assert(lamborghini.speed == 60)
function tablex.map_named_method (name,t,...)
    utils.assert_string(1,name)
    assert_arg_indexable(2,t)
    local res = {}
    for i = 1,#t do
        local val = t[i]
        local fun = val[name]
        res[i] = fun(val,...)
    end
    return setmeta(res,t,'List')
end

--- apply a function to all values of a table, in-place.
-- Any extra arguments are passed to the function.
-- @func fun A function that takes at least one argument
-- @tab t a table
-- @param ... extra arguments passed to `fun`
-- @see tablex.foreach
function tablex.transform (fun,t,...)
    assert_arg_iterable(1,t)
    fun = function_arg(1,fun)
    for k,v in pairs(t) do
        t[k] = fun(v,...)
    end
end

--- generate a table of all numbers in a range.
-- This is consistent with a numerical for loop.
-- @int start  number
-- @int finish number
-- @int[opt=1] step  make this negative for start < finish
function tablex.range (start,finish,step)
    local res
    step = step or 1
    if start == finish then
        res = {start}
    elseif (start > finish and step > 0) or (finish > start and step < 0) then
        res = {}
    else
        local k = 1
        res = {}
        for i=start,finish,step do res[k]=i; k=k+1 end
    end
    return makelist(res)
end

--- apply a function to values from two tables.
-- @within MappingAndFiltering
-- @func fun a function of at least two arguments
-- @tab t1 a table
-- @tab t2 a table
-- @param ... extra arguments
-- @return a table
-- @usage map2('+',{1,2,3,m=4},{10,20,30,m=40}) is {11,22,23,m=44}
function tablex.map2 (fun,t1,t2,...)
    assert_arg_iterable(1,t1)
    assert_arg_iterable(2,t2)
    fun = function_arg(1,fun)
    local res = {}
    for k,v in pairs(t1) do
        res[k] = fun(v,t2[k],...)
    end
    return setmeta(res,t1,'List')
end

--- apply a function to values from two arrays.
-- The result will be the length of the shortest array.
-- @within MappingAndFiltering
-- @func fun a function of at least two arguments
-- @array t1 a list-like table
-- @array t2 a list-like table
-- @param ... extra arguments
-- @usage imap2('+',{1,2,3,m=4},{10,20,30,m=40}) is {11,22,23}
function tablex.imap2 (fun,t1,t2,...)
    assert_arg_indexable(2,t1)
    assert_arg_indexable(3,t2)
    fun = function_arg(1,fun)
    local res,n = {},math.min(#t1,#t2)
    for i = 1,n do
        res[i] = fun(t1[i],t2[i],...)
    end
    return res
end

--- 'reduce' a list using a binary function.
-- @func fun a function of two arguments
-- @array t a list-like table
-- @array memo optional initial memo value. Defaults to first value in table.
-- @return the result of the function
-- @usage reduce('+',{1,2,3,4}) == 10
function tablex.reduce (fun,t,memo)
    assert_arg_indexable(2,t)
    fun = function_arg(1,fun)
    local n = #t
    if n == 0 then
        return memo
    end
    local res = memo and fun(memo, t[1]) or t[1]
    for i = 2,n do
        res = fun(res,t[i])
    end
    return res
end

--- apply a function to all elements of a table.
-- The arguments to the function will be the value,
-- the key and _finally_ any extra arguments passed to this function.
-- Note that the Lua 5.0 function table.foreach passed the _key_ first.
-- @within Iterating
-- @tab t a table
-- @func fun a function on the elements; `function(value, key, ...)`
-- @param ... extra arguments passed to `fun`
-- @see tablex.transform
function tablex.foreach(t,fun,...)
    assert_arg_iterable(1,t)
    fun = function_arg(2,fun)
    for k,v in pairs(t) do
        fun(v,k,...)
    end
end

--- apply a function to all elements of a list-like table in order.
-- The arguments to the function will be the value,
-- the index and _finally_ any extra arguments passed to this function
-- @within Iterating
-- @array t a table
-- @func fun a function with at least one argument
-- @param ... optional arguments
function tablex.foreachi(t,fun,...)
    assert_arg_indexable(1,t)
    fun = function_arg(2,fun)
    for i = 1,#t do
        fun(t[i],i,...)
    end
end

--- Apply a function to a number of tables.
-- A more general version of map
-- The result is a table containing the result of applying that function to the
-- ith value of each table. Length of output list is the minimum length of all the lists
-- @within MappingAndFiltering
-- @func fun a function of n arguments
-- @tab ... n tables
-- @usage mapn(function(x,y,z) return x+y+z end, {1,2,3},{10,20,30},{100,200,300}) is {111,222,333}
-- @usage mapn(math.max, {1,20,300},{10,2,3},{100,200,100}) is    {100,200,300}
-- @param fun A function that takes as many arguments as there are tables
function tablex.mapn(fun,...)
    fun = function_arg(1,fun)
    local res = {}
    local lists = {...}
    local minn = 1e40
    for i = 1,#lists do
        minn = min(minn,#(lists[i]))
    end
    for i = 1,minn do
        local args,k = {},1
        for j = 1,#lists do
            args[k] = lists[j][i]
            k = k + 1
        end
        res[#res+1] = fun(unpack(args))
    end
    return res
end

--- call the function with the key and value pairs from a table.
-- The function can return a value and a key (note the order!). If both
-- are not nil, then this pair is inserted into the result: if the key already exists, we convert the value for that
-- key into a table and append into it. If only value is not nil, then it is appended to the result.
-- @within MappingAndFiltering
-- @func fun A function which will be passed each key and value as arguments, plus any extra arguments to pairmap.
-- @tab t A table
-- @param ... optional arguments
-- @usage pairmap(function(k,v) return v end,{fred=10,bonzo=20}) is {10,20} _or_ {20,10}
-- @usage pairmap(function(k,v) return {k,v},k end,{one=1,two=2}) is {one={'one',1},two={'two',2}}
function tablex.pairmap(fun,t,...)
    assert_arg_iterable(1,t)
    fun = function_arg(1,fun)
    local res = {}
    for k,v in pairs(t) do
        local rv,rk = fun(k,v,...)
        if rk then
            if res[rk] then
                if type(res[rk]) == 'table' then
                    table.insert(res[rk],rv)
                else
                    res[rk] = {res[rk], rv}
                end
            else
                res[rk] = rv
            end
        else
            res[#res+1] = rv
        end
    end
    return res
end

local function keys_op(i,v) return i end

--- return all the keys of a table in arbitrary order.
-- @within Extraction
-- @tab t A list-like table where the values are the keys of the input table
function tablex.keys(t)
    assert_arg_iterable(1,t)
    return makelist(tablex.pairmap(keys_op,t))
end

local function values_op(i,v) return v end

--- return all the values of the table in arbitrary order
-- @within Extraction
-- @tab t A list-like table where the values are the values of the input table
function tablex.values(t)
    assert_arg_iterable(1,t)
    return makelist(tablex.pairmap(values_op,t))
end

local function index_map_op (i,v) return i,v end

--- create an index map from a list-like table. The original values become keys,
-- and the associated values are the indices into the original list.
-- @array t a list-like table
-- @return a map-like table
function tablex.index_map (t)
    assert_arg_indexable(1,t)
    return makemap(tablex.pairmap(index_map_op,t))
end

local function set_op(i,v) return true,v end

--- create a set from a list-like table. A set is a table where the original values
-- become keys, and the associated values are all true.
-- @array t a list-like table
-- @return a set (a map-like table)
function tablex.makeset (t)
    assert_arg_indexable(1,t)
    return setmetatable(tablex.pairmap(set_op,t),require('pl.Set'))
end

--- combine two tables, either as union or intersection. Corresponds to
-- set operations for sets () but more general. Not particularly
-- useful for list-like tables.
-- @within Merging
-- @tab t1 a table
-- @tab t2 a table
-- @bool dup true for a union, false for an intersection.
-- @usage merge({alice=23,fred=34},{bob=25,fred=34}) is {fred=34}
-- @usage merge({alice=23,fred=34},{bob=25,fred=34},true) is {bob=25,fred=34,alice=23}
-- @see tablex.index_map
function tablex.merge (t1,t2,dup)
    assert_arg_iterable(1,t1)
    assert_arg_iterable(2,t2)
    local res = {}
    for k,v in pairs(t1) do
        if dup or t2[k] then res[k] = v end
    end
    if dup then
      for k,v in pairs(t2) do
        res[k] = v
      end
    end
    return setmeta(res,t1,'Map')
end

--- the union of two map-like tables.
-- If there are duplicate keys, the second table wins.
-- @tab t1 a table
-- @tab t2 a table
-- @treturn tab
-- @see tablex.merge
function tablex.union(t1, t2)
    return tablex.merge(t1, t2, true)
end

--- the intersection of two map-like tables.
-- @tab t1 a table
-- @tab t2 a table
-- @treturn tab
-- @see tablex.merge
function tablex.intersection(t1, t2)
    return tablex.merge(t1, t2, false)
end

--- a new table which is the difference of two tables.
-- With sets (where the values are all true) this is set difference and
-- symmetric difference depending on the third parameter.
-- @within Merging
-- @tab s1 a map-like table or set
-- @tab s2 a map-like table or set
-- @bool symm symmetric difference (default false)
-- @return a map-like table or set
function tablex.difference (s1,s2,symm)
    assert_arg_iterable(1,s1)
    assert_arg_iterable(2,s2)
    local res = {}
    for k,v in pairs(s1) do
        if s2[k] == nil then res[k] = v end
    end
    if symm then
        for k,v in pairs(s2) do
            if s1[k] == nil then res[k] = v end
        end
    end
    return setmeta(res,s1,'Map')
end

--- A table where the key/values are the values and value counts of the table.
-- @array t a list-like table
-- @func cmp a function that defines equality (otherwise uses ==)
-- @return a map-like table
-- @see seq.count_map
function tablex.count_map (t,cmp)
    assert_arg_indexable(1,t)
    local res,mask = {},{}
    cmp = function_arg(2,cmp or '==')
    local n = #t
    for i = 1,#t do
        local v = t[i]
        if not mask[v] then
            mask[v] = true
            -- check this value against all other values
            res[v] = 1  -- there's at least one instance
            for j = i+1,n do
                local w = t[j]
                local ok = cmp(v,w)
                if ok then
                    res[v] = res[v] + 1
                    mask[w] = true
                end
            end
        end
    end
    return makemap(res)
end

--- filter an array's values using a predicate function
-- @within MappingAndFiltering
-- @array t a list-like table
-- @func pred a boolean function
-- @param arg optional argument to be passed as second argument of the predicate
function tablex.filter (t,pred,arg)
    assert_arg_indexable(1,t)
    pred = function_arg(2,pred)
    local res,k = {},1
    for i = 1,#t do
        local v = t[i]
        if pred(v,arg) then
            res[k] = v
            k = k + 1
        end
    end
    return setmeta(res,t,'List')
end

--- return a table where each element is a table of the ith values of an arbitrary
-- number of tables. It is equivalent to a matrix transpose.
-- @within Merging
-- @usage zip({10,20,30},{100,200,300}) is {{10,100},{20,200},{30,300}}
-- @array ... arrays to be zipped
function tablex.zip(...)
    return tablex.mapn(function(...) return {...} end,...)
end

local _copy
function _copy (dest,src,idest,isrc,nsrc,clean_tail)
    idest = idest or 1
    isrc = isrc or 1
    local iend
    if not nsrc then
        nsrc = #src
        iend = #src
    else
        iend = isrc + min(nsrc-1,#src-isrc)
    end
    if dest == src then -- special case
        if idest > isrc and iend >= idest then -- overlapping ranges
            src = tablex.sub(src,isrc,nsrc)
            isrc = 1; iend = #src
        end
    end
    for i = isrc,iend do
        dest[idest] = src[i]
        idest = idest + 1
    end
    if clean_tail then
        tablex.clear(dest,idest)
    end
    return dest
end

--- copy an array into another one, clearing `dest` after `idest+nsrc`, if necessary.
-- @within Copying
-- @array dest a list-like table
-- @array src a list-like table
-- @int[opt=1] idest where to start copying values into destination
-- @int[opt=1] isrc where to start copying values from source
-- @int[opt=#src] nsrc number of elements to copy from source
function tablex.icopy (dest,src,idest,isrc,nsrc)
    assert_arg_indexable(1,dest)
    assert_arg_indexable(2,src)
    return _copy(dest,src,idest,isrc,nsrc,true)
end

--- copy an array into another one.
-- @within Copying
-- @array dest a list-like table
-- @array src a list-like table
-- @int[opt=1] idest where to start copying values into destination
-- @int[opt=1] isrc where to start copying values from source
-- @int[opt=#src] nsrc number of elements to copy from source
function tablex.move (dest,src,idest,isrc,nsrc)
    assert_arg_indexable(1,dest)
    assert_arg_indexable(2,src)
    return _copy(dest,src,idest,isrc,nsrc,false)
end

function tablex._normalize_slice(self,first,last)
  local sz = #self
  if not first then first=1 end
  if first<0 then first=sz+first+1 end
  -- make the range _inclusive_!
  if not last then last=sz end
  if last < 0 then last=sz+1+last end
  return first,last
end

--- Extract a range from a table, like  'string.sub'.
-- If first or last are negative then they are relative to the end of the list
-- eg. sub(t,-2) gives last 2 entries in a list, and
-- sub(t,-4,-2) gives from -4th to -2nd
-- @within Extraction
-- @array t a list-like table
-- @int first An index
-- @int last An index
-- @return a new List
function tablex.sub(t,first,last)
    assert_arg_indexable(1,t)
    first,last = tablex._normalize_slice(t,first,last)
    local res={}
    for i=first,last do append(res,t[i]) end
    return setmeta(res,t,'List')
end

--- set an array range to a value. If it's a function we use the result
-- of applying it to the indices.
-- @array t a list-like table
-- @param val a value
-- @int[opt=1] i1 start range
-- @int[opt=#t] i2 end range
function tablex.set (t,val,i1,i2)
    assert_arg_indexable(1,t)
    i1,i2 = i1 or 1,i2 or #t
    if types.is_callable(val) then
        for i = i1,i2 do
            t[i] = val(i)
        end
    else
        for i = i1,i2 do
            t[i] = val
        end
    end
end

--- create a new array of specified size with initial value.
-- @int n size
-- @param val initial value (can be `nil`, but don't expect `#` to work!)
-- @return the table
function tablex.new (n,val)
    local res = {}
    tablex.set(res,val,1,n)
    return res
end

--- clear out the contents of a table.
-- @array t a list
-- @param istart optional start position
function tablex.clear(t,istart)
    istart = istart or 1
    for i = istart,#t do remove(t) end
end

--- insert values into a table.
-- similar to `table.insert` but inserts values from given table `values`,
-- not the object itself, into table `t` at position `pos`.
-- @within Copying
-- @array t the list
-- @int[opt] position (default is at end)
-- @array values
function tablex.insertvalues(t, ...)
    assert_arg(1,t,'table')
    local pos, values
    if select('#', ...) == 1 then
        pos,values = #t+1, ...
    else
        pos,values = ...
    end
    if #values > 0 then
        for i=#t,pos,-1 do
            t[i+#values] = t[i]
        end
        local offset = 1 - pos
        for i=pos,pos+#values-1 do
            t[i] = values[i + offset]
        end
    end
    return t
end

--- remove a range of values from a table.
-- End of range may be negative.
-- @array t a list-like table
-- @int i1 start index
-- @int i2 end index
-- @return the table
function tablex.removevalues (t,i1,i2)
    assert_arg(1,t,'table')
    i1,i2 = tablex._normalize_slice(t,i1,i2)
    for i = i1,i2 do
        remove(t,i1)
    end
    return t
end

local _find
_find = function (t,value,tables)
    for k,v in pairs(t) do
        if v == value then return k end
    end
    for k,v in pairs(t) do
        if not tables[v] and type(v) == 'table' then
            tables[v] = true
            local res = _find(v,value,tables)
            if res then
                res = tostring(res)
                if type(k) ~= 'string' then
                    return '['..k..']'..res
                else
                    return k..'.'..res
                end
            end
        end
    end
end

--- find a value in a table by recursive search.
-- @within Finding
-- @tab t the table
-- @param value the value
-- @array[opt] exclude any tables to avoid searching
-- @return a fieldspec, e.g. 'a.b' or 'math.sin'
-- @usage search(_G,math.sin,{package.path}) == 'math.sin'
function tablex.search (t,value,exclude)
    assert_arg_iterable(1,t)
    local tables = {[t]=true}
    if exclude then
        for _,v in pairs(exclude) do tables[v] = true end
    end
    return _find(t,value,tables)
end

--- return an iterator to a table sorted by its keys
-- @within Iterating
-- @tab t the table
-- @func f an optional comparison function (f(x,y) is true if x < y)
-- @usage for k,v in tablex.sort(t) do print(k,v) end
-- @return an iterator to traverse elements sorted by the keys
function tablex.sort(t,f)
    local keys = {}
    for k in pairs(t) do keys[#keys + 1] = k end
    tsort(keys,f)
    local i = 0
    return function()
        i = i + 1
        return keys[i], t[keys[i]]
    end
end

--- return an iterator to a table sorted by its values
-- @within Iterating
-- @tab t the table
-- @func f an optional comparison function (f(x,y) is true if x < y)
-- @usage for k,v in tablex.sortv(t) do print(k,v) end
-- @return an iterator to traverse elements sorted by the values
function tablex.sortv(t,f)
    f = function_arg(2, f or '<')
    local keys = {}
    for k in pairs(t) do keys[#keys + 1] = k end
    tsort(keys,function(x, y) return f(t[x], t[y]) end)
    local i = 0
    return function()
        i = i + 1
        return keys[i], t[keys[i]]
    end
end

--- modifies a table to be read only.
-- This only offers weak protection. Tables can still be modified with
-- `table.insert` and `rawset`.
--
-- *NOTE*: for Lua 5.1 length, pairs and ipairs will not work, since the
-- equivalent metamethods are only available in Lua 5.2 and newer.
-- @tab t the table
-- @return the table read only (a proxy).
function tablex.readonly(t)
    local mt = {
        __index=t,
        __newindex=function(t, k, v) error("Attempt to modify read-only table", 2) end,
        __pairs=function() return pairs(t) end,
        __ipairs=function() return ipairs(t) end,
        __len=function() return #t end,
        __metatable=false
    }
    return setmetatable({}, mt)
end

return tablex

end,

["pl.template"] = function()
--------------------
-- Module: 'pl.template'
--------------------
--- A template preprocessor.
-- Originally by [Ricki Lake](http://lua-users.org/wiki/SlightlyLessSimpleLuaPreprocessor)
--
-- There are two rules:
--
--  * lines starting with # are Lua
--  * otherwise, `$(expr)` is the result of evaluating `expr`
--
-- Example:
--
--    #  for i = 1,3 do
--       $(i) Hello, Word!
--    #  end
--    ===>
--    1 Hello, Word!
--    2 Hello, Word!
--    3 Hello, Word!
--
-- Other escape characters can be used, when the defaults conflict
-- with the output language.
--
--    > for _,n in pairs{'one','two','three'} do
--    static int l_${n} (luaState *state);
--    > end
--
-- See  @{03-strings.md.Another_Style_of_Template|the Guide}.
--
-- Dependencies: `pl.utils`
-- @module pl.template

local utils = require 'pl.utils'

local append, concat = table.insert, table.concat
local format, strsub, strfind, strgsub, strrep = string.format, string.sub, string.find, string.gsub, string.rep

local APPENDER = " __R_size = __R_size + 1; __R_table[__R_size] = "

-- When this function returns, `pieces` is guaranteed to hold a complete Lua
-- statement, meaning that new statements can be appended without creating
-- invalid Lua code.
local function parseDollarParen(pieces, chunk, exec_pat, newline)
    local s = 1
    for term, executed, e in chunk:gmatch(exec_pat) do
        executed = '(' .. strsub(executed, 2, -2) .. ')'
        append(pieces, APPENDER .. format("%q;", strsub(chunk, s, term - 1)))
        append(pieces, APPENDER .. format("__tostring(%s or '');", executed))
        s = e
    end
    local remainder, newlines_removed
    if newline then
        remainder, newlines_removed = strgsub(strsub(chunk, s), "\n", "")
    else
        remainder, newlines_removed = strsub(chunk, s), 0
    end
    if remainder ~= "" then
        append(pieces, APPENDER .. format("%q;", remainder))
    end
    if newlines_removed > 0 then
        append(pieces, strrep("\n", newlines_removed))
    end
end

local function parseHashLines(chunk, inline_escape, brackets, esc, newline)
    -- Escape special characters to avoid invalid expressions
    inline_escape = utils.escape(inline_escape)
    esc = utils.escape(esc)

    local exec_pat = "()" .. inline_escape .. "(%b" .. brackets .. ")()"

    local esc_pat = esc .. "+([^\n]*\n?)"
    local esc_pat1, esc_pat2 = "^" .. esc_pat, "\n" .. esc_pat
    local pieces, s = {"return function() local __R_size, __R_table, __tostring = 0, {}, __tostring; "}, 1
    while true do
        local _, e, lua = strfind(chunk, esc_pat1, s)
        if not e then
            local ss
            ss, e, lua = strfind(chunk, esc_pat2, s)
            parseDollarParen(pieces, strsub(chunk, s, ss), exec_pat, newline)
            if not e then break end
        end
        if strsub(lua, -1, -1) ~= "\n" then lua = lua .. "\n" end -- Ensure trailing newline
        append(pieces, lua)
        -- since `lua` ends with a newline, there is no danger of subsequent
        -- statements being gobbled up by comments or being altered
        s = e + 1
    end
    append(pieces, "return __R_table; end")

    -- let's check for a special case where there is nothing to template, but it's
    -- just a single static string
    local short = false
    if (#pieces == 3) and (strfind(pieces[2], APPENDER, 1, true) == 1) then
        pieces = { "return " .. strsub(pieces[2], #APPENDER + 1, -1) }
        short = true
    end
    -- if short == true, the generated function will not return a table of strings,
    -- but a single string
    return concat(pieces), short
end

local template = {}

--- expand the template using the specified environment.
-- This function will compile and render the template. For more performant
-- recurring usage use the two step approach by using `compile` and `ct:render`.
-- @string str the template string
-- @tparam[opt] table env the environment. This table has the following special fields:
-- @tparam[opt=nil] table env._parent continue looking up in this table (e.g. `_parent=_G`).
-- @tparam[opt="()"] string env._brackets bracket pair that wraps inline Lua expressions.
-- @tparam[opt="#"] string env._escape character marking Lua lines.
-- @tparam[opt="$"] string env._inline_escape character marking inline Lua expression.
-- @tparam[opt="TMP"] string env._chunk_name chunk name for loaded templates, used if there
-- is an error in Lua code.
-- @tparam[opt=false] boolean env._debug if truthy, the generated code will be printed upon a render error.
-- @treturn[1] string render result
-- @treturn[1] nil
-- @treturn[1] string source_code (only if '`env._debug`' was truthy).
-- @treturn[2] nil
-- @treturn[2] string error message
-- @treturn[2] string source_code (only if '`env._debug`' was truthy).
function template.substitute(str, env)
    env = env or {}
    local t, err = template.compile(str, {
        chunk_name = rawget(env, "_chunk_name"),
        escape = rawget(env, "_escape"),
        inline_escape = rawget(env, "_inline_escape"),
        inline_brackets = rawget(env, "_brackets"),
        newline = false,
        debug = rawget(env, "_debug")
    })
    if not t then return t, err end

    return t:render(env, rawget(env, "_parent"), rawget(env, "_debug"))
end

--- executes the previously compiled template and renders it.
-- @function ct:render
-- @tab[opt] env the environment.
-- @tab[opt] parent continue looking up in this table (e.g. `parent=_G`).
-- @bool[opt] db if thruthy, it will print the code upon a render error
-- (provided the template was compiled with the debug option).
-- @treturn[1] string render result
-- @treturn[1] nil
-- @treturn[1] string source_code (only if '`env._debug`' was truthy).
-- @treturn[2] nil
-- @treturn[2] string error message
-- @treturn[2] string source_code (only if '`env._debug`' was truthy).
-- @usage
-- local ct, err = template.compile(my_template)
-- local rendered , err = ct:render(my_env, parent)
local function render(self, env, parent, db)
    env = env or {}
    if parent then  -- parent is a bit silly, but for backward compatibility retained
        setmetatable(env, {__index = parent})
    end
    setmetatable(self.env, {__index = env})

    local res, out = xpcall(self.fn, debug.traceback)
    if not res then
        if self.code and db then print(self.code) end
        return nil, out, self.code
    end
    return concat(out), nil, self.code
end

--- compiles the template.
-- Returns an object that can repeatedly be rendered without parsing/compiling
-- the template again. Preserves the line layout of the template so that line
-- numbers in error messages should point to the correct lines in the source
-- string.
-- @tparam string str the template string
-- @tparam[opt] table opts the compilation options to use. This table supports the following options:
-- @tparam[opt="TMP"] string opts.chunk_name chunk name for loaded templates, used if there
-- is an error in Lua code.
-- @tparam[opt="#"] string opts.escape character marking Lua lines.
-- @tparam[opt="$"] string opts.inline_escape character marking inline Lua expression.
-- @tparam[opt="()"] string opts.inline_brackets bracket pair that wraps inline Lua expressions.
-- @tparam[opt=false] boolean opts.newline if truthy, newlines will be stripped from text in the template.
-- @tparam[opt=false] boolean opts.debug if truthy, the generated code will be printed upon a render error.
-- @treturn[1] ct compiled template object
-- @treturn[2] nil
-- @treturn[2] string error message
-- @treturn[2] string source_code
-- @usage
-- local ct, err = template.compile(my_template)
-- local rendered , err = ct:render(my_env, parent)
function template.compile(str, opts)
    opts = opts or {}
    local chunk_name = opts.chunk_name or 'TMP'
    local escape = opts.escape or '#'
    local inline_escape = opts.inline_escape or '$'
    local inline_brackets = opts.inline_brackets or '()'

    local code, short = parseHashLines(str, inline_escape, inline_brackets, escape, opts.newline)
    local env = { __tostring = tostring }
    local fn, err = utils.load(code, chunk_name, 't', env)
    if not fn then return nil, err, code end

    if short then
        -- the template returns a single constant string, let's optimize for that
        local constant_string = fn()
        return {
            fn = fn(),
            env = env,
            render = function(self) -- additional params can be ignored
                -- skip the metatable magic and error handling in the render
                -- function above for this special case
                return constant_string, nil, self.code
            end,
            code = opts.debug and code or nil,
        }
    end

    return {
        fn = fn(),
        env = env,
        render = render,
        code = opts.debug and code or nil,
    }
end

return template

end,

["pl.test"] = function()
--------------------
-- Module: 'pl.test'
--------------------
--- Useful test utilities.
--
--    test.asserteq({1,2},{1,2}) -- can compare tables
--    test.asserteq(1.2,1.19,0.02) -- compare FP numbers within precision
--    T = test.tuple -- used for comparing multiple results
--    test.asserteq(T(string.find(" me","me")),T(2,3))
--
-- Dependencies: `pl.utils`, `pl.tablex`, `pl.pretty`, `pl.path`, `debug`
-- @module pl.test

local tablex = require 'pl.tablex'
local utils = require 'pl.utils'
local pretty = require 'pl.pretty'
local path = require 'pl.path'
local type,unpack,pack = type,utils.unpack,utils.pack
local clock = os.clock
local debug = require 'debug'
local io = io

local function dump(x)
    if type(x) == 'table' and not (getmetatable(x) and getmetatable(x).__tostring) then
        return pretty.write(x,' ',true)
    elseif type(x) == 'string' then
        return '"'..x..'"'
    else
        return tostring(x)
    end
end

local test = {}

---- error handling for test results.
-- By default, this writes to stderr and exits the program.
-- Re-define this function to raise an error and/or redirect output
function test.error_handler(file,line,got_text, needed_text,msg)
    local err = io.stderr
    err:write(path.basename(file)..':'..line..': assertion failed\n')
    err:write("got:\t",got_text,'\n')
    err:write("needed:\t",needed_text,'\n')
    utils.quit(1,msg or "these values were not equal")
end

local function complain (x,y,msg,where)
    local i = debug.getinfo(3 + (where or 0))
    test.error_handler(i.short_src,i.currentline,dump(x),dump(y),msg)
end

--- general test complain message.
-- Useful for composing new test functions (see tests/tablex.lua for an example)
-- @param x a value
-- @param y value to compare first value against
-- @param msg message
-- @param where extra level offset for errors
-- @function complain
test.complain = complain

--- like assert, except takes two arguments that must be equal and can be tables.
-- If they are plain tables, it will use tablex.deepcompare.
-- @param x any value
-- @param y a value equal to x
-- @param eps an optional tolerance for numerical comparisons
-- @param where extra level offset
function test.asserteq (x,y,eps,where)
    local res = x == y
    if not res then
        res = tablex.deepcompare(x,y,true,eps)
    end
    if not res then
        complain(x,y,nil,where)
    end
end

--- assert that the first string matches the second.
-- @param s1 a string
-- @param s2 a string
-- @param where extra level offset
function test.assertmatch (s1,s2,where)
    if not s1:match(s2) then
        complain (s1,s2,"these strings did not match",where)
    end
end

--- assert that the function raises a particular error.
-- @param fn a function or a table of the form {function,arg1,...}
-- @param e a string to match the error against
-- @param where extra level offset
function test.assertraise(fn,e,where)
    local ok, err
    if type(fn) == 'table' then
        ok, err = pcall(unpack(fn))
    else
        ok, err = pcall(fn)
    end
    if ok or err:match(e)==nil then
        complain (err,e,"these errors did not match",where)
    end
end

--- a version of asserteq that takes two pairs of values.
-- <code>x1==y1 and x2==y2</code> must be true. Useful for functions that naturally
-- return two values.
-- @param x1 any value
-- @param x2 any value
-- @param y1 any value
-- @param y2 any value
-- @param where extra level offset
function test.asserteq2 (x1,x2,y1,y2,where)
    if x1 ~= y1 then complain(x1,y1,nil,where) end
    if x2 ~= y2 then complain(x2,y2,nil,where) end
end

-- tuple type --

local tuple_mt = {
    unpack = unpack
}
tuple_mt.__index = tuple_mt

function tuple_mt.__tostring(self)
    local ts = {}
    for i=1, self.n do
        local s = self[i]
        ts[i] = type(s) == 'string' and ('%q'):format(s) or tostring(s)
    end
    return 'tuple(' .. table.concat(ts, ', ') .. ')'
end

function tuple_mt.__eq(a, b)
    if a.n ~= b.n then return false end
    for i=1, a.n do
        if a[i] ~= b[i] then return false end
    end
    return true
end

function tuple_mt.__len(self)
    return self.n
end

--- encode an arbitrary argument list as a tuple.
-- This can be used to compare to other argument lists, which is
-- very useful for testing functions which return a number of values.
-- Unlike regular array-like tables ('sequences') they may contain nils.
-- Tuples understand equality and know how to print themselves out.
-- The # operator is defined to be the size, irrespecive of any nils,
-- and there is an `unpack` method.
-- @usage asserteq(tuple( ('ab'):find 'a'), tuple(1,1))
function test.tuple(...)
    return setmetatable(pack(...), tuple_mt)
end

--- Time a function. Call the function a given number of times, and report the number of seconds taken,
-- together with a message.  Any extra arguments will be passed to the function.
-- @string msg a descriptive message
-- @int n number of times to call the function
-- @func fun the function
-- @param ... optional arguments to fun
function test.timer(msg,n,fun,...)
    local start = clock()
    for i = 1,n do fun(...) end
    utils.printf("%s: took %7.2f sec\n",msg,clock()-start)
end

return test

end,

["pl.text"] = function()
--------------------
-- Module: 'pl.text'
--------------------
--- Text processing utilities.
--
-- This provides a Template class (modeled after the same from the Python
-- libraries, see string.Template). It also provides similar functions to those
-- found in the textwrap module.
--
-- IMPORTANT: this module has been deprecated and will be removed in a future
-- version (2.0). The contents of this module have moved to the `pl.stringx`
-- module.
--
-- See  @{03-strings.md.String_Templates|the Guide}.
--
-- Dependencies: `pl.stringx`, `pl.utils`
-- @module pl.text

local utils = require("pl.utils")

utils.raise_deprecation {
  source = "Penlight " .. utils._VERSION,
  message = "the contents of module 'pl.text' has moved into 'pl.stringx'",
  version_removed = "2.0.0",
  deprecated_after = "1.11.0",
  no_trace = true,
}

return require "pl.stringx"

end,

["pl.types"] = function()
--------------------
-- Module: 'pl.types'
--------------------
---- Dealing with Detailed Type Information

-- Dependencies `pl.utils`
-- @module pl.types

local utils = require 'pl.utils'
local math_ceil = math.ceil
local assert_arg = utils.assert_arg
local types = {}

--- is the object either a function or a callable object?.
-- @param obj Object to check.
function types.is_callable (obj)
    return type(obj) == 'function' or getmetatable(obj) and getmetatable(obj).__call and true
end

--- is the object of the specified type?.
-- If the type is a string, then use type, otherwise compare with metatable.
--
-- NOTE: this function is imported from `utils.is_type`.
-- @param obj An object to check
-- @param tp The expected type
-- @function is_type
-- @see utils.is_type
types.is_type = utils.is_type

local fileMT = getmetatable(io.stdout)

--- a string representation of a type.
-- For tables and userdata with metatables, we assume that the metatable has a `_name`
-- field. If the field is not present it will return 'unknown table' or
-- 'unknown userdata'.
-- Lua file objects return the type 'file'.
-- @param obj an object
-- @return a string like 'number', 'table', 'file' or 'List'
function types.type (obj)
    local t = type(obj)
    if t == 'table' or t == 'userdata' then
        local mt = getmetatable(obj)
        if mt == fileMT then
            return 'file'
        elseif mt == nil then
            return t
        else
            -- TODO: the "unknown" is weird, it should just return the type
            return mt._name or "unknown "..t
        end
    else
        return t
    end
end

--- is this number an integer?
-- @param x a number
-- @raise error if x is not a number
-- @return boolean
function types.is_integer (x)
    return math_ceil(x)==x
end

--- Check if the object is "empty".
-- An object is considered empty if it is:
--
-- - `nil`
-- - a table without any items (key-value pairs or indexes)
-- - a string with no content ("")
-- - not a nil/table/string
-- @param o The object to check if it is empty.
-- @param ignore_spaces If the object is a string and this is true the string is
-- considered empty if it only contains spaces.
-- @return `true` if the object is empty, otherwise a falsy value.
function types.is_empty(o, ignore_spaces)
    if o == nil then
        return true
    elseif type(o) == "table" then
        return next(o) == nil
    elseif type(o) == "string" then
        return o == "" or (not not ignore_spaces and (not not o:find("^%s+$")))
    else
        return true
    end
end

local function check_meta (val)
    if type(val) == 'table' then return true end
    return getmetatable(val)
end

--- is an object 'array-like'?
-- An object is array like if:
--
-- - it is a table, or
-- - it has a metatable with `__len` and `__index` methods
--
-- NOTE: since `__len` is 5.2+, on 5.1 is usually returns `false` for userdata
-- @param val any value.
-- @return `true` if the object is array-like, otherwise a falsy value.
function types.is_indexable (val)
    local mt = check_meta(val)
    if mt == true then return true end
    return mt and mt.__len and mt.__index and true
end

--- can an object be iterated over with `pairs`?
-- An object is iterable if:
--
-- - it is a table, or
-- - it has a metatable with a `__pairs` meta method
--
-- NOTE: since `__pairs` is 5.2+, on 5.1 is usually returns `false` for userdata
-- @param val any value.
-- @return `true` if the object is iterable, otherwise a falsy value.
function types.is_iterable (val)
    local mt = check_meta(val)
    if mt == true then return true end
    return mt and mt.__pairs and true
end

--- can an object accept new key/pair values?
-- An object is iterable if:
--
-- - it is a table, or
-- - it has a metatable with a `__newindex` meta method
--
-- @param val any value.
-- @return `true` if the object is writeable, otherwise a falsy value.
function types.is_writeable (val)
    local mt = check_meta(val)
    if mt == true then return true end
    return mt and mt.__newindex and true
end

-- Strings that should evaluate to true.   -- TODO: add on/off ???
local trues = { yes=true, y=true, ["true"]=true, t=true, ["1"]=true }
-- Conditions types should evaluate to true.
local true_types = {
    boolean=function(o, true_strs, check_objs) return o end,
    string=function(o, true_strs, check_objs)
        o = o:lower()
        if trues[o] then
            return true
        end
        -- Check alternative user provided strings.
        for _,v in ipairs(true_strs or {}) do
            if type(v) == "string" and o == v:lower() then
                return true
            end
        end
        return false
    end,
    number=function(o, true_strs, check_objs) return o ~= 0 end,
    table=function(o, true_strs, check_objs) if check_objs and next(o) ~= nil then return true end return false end
}
--- Convert to a boolean value.
-- True values are:
--
-- * boolean: true.
-- * string: 'yes', 'y', 'true', 't', '1' or additional strings specified by `true_strs`.
-- * number: Any non-zero value.
-- * table: Is not empty and `check_objs` is true.
-- * everything else: Is not `nil` and `check_objs` is true.
--
-- @param o The object to evaluate.
-- @param[opt] true_strs optional Additional strings that when matched should evaluate to true. Comparison is case insensitive.
-- This should be a List of strings. E.g. "ja" to support German.
-- @param[opt] check_objs True if objects should be evaluated.
-- @return true if the input evaluates to true, otherwise false.
function types.to_bool(o, true_strs, check_objs)
    local true_func
    if true_strs then
        assert_arg(2, true_strs, "table")
    end
    true_func = true_types[type(o)]
    if true_func then
        return true_func(o, true_strs, check_objs)
    elseif check_objs and o ~= nil then
        return true
    end
    return false
end


return types

end,

["pl.url"] = function()
--------------------
-- Module: 'pl.url'
--------------------
--- Python-style URL quoting library.
--
-- @module pl.url

local url = {}

local function quote_char(c)
    return string.format("%%%02X", string.byte(c))
end

--- Quote the url, replacing special characters using the '%xx' escape.
-- @string s the string
-- @bool quote_plus Also escape slashes and replace spaces by plus signs.
-- @return The quoted string, or if `s` wasn't a string, just plain unaltered `s`.
function url.quote(s, quote_plus)
    if type(s) ~= "string" then
        return s
    end

    s = s:gsub("\n", "\r\n")
    s = s:gsub("([^A-Za-z0-9 %-_%./])", quote_char)
    if quote_plus then
        s = s:gsub(" ", "+")
        s = s:gsub("/", quote_char)
    else
        s = s:gsub(" ", "%%20")
    end

    return s
end

local function unquote_char(h)
    return string.char(tonumber(h, 16))
end

--- Unquote the url, replacing '%xx' escapes and plus signs.
-- @string s the string
-- @return The unquoted string, or if `s` wasn't a string, just plain unaltered `s`.
function url.unquote(s)
    if type(s) ~= "string" then
        return s
    end

    s = s:gsub("+", " ")
    s = s:gsub("%%(%x%x)", unquote_char)
    s = s:gsub("\r\n", "\n")

    return s
end

return url

end,

["pl.utils"] = function()
--------------------
-- Module: 'pl.utils'
--------------------
--- Generally useful routines.
-- See  @{01-introduction.md.Generally_useful_functions|the Guide}.
--
-- Dependencies: `pl.compat`, all exported fields and functions from
-- `pl.compat` are also available in this module.
--
-- @module pl.utils
local format = string.format
local compat = require 'pl.compat'
local stdout = io.stdout
local append = table.insert
local concat = table.concat
local _unpack = table.unpack  -- always injected by 'compat'
local find = string.find
local sub = string.sub
local next = next
local floor = math.floor

local is_windows = compat.is_windows
local err_mode = 'default'
local raise
local operators
local _function_factories = {}


local utils = { _VERSION = "1.14.0" }

for k, v in pairs(compat) do utils[k] = v  end

--- Some standard patterns
-- @table patterns
utils.patterns = {
    FLOAT = '[%+%-%d]%d*%.?%d*[eE]?[%+%-]?%d*', -- floating point number
    INTEGER = '[+%-%d]%d*',                     -- integer number
    IDEN = '[%a_][%w_]*',                       -- identifier
    FILE = '[%a%.\\][:%][%w%._%-\\]*',          -- file
}


--- Standard meta-tables as used by other Penlight modules
-- @table stdmt
-- @field List the List metatable
-- @field Map the Map metatable
-- @field Set the Set metatable
-- @field MultiMap the MultiMap metatable
utils.stdmt = {
    List = {_name='List'},
    Map = {_name='Map'},
    Set = {_name='Set'},
    MultiMap = {_name='MultiMap'},
}


--- pack an argument list into a table.
-- @param ... any arguments
-- @return a table with field `n` set to the length
-- @function utils.pack
-- @see compat.pack
-- @see utils.npairs
-- @see utils.unpack
utils.pack = table.pack  -- added here to be symmetrical with unpack

--- unpack a table and return its contents.
--
-- NOTE: this implementation differs from the Lua implementation in the way
-- that this one DOES honor the `n` field in the table `t`, such that it is 'nil-safe'.
-- @param t table to unpack
-- @param[opt] i index from which to start unpacking, defaults to 1
-- @param[opt] j index of the last element to unpack, defaults to `t.n` or else `#t`
-- @return multiple return values from the table
-- @function utils.unpack
-- @see compat.unpack
-- @see utils.pack
-- @see utils.npairs
-- @usage
-- local t = table.pack(nil, nil, nil, 4)
-- local a, b, c, d = table.unpack(t)   -- this `unpack` is NOT nil-safe, so d == nil
--
-- local a, b, c, d = utils.unpack(t)   -- this is nil-safe, so d == 4
function utils.unpack(t, i, j)
    return _unpack(t, i or 1, j or t.n or #t)
end

--- print an arbitrary number of arguments using a format.
-- Output will be sent to `stdout`.
-- @param fmt The format (see `string.format`)
-- @param ... Extra arguments for format
function utils.printf(fmt, ...)
    utils.assert_string(1, fmt)
    utils.fprintf(stdout, fmt, ...)
end

--- write an arbitrary number of arguments to a file using a format.
-- @param f File handle to write to.
-- @param fmt The format (see `string.format`).
-- @param ... Extra arguments for format
function utils.fprintf(f,fmt,...)
    utils.assert_string(2,fmt)
    f:write(format(fmt,...))
end

do
    local function import_symbol(T,k,v,libname)
        local key = rawget(T,k)
        -- warn about collisions!
        if key and k ~= '_M' and k ~= '_NAME' and k ~= '_PACKAGE' and k ~= '_VERSION' then
            utils.fprintf(io.stderr,"warning: '%s.%s' will not override existing symbol\n",libname,k)
            return
        end
        rawset(T,k,v)
    end

    local function lookup_lib(T,t)
        for k,v in pairs(T) do
            if v == t then return k end
        end
        return '?'
    end

    local already_imported = {}

    --- take a table and 'inject' it into the local namespace.
    -- @param t The table (table), or module name (string), defaults to this `utils` module table
    -- @param T An optional destination table (defaults to callers environment)
    function utils.import(t,T)
        T = T or _G
        t = t or utils
        if type(t) == 'string' then
            t = require (t)
        end
        local libname = lookup_lib(T,t)
        if already_imported[t] then return end
        already_imported[t] = libname
        for k,v in pairs(t) do
            import_symbol(T,k,v,libname)
        end
    end
end

--- return either of two values, depending on a condition.
-- @param cond A condition
-- @param value1 Value returned if cond is truthy
-- @param value2 Value returned if cond is falsy
function utils.choose(cond, value1, value2)
    if cond then
        return value1
    else
        return value2
    end
end

--- convert an array of values to strings.
-- @param t a list-like table
-- @param[opt] temp (table) buffer to use, otherwise allocate
-- @param[opt] tostr custom tostring function, called with (value,index). Defaults to `tostring`.
-- @return the converted buffer
function utils.array_tostring (t,temp,tostr)
    temp, tostr = temp or {}, tostr or tostring
    for i = 1,#t do
        temp[i] = tostr(t[i],i)
    end
    return temp
end



--- is the object of the specified type?
-- If the type is a string, then use type, otherwise compare with metatable
-- @param obj An object to check
-- @param tp String of what type it should be
-- @return boolean
-- @usage utils.is_type("hello world", "string")   --> true
-- -- or check metatable
-- local my_mt = {}
-- local my_obj = setmetatable(my_obj, my_mt)
-- utils.is_type(my_obj, my_mt)  --> true
function utils.is_type (obj,tp)
    if type(tp) == 'string' then return type(obj) == tp end
    local mt = getmetatable(obj)
    return tp == mt
end



--- an iterator with indices, similar to `ipairs`, but with a range.
-- This is a nil-safe index based iterator that will return `nil` when there
-- is a hole in a list. To be safe ensure that table `t.n` contains the length.
-- @tparam table t the table to iterate over
-- @tparam[opt=1] integer i_start start index
-- @tparam[opt=t.n or #t] integer i_end end index
-- @tparam[opt=1] integer step step size
-- @treturn integer index
-- @treturn any value at index (which can be `nil`!)
-- @see utils.pack
-- @see utils.unpack
-- @usage
-- local t = utils.pack(nil, 123, nil)  -- adds an `n` field when packing
--
-- for i, v in utils.npairs(t, 2) do  -- start at index 2
--   t[i] = tostring(t[i])
-- end
--
-- -- t = { n = 3, [2] = "123", [3] = "nil" }
function utils.npairs(t, i_start, i_end, step)
  step = step or 1
  if step == 0 then
    error("iterator step-size cannot be 0", 2)
  end
  local i = (i_start or 1) - step
  i_end = i_end or t.n or #t
  if step < 0 then
    return function()
      i = i + step
      if i < i_end then
        return nil
      end
      return i, t[i]
    end

  else
    return function()
      i = i + step
      if i > i_end then
        return nil
      end
      return i, t[i]
    end
  end
end



--- an iterator over all non-integer keys (inverse of `ipairs`).
-- It will skip any key that is an integer number, so negative indices or an
-- array with holes will not return those either (so it returns slightly less than
-- 'the inverse of `ipairs`').
--
-- This uses `pairs` under the hood, so any value that is iterable using `pairs`
-- will work with this function.
-- @tparam table t the table to iterate over
-- @treturn key
-- @treturn value
-- @usage
-- local t = {
--   "hello",
--   "world",
--   hello = "hallo",
--   world = "Welt",
-- }
--
-- for k, v in utils.kpairs(t) do
--   print("German: ", v)
-- end
--
-- -- output;
-- -- German: hallo
-- -- German: Welt
function utils.kpairs(t)
  local index
  return function()
    local value
    while true do
      index, value = next(t, index)
      if type(index) ~= "number" or floor(index) ~= index then
        break
      end
    end
    return index, value
  end
end



--- Error handling
-- @section Error-handling

--- assert that the given argument is in fact of the correct type.
-- @param n argument index
-- @param val the value
-- @param tp the type
-- @param verify an optional verification function
-- @param msg an optional custom message
-- @param lev optional stack position for trace, default 2
-- @return the validated value
-- @raise if `val` is not the correct type
-- @usage
-- local param1 = assert_arg(1,"hello",'table')  --> error: argument 1 expected a 'table', got a 'string'
-- local param4 = assert_arg(4,'!@#$%^&*','string',path.isdir,'not a directory')
--      --> error: argument 4: '!@#$%^&*' not a directory
function utils.assert_arg (n,val,tp,verify,msg,lev)
    if type(val) ~= tp then
        error(("argument %d expected a '%s', got a '%s'"):format(n,tp,type(val)),lev or 2)
    end
    if verify and not verify(val) then
        error(("argument %d: '%s' %s"):format(n,val,msg),lev or 2)
    end
    return val
end

--- creates an Enum or constants lookup table for improved error handling.
-- This helps prevent magic strings in code by throwing errors for accessing
-- non-existing values, and/or converting strings/identifiers to other values.
--
-- Calling on the object does the same, but returns a soft error; `nil + err`, if
-- the call is successful (the key exists), it will return the value.
--
-- When calling with varargs or an array the values will be equal to the keys.
-- The enum object is read-only.
-- @tparam table|vararg ... the input for the Enum. If varargs or an array then the
-- values in the Enum will be equal to the names (must be strings), if a hash-table
-- then values remain (any type), and the keys must be strings.
-- @return Enum object (read-only table/object)
-- @usage -- Enum access at runtime
-- local obj = {}
-- obj.MOVEMENT = utils.enum("FORWARD", "REVERSE", "LEFT", "RIGHT")
--
-- if current_movement == obj.MOVEMENT.FORWARD then
--   -- do something
--
-- elseif current_movement == obj.MOVEMENT.REVERES then
--   -- throws error due to typo 'REVERES', so a silent mistake becomes a hard error
--   -- "'REVERES' is not a valid value (expected one of: 'FORWARD', 'REVERSE', 'LEFT', 'RIGHT')"
--
-- end
-- @usage -- standardized error codes
-- local obj = {
--   ERR = utils.enum {
--     NOT_FOUND = "the item was not found",
--     OUT_OF_BOUNDS = "the index is outside the allowed range"
--   },
--
--   some_method = function(self)
--     return nil, self.ERR.OUT_OF_BOUNDS
--   end,
-- }
--
-- local result, err = obj:some_method()
-- if not result then
--   if err == obj.ERR.NOT_FOUND then
--     -- check on error code, not magic strings
--
--   else
--     -- return the error description, contained in the constant
--     return nil, "error: "..err  -- "error: the index is outside the allowed range"
--   end
-- end
-- @usage -- validating/converting user-input
-- local color = "purple"
-- local ansi_colors = utils.enum {
--   black     = 30,
--   red       = 31,
--   green     = 32,
-- }
-- local color_code, err = ansi_colors(color) -- calling on the object, returns the value from the enum
-- if not color_code then
--   print("bad 'color', " .. err)
--   -- "bad 'color', 'purple' is not a valid value (expected one of: 'black', 'red', 'green')"
--   os.exit(1)
-- end
function utils.enum(...)
  local first = select(1, ...)
  local enum = {}
  local lst

  if type(first) ~= "table" then
    -- vararg with strings
    lst = utils.pack(...)
    for i, value in utils.npairs(lst) do
      utils.assert_arg(i, value, "string")
      enum[value] = value
    end

  else
    -- table/array with values
    utils.assert_arg(1, first, "table")
    lst = {}
    -- first add array part
    for i, value in ipairs(first) do
      if type(value) ~= "string" then
        error(("expected 'string' but got '%s' at index %d"):format(type(value), i), 2)
      end
      lst[i] = value
      enum[value] = value
    end
    -- add key-ed part
    for key, value in utils.kpairs(first) do
      if type(key) ~= "string" then
        error(("expected key to be 'string' but got '%s'"):format(type(key)), 2)
      end
      if enum[key] then
        error(("duplicate entry in array and hash part: '%s'"):format(key), 2)
      end
      enum[key] = value
      lst[#lst+1] = key
    end
  end

  if not lst[1] then
    error("expected at least 1 entry", 2)
  end

  local valid = "(expected one of: '" .. concat(lst, "', '") .. "')"
  setmetatable(enum, {
    __index = function(self, key)
      error(("'%s' is not a valid value %s"):format(tostring(key), valid), 2)
    end,
    __newindex = function(self, key, value)
      error("the Enum object is read-only", 2)
    end,
    __call = function(self, key)
      if type(key) == "string" then
        local v = rawget(self, key)
        if v ~= nil then
          return v
        end
      end
      return nil, ("'%s' is not a valid value %s"):format(tostring(key), valid)
    end
  })

  return enum
end


--- process a function argument.
-- This is used throughout Penlight and defines what is meant by a function:
-- Something that is callable, or an operator string as defined by <code>pl.operator</code>,
-- such as '>' or '#'. If a function factory has been registered for the type, it will
-- be called to get the function.
-- @param idx argument index
-- @param f a function, operator string, or callable object
-- @param msg optional error message
-- @return a callable
-- @raise if idx is not a number or if f is not callable
function utils.function_arg (idx,f,msg)
    utils.assert_arg(1,idx,'number')
    local tp = type(f)
    if tp == 'function' then return f end  -- no worries!
    -- ok, a string can correspond to an operator (like '==')
    if tp == 'string' then
        if not operators then operators = require 'pl.operator'.optable end
        local fn = operators[f]
        if fn then return fn end
        local fn, err = utils.string_lambda(f)
        if not fn then error(err..': '..f) end
        return fn
    elseif tp == 'table' or tp == 'userdata' then
        local mt = getmetatable(f)
        if not mt then error('not a callable object',2) end
        local ff = _function_factories[mt]
        if not ff then
            if not mt.__call then error('not a callable object',2) end
            return f
        else
            return ff(f) -- we have a function factory for this type!
        end
    end
    if not msg then msg = " must be callable" end
    if idx > 0 then
        error("argument "..idx..": "..msg,2)
    else
        error(msg,2)
    end
end


--- assert the common case that the argument is a string.
-- @param n argument index
-- @param val a value that must be a string
-- @return the validated value
-- @raise val must be a string
-- @usage
-- local val = 42
-- local param2 = utils.assert_string(2, val) --> error: argument 2 expected a 'string', got a 'number'
function utils.assert_string (n, val)
    return utils.assert_arg(n,val,'string',nil,nil,3)
end

--- control the error strategy used by Penlight.
-- This is a global setting that controls how `utils.raise` behaves:
--
-- - 'default': return `nil + error` (this is the default)
-- - 'error': throw a Lua error
-- - 'quit': exit the program
--
-- @param mode either 'default', 'quit'  or 'error'
-- @see utils.raise
function utils.on_error (mode)
    mode = tostring(mode)
    if ({['default'] = 1, ['quit'] = 2, ['error'] = 3})[mode] then
      err_mode = mode
    else
      -- fail loudly
      local err = "Bad argument expected string; 'default', 'quit', or 'error'. Got '"..tostring(mode).."'"
      if err_mode == 'default' then
        error(err, 2)  -- even in 'default' mode fail loud in this case
      end
      raise(err)
    end
end

--- used by Penlight functions to return errors. Its global behaviour is controlled
-- by `utils.on_error`.
-- To use this function you MUST use it in conjunction with `return`, since it might
-- return `nil + error`.
-- @param err the error string.
-- @see utils.on_error
-- @usage
-- if some_condition then
--   return utils.raise("some condition was not met")  -- MUST use 'return'!
-- end
function utils.raise (err)
    if err_mode == 'default' then
        return nil, err
    elseif err_mode == 'quit' then
        return utils.quit(err)
    else
        error(err, 2)
    end
end
raise = utils.raise



--- File handling
-- @section files

--- return the contents of a file as a string
-- @param filename The file path
-- @param is_bin open in binary mode
-- @return file contents
function utils.readfile(filename,is_bin)
    local mode = is_bin and 'b' or ''
    utils.assert_string(1,filename)
    local f,open_err = io.open(filename,'r'..mode)
    if not f then return raise (open_err) end
    local res,read_err = f:read('*a')
    f:close()
    if not res then
        -- Errors in io.open have "filename: " prefix,
        -- error in file:read don't, add it.
        return raise (filename..": "..read_err)
    end
    return res
end

--- write a string to a file
-- @param filename The file path
-- @param str The string
-- @param is_bin open in binary mode
-- @return true or nil
-- @return error message
-- @raise error if filename or str aren't strings
function utils.writefile(filename,str,is_bin)
    local mode = is_bin and 'b' or ''
    utils.assert_string(1,filename)
    utils.assert_string(2,str)
    local f,err = io.open(filename,'w'..mode)
    if not f then return raise(err) end
    local ok, write_err = f:write(str)
    f:close()
    if not ok then
        -- Errors in io.open have "filename: " prefix,
        -- error in file:write don't, add it.
        return raise (filename..": "..write_err)
    end
    return true
end

--- return the contents of a file as a list of lines
-- @param filename The file path
-- @return file contents as a table
-- @raise error if filename is not a string
function utils.readlines(filename)
    utils.assert_string(1,filename)
    local f,err = io.open(filename,'r')
    if not f then return raise(err) end
    local res = {}
    for line in f:lines() do
        append(res,line)
    end
    f:close()
    return res
end

--- OS functions
-- @section OS-functions

--- Execute a shell command.
-- This function is a copy of `compat.execute`.
-- @class function
-- @name utils.execute

--- execute a shell command and return the output.
-- This function redirects the output to tempfiles and returns the content of those files.
-- @param cmd a shell command
-- @param bin boolean, if true, read output as binary file
-- @return true if successful
-- @return actual return code
-- @return stdout output (string)
-- @return errout output (string)
function utils.executeex(cmd, bin)
    local outfile = os.tmpname()
    local errfile = os.tmpname()

    if is_windows and not outfile:find(':') then
        outfile = os.getenv('TEMP')..outfile
        errfile = os.getenv('TEMP')..errfile
    end
    cmd = cmd .. " > " .. utils.quote_arg(outfile) .. " 2> " .. utils.quote_arg(errfile)

    local success, retcode = utils.execute(cmd)
    local outcontent = utils.readfile(outfile, bin)
    local errcontent = utils.readfile(errfile, bin)
    os.remove(outfile)
    os.remove(errfile)
    return success, retcode, (outcontent or ""), (errcontent or "")
end

--- Quote and escape an argument of a command.
-- Quotes a single (or list of) argument(s) of a command to be passed
-- to `os.execute`, `pl.utils.execute` or `pl.utils.executeex`.
-- @param argument (string or table/list) the argument to quote. If a list then
-- all arguments in the list will be returned as a single string quoted.
-- @return quoted and escaped argument.
-- @usage
-- local options = utils.quote_arg {
--     "-lluacov",
--     "-e",
--     "utils = print(require('pl.utils')._VERSION",
-- }
-- -- returns: -lluacov -e 'utils = print(require('\''pl.utils'\'')._VERSION'
function utils.quote_arg(argument)
    if type(argument) == "table" then
        -- encode an entire table
        local r = {}
        for i, arg in ipairs(argument) do
            r[i] = utils.quote_arg(arg)
        end

        return concat(r, " ")
    end
    -- only a single argument
    if is_windows then
        if argument == "" or argument:find('[ \f\t\v]') then
            -- Need to quote the argument.
            -- Quotes need to be escaped with backslashes;
            -- additionally, backslashes before a quote, escaped or not,
            -- need to be doubled.
            -- See documentation for CommandLineToArgvW Windows function.
            argument = '"' .. argument:gsub([[(\*)"]], [[%1%1\"]]):gsub([[\+$]], "%0%0") .. '"'
        end

        -- os.execute() uses system() C function, which on Windows passes command
        -- to cmd.exe. Escape its special characters.
        return (argument:gsub('["^<>!|&%%]', "^%0"))
    else
        if argument == "" or argument:find('[^a-zA-Z0-9_@%+=:,./-]') then
            -- To quote arguments on posix-like systems use single quotes.
            -- To represent an embedded single quote close quoted string ('),
            -- add escaped quote (\'), open quoted string again (').
            argument = "'" .. argument:gsub("'", [['\'']]) .. "'"
        end

        return argument
    end
end

--- error out of this program gracefully.
-- @param[opt] code The exit code, defaults to -`1` if omitted
-- @param msg The exit message will be sent to `stderr` (will be formatted with the extra parameters)
-- @param ... extra arguments for message's format'
-- @see utils.fprintf
-- @usage utils.quit(-1, "Error '%s' happened", "42")
-- -- is equivalent to
-- utils.quit("Error '%s' happened", "42")  --> Error '42' happened
function utils.quit(code, msg, ...)
    if type(code) == 'string' then
        utils.fprintf(io.stderr, code, msg, ...)
        io.stderr:write('\n')
        code = -1 -- TODO: this is odd, see the test. Which returns 255 as exit code
    elseif msg then
        utils.fprintf(io.stderr, msg, ...)
        io.stderr:write('\n')
    end
    os.exit(code, true)
end


--- String functions
-- @section string-functions

--- escape any Lua 'magic' characters in a string
-- @param s The input string
function utils.escape(s)
    utils.assert_string(1,s)
    return (s:gsub('[%-%.%+%[%]%(%)%$%^%%%?%*]','%%%1'))
end

--- split a string into a list of strings separated by a delimiter.
-- @param s The input string
-- @param re optional A Lua string pattern; defaults to '%s+'
-- @param plain optional If truthy don't use Lua patterns
-- @param n optional maximum number of elements (if there are more, the last will remain un-split)
-- @return a list-like table
-- @raise error if s is not a string
-- @see splitv
function utils.split(s,re,plain,n)
    utils.assert_string(1,s)
    local i1,ls = 1,{}
    if not re then re = '%s+' end
    if re == '' then return {s} end
    while true do
        local i2,i3 = find(s,re,i1,plain)
        if not i2 then
            local last = sub(s,i1)
            if last ~= '' then append(ls,last) end
            if #ls == 1 and ls[1] == '' then
                return {}
            else
                return ls
            end
        end
        append(ls,sub(s,i1,i2-1))
        if n and #ls == n then
            ls[#ls] = sub(s,i1)
            return ls
        end
        i1 = i3+1
    end
end

--- split a string into a number of return values.
-- Identical to `split` but returns multiple sub-strings instead of
-- a single list of sub-strings.
-- @param s the string
-- @param re A Lua string pattern; defaults to '%s+'
-- @param plain don't use Lua patterns
-- @param n optional maximum number of splits
-- @return n values
-- @usage first,next = splitv('user=jane=doe','=', false, 2)
-- assert(first == "user")
-- assert(next == "jane=doe")
-- @see split
function utils.splitv (s,re, plain, n)
    return _unpack(utils.split(s,re, plain, n))
end


--- Functional
-- @section functional


--- 'memoize' a function (cache returned value for next call).
-- This is useful if you have a function which is relatively expensive,
-- but you don't know in advance what values will be required, so
-- building a table upfront is wasteful/impossible.
-- @param func a function that takes exactly one argument (which later serves as the cache key) and returns a single value
-- @return a function taking one argument and returning a single value either from the cache or by running the original input function
function utils.memoize(func)
    local cache = {}
    return function(k)
        local res = cache[k]
        if res == nil then
            res = func(k)
            cache[k] = res
        end
        return res
    end
end


--- associate a function factory with a type.
-- A function factory takes an object of the given type and
-- returns a function for evaluating it
-- @tab mt metatable
-- @func fun a callable that returns a function
function utils.add_function_factory (mt,fun)
    _function_factories[mt] = fun
end

local function _string_lambda(f)
    if f:find '^|' or f:find '_' then
        local args,body = f:match '|([^|]*)|(.+)'
        if f:find '_' then
            args = '_'
            body = f
        else
            if not args then return raise 'bad string lambda' end
        end
        local fstr = 'return function('..args..') return '..body..' end'
        local fn,err = utils.load(fstr)
        if not fn then return raise(err) end
        fn = fn()
        return fn
    else
        return raise 'not a string lambda'
    end
end


--- an anonymous function as a string. This string is either of the form
-- '|args| expression' or is a function of one argument, '_'
-- @param lf function as a string
-- @return a function
-- @function utils.string_lambda
-- @usage
-- string_lambda '|x|x+1' (2) == 3
-- string_lambda '_+1' (2) == 3
utils.string_lambda = utils.memoize(_string_lambda)


--- bind the first argument of the function to a value.
-- @param fn a function of at least two values (may be an operator string)
-- @param p a value
-- @return a function such that f(x) is fn(p,x)
-- @raise same as @{function_arg}
-- @see func.bind1
-- @usage local function f(msg, name)
--   print(msg .. " " .. name)
-- end
--
-- local hello = utils.bind1(f, "Hello")
--
-- print(hello("world"))     --> "Hello world"
-- print(hello("sunshine"))  --> "Hello sunshine"
function utils.bind1 (fn,p)
    fn = utils.function_arg(1,fn)
    return function(...) return fn(p,...) end
end


--- bind the second argument of the function to a value.
-- @param fn a function of at least two values (may be an operator string)
-- @param p a value
-- @return a function such that f(x) is fn(x,p)
-- @raise same as @{function_arg}
-- @usage local function f(a, b, c)
--   print(a .. " " .. b .. " " .. c)
-- end
--
-- local hello = utils.bind1(f, "world")
--
-- print(hello("Hello", "!"))  --> "Hello world !"
-- print(hello("Bye", "?"))    --> "Bye world ?"
function utils.bind2 (fn,p)
    fn = utils.function_arg(1,fn)
    return function(x,...) return fn(x,p,...) end
end




--- Deprecation
-- @section deprecation

do
  -- the default implementation
  local deprecation_func = function(msg, trace)
    if trace then
      warn(msg, "\n", trace)  -- luacheck: ignore
    else
      warn(msg)  -- luacheck: ignore
    end
  end

  --- Sets a deprecation warning function.
  -- An application can override this function to support proper output of
  -- deprecation warnings. The warnings can be generated from libraries or
  -- functions by calling `utils.raise_deprecation`. The default function
  -- will write to the 'warn' system (introduced in Lua 5.4, or the compatibility
  -- function from the `compat` module for earlier versions).
  --
  -- Note: only applications should set/change this function, libraries should not.
  -- @param func a callback with signature: `function(msg, trace)` both arguments are strings, the latter being optional.
  -- @see utils.raise_deprecation
  -- @usage
  -- -- write to the Nginx logs with OpenResty
  -- utils.set_deprecation_func(function(msg, trace)
  --   ngx.log(ngx.WARN, msg, (trace and (" " .. trace) or nil))
  -- end)
  --
  -- -- disable deprecation warnings
  -- utils.set_deprecation_func()
  function utils.set_deprecation_func(func)
    if func == nil then
      deprecation_func = function() end
    else
      utils.assert_arg(1, func, "function")
      deprecation_func = func
    end
  end

  --- raises a deprecation warning.
  -- For options see the usage example below.
  --
  -- Note: the `opts.deprecated_after` field is the last version in which
  -- a feature or option was NOT YET deprecated! Because when writing the code it
  -- is quite often not known in what version the code will land. But the last
  -- released version is usually known.
  -- @param opts options table
  -- @see utils.set_deprecation_func
  -- @usage
  -- warn("@on")   -- enable Lua warnings, they are usually off by default
  --
  -- function stringx.islower(str)
  --   raise_deprecation {
  --     source = "Penlight " .. utils._VERSION,                   -- optional
  --     message = "function 'islower' was renamed to 'is_lower'", -- required
  --     version_removed = "2.0.0",                                -- optional
  --     deprecated_after = "1.2.3",                               -- optional
  --     no_trace = true,                                          -- optional
  --   }
  --   return stringx.is_lower(str)
  -- end
  -- -- output: "[Penlight 1.9.2] function 'islower' was renamed to 'is_lower' (deprecated after 1.2.3, scheduled for removal in 2.0.0)"
  function utils.raise_deprecation(opts)
    utils.assert_arg(1, opts, "table")
    if type(opts.message) ~= "string" then
      error("field 'message' of the options table must be a string", 2)
    end
    local trace
    if not opts.no_trace then
      trace = debug.traceback("", 2):match("[\n%s]*(.-)$")
    end
    local msg
    if opts.deprecated_after and opts.version_removed then
      msg = (" (deprecated after %s, scheduled for removal in %s)"):format(
        tostring(opts.deprecated_after), tostring(opts.version_removed))
    elseif opts.deprecated_after then
      msg = (" (deprecated after %s)"):format(tostring(opts.deprecated_after))
    elseif opts.version_removed then
      msg = (" (scheduled for removal in %s)"):format(tostring(opts.version_removed))
    else
      msg = ""
    end

    msg = opts.message .. msg

    if opts.source then
      msg = "[" .. opts.source .."] " .. msg
    else
      if msg:sub(1,1) == "@" then
        -- in Lua 5.4 "@" prefixed messages are control messages to the warn system
        error("message cannot start with '@'", 2)
      end
    end

    deprecation_func(msg, trace)
  end

end


return utils



end,

----------------------
-- Modules part end --
----------------------
        }
        if files[path] then
            return files[path]
        else
            return origin_seacher(path)
        end
    end
end
---------------------------------------------------------
----------------Auto generated code block----------------
---------------------------------------------------------
return require('pl.import_into')()