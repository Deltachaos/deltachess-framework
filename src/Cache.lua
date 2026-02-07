--[[
  Cache - Decorator that wraps an object/module and caches method results.
  Uses a simple in-memory LRU cache: when full, the least recently used entry is evicted.
  Calls with any table/object in the arguments are never cached.
  Plain tables (dicts/lists without methods) are cached; class-like tables (with methods) are not.
  Cached table results are stored and returned as deep copies so entries are immutable.
  Usage:
    local Cached = M.new(SomeModule)
    Cached.Foo(1, 2)  -- calls SomeModule.Foo(1, 2), caches result
    Cached.Foo(1, 2)  -- returns cached result without calling SomeModule.Foo
    -- Optional: only cache specific methods
    local Cached = M.new(SomeModule, { methods = { "Add", "Mul" } })
]]

DeltaChess = DeltaChess or {}
local M = {}

local DEFAULT_MAX_SIZE = 512
local KEY_SEP = "\1"

--- Return true if any argument is a table (object); such calls are never cached.
local function hasTableArg(...)
  local n = select("#", ...)
  for i = 1, n do
    if type(select(i, ...)) == "table" then return true end
  end
  return false
end

--- Return true if t is a class-like table (has any function value, including nested).
local function isClassLike(t, visited)
  if type(t) ~= "table" then return false end
  visited = visited or {}
  if visited[t] then return false end
  visited[t] = true
  for _, v in pairs(t) do
    if type(v) == "function" then return true end
    if type(v) == "table" and isClassLike(v, visited) then return true end
  end
  return false
end

--- Deep copy of a value. Tables are copied recursively; cycles reuse the same copy.
local function deepCopy(x, visited)
  if type(x) ~= "table" then return x end
  visited = visited or {}
  if visited[x] then return visited[x] end
  local out = {}
  visited[x] = out
  for k, v in pairs(x) do
    out[deepCopy(k, visited)] = deepCopy(v, visited)
  end
  return out
end

--- Capture all return values (including nils) and count. Lua 5.1–compatible (no table.pack).
local function pack(...)
  local n = select("#", ...)
  local t = { n = n }
  for i = 1, n do
    t[i] = select(i, ...)
  end
  return t
end

local unpack = table.unpack or unpack

--- Build a cache key from method name and arguments (only when no args are tables).
local function cacheKey(methodName, ...)
  local n = select("#", ...)
  local parts = { methodName }
  for i = 1, n do
    parts[#parts + 1] = tostring(select(i, ...))
  end
  return table.concat(parts, KEY_SEP)
end

--- Create a proxy that caches results of the wrapped object's methods.
--- @param obj table Object or module whose methods should be cached
--- @param options table|nil Optional: { maxSize = number, methods = { "Name1", "Name2" } }
---   maxSize: max cache entries (default 512). methods: if set, only these methods are cached; nil = cache all.
--- @return table Proxy with same interface as obj; method calls are cached by (method, args).
---   Table args are never cached. Plain tables (no methods) are cached as immutable deep copies; class-like tables are not cached.
function M.new(obj, options)
  if type(obj) ~= "table" then
    error("M.new: obj must be a table")
  end
  local maxSize = (options and options.maxSize) or DEFAULT_MAX_SIZE
  local methodsList = options and options.methods
  local methodsToCache = nil  -- nil = cache all; otherwise set of method names
  if methodsList then
    methodsToCache = {}
    for _, name in ipairs(methodsList) do
      methodsToCache[name] = true
    end
  end
  local cache = {}   -- key -> { value = any, order = number }
  local orderCounter = 0

  local function nextOrder()
    orderCounter = orderCounter + 1
    return orderCounter
  end

  --- Return cached value; for tables return a deep copy so cache stays immutable.
  local function get(key)
    local entry = cache[key]
    if not entry then return nil, false end
    entry.order = nextOrder()
    local v = entry.value
    if type(v) == "table" then
      return deepCopy(v), true
    end
    return v, true
  end

  local function set(key, value)
    if not cache[key] then
      local size = 0
      for _ in pairs(cache) do size = size + 1 end
      if size >= maxSize then
        local minKey, minOrder = nil, math.huge
        for k, v in pairs(cache) do
          if v.order < minOrder then minOrder = v.order; minKey = k end
        end
        if minKey then cache[minKey] = nil end
      end
    end
    local toStore = (type(value) == "table") and deepCopy(value) or value
    cache[key] = { value = toStore, order = nextOrder() }
  end

  local proxy = setmetatable({}, {
    __index = function(_, key)
      local original = obj[key]
      if type(original) ~= "function" then
        return original
      end
      if methodsToCache and not methodsToCache[key] then
        return original
      end
      return function(...)
        if hasTableArg(...) then
          return original(...)
        end
        local keyStr = cacheKey(key, ...)
        local value, found = get(keyStr)
        if found then
          if type(value) == "table" and value.n then
            return unpack(deepCopy(value), 1, value.n)
          end
          return value
        end
        local packed = pack(original(...))
        local result = packed[1]
        if type(result) == "table" and isClassLike(result) then
          return unpack(packed, 1, packed.n)
        end
        local stored = { n = packed.n }
        for i = 1, packed.n do
          local v = packed[i]
          stored[i] = (type(v) == "table" and not isClassLike(v)) and deepCopy(v) or v
        end
        set(keyStr, stored)
        return unpack(deepCopy(stored), 1, stored.n)
      end
    end
  })

  return proxy
end

DeltaChess.Cache = M
return M
