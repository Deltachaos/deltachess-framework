--[[
  Cache unit tests.
  Tests the decorator cache: same-args return cached result, call count, LRU eviction.
]]

local Test = require("test")
local Cache = require("Cache")

Test.suite("Cache", "cache")

--------------------------------------------------------------------------------
-- Basic caching: same args return same result, underlying called once
--------------------------------------------------------------------------------

Test.test("Cache.new: same args return cached result", function()
  local SomeClass = {}
  function SomeClass.Add(a, b)
    return a + b
  end

  local CachedSomeClass = Cache.new(SomeClass)

  Test.assertEq(CachedSomeClass.Add(1, 2), 3, "first call returns 3")
  Test.assertEq(CachedSomeClass.Add(1, 2), 3, "second call returns 3 from cache")
  Test.assertEq(CachedSomeClass.Add(2, 3), 5, "different args return new result")
  Test.assertEq(CachedSomeClass.Add(2, 3), 5, "same args again return cached 5")
end)

Test.test("Cache.new: underlying function called only once per distinct args", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.Add(a, b)
    callCount = callCount + 1
    return a + b
  end

  local CachedSomeClass = Cache.new(SomeClass)

  CachedSomeClass.Add(1, 2)
  CachedSomeClass.Add(1, 2)
  CachedSomeClass.Add(1, 2)
  Test.assertEq(callCount, 1, "Add(1, 2) should have been called once")

  CachedSomeClass.Add(3, 4)
  CachedSomeClass.Add(3, 4)
  Test.assertEq(callCount, 2, "Add(3, 4) should have been called once")

  CachedSomeClass.Add(1, 2)
  Test.assertEq(callCount, 2, "cached Add(1, 2) should not call again")
end)

--------------------------------------------------------------------------------
-- Nil and multiple return values
--------------------------------------------------------------------------------

Test.test("Cache.new: nil return value is cached", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.MaybeNil(x)
    callCount = callCount + 1
    if x == 0 then return nil end
    return x
  end

  local Cached = Cache.new(SomeClass)

  Test.assertNil(Cached.MaybeNil(0), "first call returns nil")
  Test.assertNil(Cached.MaybeNil(0), "second call returns cached nil")
  Test.assertEq(callCount, 1, "MaybeNil(0) should have been called once")
end)

--------------------------------------------------------------------------------
-- Non-function keys pass through
--------------------------------------------------------------------------------

Test.test("Cache.new: non-function keys are passed through", function()
  local SomeClass = {}
  SomeClass.Add = function(a, b) return a + b end
  SomeClass.Constant = 42
  SomeClass.Name = "SomeClass"

  local Cached = Cache.new(SomeClass)

  Test.assertEq(Cached.Constant, 42, "constant passed through")
  Test.assertEq(Cached.Name, "SomeClass", "name passed through")
  Test.assertEq(Cached.Add(1, 2), 3, "method still works")
end)

--------------------------------------------------------------------------------
-- LRU eviction: least recently used is evicted when full
--------------------------------------------------------------------------------

Test.test("Cache.new: LRU eviction when maxSize exceeded", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.Id(x)
    callCount = callCount + 1
    return x
  end

  local Cached = Cache.new(SomeClass, { maxSize = 2 })

  Cached.Id(1)  -- cache: 1
  Cached.Id(2)  -- cache: 1, 2
  Cached.Id(1)  -- 1 is hit, no new call
  Test.assertEq(callCount, 2, "Id(1) and Id(2) each called once")

  Cached.Id(3)  -- cache full; evict LRU (2), add 3. cache: 1, 3
  Test.assertEq(Cached.Id(3), 3, "3 is cached")
  Test.assertEq(callCount, 3, "Id(3) called once")

  -- 1 was used more recently than 2, so 2 was evicted. 3 replaced 2.
  -- Now access 1 (still cached), then 4 to force eviction of 3 (LRU if 1 was accessed after 3).
  Cached.Id(1)
  Cached.Id(4)  -- evict something; then 3 should be evicted (3 was LRU before we called Id(1))
  Test.assertEq(Cached.Id(4), 4, "4 is cached")
  Test.assertEq(Cached.Id(3), 3, "3 was evicted, so Id(3) is called again")
  Test.assertEq(callCount, 5, "Id(3) called again after eviction")
end)

Test.test("Cache.new: custom maxSize option", function()
  local calls = 0
  local SomeClass = {}
  function SomeClass.F(x) calls = calls + 1; return x end

  local Cached = Cache.new(SomeClass, { maxSize = 1 })
  Cached.F(1)
  Cached.F(1)
  Test.assertEq(calls, 1, "one entry cached")
  Cached.F(2)   -- evicts 1
  Test.assertEq(Cached.F(2), 2, "2 cached")
  Test.assertEq(Cached.F(1), 1, "1 was evicted, recomputed")
  Test.assertEq(calls, 3, "F(1), F(2), F(1) again")
end)

--------------------------------------------------------------------------------
-- Different methods cached independently
--------------------------------------------------------------------------------

Test.test("Cache.new: different methods have separate cache entries", function()
  local addCalls, mulCalls = 0, 0
  local SomeClass = {}
  function SomeClass.Add(a, b) addCalls = addCalls + 1; return a + b end
  function SomeClass.Mul(a, b) mulCalls = mulCalls + 1; return a * b end

  local Cached = Cache.new(SomeClass)

  Cached.Add(1, 2)
  Cached.Add(1, 2)
  Cached.Mul(1, 2)
  Cached.Mul(1, 2)
  Test.assertEq(addCalls, 1, "Add called once")
  Test.assertEq(mulCalls, 1, "Mul called once")
  Test.assertEq(Cached.Add(1, 2), 3, "Add result correct")
  Test.assertEq(Cached.Mul(1, 2), 2, "Mul result correct")
end)

--------------------------------------------------------------------------------
-- Optional methods list: only cache specified methods
--------------------------------------------------------------------------------

Test.test("Cache.new: options.methods limits caching to listed methods", function()
  local addCalls, mulCalls, subCalls = 0, 0, 0
  local SomeClass = {}
  function SomeClass.Add(a, b) addCalls = addCalls + 1; return a + b end
  function SomeClass.Mul(a, b) mulCalls = mulCalls + 1; return a * b end
  function SomeClass.Sub(a, b) subCalls = subCalls + 1; return a - b end  -- not in list

  local Cached = Cache.new(SomeClass, { methods = { "Add", "Mul" } })

  Cached.Add(1, 2)
  Cached.Add(1, 2)
  Test.assertEq(addCalls, 1, "Add is cached, called once")
  Test.assertEq(Cached.Add(1, 2), 3, "Add result correct")

  Cached.Mul(2, 3)
  Cached.Mul(2, 3)
  Test.assertEq(mulCalls, 1, "Mul is cached, called once")

  Test.assertEq(Cached.Sub(5, 1), 4, "Sub not in list, works")
  Test.assertEq(Cached.Sub(5, 1), 4, "Sub returns same result")
  Test.assertEq(subCalls, 2, "Sub not in methods list: never cached, called twice")
end)

Test.test("Cache.new: without options.methods all methods are cached", function()
  local calls = 0
  local SomeClass = {}
  function SomeClass.F(x) calls = calls + 1; return x end
  local Cached = Cache.new(SomeClass)
  Cached.F(1)
  Cached.F(1)
  Test.assertEq(calls, 1, "all methods cached when methods option omitted")
end)

--------------------------------------------------------------------------------
-- Table/object args: never cached
--------------------------------------------------------------------------------

Test.test("Cache.new: calls with table argument are never cached", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.WithTable(t)
    callCount = callCount + 1
    return t and (t.x or 0) or -1
  end

  local Cached = Cache.new(SomeClass)
  local t1 = { x = 10 }

  Test.assertEq(Cached.WithTable(t1), 10, "first call")
  Test.assertEq(Cached.WithTable(t1), 10, "second call with same table")
  Test.assertEq(callCount, 2, "with table arg, never cached: called twice")
end)

Test.test("Cache.new: calls with only primitive args are cached", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.PrimitiveOnly(a, b)
    callCount = callCount + 1
    return a + b
  end

  local Cached = Cache.new(SomeClass)
  Cached.PrimitiveOnly(1, 2)
  Cached.PrimitiveOnly(1, 2)
  Test.assertEq(callCount, 1, "primitive-only args are cached")
end)

Test.test("Cache.new: mixed args (table in middle) never cached", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.Mixed(a, t, b)
    callCount = callCount + 1
    return a + (t and t.v or 0) + b
  end

  local Cached = Cache.new(SomeClass)
  local t = { v = 1 }
  Cached.Mixed(1, t, 2)
  Cached.Mixed(1, t, 2)
  Test.assertEq(callCount, 2, "table in args: never cached")
  Test.assertEq(Cached.Mixed(1, t, 2), 4, "result correct")
end)

Test.test("Cache.new: when method returns class-like table (has methods) we do not cache", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.ReturnObject(x)
    callCount = callCount + 1
    return { value = x, method = function() return x end }
  end

  local Cached = Cache.new(SomeClass)
  local r1 = Cached.ReturnObject(1)
  local r2 = Cached.ReturnObject(1)
  Test.assertEq(callCount, 2, "class-like table: never cached, called twice")
  Test.assertEq(r1.value, 1, "first result correct")
  Test.assertEq(r2.value, 1, "second result correct")
end)

Test.test("Cache.new: plain dict/list is cached and returned as new instance", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.ReturnDict(x)
    callCount = callCount + 1
    return { value = x, list = { 1, 2, 3 } }
  end

  local Cached = Cache.new(SomeClass)
  local r1 = Cached.ReturnDict(1)
  local r2 = Cached.ReturnDict(1)
  Test.assertEq(callCount, 1, "plain table is cached, called once")
  Test.assertEq(r1.value, 1, "first result correct")
  Test.assertEq(r2.value, 1, "second result correct")
  r1.value = 99
  r1.list[1] = 99
  Test.assertEq(r2.value, 1, "cached return is immutable: r2 unchanged after mutating r1")
  Test.assertEq(r2.list[1], 1, "nested list also a copy")
  local r3 = Cached.ReturnDict(1)
  Test.assertEq(r3.value, 1, "each call gets a new copy")
  Test.assertEq(r3.list[1], 1, "nested list in new copy unchanged")
end)

Test.test("Cache.new: primitive return is cached", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.ReturnNumber(x)
    callCount = callCount + 1
    return x * 2
  end

  local Cached = Cache.new(SomeClass)
  Test.assertEq(Cached.ReturnNumber(5), 10, "first call")
  Test.assertEq(Cached.ReturnNumber(5), 10, "second call from cache")
  Test.assertEq(callCount, 1, "primitive return is cached")
end)

--------------------------------------------------------------------------------
-- (nil, err) return pattern: never cached so error message is preserved
--------------------------------------------------------------------------------

Test.test("Cache.new: (nil, err) multiple returns are cached and all values returned", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.Parse(x)
    callCount = callCount + 1
    if x == "" or x == nil then return nil, "invalid" end
    return { value = x }
  end

  local Cached = Cache.new(SomeClass)
  local ok1, err1 = Cached.Parse("")
  local ok2, err2 = Cached.Parse("")
  Test.assertNil(ok1, "first call returns nil")
  Test.assertEq(err1, "invalid", "first call returns error message")
  Test.assertNil(ok2, "second call returns nil")
  Test.assertEq(err2, "invalid", "second call returns error message from cache")
  Test.assertEq(callCount, 1, "multi-return (nil, err) is cached, underlying called once")
end)

Test.test("Cache.new: three or more return values cached and returned", function()
  local callCount = 0
  local SomeClass = {}
  function SomeClass.Three(a, b)
    callCount = callCount + 1
    return a, b, a + b, a * b
  end

  local Cached = Cache.new(SomeClass)
  local r1, r2, r3, r4 = Cached.Three(2, 3)
  Test.assertEq(r1, 2, "first return")
  Test.assertEq(r2, 3, "second return")
  Test.assertEq(r3, 5, "third return")
  Test.assertEq(r4, 6, "fourth return")
  local s1, s2, s3, s4 = Cached.Three(2, 3)
  Test.assertEq(s1, 2, "cached first")
  Test.assertEq(s2, 3, "cached second")
  Test.assertEq(s3, 5, "cached third")
  Test.assertEq(s4, 6, "cached fourth")
  Test.assertEq(callCount, 1, "underlying called once")
end)

--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

Test.test("Cache.new: rejects non-table", function()
  Test.assertError(function()
    Cache.new(42)
  end, "Cache.new(42) should error")
  Test.assertError(function()
    Cache.new("module")
  end, "Cache.new(string) should error")
end)
