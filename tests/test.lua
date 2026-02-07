--[[
  Test framework for DeltaChess.
  Provides assertion functions, test runner utilities, snapshot testing, and engine game record helpers.
]]

-- JSON: use DeltaChess.Util (WoW C_EncodingUtil or polyfill) when available, else require("json")

local file = require("file")
local Test = {}

-- Test state
Test.passed = 0
Test.failed = 0
Test.currentSuite = nil
Test.currentSlug = nil
Test.suites = {}
-- Parallel workers: when set, Test.test only registers; runWorkerSlice runs this worker's tests
Test.tests = {}
Test.workerId = tonumber(os.getenv("TEST_WORKER")) or nil
Test.totalWorkers = tonumber(os.getenv("TEST_WORKERS")) or 1
Test.parallelMode = (Test.totalWorkers and Test.totalWorkers > 1)
-- Slug filters: set via command-line args (--only=slug1,slug2 or --ignore=slug1,slug2)
Test.onlySlugs = {}   -- if not empty, only run tests with these slugs
Test.ignoreSlugs = {} -- skip tests with these slugs

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

function Test.assertEq(got, expected, msg)
  if got ~= expected then
    error((msg or "assertion failed") .. ": expected " .. tostring(expected) .. ", got " .. tostring(got))
  end
end

function Test.assertTrue(cond, msg)
  if not cond then error(msg or "expected true, got false") end
end

function Test.assertFalse(cond, msg)
  if cond then error(msg or "expected false, got true") end
end

function Test.assertNil(val, msg)
  if val ~= nil then error((msg or "expected nil") .. ", got " .. tostring(val)) end
end

function Test.assertNotNil(val, msg)
  if val == nil then error(msg or "expected non-nil value, got nil") end
end

function Test.assertError(fn, msg)
  local ok = pcall(fn)
  if ok then error(msg or "expected function to throw an error") end
end

function Test.assertTableEq(got, expected, msg)
  if type(got) ~= "table" or type(expected) ~= "table" then
    error((msg or "assertion failed") .. ": both values must be tables")
  end
  for k, v in pairs(expected) do
    if got[k] ~= v then
      error((msg or "assertion failed") .. ": key '" .. tostring(k) .. "' expected " .. tostring(v) .. ", got " .. tostring(got[k]))
    end
  end
  for k, v in pairs(got) do
    if expected[k] == nil then
      error((msg or "assertion failed") .. ": unexpected key '" .. tostring(k) .. "' with value " .. tostring(v))
    end
  end
end

--------------------------------------------------------------------------------
-- Snapshot testing (one file per assertion; uses file.lua for io/dir)
--------------------------------------------------------------------------------

local UPDATE_SNAPSHOTS = (os.getenv("UPDATE_SNAPSHOTS") or "") == "1"

--- Sanitize snapshot name to a filesystem-safe filename (one file per assertion).
--- Replaces all characters other than alphanumeric, hyphen, and underscore with underscore.
local function snapshotNameToFilename(name)
  if type(name) ~= "string" or name == "" then return "snapshot" end
  local safe = name:gsub("[^%w_-]", "_"):gsub("_+", "_"):gsub("^_", ""):gsub("_$", ""):lower()
  return (safe == "" and "snapshot") or safe
end

--- Assert current value matches snapshot, or create/update snapshot file.
--- snapshotDir: directory for snapshot files (one file per name).
--- name: unique name for this assertion (used as filename).
--- currentValue: string value to compare or save.
--- First run (or new name): creates snapshot file. Later runs: assert currentValue == file content.
--- Run with UPDATE_SNAPSHOTS=1 to overwrite existing snapshot files.
function Test.assertSnapshot(snapshotDir, name, currentValue)
  Test.assertNotNil(snapshotDir, "assertSnapshot: snapshotDir required")
  Test.assertNotNil(name, "assertSnapshot: name required")
  local valueStr = (currentValue == nil and "" or tostring(currentValue))
  local filename = snapshotNameToFilename(name) .. ".txt"
  local path = snapshotDir .. package.config:sub(1, 1) .. filename
  if UPDATE_SNAPSHOTS then
    file.mkdir(snapshotDir)
    file.write(path, valueStr)
    return
  end
  if not file.exists(path) then
    file.mkdir(snapshotDir)
    file.write(path, valueStr)
    return
  end
  local expected, err = file.read(path)
  if err then
    error("assertSnapshot: could not read snapshot '" .. name .. "': " .. tostring(err))
  end
  Test.assertEq(valueStr, expected,
    "Snapshot mismatch for '" .. name .. "': expected " .. tostring(expected) .. ", got " .. tostring(valueStr))
end

--------------------------------------------------------------------------------
-- Test Registration and Running
--------------------------------------------------------------------------------

function Test.test(name, fn)
  if Test.parallelMode then
    table.insert(Test.tests, { name = name, fn = fn, suite = Test.currentSuite, slug = Test.currentSlug })
    return
  end
  -- Skip test if slug filters apply
  if Test.shouldSkipSlug(Test.currentSlug) then
    return
  end
  local ok, err = pcall(fn)
  local prefix = Test.currentSuite and (Test.currentSuite .. ": ") or ""
  if ok then
    print("PASS: " .. prefix .. name)
    Test.passed = Test.passed + 1
  else
    print("FAIL: " .. prefix .. name)
    print("      " .. tostring(err))
    Test.failed = Test.failed + 1
  end
end

function Test.suite(name, slug)
  Test.currentSuite = name
  Test.currentSlug = slug
  table.insert(Test.suites, { name = name, slug = slug })
end

function Test.shouldSkipSlug(slug)
  -- If --only is specified and this slug is not in the list, skip it
  if #Test.onlySlugs > 0 then
    local found = false
    for _, s in ipairs(Test.onlySlugs) do
      if s == slug then
        found = true
        break
      end
    end
    if not found then return true end
  end
  -- If --ignore is specified and this slug is in the list, skip it
  for _, s in ipairs(Test.ignoreSlugs) do
    if s == slug then return true end
  end
  return false
end

function Test.reset()
  Test.passed = 0
  Test.failed = 0
  Test.currentSuite = nil
  Test.currentSlug = nil
  Test.suites = {}
  Test.tests = {}
end

function Test.summary()
  print("")
  print("=====================================")
  print(string.format("Tests: %d passed, %d failed", Test.passed, Test.failed))
  print("=====================================")
  return Test.failed == 0
end

function Test.exit()
  if Test.failed > 0 then
    os.exit(1)
  end
end

--------------------------------------------------------------------------------
-- Test Discovery and Running
--------------------------------------------------------------------------------

-- Run a single test file
function Test.runFile(filepath)
  local chunk, err = loadfile(filepath)
  if not chunk then
    print("ERROR: Could not load " .. filepath)
    print("       " .. tostring(err))
    Test.failed = Test.failed + 1
    return false
  end
  
  local ok, err = pcall(chunk)
  if not ok then
    print("ERROR: Failed to run " .. filepath)
    print("       " .. tostring(err))
    Test.failed = Test.failed + 1
    return false
  end
  
  return true
end

-- Discover test files in a directory (uses file.listDir)
function Test.discoverTests(dir)
  local tests = {}
  local sep = package.config:sub(1, 1)
  local pathForShell = dir:gsub("/", sep):gsub(sep .. "$", "")

  local names, err = file.listDir(pathForShell)
  if not names then return tests end
  for _, name in ipairs(names) do
    if name:match("_test%.lua$") then
      table.insert(tests, pathForShell .. sep .. name)
    end
  end
  table.sort(tests)
  return tests
end

--- Run only this worker's slice of registered tests (index % totalWorkers == workerId).
--- Call after all test files have been loaded in parallel mode.
function Test.runWorkerSlice(workerId, totalWorkers)
  workerId = workerId or Test.workerId or 0
  totalWorkers = totalWorkers or Test.totalWorkers or 1
  for i, t in ipairs(Test.tests) do
    if (i - 1) % totalWorkers == workerId then
      -- Skip test if slug filters apply
      if not Test.shouldSkipSlug(t.slug) then
        local ok, err = pcall(t.fn)
        local prefix = t.suite and (t.suite .. ": ") or ""
        if ok then
          print("PASS: " .. prefix .. t.name)
          Test.passed = Test.passed + 1
        else
          print("FAIL: " .. prefix .. t.name)
          print("      " .. tostring(err))
          Test.failed = Test.failed + 1
        end
      end
    end
  end
end

-- Run all tests in a directory
function Test.runAll(dir)
  local testFiles = Test.discoverTests(dir)

  if #testFiles == 0 then
    print("No test files found in " .. dir)
    return false
  end

  if not Test.parallelMode then
    print("Discovered " .. #testFiles .. " test file(s)")
    print("")
  end

  for _, filepath in ipairs(testFiles) do
    if not Test.parallelMode then
      print("--- Running: " .. filepath .. " ---")
    end
    Test.runFile(filepath)
    if not Test.parallelMode then
      print("")
    end
  end

  if Test.parallelMode then
    -- In parallel mode, tests were only registered; caller runs runWorkerSlice after all dirs
    return true
  end

  return Test.summary()
end

--------------------------------------------------------------------------------
-- Engine helpers (integration tests)
--------------------------------------------------------------------------------

--- Get list of all registered engines, optionally filtered by ALLOWED_ENGINES environment variable.
--- ALLOWED_ENGINES: comma-separated list of engine IDs to include (e.g. "garbochess,dumbgoblin").
function Test.getEngineList()
  if not _G.DeltaChess or not _G.DeltaChess.Engines then
    return {}
  end
  local list = _G.DeltaChess.Engines:GetEngineList()
  if not list or #list == 0 then return {} end
  
  -- Filter by ALLOWED_ENGINES if set
  local allowedEnginesEnv = os.getenv("ALLOWED_ENGINES")
  if allowedEnginesEnv and allowedEnginesEnv ~= "" then
    local allowed = {}
    for engineId in allowedEnginesEnv:gmatch("[^,]+") do
      local trimmed = engineId:gsub("^%s+", ""):gsub("%s+$", ""):lower()
      if trimmed ~= "" then
        allowed[trimmed] = true
      end
    end
    
    local filtered = {}
    for _, engine in ipairs(list) do
      local engineId = (engine.id or ""):lower()
      if allowed[engineId] then
        table.insert(filtered, engine)
      end
    end
    return filtered
  end
  
  return list
end

return Test
