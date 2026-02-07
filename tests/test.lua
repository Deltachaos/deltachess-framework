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
Test.suites = {}
-- Parallel workers: when set, Test.test only registers; runWorkerSlice runs this worker's tests
Test.tests = {}
Test.workerId = tonumber(os.getenv("TEST_WORKER")) or nil
Test.totalWorkers = tonumber(os.getenv("TEST_WORKERS")) or 1
Test.parallelMode = (Test.totalWorkers and Test.totalWorkers > 1)

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
    table.insert(Test.tests, { name = name, fn = fn, suite = Test.currentSuite })
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

function Test.suite(name)
  Test.currentSuite = name
  table.insert(Test.suites, name)
end

function Test.reset()
  Test.passed = 0
  Test.failed = 0
  Test.currentSuite = nil
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
-- Engine game record helpers (integration tests)
--------------------------------------------------------------------------------

--- Build a game record id from white/black engine names.
function Test.recordId(whiteEngine, blackEngine, suffix)
  suffix = suffix or ""
  return (whiteEngine .. "_vs_" .. blackEngine .. suffix):gsub("[^%w_]", "_")
end

--- Replay moves on a fresh board and assert final result and end reason match the record.
function Test.assertGameRecord(record)
  Test.assertNotNil(record.moves, "record must have moves")
  Test.assertNotNil(record.result, "record must have result")
  local Board = DeltaChess.Board
  local Constants = DeltaChess.Constants
  local board = Board.New()
  if record.positions and record.positions[0] then
    Test.assertEq(board:GetFen(), record.positions[0], "initial FEN")
  end
  for i, uci in ipairs(record.moves) do
    local ok, err = board:MakeMoveUci(uci)
    Test.assertNotNil(ok, "move " .. i .. " " .. uci .. " must be legal: " .. tostring(err))
    if record.positions and record.positions[i] then
      local expectedFen = record.positions[i]
      local actualFen = board:GetFen()
      Test.assertEq(actualFen, expectedFen, "FEN after move " .. i .. " " .. uci)
    end
  end
  Test.assertEq(board:GetStatus(), Constants.ENDED, "game must be ended after replay")
  Test.assertEq(board:GetResult(), record.result, "result must match record")
  if record.endReason ~= nil and record.endReason ~= "" then
    Test.assertEq(board:GetEndReason(), record.endReason, "endReason must match record")
  end
end

--- Load game records from a Lua file that returns a table of records.
function Test.loadGameRecords(path)
  local chunk, err = loadfile(path)
  if not chunk then
    error("failed to load game records from " .. tostring(path) .. ": " .. tostring(err))
  end
  local records = chunk()
  if type(records) ~= "table" then
    error("game records file must return a table")
  end
  return Test.normalizeRecordList(records)
end

--- Normalize a table of records into a sorted array (by id).
function Test.normalizeRecordList(records)
  local list = {}
  for k, v in pairs(records) do
    if type(k) == "number" then
      list[#list + 1] = v
    else
      local r = (type(v) == "table") and v or { id = k, data = v }
      if not r.id then r.id = k end
      list[#list + 1] = r
    end
  end
  table.sort(list, function(a, b) return (a.id or "") < (b.id or "") end)
  return list
end

--- Normalize a record from JSON: positions may have string keys ("0", "1", ...); convert to number keys.
function Test.normalizeRecordFromJson(record)
  if not record then return record end
  if record.positions and type(record.positions) == "table" then
    local normalized = {}
    for k, v in pairs(record.positions) do
      local n = tonumber(k)
      if n then normalized[n] = v end
    end
    record.positions = normalized
  end
  return record
end

--- Load one game record from a JSON file (uses file.read and global json).
function Test.loadGameRecordFromJson(path)
  if not _G.json then return nil, "json library not available" end
  local content, err = file.read(path)
  if not content then return nil, err or "file not found" end
  local ok, data = pcall(_G.json.decode, content)
  if not ok then return nil, data end
  return Test.normalizeRecordFromJson(data)
end

--- Load all game records from a directory of JSON snapshot files (*.json).
function Test.loadGameRecordsFromSnapshotDir(dirPath)
  local list = {}
  if not _G.json then return list end
  local sep = package.config:sub(1, 1)
  local dirNormalized = dirPath:gsub("/", sep):gsub(sep .. "$", "")
  local names, err = file.listDir(dirNormalized)
  if not names then return list end
  for _, trimmed in ipairs(names) do
    if trimmed ~= "" and trimmed:match("%.json$") then
      local fullPath = dirNormalized .. sep .. trimmed
      local rec = Test.loadGameRecordFromJson(fullPath)
      if rec then
        rec.id = rec.id or trimmed:gsub("%.json$", "")
        list[#list + 1] = rec
      end
    end
  end
  table.sort(list, function(a, b) return (a.id or "") < (b.id or "") end)
  return list
end

return Test
