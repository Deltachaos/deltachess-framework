--[[
  Engine integration tests.
  Data-driven: loads recorded games from tests/integration/__snapshot__/*.json
  and replays them to verify moves are legal and final result/endReason match.
  Run bin/benchmark.lua to generate snapshot files.
]]

local Test = require("test")

Test.suite("Engine integration")

-- Resolve project root for snapshot path
local source = debug.getinfo(1, "S").source:match("@?(.*)")
local dir = (source and source:match("(.*)[/\\]")) or "."
local testsDir = dir:match("(.*)[/\\]")
local projectRoot = (testsDir and testsDir:match("(.*)[/\\]")) or dir
local sep = package.config:sub(1, 1)
local snapshotDir = projectRoot .. sep .. "tests" .. sep .. "integration" .. sep .. "__snapshot__"

local records = Test.loadGameRecordsFromSnapshotDir(snapshotDir)

-- Data-driven tests: one test per record
for _, rec in ipairs(records) do
  local id = rec.id or (rec.whiteEngine .. "_vs_" .. rec.blackEngine)
  Test.test("Recorded game: " .. id, function()
    Test.assertGameRecord(rec)
  end)
end

-- If no records at all, add a placeholder so the suite doesn't look empty
if #records == 0 then
  Test.test("No snapshot data (run bin/benchmark.lua)", function()
    Test.assertTrue(true, "placeholder when no snapshots")
  end)
end
