--[[
  EngineInterface unit tests.
  Tests the engine registry and query methods.
]]

local Test = require("test")

local Engines = DeltaChess.Engines

Test.suite("EngineInterface", "engineinterface")

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Create a minimal valid mock engine
local function createMockEngine(opts)
  opts = opts or {}
  return {
    id = opts.id or "mock_engine",
    name = opts.name or "Mock Engine",
    description = opts.description or "A mock engine for testing",
    GetEloRange = opts.GetEloRange or function(self)
      return { opts.minElo or 1000, opts.maxElo or 2000 }
    end,
    GetAverageCpuTime = opts.GetAverageCpuTime,
    Calculate = opts.Calculate or function(self, state, loopFn, onComplete)
      onComplete({ move = "e2e4" }, nil)
    end
  }
end

-- Save and restore registry state for isolation
local savedRegistry = {}
local savedDefaultId = nil

local function saveRegistryState()
  savedRegistry = {}
  for id, engine in pairs(Engines.Registry) do
    savedRegistry[id] = engine
  end
  savedDefaultId = Engines.defaultId
end

local function restoreRegistryState()
  -- Clear current registry
  for id in pairs(Engines.Registry) do
    Engines.Registry[id] = nil
  end
  -- Restore saved state
  for id, engine in pairs(savedRegistry) do
    Engines.Registry[id] = engine
  end
  Engines.defaultId = savedDefaultId
end

local function clearRegistry()
  for id in pairs(Engines.Registry) do
    Engines.Registry[id] = nil
  end
  Engines.defaultId = nil
end

--------------------------------------------------------------------------------
-- Register Tests
--------------------------------------------------------------------------------

Test.test("Register: accepts valid engine", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "test_valid" })
  local result = Engines:Register(engine)
  
  Test.assertTrue(result, "Register should return true for valid engine")
  Test.assertNotNil(Engines.Registry["test_valid"], "Engine should be in registry")
  
  restoreRegistryState()
end)

Test.test("Register: rejects nil engine", function()
  saveRegistryState()
  
  local result = Engines:Register(nil)
  
  Test.assertFalse(result, "Register should return false for nil engine")
  
  restoreRegistryState()
end)

Test.test("Register: rejects engine without id", function()
  saveRegistryState()
  
  local engine = createMockEngine()
  engine.id = nil
  local result = Engines:Register(engine)
  
  Test.assertFalse(result, "Register should return false for engine without id")
  
  restoreRegistryState()
end)

Test.test("Register: rejects engine without name", function()
  saveRegistryState()
  
  local engine = createMockEngine()
  engine.name = nil
  local result = Engines:Register(engine)
  
  Test.assertFalse(result, "Register should return false for engine without name")
  
  restoreRegistryState()
end)

Test.test("Register: rejects engine without GetEloRange", function()
  saveRegistryState()
  
  local engine = createMockEngine()
  engine.GetEloRange = nil
  local result = Engines:Register(engine)
  
  Test.assertFalse(result, "Register should return false for engine without GetEloRange")
  
  restoreRegistryState()
end)

Test.test("Register: rejects engine without Calculate", function()
  saveRegistryState()
  
  local engine = createMockEngine()
  engine.Calculate = nil
  local result = Engines:Register(engine)
  
  Test.assertFalse(result, "Register should return false for engine without Calculate")
  
  restoreRegistryState()
end)

Test.test("Register: overwrites existing engine with same id", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "overwrite_test", name = "Engine 1" })
  local engine2 = createMockEngine({ id = "overwrite_test", name = "Engine 2" })
  
  Engines:Register(engine1)
  Engines:Register(engine2)
  
  Test.assertEq(Engines.Registry["overwrite_test"].name, "Engine 2", "Second engine should overwrite first")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- Unregister Tests
--------------------------------------------------------------------------------

Test.test("Unregister: removes engine from registry", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "to_unregister" })
  Engines:Register(engine)
  Test.assertNotNil(Engines.Registry["to_unregister"], "Engine should be registered")
  
  Engines:Unregister("to_unregister")
  Test.assertNil(Engines.Registry["to_unregister"], "Engine should be removed")
  
  restoreRegistryState()
end)

Test.test("Unregister: handles non-existent id gracefully", function()
  saveRegistryState()
  
  -- Should not throw
  Engines:Unregister("nonexistent_engine_xyz")
  Test.assertTrue(true, "Unregister should not throw for non-existent id")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- Get Tests
--------------------------------------------------------------------------------

Test.test("Get: returns engine by id", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "get_test", name = "Get Test Engine" })
  Engines:Register(engine)
  
  local retrieved = Engines:Get("get_test")
  Test.assertNotNil(retrieved, "Should return the engine")
  Test.assertEq(retrieved.name, "Get Test Engine", "Should return correct engine")
  
  restoreRegistryState()
end)

Test.test("Get: returns nil for non-existent id", function()
  saveRegistryState()
  clearRegistry()
  
  local retrieved = Engines:Get("nonexistent_xyz")
  Test.assertNil(retrieved, "Should return nil for non-existent id")
  
  restoreRegistryState()
end)

Test.test("Get: returns default engine when id is nil", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "default_test", maxElo = 2500 })
  Engines:Register(engine)
  Engines:SetDefaultId("default_test")
  
  local retrieved = Engines:Get(nil)
  Test.assertNotNil(retrieved, "Should return default engine when id is nil")
  Test.assertEq(retrieved.id, "default_test", "Should return the default engine")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- GetDefaultId / SetDefaultId Tests
--------------------------------------------------------------------------------

Test.test("GetDefaultId: returns nil initially", function()
  saveRegistryState()
  clearRegistry()
  
  local defaultId = Engines:GetDefaultId()
  Test.assertNil(defaultId, "Default should be nil when not set")
  
  restoreRegistryState()
end)

Test.test("SetDefaultId: sets default for existing engine", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "set_default_test" })
  Engines:Register(engine)
  
  local result = Engines:SetDefaultId("set_default_test")
  Test.assertTrue(result, "Should return true for existing engine")
  Test.assertEq(Engines:GetDefaultId(), "set_default_test", "Default should be set")
  
  restoreRegistryState()
end)

Test.test("SetDefaultId: returns false for non-existent engine", function()
  saveRegistryState()
  clearRegistry()
  
  local result = Engines:SetDefaultId("nonexistent_engine")
  Test.assertFalse(result, "Should return false for non-existent engine")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- GetEffectiveDefaultId Tests
--------------------------------------------------------------------------------

Test.test("GetEffectiveDefaultId: returns defaultId if set and exists", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "eff_test_1", maxElo = 1500 })
  local engine2 = createMockEngine({ id = "eff_test_2", maxElo = 2500 })
  Engines:Register(engine1)
  Engines:Register(engine2)
  Engines:SetDefaultId("eff_test_1")
  
  local effective = Engines:GetEffectiveDefaultId()
  Test.assertEq(effective, "eff_test_1", "Should return explicitly set default")
  
  restoreRegistryState()
end)

Test.test("GetEffectiveDefaultId: returns highest ELO engine if no default set", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "eff_high", maxElo = 2500 })
  local engine2 = createMockEngine({ id = "eff_low", maxElo = 1500 })
  Engines:Register(engine1)
  Engines:Register(engine2)
  
  local effective = Engines:GetEffectiveDefaultId()
  Test.assertEq(effective, "eff_high", "Should return highest ELO engine")
  
  restoreRegistryState()
end)

Test.test("GetEffectiveDefaultId: returns nil if no engines registered", function()
  saveRegistryState()
  clearRegistry()
  
  local effective = Engines:GetEffectiveDefaultId()
  Test.assertNil(effective, "Should return nil with no engines")
  
  restoreRegistryState()
end)

Test.test("GetEffectiveDefaultId: falls back if default engine was unregistered", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "fb_test_1", maxElo = 2000 })
  local engine2 = createMockEngine({ id = "fb_test_2", maxElo = 1500 })
  Engines:Register(engine1)
  Engines:Register(engine2)
  Engines:SetDefaultId("fb_test_1")
  Engines:Unregister("fb_test_1")
  
  local effective = Engines:GetEffectiveDefaultId()
  Test.assertEq(effective, "fb_test_2", "Should fall back to remaining engine")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- GetAll Tests
--------------------------------------------------------------------------------

Test.test("GetAll: returns registry table", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "all_test_1" })
  local engine2 = createMockEngine({ id = "all_test_2" })
  Engines:Register(engine1)
  Engines:Register(engine2)
  
  local all = Engines:GetAll()
  Test.assertNotNil(all["all_test_1"], "Should contain first engine")
  Test.assertNotNil(all["all_test_2"], "Should contain second engine")
  
  restoreRegistryState()
end)

Test.test("GetAll: returns empty table when no engines", function()
  saveRegistryState()
  clearRegistry()
  
  local all = Engines:GetAll()
  local count = 0
  for _ in pairs(all) do count = count + 1 end
  Test.assertEq(count, 0, "Should be empty")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- GetEloRange Tests
--------------------------------------------------------------------------------

Test.test("GetEloRange: returns range for existing engine", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "elo_range_test", minElo = 800, maxElo = 2200 })
  Engines:Register(engine)
  
  local range = Engines:GetEloRange("elo_range_test")
  Test.assertNotNil(range, "Should return range")
  Test.assertEq(range[1], 800, "Min ELO should be 800")
  Test.assertEq(range[2], 2200, "Max ELO should be 2200")
  
  restoreRegistryState()
end)

Test.test("GetEloRange: returns nil for non-existent engine", function()
  saveRegistryState()
  clearRegistry()
  
  local range = Engines:GetEloRange("nonexistent_xyz")
  Test.assertNil(range, "Should return nil for non-existent engine")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- GetEngineList Tests
--------------------------------------------------------------------------------

Test.test("GetEngineList: returns list sorted by max ELO descending", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "list_low", name = "Low ELO", maxElo = 1000 })
  local engine2 = createMockEngine({ id = "list_high", name = "High ELO", maxElo = 2500 })
  local engine3 = createMockEngine({ id = "list_mid", name = "Mid ELO", maxElo = 1800 })
  Engines:Register(engine1)
  Engines:Register(engine2)
  Engines:Register(engine3)
  
  local list = Engines:GetEngineList()
  Test.assertEq(#list, 3, "Should have 3 engines")
  Test.assertEq(list[1].id, "list_high", "First should be highest ELO")
  Test.assertEq(list[2].id, "list_mid", "Second should be mid ELO")
  Test.assertEq(list[3].id, "list_low", "Third should be lowest ELO")
  
  restoreRegistryState()
end)

Test.test("GetEngineList: includes all engine info", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({
    id = "list_info_test",
    name = "Info Test",
    description = "Test description",
    maxElo = 1500
  })
  Engines:Register(engine)
  
  local list = Engines:GetEngineList()
  Test.assertEq(list[1].id, "list_info_test", "Should have id")
  Test.assertEq(list[1].name, "Info Test", "Should have name")
  Test.assertEq(list[1].description, "Test description", "Should have description")
  Test.assertEq(list[1].maxElo, 1500, "Should have maxElo")
  
  restoreRegistryState()
end)

Test.test("GetEngineList: returns empty list when no engines", function()
  saveRegistryState()
  clearRegistry()
  
  local list = Engines:GetEngineList()
  Test.assertEq(#list, 0, "Should be empty list")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- GetGlobalEloRange Tests
--------------------------------------------------------------------------------

Test.test("GetGlobalEloRange: returns min of mins and max of maxes", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "global_1", minElo = 800, maxElo = 1500 })
  local engine2 = createMockEngine({ id = "global_2", minElo = 1200, maxElo = 2500 })
  local engine3 = createMockEngine({ id = "global_3", minElo = 1000, maxElo = 2000 })
  Engines:Register(engine1)
  Engines:Register(engine2)
  Engines:Register(engine3)
  
  local range = Engines:GetGlobalEloRange()
  Test.assertNotNil(range, "Should return range")
  Test.assertEq(range[1], 800, "Global min should be 800 (lowest min)")
  Test.assertEq(range[2], 2500, "Global max should be 2500 (highest max)")
  
  restoreRegistryState()
end)

Test.test("GetGlobalEloRange: returns nil when no engines", function()
  saveRegistryState()
  clearRegistry()
  
  local range = Engines:GetGlobalEloRange()
  Test.assertNil(range, "Should return nil with no engines")
  
  restoreRegistryState()
end)

Test.test("GetGlobalEloRange: works with single engine", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "single_global", minElo = 1000, maxElo = 2000 })
  Engines:Register(engine)
  
  local range = Engines:GetGlobalEloRange()
  Test.assertEq(range[1], 1000, "Min should match single engine")
  Test.assertEq(range[2], 2000, "Max should match single engine")
  
  restoreRegistryState()
end)

--------------------------------------------------------------------------------
-- GetEnginesForElo Tests
--------------------------------------------------------------------------------

Test.test("GetEnginesForElo: returns engines that support the ELO", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({ id = "elo_match", minElo = 1000, maxElo = 2000 })
  local engine2 = createMockEngine({ id = "elo_nomatch", minElo = 2100, maxElo = 2500 })
  Engines:Register(engine1)
  Engines:Register(engine2)
  
  local list = Engines:GetEnginesForElo(1500)
  Test.assertEq(#list, 1, "Should return only matching engine")
  Test.assertEq(list[1].id, "elo_match", "Should be the matching engine")
  
  restoreRegistryState()
end)

Test.test("GetEnginesForElo: includes boundary values", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "boundary_test", minElo = 1000, maxElo = 2000 })
  Engines:Register(engine)
  
  local listMin = Engines:GetEnginesForElo(1000)
  Test.assertEq(#listMin, 1, "Should include engine at min boundary")
  
  local listMax = Engines:GetEnginesForElo(2000)
  Test.assertEq(#listMax, 1, "Should include engine at max boundary")
  
  restoreRegistryState()
end)

Test.test("GetEnginesForElo: sorted by CPU time then max ELO", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({
    id = "cpu_slow",
    minElo = 1000, maxElo = 2000,
    GetAverageCpuTime = function() return 500 end
  })
  local engine2 = createMockEngine({
    id = "cpu_fast",
    minElo = 1000, maxElo = 1800,
    GetAverageCpuTime = function() return 100 end
  })
  Engines:Register(engine1)
  Engines:Register(engine2)
  
  local list = Engines:GetEnginesForElo(1500)
  Test.assertEq(#list, 2, "Both engines should match")
  Test.assertEq(list[1].id, "cpu_fast", "Faster engine should be first")
  Test.assertEq(list[2].id, "cpu_slow", "Slower engine should be second")
  
  restoreRegistryState()
end)

Test.test("GetEnginesForElo: engines without ELO support are included but sorted last", function()
  saveRegistryState()
  clearRegistry()
  
  local engineWithElo = createMockEngine({
    id = "with_elo",
    minElo = 1000, maxElo = 2000
  })
  local engineWithoutElo = createMockEngine({ id = "without_elo" })
  engineWithoutElo.GetEloRange = function() return nil end
  Engines:Register(engineWithElo)
  Engines:Register(engineWithoutElo)
  
  local list = Engines:GetEnginesForElo(1500)
  Test.assertEq(#list, 2, "Both should be included")
  Test.assertEq(list[1].id, "with_elo", "Engine with ELO support should be first")
  Test.assertEq(list[2].id, "without_elo", "Engine without ELO support should be last")
  Test.assertTrue(list[1].hasEloSupport, "First should have ELO support flag")
  Test.assertFalse(list[2].hasEloSupport, "Second should not have ELO support flag")
  
  restoreRegistryState()
end)

Test.test("GetEnginesForElo: returns empty list when no match", function()
  saveRegistryState()
  clearRegistry()
  
  local engine = createMockEngine({ id = "no_match", minElo = 2000, maxElo = 2500 })
  Engines:Register(engine)
  
  local list = Engines:GetEnginesForElo(1000)
  Test.assertEq(#list, 0, "Should return empty list when ELO out of range")
  
  restoreRegistryState()
end)

Test.test("GetEnginesForElo: same CPU time sorts by max ELO descending", function()
  saveRegistryState()
  clearRegistry()
  
  local engine1 = createMockEngine({
    id = "same_cpu_low",
    minElo = 1000, maxElo = 1500,
    GetAverageCpuTime = function() return 200 end
  })
  local engine2 = createMockEngine({
    id = "same_cpu_high",
    minElo = 1000, maxElo = 2000,
    GetAverageCpuTime = function() return 200 end
  })
  Engines:Register(engine1)
  Engines:Register(engine2)
  
  local list = Engines:GetEnginesForElo(1200)
  Test.assertEq(list[1].id, "same_cpu_high", "Higher max ELO should be first when CPU time is same")
  Test.assertEq(list[2].id, "same_cpu_low", "Lower max ELO should be second")
  
  restoreRegistryState()
end)
