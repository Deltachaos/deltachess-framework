--[[
  EngineRunner unit tests.
  Tests the async engine runner and its error handling capabilities.
]]

local Test = require("test")

-- Ensure DeltaChess modules are loaded
local MoveGen = DeltaChess.MoveGen
local Engines = DeltaChess.Engines
local EngineRunner = DeltaChess.EngineRunner

Test.suite("EngineRunner")

--------------------------------------------------------------------------------
-- Mock Engine Setup
--------------------------------------------------------------------------------

-- Create a mock engine that we can control for testing
local function createMockEngine(id, behavior)
  return {
    id = id,
    name = "Mock Engine " .. id,
    GetEloRange = function(self)
      return { 1000, 2000 }
    end,
    Calculate = function(self, state, loopFn, onComplete)
      loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
      if behavior.immediate then
        -- Return result immediately
        if behavior.error then
          onComplete(nil, behavior.error)
        elseif behavior.move then
          onComplete({ move = behavior.move, san = behavior.move }, nil)
        else
          onComplete(nil, { message = "no move" })
        end
      else
        -- Use loopFn to simulate async (single step)
        local result_val, err_val
        loopFn(function()
          if behavior.error then
            err_val = behavior.error
          elseif behavior.move then
            result_val = { move = behavior.move, san = behavior.move }
          else
            err_val = { message = "no move" }
          end
          return false
        end, function()
          onComplete(result_val, err_val)
        end)
      end
    end
  }
end

-- Register a mock engine
local function registerMockEngine(id, behavior)
  local engine = createMockEngine(id, behavior)
  DeltaChess.Engines.Registry[id] = engine
  return engine
end

-- Cleanup mock engine
local function unregisterMockEngine(id)
  DeltaChess.Engines.Registry[id] = nil
end

--------------------------------------------------------------------------------
-- Builder Pattern Tests
--------------------------------------------------------------------------------

Test.test("Create returns a builder", function()
  local builder = EngineRunner.Create("test_engine")
  Test.assertNotNil(builder, "Create should return a builder")
end)

Test.test("Builder methods return self for chaining", function()
  local builder = EngineRunner.Create("test_engine")
  local b1 = builder:Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  Test.assertEq(b1, builder, "Fen should return self")
  
  local b2 = builder:Elo(1500)
  Test.assertEq(b2, builder, "Elo should return self")
  
  local b3 = builder:TimeLimitMs(5000)
  Test.assertEq(b3, builder, "TimeLimitMs should return self")
  
  local b4 = builder:OnComplete(function() end)
  Test.assertEq(b4, builder, "OnComplete should return self")
  
  local b5 = builder:LoopFn(function(step, done) while step() ~= false do end; done() end)
  Test.assertEq(b5, builder, "LoopFn should return self")
  
  local b6 = builder:HandleError(function() end)
  Test.assertEq(b6, builder, "HandleError should return self")
end)

--------------------------------------------------------------------------------
-- Engine Not Found Tests
--------------------------------------------------------------------------------

Test.test("Returns error when engine not found", function()
  local completed = false
  local resultVal, errorVal
  
  EngineRunner.Create("nonexistent_engine_12345")
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
      errorVal = err
    end)
    :Run()
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNil(resultVal, "Result should be nil")
  Test.assertNotNil(errorVal, "Error should not be nil")
  Test.assertTrue(type(errorVal) == "string" and errorVal:find("not found"), "Error should mention 'not found'")
end)

--------------------------------------------------------------------------------
-- Successful Calculation Tests
--------------------------------------------------------------------------------

Test.test("Returns move from successful engine calculation", function()
  registerMockEngine("mock_success", { move = "e2e4", immediate = true })
  
  local completed = false
  local resultVal, errorVal
  
  EngineRunner.Create("mock_success")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
      errorVal = err
    end)
    :Run()
  
  unregisterMockEngine("mock_success")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNil(errorVal, "Error should be nil")
  Test.assertNotNil(resultVal, "Result should not be nil")
  Test.assertEq(resultVal.move, "e2e4", "Move should be e2e4")
end)

Test.test("Uses default FEN when none provided", function()
  registerMockEngine("mock_default_fen", { move = "e2e4", immediate = true })
  
  local completed = false
  local resultVal
  
  EngineRunner.Create("mock_default_fen")
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
    end)
    :Run()
  
  unregisterMockEngine("mock_default_fen")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNotNil(resultVal, "Should succeed with default FEN")
end)

Test.test("Uses async loopFn correctly", function()
  registerMockEngine("mock_async", { move = "d2d4", immediate = false })
  
  local loopFnCalled = false
  local completed = false
  local resultVal
  
  EngineRunner.Create("mock_async")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :LoopFn(function(step, done)
      loopFnCalled = true
      while step() ~= false do end
      done()
    end)
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
    end)
    :Run()
  
  unregisterMockEngine("mock_async")
  
  Test.assertTrue(loopFnCalled, "LoopFn should be called")
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNotNil(resultVal, "Result should not be nil")
  Test.assertEq(resultVal.move, "d2d4", "Move should be d2d4")
end)

--------------------------------------------------------------------------------
-- Error Handling Tests
--------------------------------------------------------------------------------

Test.test("Propagates engine error to OnComplete", function()
  registerMockEngine("mock_error", { error = { message = "test error" }, immediate = true })
  
  local completed = false
  local resultVal, errorVal
  
  EngineRunner.Create("mock_error")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
      errorVal = err
    end)
    :Run()
  
  unregisterMockEngine("mock_error")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNil(resultVal, "Result should be nil on error")
  Test.assertNotNil(errorVal, "Error should not be nil")
end)

Test.test("HandleError callback is called on error", function()
  registerMockEngine("mock_error_handler", { error = { message = "engine failed" }, immediate = true })
  
  local handleErrorCalled = false
  local errorReceived = nil
  
  EngineRunner.Create("mock_error_handler")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :HandleError(function(err)
      handleErrorCalled = true
      errorReceived = err
      return true  -- Request fallback
    end)
    :OnComplete(function(result, err)
    end)
    :Run()
  
  unregisterMockEngine("mock_error_handler")
  
  Test.assertTrue(handleErrorCalled, "HandleError should be called")
  Test.assertNotNil(errorReceived, "Error should be passed to HandleError")
end)

Test.test("HandleError returning false propagates error", function()
  registerMockEngine("mock_propagate", { error = { message = "must fail" }, immediate = true })
  
  local completed = false
  local resultVal, errorVal
  
  EngineRunner.Create("mock_propagate")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :HandleError(function(err)
      return false  -- Propagate error, don't use fallback
    end)
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
      errorVal = err
    end)
    :Run()
  
  unregisterMockEngine("mock_propagate")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNil(resultVal, "Result should be nil when error propagates")
  Test.assertNotNil(errorVal, "Error should be passed through")
end)

Test.test("HandleError returning true provides random legal move", function()
  registerMockEngine("mock_fallback", { error = { message = "use fallback" }, immediate = true })
  
  local completed = false
  local resultVal, errorVal
  
  EngineRunner.Create("mock_fallback")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :HandleError(function(err)
      return true  -- Use fallback
    end)
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
      errorVal = err
    end)
    :Run()
  
  unregisterMockEngine("mock_fallback")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNil(errorVal, "Error should be nil when using fallback")
  Test.assertNotNil(resultVal, "Result should not be nil")
  Test.assertNotNil(resultVal.move, "Result should have a move")
  Test.assertTrue(resultVal.fallback == true, "Result should have fallback flag")
  
  -- Verify it's a legal move
  Test.assertTrue(MoveGen.IsLegalMove("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", resultVal.move),
    "Fallback move should be legal")
end)

Test.test("HandleError returning nil (truthy) also provides random legal move", function()
  registerMockEngine("mock_fallback_nil", { error = { message = "use fallback" }, immediate = true })
  
  local completed = false
  local resultVal, errorVal
  
  EngineRunner.Create("mock_fallback_nil")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :HandleError(function(err)
      -- Return nothing (nil), which is not false
    end)
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
      errorVal = err
    end)
    :Run()
  
  unregisterMockEngine("mock_fallback_nil")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNil(errorVal, "Error should be nil when using fallback")
  Test.assertNotNil(resultVal, "Result should not be nil")
  Test.assertTrue(resultVal.fallback == true, "Result should have fallback flag")
end)

--------------------------------------------------------------------------------
-- Illegal Move Detection Tests
--------------------------------------------------------------------------------

Test.test("Rejects illegal move from engine", function()
  -- e2e5 is not legal in starting position
  registerMockEngine("mock_illegal", { move = "e2e5", immediate = true })
  
  local completed = false
  local resultVal, errorVal
  
  EngineRunner.Create("mock_illegal")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
      errorVal = err
    end)
    :Run()
  
  unregisterMockEngine("mock_illegal")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNil(resultVal, "Result should be nil for illegal move")
  Test.assertNotNil(errorVal, "Error should not be nil")
  Test.assertTrue(errorVal.message:find("illegal"), "Error should mention 'illegal'")
end)

Test.test("HandleError works for illegal move errors", function()
  -- e2e5 is not legal in starting position
  registerMockEngine("mock_illegal_handled", { move = "e2e5", immediate = true })
  
  local handleErrorCalled = false
  local completed = false
  local resultVal
  
  EngineRunner.Create("mock_illegal_handled")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :HandleError(function(err)
      handleErrorCalled = true
      return true  -- Use fallback
    end)
    :OnComplete(function(result, err)
      completed = true
      resultVal = result
    end)
    :Run()
  
  unregisterMockEngine("mock_illegal_handled")
  
  Test.assertTrue(handleErrorCalled, "HandleError should be called for illegal move")
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertNotNil(resultVal, "Should get fallback move")
  Test.assertTrue(resultVal.fallback == true, "Should be marked as fallback")
end)

--------------------------------------------------------------------------------
-- ELO Default Tests
--------------------------------------------------------------------------------

Test.test("Uses engine default ELO when not specified", function()
  local eloUsed = nil
  
  -- Create engine that captures the ELO
  DeltaChess.Engines.Registry["mock_elo_check"] = {
    GetEloRange = function(self)
      return { 1200, 1800 }
    end,
    Calculate = function(self, state, loopFn, onComplete)
      eloUsed = state.elo
      onComplete({ move = "e2e4" }, nil)
    end
  }
  
  local completed = false
  
  EngineRunner.Create("mock_elo_check")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :OnComplete(function(result, err)
      completed = true
    end)
    :Run()
  
  unregisterMockEngine("mock_elo_check")
  
  Test.assertTrue(completed, "OnComplete should be called")
  Test.assertEq(eloUsed, 1500, "ELO should be average of range (1500)")
end)

Test.test("Uses provided ELO over default", function()
  local eloUsed = nil
  
  DeltaChess.Engines.Registry["mock_elo_override"] = {
    GetEloRange = function(self)
      return { 1200, 1800 }
    end,
    Calculate = function(self, state, loopFn, onComplete)
      eloUsed = state.elo
      onComplete({ move = "e2e4" }, nil)
    end
  }
  
  EngineRunner.Create("mock_elo_override")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :Elo(1700)
    :OnComplete(function() end)
    :Run()
  
  unregisterMockEngine("mock_elo_override")
  
  Test.assertEq(eloUsed, 1700, "ELO should be the provided value (1700)")
end)

--------------------------------------------------------------------------------
-- Time Limit Tests
--------------------------------------------------------------------------------

Test.test("Uses default time limit when not specified", function()
  local timeLimitUsed = nil
  
  DeltaChess.Engines.Registry["mock_time_default"] = {
    Calculate = function(self, state, loopFn, onComplete)
      timeLimitUsed = state.time_limit_ms
      onComplete({ move = "e2e4" }, nil)
    end
  }
  
  EngineRunner.Create("mock_time_default")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :OnComplete(function() end)
    :Run()
  
  unregisterMockEngine("mock_time_default")
  
  Test.assertEq(timeLimitUsed, 20000, "Time limit should be default (20000ms)")
end)

Test.test("Uses provided time limit", function()
  local timeLimitUsed = nil
  
  DeltaChess.Engines.Registry["mock_time_custom"] = {
    Calculate = function(self, state, loopFn, onComplete)
      timeLimitUsed = state.time_limit_ms
      onComplete({ move = "e2e4" }, nil)
    end
  }
  
  EngineRunner.Create("mock_time_custom")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :TimeLimitMs(5000)
    :OnComplete(function() end)
    :Run()
  
  unregisterMockEngine("mock_time_custom")
  
  Test.assertEq(timeLimitUsed, 5000, "Time limit should be the provided value (5000ms)")
end)

--------------------------------------------------------------------------------
-- Build FEN from Moves Tests
--------------------------------------------------------------------------------

Test.test("Builds FEN from moves when FEN not provided", function()
  local fenUsed = nil
  
  DeltaChess.Engines.Registry["mock_moves_to_fen"] = {
    Calculate = function(self, state, loopFn, onComplete)
      fenUsed = state.fen
      onComplete({ move = "e7e5" }, nil)  -- Legal response after e2e4
    end
  }
  
  EngineRunner.Create("mock_moves_to_fen")
    :Moves({"e2e4"})
    :OnComplete(function() end)
    :Run()
  
  unregisterMockEngine("mock_moves_to_fen")
  
  Test.assertNotNil(fenUsed, "FEN should be generated from moves")
  -- After e2e4, it's black's turn
  Test.assertTrue(fenUsed:find(" b "), "FEN should show black to move after e2e4")
end)
