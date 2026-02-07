--[[
  Fruit 2.1 engine wrapper for the DeltaChess engine interface.
  Uses DeltaChess.LibFruit21:CreateInstance() to create a Fruit 2.1 engine
  instance and wraps its UCI-like command interface for the framework.

  Underlying engine features (Fruit 2.1 by Fabien Letouzey, Lua port by Chessforeva):
  - Alpha-beta search with iterative deepening
  - Transposition tables with Zobrist hashing
  - Null move pruning and forward pruning
  - Late move reductions
  - Sophisticated evaluation (material, piece-square tables, pawn structure,
    king safety, mobility, passed pawns)

  The search uses a step-based loopFn(stepFn, doneFn) that yields between
  iterative-deepening depths so the event loop / UI stays responsive.
  Each depth runs in a flat call (no growing stack) because the loop driver
  calls the step function, which returns, and then the next tick calls it again.
  The loopFn is provided by the EngineRunner.

  Registers globally: DeltaChess.Engines.Registry.fruit21
]]

DeltaChess = DeltaChess or {}
DeltaChess.Engines = DeltaChess.Engines or {}
DeltaChess.Engines.Registry = DeltaChess.Engines.Registry or {}

local M = {
    id = "fruit21",
    name = "Fruit 2.1",
    description = "Highly influential open-source engine featuring alpha-beta search, null-move pruning, late-move reductions, history heuristics, and tapered evaluation.",
    author = "Fabien Letouzey",
    portedBy = "Chessforeva",
    url = "https://github.com/Chessforeva/Lua4chess/blob/master/fruit21chess.lua",
    license = "GPL-3.0"
}

--------------------------------------------------------------------------------
-- Lazy-initialized Fruit 2.1 engine instance.
-- Created once and reused across calculations. The engine's internal ClearAll()
-- (called by do_input("go ...")) resets search state between calculations while
-- keeping the warm transposition table for better performance.
--------------------------------------------------------------------------------
-- local fruit = nil

local function getEngine()
    -- if not fruit then
        fruit = DeltaChess.LibFruit21:CreateInstance(function()
            return DeltaChess.Util.TimeNow()
        end)
        fruit:main()
    -- end
    return fruit
end

--------------------------------------------------------------------------------
-- Engine Interface Implementation
--------------------------------------------------------------------------------

function M:GetEloRange()
    return { 1800, 2600 }
end

function M:GetAverageCpuTime(elo)
    local range = self:GetEloRange()
    local t = (elo - range[1]) / math.max(1, range[2] - range[1])
    return 200 + t * 2000 -- 200ms at ELO 1800 to 2200ms at ELO 2600
end

--- Map state.elo to search depth (ply). Uses state.ply_limit when explicitly set.
function M:GetSearchDepth(state)
    if state.ply_limit ~= nil then
        return state.ply_limit
    end
    return DeltaChess.Util.EloToPly(state.elo, self:GetEloRange(), 2, 10)
end

--------------------------------------------------------------------------------
-- Calculate - main entry point called by the engine runner.
--
-- state     : see EngineInterface.lua for the full state contract.
-- loopFn    : loopFn(stepFn, doneFn) - loop driver for async iteration.
-- onComplete: function(result, err) - call when done.
--------------------------------------------------------------------------------
function M:Calculate(state, loopFn, stepFn, onComplete)
    loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
    onComplete = onComplete or function() end

    if state.cancelled then
        onComplete(nil, "cancelled")
        return
    end

    local engine = getEngine()

    -- Validate FEN
    local fen = state.fen
    if not fen or fen == "" then
        onComplete(nil, "invalid FEN")
        return
    end

    -- Set position: prefer move replay from start for repetition detection,
    -- fall back to FEN if moves are unavailable or replay fails.
    local positionSet = false

    if state.moves and #state.moves > 0 then
        -- state.moves may contain BoardMove objects or plain UCI strings;
        -- extract UCI strings from each entry.
        local uciParts = {}
        local allValid = true
        for _, mv in ipairs(state.moves) do
            local uci
            if type(mv) == "string" then
                uci = mv
            elseif type(mv) == "table" and type(mv.GetUci) == "function" then
                uci = mv:GetUci()
            elseif type(mv) == "table" and type(mv.uci) == "string" then
                uci = mv.uci
            end
            if uci and type(uci) == "string" and #uci >= 4 then
                uciParts[#uciParts + 1] = uci
            else
                allValid = false
                break
            end
        end

        if allValid and #uciParts > 0 then
            local moveStr = table.concat(uciParts, " ")
            local ok = pcall(function()
                engine:do_input("position moves " .. moveStr)
            end)
            if ok then
                positionSet = true
            end
        end
    end

    if not positionSet then
        local ok, err = pcall(function()
            engine:do_input("position fen " .. fen)
        end)
        if not ok then
            onComplete(nil, "position error: " .. tostring(err))
            return
        end
    end

    -- Determine search parameters
    local depth = self:GetSearchDepth(state)

    -- Build go command
    local goCmd
    if state.time_limit_ms then
        -- Fruit21 movetime is in seconds
        local timeSec = math.max(1, math.floor(state.time_limit_ms / 1000))
        goCmd = "go movetime " .. timeSec
    else
        goCmd = "go depth " .. depth
    end

    -- Run search (loopFn yields between iterative-deepening depths)
    local searchOk, searchErr = pcall(function()
        engine:do_input(goCmd, loopFn, stepFn, function()
            -- Search complete - extract results

            -- Extract best move (UCI format, e.g. "e2e4", "e7e8q")
            local bestMove = engine.bestmv
            if not bestMove or bestMove == "" then
                onComplete(nil, "no legal moves")
                return
            end

            -- Build result table
            local result = {
                move = bestMove,
                san  = engine.bestmv2 or bestMove,
                depth = (engine.SearchCurrent and engine.SearchCurrent.depth) or depth,
                nodes = (engine.SearchCurrent and engine.SearchCurrent.node_nb) or 0,
                score = (engine.SearchBest and engine.SearchBest.value) or 0,
            }

            -- Report mate distance if detected
            if engine.SearchCurrent and engine.SearchCurrent.mate
               and engine.SearchCurrent.mate ~= 0 then
                result.mate = engine.SearchCurrent.mate
            end

            onComplete(result, nil)
        end)
    end)

    if not searchOk then
        onComplete(nil, "search error: " .. tostring(searchErr))
        return
    end
end

DeltaChess.Engines.Registry[M.id] = M
return M
