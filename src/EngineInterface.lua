-- EngineInterface.lua - Chess engine interface and registry
-- Plug in different chess engines by implementing the interface below.

DeltaChess = DeltaChess or {}
DeltaChess.Engines = DeltaChess.Engines or {}
DeltaChess.Engines.Registry = DeltaChess.Engines.Registry or {}
DeltaChess.Engines.defaultId = nil  -- No longer auto-set; default determined by highest ELO engine

--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------
-- State keys passed to Calculate(): FEN, moves (optional), elo, ply_limit (optional), node_limit (optional), time_limit_ms (optional).
-- Runner sets state.cancelled when the calculation should abort.
--------------------------------------------------------------------------------
-- STATE INTERFACE (contract for engines)
--------------------------------------------------------------------------------
-- Engines receive a state table (stateless; state is passed per calculation):
--
--   state.fen           (string)  - Current position in FEN. Required.
--   state.moves         (table)   - Optional. List of moves that led to this position (UCI or SAN).
--   state.elo           (number)  - Requested difficulty (engine should play near this strength).
--   state.ply_limit     (number)  - Optional. When set, use as max search depth instead of ELO-derived depth.
--   state.node_limit    (number)  - Optional. When set, use as max nodes per search instead of ELO-derived nodes.
--   state.time_limit_ms (number)  - Optional. Max time in ms; engine should try to respect it.
--   state.cancelled     (boolean) - Set by runner. If true, engine should abort and return.
--
--------------------------------------------------------------------------------
-- ENGINE INTERFACE (contract engines must implement)
--------------------------------------------------------------------------------
-- Each engine must be a table with:
--
--   .id          (string)  - Unique identifier, e.g. "beat_highest_piece"
--   .name        (string)  - Display name, e.g. "Beat Highest Piece"
--   .description (string)  - Optional short description
--
--   GetEloRange() -> {min, max}
--     Returns ELO range for difficulty. min, max are numbers.
--
--   GetAverageCpuTime(elo) -> number | nil  (optional)
--     Returns estimated average CPU time in milliseconds for a move at the given ELO.
--     Used to sort engines by efficiency (faster engines listed first).
--     Return nil if unknown or not applicable.
--
--   Calculate(state, loopFn, onComplete)
--     state     - State table (see above). Use state.fen; never mutate state.
--     loopFn    - function(stepFn, doneFn). Loop driver for async iteration.
--                 stepFn() does one unit of work; returns false when done, truthy to continue.
--                 doneFn() is called once after stepFn returns false.
--                 The runner defers between steps (e.g. C_Timer.NewTicker in WoW) so
--                 the call stack stays flat and the UI remains responsive.
--                 Inside stepFn, check state.cancelled to detect abort requests.
--     onComplete - function(resultTable, err). Call when done. Do not return a result.
--                  On success, err is nil and resultTable.move (string, UCI) is required.
--                  Optional fields: .san, .ponder, .depth, .nodes, .score, .mate.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Register a chess engine
--------------------------------------------------------------------------------
function DeltaChess.Engines:Register(engine)
    if not engine or not engine.id or not engine.name or type(engine.GetEloRange) ~= "function" or type(engine.Calculate) ~= "function" then
        return false
    end
    self.Registry[engine.id] = engine
    return true
end

--------------------------------------------------------------------------------
-- Unregister an engine by id
--------------------------------------------------------------------------------
function DeltaChess.Engines:Unregister(id)
    self.Registry[id] = nil
end

--------------------------------------------------------------------------------
-- Get engine by id
--------------------------------------------------------------------------------
function DeltaChess.Engines:Get(id)
    return self.Registry[id or self:GetEffectiveDefaultId()]
end

--------------------------------------------------------------------------------
-- Get default engine id
--------------------------------------------------------------------------------
function DeltaChess.Engines:GetDefaultId()
    return self.defaultId
end

--------------------------------------------------------------------------------
-- Get effective default: defaultId if it exists, else the engine with the highest max ELO, else nil
--------------------------------------------------------------------------------
function DeltaChess.Engines:GetEffectiveDefaultId()
    if self.defaultId and self.Registry[self.defaultId] then
        return self.defaultId
    end
    local list = self:GetEngineList()
    if #list > 0 then
        return list[1].id
    end
    return nil
end

--------------------------------------------------------------------------------
-- Set default engine id
--------------------------------------------------------------------------------
function DeltaChess.Engines:SetDefaultId(id)
    if self.Registry[id] then
        self.defaultId = id
        return true
    end
    return false
end

--------------------------------------------------------------------------------
-- Get all registered engines as {id = engine, ...}
--------------------------------------------------------------------------------
function DeltaChess.Engines:GetAll()
    return self.Registry
end

--------------------------------------------------------------------------------
-- Get ELO range for an engine. Returns {min, max} or nil if engine not found.
--------------------------------------------------------------------------------
function DeltaChess.Engines:GetEloRange(engineId)
    local engine = self:Get(engineId)
    if not engine or type(engine.GetEloRange) ~= "function" then
        return nil
    end
    return engine:GetEloRange()
end

--------------------------------------------------------------------------------
-- Get list of engine ids and display names for UI (sorted by max ELO, strongest first)
--------------------------------------------------------------------------------
function DeltaChess.Engines:GetEngineList()
    local list = {}
    for id, engine in pairs(self.Registry) do
        local eloRange = engine.GetEloRange and engine:GetEloRange()
        local maxElo = eloRange and eloRange[2] or 0
        table.insert(list, {
            id = id,
            name = engine.name or id,
            description = engine.description or "",
            maxElo = maxElo
        })
    end
    table.sort(list, function(a, b)
        return a.maxElo > b.maxElo
    end)
    return list
end

--------------------------------------------------------------------------------
-- Get global ELO range across all engines (min of all mins, max of all maxes).
-- Returns {min, max} or nil if no engines support ELO.
--------------------------------------------------------------------------------
function DeltaChess.Engines:GetGlobalEloRange()
    local globalMin, globalMax = nil, nil
    for id, engine in pairs(self.Registry) do
        local range = engine.GetEloRange and engine:GetEloRange()
        if range then
            local minVal, maxVal = range[1], range[2]
            if not globalMin or minVal < globalMin then
                globalMin = minVal
            end
            if not globalMax or maxVal > globalMax then
                globalMax = maxVal
            end
        end
    end
    if globalMin and globalMax then
        return { globalMin, globalMax }
    end
    return nil
end

--------------------------------------------------------------------------------
-- Get list of engines that support a given ELO value.
-- Returns list sorted by CPU efficiency (fastest first), then by max ELO descending.
--------------------------------------------------------------------------------
function DeltaChess.Engines:GetEnginesForElo(elo)
    local list = {}
    for id, engine in pairs(self.Registry) do
        local range = engine.GetEloRange and engine:GetEloRange()
        local include = false
        local maxElo = 0

        if not range then
            include = true
            maxElo = 0
        else
            local minVal, maxVal = range[1], range[2]
            maxElo = maxVal
            if elo >= minVal and elo <= maxVal then
                include = true
            end
        end

        if include then
            local cpuTime = engine.GetAverageCpuTime and engine:GetAverageCpuTime(elo) or 999999
            table.insert(list, {
                id = id,
                name = engine.name or id,
                description = engine.description or "",
                maxElo = maxElo,
                hasEloSupport = (range ~= nil),
                cpuTime = cpuTime
            })
        end
    end
    table.sort(list, function(a, b)
        if a.hasEloSupport and not b.hasEloSupport then return true end
        if not a.hasEloSupport and b.hasEloSupport then return false end
        if a.cpuTime ~= b.cpuTime then
            return a.cpuTime < b.cpuTime
        end
        return a.maxElo > b.maxElo
    end)
    return list
end

return DeltaChess.Engines
