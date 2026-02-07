--[[
  Async runner for chess engines. WoW-compatible: no coroutines.
  Builder pattern: Create(engineId) then Fen(), Elo(), OnComplete(), etc., then Run().
  Uses loopFn(stepFn, doneFn) for async iteration with flat call stacks.
  Registers globally: DeltaChess.EngineRunner
]]

DeltaChess = DeltaChess or {}
local Engines = DeltaChess.Engines
local M = {}

local START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
local DEFAULT_TIME_LIMIT_MS = 20000

--------------------------------------------------------------------------------
-- Default synchronous loopFn: runs all steps in a tight while-loop.
-- No stack growth; suitable for tests or non-async environments.
--------------------------------------------------------------------------------
local function syncLoop(step, done)
    while step() ~= false do end
    done()
end

local function syncDelay(next)
    next()
end

--------------------------------------------------------------------------------
-- Builder: Create(engineId) returns a builder. Chain state/options then Run().
--   :Fen(fen)           - Set position. Default: build from :Moves(), or initial position.
--   :Moves(moves)       - Optional. Moves that led to this position (UCI). Used to build FEN if :Fen() not set.
--   :Elo(elo)           - Optional. Requested difficulty. Default: average of engine's ELO range.
--   :PlyLimit(n)        - Optional. Max search depth (plies). When set, engines use this instead of ELO-derived depth.
--   :NodeLimit(n)       - Optional. Max nodes per search. When set, engines use this instead of ELO-derived nodes.
--   :TimeLimitMs(ms)    - Optional. Max time in ms. Default: 20000 (20 seconds).
--   :OnComplete(cb)     - Required before Run(). function(result, err).
--   :LoopFn(fn)         - Optional. loopFn(stepFn, doneFn) loop driver. Default: syncLoop.
--   :Scheduler(fn)      - Deprecated alias for :LoopFn(). Kept for backward compatibility.
--   :HandleError(cb)    - Optional. function(err) called on engine error. If returns false,
--                         error propagates. Otherwise, a random legal move is returned.
--   :Run()              - Start calculation.
--------------------------------------------------------------------------------

local function BuildFenFromMoves(moves)
    local MoveGen = DeltaChess.MoveGen
    if not MoveGen or not moves or #moves == 0 then
        return START_FEN
    end
    local pos = MoveGen.ParseFen(START_FEN)
    if not pos then return START_FEN end
    for _, uci in ipairs(moves) do
        local mv = MoveGen.MoveFromUci(pos, uci)
        if not mv then return nil end
        pos = MoveGen.MakeMove(pos, mv)
        if not pos then return nil end
    end
    return MoveGen.PosToFen(pos)
end

local function Builder(engineId)
    local b = {
        engineId = engineId,
        state = {},
        onComplete = nil,
        loopFn = nil,
        handleError = nil,
    }
    setmetatable(b, {
        __index = {
            Fen = function(self, fen)
                self.state.fen = fen
                return self
            end,
            Moves = function(self, moves)
                self.state.moves = moves
                return self
            end,
            Elo = function(self, elo)
                self.state.elo = elo
                return self
            end,
            PlyLimit = function(self, n)
                self.state.ply_limit = n
                return self
            end,
            NodeLimit = function(self, n)
                self.state.node_limit = n
                return self
            end,
            TimeLimitMs = function(self, ms)
                self.state.time_limit_ms = ms
                return self
            end,
            OnComplete = function(self, cb)
                self.onComplete = cb
                return self
            end,
            LoopFn = function(self, fn)
                self.loopFn = fn
                return self
            end,
            DelayFn = function(self, fn)
                self.delayFn = fn
                return self
            end,
            HandleError = function(self, cb)
                self.handleError = cb
                return self
            end,
            Run = function(self)
                local state = self.state
                state.cancelled = false
                local originalOnComplete = self.onComplete or function() end
                local rawDelay = self.delayFn or syncDelay
                local rawLoop = self.loopFn or syncLoop
                local handleError = self.handleError

                local engine = Engines:Get(self.engineId)
                if not engine then
                    originalOnComplete(nil, "engine not found: " .. tostring(self.engineId))
                    return
                end

                -- Default ELO: average of engine's range
                if state.elo == nil and engine.GetEloRange then
                    local ok, range = pcall(function() return engine:GetEloRange() end)
                    if ok and range and range[1] and range[2] then
                        state.elo = math.floor((range[1] + range[2]) / 2)
                    end
                end

                -- Default time limit: 20 seconds
                if state.time_limit_ms == nil then
                    state.time_limit_ms = DEFAULT_TIME_LIMIT_MS
                end

                -- Default FEN: build from moves, or initial position
                if not state.fen or state.fen == "" then
                    if state.moves and #state.moves > 0 then
                        state.fen = BuildFenFromMoves(state.moves)
                        if not state.fen then
                            originalOnComplete(nil, { message = "invalid moves (could not build FEN)", move = nil })
                            return
                        end
                    else
                        state.fen = START_FEN
                    end
                end

                -- Helper to pick a random legal move
                local function getRandomLegalMove()
                    local MoveGen = DeltaChess.MoveGen
                    if not MoveGen then return nil end
                    local pos = MoveGen.ParseFen(state.fen)
                    if not pos then return nil end
                    local legalMoves = MoveGen.LegalMoves(pos)
                    if not legalMoves or #legalMoves == 0 then return nil end
                    local randomIdx = math.random(1, #legalMoves)
                    local mv = legalMoves[randomIdx]
                    return MoveGen.MoveToUci(mv)
                end

                local delayError = nil

                local function wrappedStepFn(items)
                    if type(items) == "function" then
                        rawDelay(function()
                            local ok, err = pcall(items)
                            if not ok then
                                delayError = err
                            end
                        end)
                    else
                        local i = 1
                        rawLoop(function()
                            if i > #items then return false end

                            local next = items[i]
                            i = i + 1

                            local ok, err = pcall(next)
                            if not ok then
                                delayError = err
                                return false
                            end
                        end, function()
                        end)
                    end
                end

                -- Decorate onComplete to handle errors with HandleError callback
                local function onComplete(result, err)
                    if err and handleError then
                        -- Call the error handler
                        local handlerResult = handleError(err)
                        if handlerResult == false then
                            -- Handler returned false: propagate the error
                            originalOnComplete(nil, err)
                        else
                            -- Handler did not return false: return a random legal move
                            local randomMove = getRandomLegalMove()
                            if randomMove then
                                originalOnComplete({ move = randomMove, san = randomMove, fallback = true }, nil)
                            else
                                -- No legal moves available (checkmate/stalemate)
                                originalOnComplete(nil, err)
                            end
                        end
                        return
                    end
                    -- No error or no handler: pass through
                    originalOnComplete(result, err)
                end

                -- Wrap the raw loopFn with pcall error handling around step and done calls.
                local function wrappedLoopFn(step, done)
                    rawLoop(function()
                        if delayError ~= nil then
                            onComplete(nil, { message = "engine error: " .. tostring(delayError), move = nil })
                            return false
                        end
                        local ok, result = pcall(step)
                        if not ok then
                            onComplete(nil, { message = "engine error: " .. tostring(result), move = nil })
                            return false
                        end
                        return result
                    end, function()
                        local ok, err = pcall(done)
                        if not ok then
                            pcall(function()
                                onComplete(nil, { message = "engine error: " .. tostring(err), move = nil })
                            end)
                        end
                    end)
                end

                local function done(result, err)
                    local ok, callbackErr = pcall(function()
                        if err or not result then
                            onComplete(result, err)
                            return
                        end
                        local move = result.move
                        if not move or type(move) ~= "string" or #move < 4 then
                            onComplete(nil, { message = "engine returned no valid move", move = move })
                            return
                        end
                        local MoveGen = DeltaChess.MoveGen
                        if not MoveGen or not MoveGen.IsLegalMove(state.fen, move) then
                            onComplete(nil, { message = "illegal move", move = move })
                            return
                        end
                        onComplete(result, nil)
                    end)
                    if not ok then
                        pcall(function()
                            onComplete(nil, { message = "callback error: " .. tostring(callbackErr), move = nil })
                        end)
                    end
                end

                local ok, err = pcall(function()
                    engine:Calculate(state, wrappedLoopFn, wrappedDelayFn, done)
                end)
                if not ok then
                    onComplete(nil, { message = "engine error: " .. tostring(err), move = nil })
                end
            end,
        },
    })
    return b
end

function M.Create(engineId)
    return Builder(engineId)
end

DeltaChess.EngineRunner = M
return M
