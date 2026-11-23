--[[
  Async runner for chess engines.
  Builder pattern: Create(engineId) then Fen(), Elo(), OnComplete(), etc.,
  then either Run() or Start()+Compute(ms).
  Search runs in a coroutine (DeltaChess.Slice); LoopFn/Compute resume it.
  Registers globally: DeltaChess.EngineRunner
]]

local DeltaChess = LibStub("DeltachessFramework-1.0")
local Engines = DeltaChess.Engines
local Slice = DeltaChess.Slice
local M = {}

local START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
local DEFAULT_TIME_LIMIT_MS = 20000
local DEFAULT_SLICE_MS = 8

--------------------------------------------------------------------------------
-- Default synchronous loopFn: runs all steps in a tight while-loop.
--------------------------------------------------------------------------------
local function syncLoop(step, done)
    while step() ~= false do end
    done()
end

local function syncDelay(next)
    next()
end

--------------------------------------------------------------------------------
-- Builder: Create(engineId) returns a builder. Chain state/options then Run()
-- or Start().
--   :Fen(fen)           - Set position. Default: build from :Moves(), or initial position.
--   :Moves(moves)       - Optional. Moves that led to this position (UCI). Used to build FEN if :Fen() not set.
--   :Elo(elo)           - Optional. Requested difficulty. Default: average of engine's ELO range.
--   :PlyLimit(n)        - Optional. Max search depth (plies). When set, engines use this instead of ELO-derived depth.
--   :NodeLimit(n)       - Optional. Max nodes per search. When set, engines use this instead of ELO-derived nodes.
--   :TimeLimitMs(ms)    - Optional. Max compute time in ms. Default: 20000 (20 seconds).
--   :OnComplete(cb)     - Required before Run(). function(result, err).
--   :LoopFn(fn)         - Optional. loopFn(stepFn, doneFn) loop driver. Default: syncLoop.
--   :DelayFn(fn)        - Optional. function(next) to defer callbacks. Default: call next() immediately.
--   :SliceMs(ms)        - Optional. Milliseconds per LoopFn step. Default: 8 when a LoopFn is set, unlimited otherwise.
--   :HandleError(cb)    - Optional. function(err) called on engine error. If returns false,
--                         error propagates. Otherwise, a random legal move is returned.
--   :UseOpeningBook(on) - Probe LibChessOpeningBook before search. Default: on.
--   :OpeningBooks(ids)  - Optional. Book id or list of ids. Default: auto-select by ELO.
--   :BookDepth(n)       - Optional. Max book ply. Default: derived from ELO.
--   :Run()              - Start calculation and drive it with LoopFn (or a single unlimited slice).
--   :Start()            - Prepare calculation and return a session. Call session:Compute(ms) to search.
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

local function beginCalculation(builder, external)
    local state = builder.state
    state.cancelled = false
    local originalOnComplete = builder.onComplete or function() end
    local rawDelay = builder.delayFn or syncDelay
    local handleError = builder.handleError

    local engine = Engines:Get(builder.engineId)
    if not engine then
        originalOnComplete(nil, "engine not found: " .. tostring(builder.engineId))
        return nil
    end

    if state.elo == nil and engine.GetEloRange then
        local ok, range = pcall(function() return engine:GetEloRange() end)
        if ok and range and range[1] and range[2] then
            state.elo = math.floor((range[1] + range[2]) / 2)
        end
    end

    if state.time_limit_ms == nil then
        state.time_limit_ms = DEFAULT_TIME_LIMIT_MS
    end

    if not state.fen or state.fen == "" then
        if state.moves and #state.moves > 0 then
            state.fen = BuildFenFromMoves(state.moves)
            if not state.fen then
                originalOnComplete(nil, { message = "invalid moves (could not build FEN)", move = nil })
                return nil
            end
        else
            state.fen = START_FEN
        end
    end

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
            syncLoop(function()
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

    local function onComplete(result, err)
        if err and handleError then
            local handlerResult = handleError(err)
            if handlerResult == false then
                originalOnComplete(nil, err)
            else
                local function doFallback()
                    local randomMove = getRandomLegalMove()
                    if randomMove then
                        originalOnComplete({ move = randomMove, san = randomMove, fallback = true }, nil)
                    else
                        originalOnComplete(nil, err)
                    end
                end
                if C_Timer and C_Timer.After then
                    C_Timer.After(1, doFallback)
                else
                    doFallback()
                end
            end
            return
        end
        originalOnComplete(result, err)
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

    local session = {
        state = state,
        external = external,
        completed = false,
        _step = nil,
        _done = nil,
        _finished = false,
    }

    local function wrappedLoopFn(step, doneFn)
        session._step = function()
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
        end
        session._done = function()
            local ok, err = pcall(doneFn)
            if not ok then
                pcall(function()
                    onComplete(nil, { message = "engine error: " .. tostring(err), move = nil })
                end)
            end
        end
    end

    function session:_complete()
        if self._finished then
            return
        end
        self._finished = true
        self.completed = true
        if self._done then
            self._done()
        end
    end

    function session:Compute(ms)
        if self.completed then
            return false
        end
        if not self._step then
            self.completed = true
            return false
        end
        if Slice then
            Slice.SetBudget(ms)
        end
        local more = self._step()
        if more == false then
            self.completed = true
            if self.external then
                self:_complete()
            end
            return false
        end
        return true
    end

    function session:Cancel()
        self.state.cancelled = true
    end

    local useOpeningBook = builder.useOpeningBook
    if useOpeningBook == nil then
        useOpeningBook = true
    end
    if useOpeningBook then
        local OpeningBook = DeltaChess.OpeningBook
        local explicitBooks = builder.openingBooks ~= nil
        if not OpeningBook or not OpeningBook.IsAvailable() then
            if explicitBooks then
                originalOnComplete(nil, "opening book library not available")
                return nil
            end
        else
            local bookIds = builder.openingBooks
            if bookIds ~= nil then
                local missing = OpeningBook.UnknownBookIds(bookIds)
                if missing and #missing > 0 then
                    originalOnComplete(nil, "opening book not found: " .. table.concat(missing, ", "))
                    return nil
                end
            else
                local selected = OpeningBook.GetBooksForElo(state.elo)
                bookIds = {}
                for i = 1, #(selected or {}) do
                    bookIds[i] = selected[i].id
                end
            end
            local depth = builder.bookDepth
            if depth == nil then
                depth = OpeningBook.DepthForElo(state.elo)
            end
            local hit = OpeningBook.SelectMove(state.fen, bookIds, depth)
            if hit then
                local bookSession = {
                    state = state,
                    external = external,
                    completed = true,
                    _finished = true,
                }
                function bookSession:Compute()
                    return false
                end
                function bookSession:Cancel()
                    self.state.cancelled = true
                end
                function bookSession:_complete()
                end
                done({
                    move = hit.move,
                    san = hit.san,
                    book = hit.bookId,
                    weight = hit.weight,
                }, nil)
                return bookSession
            end
        end
    end

    local ok, err = pcall(function()
        engine:Calculate(state, wrappedLoopFn, wrappedStepFn, done)
    end)
    if not ok then
        onComplete(nil, { message = "engine error: " .. tostring(err), move = nil })
        session.completed = true
        return session
    end

    if not session._step then
        session.completed = true
    end

    return session
end

local function Builder(engineId)
    local b = {
        engineId = engineId,
        state = {},
        onComplete = nil,
        loopFn = nil,
        handleError = nil,
        sliceMs = nil,
        useOpeningBook = true,
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
            SliceMs = function(self, ms)
                self.sliceMs = ms
                return self
            end,
            HandleError = function(self, cb)
                self.handleError = cb
                return self
            end,
            UseOpeningBook = function(self, enabled)
                if enabled == nil then
                    self.useOpeningBook = true
                else
                    self.useOpeningBook = not not enabled
                end
                return self
            end,
            OpeningBooks = function(self, idOrList)
                if type(idOrList) == "string" then
                    self.openingBooks = { idOrList }
                else
                    self.openingBooks = idOrList
                end
                return self
            end,
            BookDepth = function(self, n)
                self.bookDepth = n
                return self
            end,
            Start = function(self)
                return beginCalculation(self, true)
            end,
            Run = function(self)
                local session = beginCalculation(self, false)
                if not session or session.completed then
                    return
                end
                local slice = math.huge
                if self.loopFn then
                    slice = self.sliceMs or DEFAULT_SLICE_MS
                end
                local rawLoop = self.loopFn or syncLoop
                rawLoop(function()
                    return session:Compute(slice)
                end, function()
                    session:_complete()
                end)
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
