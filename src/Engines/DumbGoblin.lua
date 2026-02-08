--[[
  Dumb Goblin engine: prefers checkmate, then highest-value capture, else first legal move.
  Deterministic (no randomness). Uses DeltaChess.MoveGen. Registers globally: DeltaChess.Engines.BeatHighestPiece
]]

DeltaChess = DeltaChess or {}
DeltaChess.Engines = DeltaChess.Engines or {}
DeltaChess.Engines.Registry = DeltaChess.Engines.Registry or {}

local M = {
    id = "dumbgoblin",
    name = "Dumb Goblin",
    description = "Prefers checkmate, then highest-value capture, else first legal move.",
    author = "Deltachaos",
    url = "https://github.com/Deltachaos/deltachess-engine-framework",
    license = "GPL-3.0"
}

local PIECE_VALUE = {
    P = 1, p = 1,
    N = 3, n = 3,
    B = 3, b = 3,
    R = 5, r = 5,
    Q = 9, q = 9,
    K = 0, k = 0,
}

function M:GetEloRange()
    return { 100, 200 }
end

function M:GetAverageCpuTime(elo)
    return 200
end

local function captureValue(pos, mv)
    local p = pos.board[mv.to]
    if p and p ~= "." then return PIECE_VALUE[p] or 0 end
    if mv.ep then return 1 end
    return 0
end

local function isCheckmate(MoveGen, pos, mv)
    local pos2 = MoveGen.MakeMove(pos, mv)
    if not pos2 then return false end
    local legal = MoveGen.LegalMoves(pos2)
    if #legal ~= 0 then return false end
    return MoveGen.InCheck(pos2)
end

function M:Calculate(state, loopFn, stepFn, onComplete)
    loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
    onComplete = onComplete or function() end
    if state.cancelled then
        onComplete(nil, "cancelled")
        return
    end
    local MoveGen = DeltaChess.MoveGen
    if not MoveGen then
        onComplete(nil, "MoveGen not available")
        return
    end
    local pos = MoveGen.ParseFen(state.fen)
    if not pos then
        onComplete(nil, "invalid FEN")
        return
    end
    local legal = MoveGen.LegalMoves(pos)
    if not legal or #legal == 0 then
        onComplete(nil, "no legal moves")
        return
    end

    -- All work fits in a single step; store result in a local for doneFn.
    local result, err

    loopFn(function()
        -- stepFn: do all work in one step
        if state.cancelled then
            err = "cancelled"
            return false
        end
        local checkmateMoves = {}
        for _, mv in ipairs(legal) do
            if isCheckmate(MoveGen, pos, mv) then
                checkmateMoves[#checkmateMoves + 1] = mv
            end
        end
        local chosen
        if #checkmateMoves > 0 then
            chosen = checkmateMoves[1]
        else
            local bestValue = -1
            local bestMoves = {}
            for _, mv in ipairs(legal) do
                local v = captureValue(pos, mv)
                if v > bestValue then
                    bestValue = v
                    bestMoves = { mv }
                elseif v == bestValue and v > 0 then
                    bestMoves[#bestMoves + 1] = mv
                end
            end
            if bestValue > 0 and #bestMoves > 0 then
                chosen = bestMoves[1]
            else
                chosen = legal[1]
            end
        end
        local uci = MoveGen.MoveToUci(chosen)
        result = {
            move = uci,
            san = uci,
            depth = 0,
            nodes = #legal,
        }
        return false  -- done in one step
    end, function()
        -- doneFn: report result
        onComplete(result, err)
    end)
end

DeltaChess.Engines.Registry[M.id] = M
return M
