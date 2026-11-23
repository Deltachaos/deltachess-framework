-- ZugzugEngine.lua - Alpha-beta minimax chess engine
-- Implements the DeltaChess engine interface for pluggable AI.

local DeltaChess = LibStub("DeltachessFramework-1.0")
DeltaChess.Engines = DeltaChess.Engines or {}
DeltaChess.Engines.Registry = DeltaChess.Engines.Registry or {}

-- Load MoveGen for legal move validation
local MoveGen = DeltaChess.MoveGen

local M = {
    id = "zugzug",
    name = "Zugzug",
    description = "Simple minimax with alpha-beta pruning, iterative deepening, and piece-square tables",
    author = "Deltachaos",
    url = "https://github.com/Deltachaos/deltachess",
    license = "GPL-3.0"
}

function M:GetEloRange()
    return { 100, 1000 }
end

-- Estimated average CPU time in milliseconds for a move at given ELO
-- Minimax is relatively fast at low depths but scales poorly
-- Faster than garbochess until depth 4, then slower
function M:GetAverageCpuTime(elo)
    -- Based on depth used at each ELO level (from eloToParams)
    -- depth 1: ~40ms, depth 2: ~150ms, depth 3: ~600ms, depth 4: ~2800ms
    -- At depth 4, zugzug becomes slower than garbochess at equivalent ELO
    if elo <= 400 then return 40 end       -- depth 1 (faster than garbochess)
    if elo <= 700 then return 600 end      -- depth 2 (faster than garbochess)
    if elo <= 850 then return 800 end      -- depth 3 (faster than garbochess)
    return 2800                            -- depth 4 (slower than garbochess at equivalent ELO)
end

-- Piece values for evaluation
local PIECE_VALUES = {
    P = 100,
    N = 320,
    B = 330,
    R = 500,
    Q = 900,
    K = 20000,
    p = -100,
    n = -320,
    b = -330,
    r = -500,
    q = -900,
    k = -20000
}

-- Position bonuses (white's perspective; flip for black)
local PAWN_TABLE = {
    {0,  0,  0,  0,  0,  0,  0,  0},
    {50, 50, 50, 50, 50, 50, 50, 50},
    {10, 10, 20, 30, 30, 20, 10, 10},
    {5,  5, 10, 25, 25, 10,  5,  5},
    {0,  0,  0, 20, 20,  0,  0,  0},
    {5, -5,-10,  0,  0,-10, -5,  5},
    {5, 10, 10,-20,-20, 10, 10,  5},
    {0,  0,  0,  0,  0,  0,  0,  0}
}

local KNIGHT_TABLE = {
    {-50,-40,-30,-30,-30,-30,-40,-50},
    {-40,-20,  0,  0,  0,  0,-20,-40},
    {-30,  0, 10, 15, 15, 10,  0,-30},
    {-30,  5, 15, 20, 20, 15,  5,-30},
    {-30,  0, 15, 20, 20, 15,  0,-30},
    {-30,  5, 10, 15, 15, 10,  5,-30},
    {-40,-20,  0,  5,  5,  0,-20,-40},
    {-50,-40,-30,-30,-30,-30,-40,-50}
}

local BISHOP_TABLE = {
    {-20,-10,-10,-10,-10,-10,-10,-20},
    {-10,  0,  0,  0,  0,  0,  0,-10},
    {-10,  0,  5, 10, 10,  5,  0,-10},
    {-10,  5,  5, 10, 10,  5,  5,-10},
    {-10,  0, 10, 10, 10, 10,  0,-10},
    {-10, 10, 10, 10, 10, 10, 10,-10},
    {-10,  5,  0,  0,  0,  0,  5,-10},
    {-20,-10,-10,-10,-10,-10,-10,-20}
}

local ROOK_TABLE = {
    {0,  0,  0,  0,  0,  0,  0,  0},
    {5, 10, 10, 10, 10, 10, 10,  5},
    {-5,  0,  0,  0,  0,  0,  0, -5},
    {-5,  0,  0,  0,  0,  0,  0, -5},
    {-5,  0,  0,  0,  0,  0,  0, -5},
    {-5,  0,  0,  0,  0,  0,  0, -5},
    {-5,  0,  0,  0,  0,  0,  0, -5},
    {0,  0,  0,  5,  5,  0,  0,  0}
}

local QUEEN_TABLE = {
    {-20,-10,-10, -5, -5,-10,-10,-20},
    {-10,  0,  0,  0,  0,  0,  0,-10},
    {-10,  0,  5,  5,  5,  5,  0,-10},
    {-5,  0,  5,  5,  5,  5,  0, -5},
    {0,  0,  5,  5,  5,  5,  0, -5},
    {-10,  5,  5,  5,  5,  5,  0,-10},
    {-10,  0,  5,  0,  0,  0,  0,-10},
    {-20,-10,-10, -5, -5,-10,-10,-20}
}

local KING_TABLE = {
    {-30,-40,-40,-50,-50,-40,-40,-30},
    {-30,-40,-40,-50,-50,-40,-40,-30},
    {-30,-40,-40,-50,-50,-40,-40,-30},
    {-30,-40,-40,-50,-50,-40,-40,-30},
    {-20,-30,-30,-40,-40,-30,-30,-20},
    {-10,-20,-20,-20,-20,-20,-20,-10},
    {20, 20,  0,  0,  0,  0, 20, 20},
    {20, 30, 10,  0,  0, 10, 30, 20}
}

local POSITION_TABLES = {
    P = PAWN_TABLE,
    N = KNIGHT_TABLE,
    B = BISHOP_TABLE,
    R = ROOK_TABLE,
    Q = QUEEN_TABLE,
    K = KING_TABLE
}

-- Convert square index (1-64) to rank/file (1-8, 1-8)
local function sqToRankFile(sq)
    local rank = math.floor((sq - 1) / 8) + 1
    local file = ((sq - 1) % 8) + 1
    return rank, file
end

-- Evaluate position using MoveGen position format
-- Always evaluates from white's perspective (positive = good for white)
local function evaluatePosition(pos)
    local score = 0
    local board = pos.board
    
    for sq = 1, 64 do
        local piece = board[sq]
        if piece and piece ~= "." then
            local pieceUpper = piece:upper()
            local rank, file = sqToRankFile(sq)
            local isWhitePiece = (piece == pieceUpper)
            
            -- Piece value (already signed: positive for white, negative for black)
            local pieceValue = PIECE_VALUES[piece] or 0
            
            -- Position bonus
            local positionBonus = 0
            local posTable = POSITION_TABLES[pieceUpper]
            if posTable then
                -- For white pieces, use rank directly; for black, flip rank (9-rank)
                local tableRank = isWhitePiece and (9 - rank) or rank
                local bonus = posTable[tableRank] and posTable[tableRank][file] or 0
                -- White pieces add bonus, black pieces subtract it
                positionBonus = isWhitePiece and bonus or -bonus
            end
            
            score = score + pieceValue + positionBonus
        end
    end
    
    return score
end

-- Order moves by capture value (MVV-LVA: Most Valuable Victim - Least Valuable Attacker)
local function orderMoves(pos, moves)
    local board = pos.board
    local function moveScore(mv)
        local captured = board[mv.to]
        local attacker = board[mv.from]
        if captured and captured ~= "." then
            local victimVal = math.abs(PIECE_VALUES[captured] or 100)
            local attackerVal = math.abs(PIECE_VALUES[attacker] or 100)
            return 10000 + victimVal * 10 - attackerVal
        end
        return 0
    end
    table.sort(moves, function(a, b) return moveScore(a) > moveScore(b) end)
    return moves
end

-- Minimax with alpha-beta pruning
local function minimax(pos, depth, alpha, beta, maximizingPlayer, state)
    local Slice = DeltaChess.Slice
    if Slice then
        Slice.Poll()
        if Slice.Stopped() then
            return evaluatePosition(pos), nil
        end
    end

    if state.cancelled then
        return evaluatePosition(pos), nil
    end
    
    if depth == 0 then
        return evaluatePosition(pos), nil
    end
    
    local moves = MoveGen.LegalMoves(pos)
    if #moves == 0 then
        -- Checkmate or stalemate
        if MoveGen.InCheck(pos) then
            return maximizingPlayer and -100000 or 100000, nil
        else
            return 0, nil
        end
    end
    
    orderMoves(pos, moves)
    local bestMove = nil
    
    if maximizingPlayer then
        local maxEval = -math.huge
        for _, move in ipairs(moves) do
            local newPos = MoveGen.MakeMove(pos, move)
            local eval = minimax(newPos, depth - 1, alpha, beta, false, state)
            if Slice and Slice.Stopped() then
                return evaluatePosition(pos), nil
            end
            if eval > maxEval then maxEval, bestMove = eval, move end
            alpha = math.max(alpha, eval)
            if beta <= alpha then break end
        end
        return maxEval, bestMove
    else
        local minEval = math.huge
        for _, move in ipairs(moves) do
            local newPos = MoveGen.MakeMove(pos, move)
            local eval = minimax(newPos, depth - 1, alpha, beta, true, state)
            if Slice and Slice.Stopped() then
                return evaluatePosition(pos), nil
            end
            if eval < minEval then minEval, bestMove = eval, move end
            beta = math.min(beta, eval)
            if beta <= alpha then break end
        end
        return minEval, bestMove
    end
end

-- Convert ELO to search parameters
local function eloToParams(elo)
    local maxElo = DeltaChess.Engines:GetGlobalEloRange()
    maxElo = maxElo[2]
    elo = math.max(100, math.min(maxElo, tonumber(elo) or 100))
    local depth = math.min(5, math.max(1, math.floor(1 + (elo - 100) / 650)))
    return depth
end

-- Put best move from previous iteration first
local function putFirst(moves, bestMove)
    if not bestMove then return moves end
    local bestUci = MoveGen.MoveToUci(bestMove)
    for i, m in ipairs(moves) do
        if MoveGen.MoveToUci(m) == bestUci then
            table.remove(moves, i)
            table.insert(moves, 1, m)
            break
        end
    end
    return moves
end

-- Main Calculate function implementing the new interface
function M:Calculate(state, loopFn, stepFn, onComplete)
    local Slice = DeltaChess.Slice
    onComplete = onComplete or function() end

    if state.cancelled then
        onComplete(nil, "cancelled")
        return
    end

    local pos, err = MoveGen.ParseFen(state.fen)
    if not pos then
        onComplete(nil, err or "invalid FEN")
        return
    end

    Slice.Run(loopFn, function()
        local isWhite = (pos.stm == "w")
        local maxDepth = eloToParams(state.elo)
        local bestMoveSoFar = nil
        local bestEvalSoFar = nil
        local nodeCount = 0

        for depth = 1, maxDepth do
            if (Slice and Slice.Stopped()) or state.cancelled then
                break
            end

            local currentMoves = MoveGen.LegalMoves(pos)
            if #currentMoves == 0 then
                break
            end

            orderMoves(pos, currentMoves)
            putFirst(currentMoves, bestMoveSoFar)

            local bestMove = nil
            local bestEval = isWhite and -math.huge or math.huge
            local alpha, beta = -math.huge, math.huge

            for _, move in ipairs(currentMoves) do
                if (Slice and Slice.Stopped()) or state.cancelled then
                    break
                end

                local newPos = MoveGen.MakeMove(pos, move)
                local eval
                if depth <= 1 then
                    eval = evaluatePosition(newPos)
                else
                    eval = minimax(newPos, depth - 1, alpha, beta, not isWhite, state)
                end
                if (Slice and Slice.Stopped()) or state.cancelled then
                    break
                end

                nodeCount = nodeCount + 1

                if isWhite then
                    if eval > bestEval then bestEval, bestMove = eval, move end
                    alpha = math.max(alpha, eval)
                else
                    if eval < bestEval then bestEval, bestMove = eval, move end
                    beta = math.min(beta, eval)
                end

                if bestMove then
                    bestMoveSoFar = bestMove
                    bestEvalSoFar = bestEval
                    if Slice then Slice.AllowStop(true) end
                end

                if beta <= alpha then
                    break
                end
            end

            if bestMove then
                bestMoveSoFar = bestMove
                bestEvalSoFar = bestEval
                if Slice then Slice.AllowStop(true) end
            end
        end

        local move = bestMoveSoFar
        if not move then
            local moves = MoveGen.LegalMoves(pos)
            if #moves > 0 then
                move = moves[1]
            else
                return nil, "no legal moves"
            end
        end

        local uci = MoveGen.MoveToUci(move)
        if not uci then
            return nil, "failed to convert move"
        end

        return {
            move = uci,
            san = MoveGen.MoveToSan(pos, move) or uci,
            nodes = nodeCount,
            score = bestEvalSoFar or 0,
            mate = nil
        }, nil
    end, onComplete, state)
end

-- Register this engine when loaded
DeltaChess.Engines.Registry[M.id] = M
return M
