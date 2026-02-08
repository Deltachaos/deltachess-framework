--[[
  GarboChess engine: Ported from JavaScript by Gary Linscott
  Lua port by Chessforeva, adapted to DeltaChess engine interface.

  Features:
  - Alpha-beta pruning with iterative deepening
  - Transposition tables with Zobrist hashing
  - Quiescence search
  - Null move pruning
  - Late move reductions
  - Killer moves and history heuristic
  - Static Exchange Evaluation (SEE)
  - Piece-square tables and mobility evaluation

  Registers globally: DeltaChess.Engines.Registry.garbochess
]]

DeltaChess = DeltaChess or {}
DeltaChess.Engines = DeltaChess.Engines or {}
DeltaChess.Engines.Registry = DeltaChess.Engines.Registry or {}

local M = {
    id = "garbochess",
    name = "GarboChess",
    description = "Built around classic search algorithms, bitboard-based move generation, and a traditional, positional evaluation function",
    author = "Gary Linscott",
    portedBy = "Chessforeva",
    url = "https://github.com/glinscott/Garbochess-JS",
    license = "BSD-3-Clause"
}

local bit = DeltaChess.Util.bit

--------------------------------------------------------------------------------
-- GarboChess Engine State (encapsulated to avoid globals)
--------------------------------------------------------------------------------
local function createEngineState()
    local S = {}

    -- Configuration (g_timeout nil = no time limit; set in Calculate when state.time_limit_ms is provided)
    S.g_timeout = nil
    S.g_maxfinCnt = 100000
    S.g_startTime = 0
    S.g_finCnt = 0
    S.g_foundmove = 0

    -- Piece/color constants
    S.colorBlack = 0x10
    S.colorWhite = 0x08
    S.pieceEmpty = 0x00
    S.piecePawn = 0x01
    S.pieceKnight = 0x02
    S.pieceBishop = 0x03
    S.pieceRook = 0x04
    S.pieceQueen = 0x05
    S.pieceKing = 0x06

    -- Move deltas
    S.g_vectorDelta = {}
    S.g_bishopDeltas = {-15, -17, 15, 17}
    S.g_knightDeltas = {31, 33, 14, -14, -31, -33, 18, -18}
    S.g_rookDeltas = {-1, 1, -16, 16}
    S.g_queenDeltas = {-1, 1, -15, 15, -17, 17, -16, 16}

    S.g_seeValues = {0, 1, 3, 3, 5, 9, 900, 0, 0, 1, 3, 3, 5, 9, 900, 0}

    S.g_castleRightsMask = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 7,15,15,15, 3,15,15,11, 0, 0, 0, 0,
        0, 0, 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0,
        0, 0, 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0,
        0, 0, 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0,
        0, 0, 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0,
        0, 0, 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0,
        0, 0, 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0,
        0, 0, 0, 0,13,15,15,15,12,15,15,14, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }

    S.moveflagEPC = bit.lshift(0x2, 16)
    S.moveflagCastleKing = bit.lshift(0x4, 16)
    S.moveflagCastleQueen = bit.lshift(0x8, 16)
    S.moveflagPromotion = bit.lshift(0x10, 16)
    S.moveflagPromoteKnight = bit.lshift(0x20, 16)
    S.moveflagPromoteQueen = bit.lshift(0x40, 16)
    S.moveflagPromoteBishop = bit.lshift(0x80, 16)

    -- Position variables
    S.g_board = {}
    S.g_toMove = 0
    S.g_castleRights = 0
    S.g_enPassentSquare = 0
    S.g_baseEval = 0
    S.g_hashKeyLow = 0
    S.g_hashKeyHigh = 0
    S.g_inCheck = false

    -- Utility variables
    S.g_moveCount = 0
    S.g_moveUndoStack = {}
    S.g_move50 = 0
    S.g_repMoveStack = {}

    S.g_hashSize = bit.lshift(1, 22)
    S.g_hashMask = S.g_hashSize - 1
    S.g_hashTable = {}

    S.g_killers = 0
    S.historyTable = {}

    S.g_zobristLow = {}
    S.g_zobristHigh = {}
    S.g_zobristBlackLow = 0
    S.g_zobristBlackHigh = 0

    -- Evaluation variables
    S.g_mobUnit = {}

    S.hashflagAlpha = 1
    S.hashflagBeta = 2
    S.hashflagExact = 3

    -- Search variables
    S.g_nodeCount = 0
    S.g_qNodeCount = 0
    S.g_searchValid = true
    S.g_globalPly = 0

    S.minEval = -2000000
    S.maxEval = 2000000
    S.minMateBuffer = S.minEval + 2000
    S.maxMateBuffer = S.maxEval - 2000

    S.materialTable = {0, 800, 3350, 3450, 5000, 9750, 600000}

    -- Piece-square tables
    S.pawnAdj = {
        0, 0, 0, 0, 0, 0, 0, 0,
        -25, 105, 135, 270, 270, 135, 105, -25,
        -80, 0, 30, 176, 176, 30, 0, -80,
        -85, -5, 25, 175, 175, 25, -5, -85,
        -90, -10, 20, 125, 125, 20, -10, -90,
        -95, -15, 15, 75, 75, 15, -15, -95,
        -100, -20, 10, 70, 70, 10, -20, -100,
        0, 0, 0, 0, 0, 0, 0, 0
    }

    S.knightAdj = {
        -200, -100, -50, -50, -50, -50, -100, -200,
        -100, 0, 0, 0, 0, 0, 0, -100,
        -50, 0, 60, 60, 60, 60, 0, -50,
        -50, 0, 30, 60, 60, 30, 0, -50,
        -50, 0, 30, 60, 60, 30, 0, -50,
        -50, 0, 30, 30, 30, 30, 0, -50,
        -100, 0, 0, 0, 0, 0, 0, -100,
        -200, -50, -25, -25, -25, -25, -50, -200
    }

    S.bishopAdj = {
        -50,-50,-25,-10,-10,-25,-50,-50,
        -50,-25,-10,  0,  0,-10,-25,-50,
        -25,-10,  0, 25, 25,  0,-10,-25,
        -10,  0, 25, 40, 40, 25,  0,-10,
        -10,  0, 25, 40, 40, 25,  0,-10,
        -25,-10,  0, 25, 25,  0,-10,-25,
        -50,-25,-10,  0,  0,-10,-25,-50,
        -50,-50,-25,-10,-10,-25,-50,-50
    }

    S.rookAdj = {
        -60, -30, -10, 20, 20, -10, -30, -60,
        40,  70,  90,120,120,  90,  70,  40,
        -60, -30, -10, 20, 20, -10, -30, -60,
        -60, -30, -10, 20, 20, -10, -30, -60,
        -60, -30, -10, 20, 20, -10, -30, -60,
        -60, -30, -10, 20, 20, -10, -30, -60,
        -60, -30, -10, 20, 20, -10, -30, -60,
        -60, -30, -10, 20, 20, -10, -30, -60
    }

    S.kingAdj = {
        50, 150, -25, -125, -125, -25, 150, 50,
        50, 150, -25, -125, -125, -25, 150, 50,
        50, 150, -25, -125, -125, -25, 150, 50,
        50, 150, -25, -125, -125, -25, 150, 50,
        50, 150, -25, -125, -125, -25, 150, 50,
        50, 150, -25, -125, -125, -25, 150, 50,
        50, 150, -25, -125, -125, -25, 150, 50,
        150, 250, 75, -25, -25, 75, 250, 150
    }

    S.emptyAdj = {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
    }

    S.pieceSquareAdj = {}
    S.flipTable = {}
    S.g_pieceIndex = {}
    S.g_pieceList = {}
    S.g_pieceCount = {}

    S.PieceCharList = {" ", "p", "n", "b", "r", "q", "k", " "}

    -- Mersenne Twister state
    S.mt = {}
    S.mt.N = 624
    S.mt.M = 397
    S.mt.MAG01 = {0x0, 0x9908b0df}
    S.mt.mt = {}
    S.mt.mti = S.mt.N + 1

    return S
end

--------------------------------------------------------------------------------
-- Helper functions (operating on state S)
--------------------------------------------------------------------------------

local function iif(ask, ontrue, onfalse)
    if ask then return ontrue end
    return onfalse
end

local function FormatSquare(S, square)
    return string.char(string.byte("a",1) + bit.band(square, 0xF) - 4) ..
           string.format("%d", (9 - bit.rshift(square, 4)) + 1)
end

local function deFormatSquare(at)
    local h = string.byte(at,1) - string.byte("a",1) + 4
    local v = 9 - (string.byte(at,2) - string.byte("0",1) - 1)
    return bit.bor(h, bit.lshift(v, 4))
end

local function MakeSquare(row, column)
    return bit.bor(bit.lshift((row + 2), 4), (column + 4))
end

local function MakeTable(S, tbl)
    local result = {}
    for i = 0, 255 do result[1+i] = 0 end
    for row = 0, 7 do
        for col = 0, 7 do
            result[1 + MakeSquare(row, col)] = tbl[1 + ((row * 8) + col)]
        end
    end
    return result
end

-- Mersenne Twister functions
local function mtSetSeed(S, N0)
    S.mt.mt[1] = N0
    for i = 1, S.mt.N - 1 do
        local s = bit.bxor(S.mt.mt[1 + (i - 1)], bit.rshift(S.mt.mt[1 + (i - 1)], 30))
        S.mt.mt[1 + i] = bit.lshift((1812433253 * bit.rshift(bit.band(s, 0xffff0000), 16)), 16) +
                         1812433253 * bit.band(s, 0x0000ffff) + i
    end
    S.mt.mti = S.mt.N
end

local function mtNext(S, bits)
    local x, y, k
    if S.mt.mti >= S.mt.N then
        for k = 0, (S.mt.N - S.mt.M) - 1 do
            x = bit.bor(bit.band(S.mt.mt[1 + k], 0x80000000), bit.band(S.mt.mt[1 + (k + 1)], 0x7fffffff))
            S.mt.mt[1 + k] = bit.bxor(bit.bxor(S.mt.mt[1 + (k + S.mt.M)], bit.rshift(x, 1)),
                                       S.mt.MAG01[1 + bit.band(x, 0x1)])
        end
        for k = S.mt.N - S.mt.M, (S.mt.N - 1) - 1 do
            x = bit.bor(bit.band(S.mt.mt[1 + k], 0x80000000), bit.band(S.mt.mt[1 + (k + 1)], 0x7fffffff))
            S.mt.mt[1 + k] = bit.bxor(bit.bxor(S.mt.mt[1 + k + (S.mt.M - S.mt.N)], bit.rshift(x, 1)),
                                       S.mt.MAG01[1 + bit.band(x, 0x1)])
        end
        x = bit.bor(bit.band(S.mt.mt[1 + (S.mt.N - 1)], 0x80000000), bit.band(S.mt.mt[1], 0x7fffffff))
        S.mt.mt[1 + (S.mt.N - 1)] = bit.bxor(bit.bxor(S.mt.mt[1 + (S.mt.M - 1)], bit.rshift(x, 1)),
                                              S.mt.MAG01[1 + bit.band(x, 0x1)])
        S.mt.mti = 0
    end
    y = S.mt.mt[1 + S.mt.mti]
    S.mt.mti = S.mt.mti + 1
    y = bit.bxor(y, bit.rshift(y, 11))
    y = bit.bxor(y, bit.band(bit.lshift(y, 7), 0x9d2c5680))
    y = bit.bxor(y, bit.band(bit.lshift(y, 15), 0xefc60000))
    y = bit.bxor(y, bit.rshift(y, 18))
    y = bit.band(bit.rshift(y, (32 - bits)), 0xFFFFFFFF)
    return y
end

--------------------------------------------------------------------------------
-- Initialize functions
--------------------------------------------------------------------------------

local function InitializeEval(S)
    S.g_mobUnit = {}
    for i = 0, 1 do
        S.g_mobUnit[1 + i] = {}
        local enemy = iif(i == 0, 0x10, 8)
        local friend = iif(i == 0, 8, 0x10)
        S.g_mobUnit[1 + i][1] = 1
        S.g_mobUnit[1 + i][1 + 0x80] = 0
        S.g_mobUnit[1 + i][1 + bit.bor(enemy, S.piecePawn)] = 1
        S.g_mobUnit[1 + i][1 + bit.bor(enemy, S.pieceBishop)] = 1
        S.g_mobUnit[1 + i][1 + bit.bor(enemy, S.pieceKnight)] = 1
        S.g_mobUnit[1 + i][1 + bit.bor(enemy, S.pieceRook)] = 1
        S.g_mobUnit[1 + i][1 + bit.bor(enemy, S.pieceQueen)] = 1
        S.g_mobUnit[1 + i][1 + bit.bor(enemy, S.pieceKing)] = 1
        S.g_mobUnit[1 + i][1 + bit.bor(friend, S.piecePawn)] = 0
        S.g_mobUnit[1 + i][1 + bit.bor(friend, S.pieceBishop)] = 0
        S.g_mobUnit[1 + i][1 + bit.bor(friend, S.pieceKnight)] = 0
        S.g_mobUnit[1 + i][1 + bit.bor(friend, S.pieceRook)] = 0
        S.g_mobUnit[1 + i][1 + bit.bor(friend, S.pieceQueen)] = 0
        S.g_mobUnit[1 + i][1 + bit.bor(friend, S.pieceKing)] = 0
    end
end

local function HashEntry(lock, value, flags, hashDepth, bestMove, globalPly)
    return {
        lock = lock,
        value = value,
        flags = flags,
        hashDepth = hashDepth,
        bestMove = bestMove
    }
end

local function StoreHash(S, value, flags, ply, move, depth)
    if value >= S.maxMateBuffer then
        value = value + depth
    elseif value <= S.minMateBuffer then
        value = value - depth
    end
    S.g_hashTable[1 + bit.band(S.g_hashKeyLow, S.g_hashMask)] =
        HashEntry(S.g_hashKeyHigh, value, flags, ply, move)
end

local function SetHash(S)
    local result = { hashKeyLow = 0, hashKeyHigh = 0 }
    for i = 0, 255 do
        local piece = S.g_board[1 + i]
        if bit.band(piece, 0x18) > 0 then
            result.hashKeyLow = bit.bxor(result.hashKeyLow, S.g_zobristLow[1 + i][1 + bit.band(piece, 0xF)])
            result.hashKeyHigh = bit.bxor(result.hashKeyHigh, S.g_zobristHigh[1 + i][1 + bit.band(piece, 0xF)])
        end
    end
    if S.g_toMove == 0 then
        result.hashKeyLow = bit.bxor(result.hashKeyLow, S.g_zobristBlackLow)
        result.hashKeyHigh = bit.bxor(result.hashKeyHigh, S.g_zobristBlackHigh)
    end
    return result
end

local function InitializePieceList(S)
    for i = 0, 15 do
        S.g_pieceCount[1 + i] = 0
        for j = 0, 15 do
            S.g_pieceList[1 + bit.bor(bit.lshift(i, 4), j)] = 0
        end
    end
    for i = 0, 255 do
        S.g_pieceIndex[1 + i] = 0
        if bit.band(S.g_board[1 + i], bit.bor(S.colorWhite, S.colorBlack)) > 0 then
            local piece = bit.band(S.g_board[1 + i], 0xF)
            S.g_pieceList[1 + bit.bor(bit.lshift(piece, 4), S.g_pieceCount[1 + piece])] = i
            S.g_pieceIndex[1 + i] = S.g_pieceCount[1 + piece]
            S.g_pieceCount[1 + piece] = S.g_pieceCount[1 + piece] + 1
        end
    end
end

local function InitializeFromFen(S, fen)
    local chunks = {}
    local fen2 = fen
    while string.len(fen2) > 0 do
        local s1 = string.find(fen2, " ")
        if s1 == nil then
            table.insert(chunks, fen2)
            fen2 = ""
        else
            table.insert(chunks, string.sub(fen2, 1, s1 - 1))
            fen2 = string.sub(fen2, s1 + 1)
        end
    end

    for i = 0, 255 do
        S.g_board[1 + i] = 0x80
    end

    local row, col = 0, 0
    local pieces = chunks[1]
    for i = 0, string.len(pieces) - 1 do
        local c = string.sub(pieces, i + 1, i + 1)
        if c == '/' then
            row = row + 1
            col = 0
        elseif c >= '0' and c <= '9' then
            for j = 0, tonumber(c) - 1 do
                S.g_board[1 + ((row + 2) * 0x10) + (col + 4)] = 0
                col = col + 1
            end
        else
            local isBlack = (c >= 'a' and c <= 'z')
            local piece = iif(isBlack, S.colorBlack, S.colorWhite)
            local lc = isBlack and c or string.lower(c)
            if lc == 'p' then piece = bit.bor(piece, S.piecePawn)
            elseif lc == 'b' then piece = bit.bor(piece, S.pieceBishop)
            elseif lc == 'n' then piece = bit.bor(piece, S.pieceKnight)
            elseif lc == 'r' then piece = bit.bor(piece, S.pieceRook)
            elseif lc == 'q' then piece = bit.bor(piece, S.pieceQueen)
            elseif lc == 'k' then piece = bit.bor(piece, S.pieceKing)
            end
            S.g_board[1 + ((row + 2) * 0x10) + (col + 4)] = piece
            col = col + 1
        end
    end

    InitializePieceList(S)

    S.g_toMove = iif(chunks[2] == 'w', S.colorWhite, 0)

    S.g_castleRights = 0
    if chunks[3] and string.find(chunks[3], 'K') then S.g_castleRights = bit.bor(S.g_castleRights, 1) end
    if chunks[3] and string.find(chunks[3], 'Q') then S.g_castleRights = bit.bor(S.g_castleRights, 2) end
    if chunks[3] and string.find(chunks[3], 'k') then S.g_castleRights = bit.bor(S.g_castleRights, 4) end
    if chunks[3] and string.find(chunks[3], 'q') then S.g_castleRights = bit.bor(S.g_castleRights, 8) end

    S.g_enPassentSquare = -1
    if chunks[4] and not string.find(chunks[4], '-') then
        S.g_enPassentSquare = deFormatSquare(chunks[4])
    end

    local hashResult = SetHash(S)
    S.g_hashKeyLow = hashResult.hashKeyLow
    S.g_hashKeyHigh = hashResult.hashKeyHigh

    S.g_baseEval = 0
    for i = 0, 255 do
        if bit.band(S.g_board[1 + i], S.colorWhite) > 0 then
            S.g_baseEval = S.g_baseEval + S.pieceSquareAdj[1 + bit.band(S.g_board[1 + i], 0x7)][1 + i]
            S.g_baseEval = S.g_baseEval + S.materialTable[1 + bit.band(S.g_board[1 + i], 0x7)]
        elseif bit.band(S.g_board[1 + i], S.colorBlack) > 0 then
            S.g_baseEval = S.g_baseEval - S.pieceSquareAdj[1 + bit.band(S.g_board[1 + i], 0x7)][1 + S.flipTable[1 + i]]
            S.g_baseEval = S.g_baseEval - S.materialTable[1 + bit.band(S.g_board[1 + i], 0x7)]
        end
    end
    if S.g_toMove == 0 then
        S.g_baseEval = -S.g_baseEval
    end

    S.g_move50 = 0
    S.g_inCheck = IsSquareAttackable(S, S.g_pieceList[1 + bit.lshift(bit.bor(S.g_toMove, S.pieceKing), 4)], 8 - S.g_toMove)
end

local function ResetGame(S)
    mtSetSeed(S, 0x1BADF00D)

    S.g_killers = {}
    for i = 0, 127 do
        S.g_killers[1 + i] = {0, 0}
    end

    S.g_hashTable = {}

    for i = 0, 31 do
        S.historyTable[1 + i] = {}
        for j = 0, 255 do
            S.historyTable[1 + i][1 + j] = 0
        end
    end

    S.g_zobristLow = {}
    S.g_zobristHigh = {}
    for i = 0, 255 do
        S.g_zobristLow[1 + i] = {}
        S.g_zobristHigh[1 + i] = {}
        for j = 0, 15 do
            S.g_zobristLow[1 + i][1 + j] = mtNext(S, 32)
            S.g_zobristHigh[1 + i][1 + j] = mtNext(S, 32)
        end
    end
    S.g_zobristBlackLow = mtNext(S, 32)
    S.g_zobristBlackHigh = mtNext(S, 32)

    for row = 0, 7 do
        for col = 0, 7 do
            local square = MakeSquare(row, col)
            S.flipTable[1 + square] = MakeSquare(7 - row, col)
        end
    end

    S.pieceSquareAdj[1 + S.piecePawn] = MakeTable(S, S.pawnAdj)
    S.pieceSquareAdj[1 + S.pieceKnight] = MakeTable(S, S.knightAdj)
    S.pieceSquareAdj[1 + S.pieceBishop] = MakeTable(S, S.bishopAdj)
    S.pieceSquareAdj[1 + S.pieceRook] = MakeTable(S, S.rookAdj)
    S.pieceSquareAdj[1 + S.pieceQueen] = MakeTable(S, S.emptyAdj)
    S.pieceSquareAdj[1 + S.pieceKing] = MakeTable(S, S.kingAdj)

    local pieceDeltas = {{}, {}, S.g_knightDeltas, S.g_bishopDeltas, S.g_rookDeltas, S.g_queenDeltas, S.g_queenDeltas}

    for i = 0, 255 do
        S.g_vectorDelta[1 + i] = {}
        S.g_vectorDelta[1 + i].delta = 0
        S.g_vectorDelta[1 + i].pieceMask = {}
        S.g_vectorDelta[1 + i].pieceMask[1] = 0
        S.g_vectorDelta[1 + i].pieceMask[2] = 0
    end

    -- Initialize the vector delta table
    local row = 0
    while row < 0x80 do
        local col = 0
        while col < 0x8 do
            local square = bit.bor(row, col)
            -- Pawn moves
            local index = square - (square - 17) + 128
            S.g_vectorDelta[1 + index].pieceMask[1 + bit.rshift(S.colorWhite, 3)] =
                bit.bor(S.g_vectorDelta[1 + index].pieceMask[1 + bit.rshift(S.colorWhite, 3)], bit.lshift(1, S.piecePawn))
            index = square - (square - 15) + 128
            S.g_vectorDelta[1 + index].pieceMask[1 + bit.rshift(S.colorWhite, 3)] =
                bit.bor(S.g_vectorDelta[1 + index].pieceMask[1 + bit.rshift(S.colorWhite, 3)], bit.lshift(1, S.piecePawn))
            index = square - (square + 17) + 128
            S.g_vectorDelta[1 + index].pieceMask[1] =
                bit.bor(S.g_vectorDelta[1 + index].pieceMask[1], bit.lshift(1, S.piecePawn))
            index = square - (square + 15) + 128
            S.g_vectorDelta[1 + index].pieceMask[1] =
                bit.bor(S.g_vectorDelta[1 + index].pieceMask[1], bit.lshift(1, S.piecePawn))

            for i = S.pieceKnight, S.pieceKing do
                local dir = 0
                while dir < #pieceDeltas[1 + i] do
                    local target = square + pieceDeltas[1 + i][1 + dir]
                    while bit.band(target, 0x88) == 0 do
                        index = square - target + 128
                        S.g_vectorDelta[1 + index].pieceMask[1 + bit.rshift(S.colorWhite, 3)] =
                            bit.bor(S.g_vectorDelta[1 + index].pieceMask[1 + bit.rshift(S.colorWhite, 3)], bit.lshift(1, i))
                        S.g_vectorDelta[1 + index].pieceMask[1] =
                            bit.bor(S.g_vectorDelta[1 + index].pieceMask[1], bit.lshift(1, i))

                        local flip = iif(square < target, 1, -1)
                        if bit.band(square, 0xF0) == bit.band(target, 0xF0) then
                            S.g_vectorDelta[1 + index].delta = flip * 1
                        elseif bit.band(square, 0x0F) == bit.band(target, 0x0F) then
                            S.g_vectorDelta[1 + index].delta = flip * 16
                        elseif (square % 15) == (target % 15) then
                            S.g_vectorDelta[1 + index].delta = flip * 15
                        elseif (square % 17) == (target % 17) then
                            S.g_vectorDelta[1 + index].delta = flip * 17
                        end

                        if i == S.pieceKnight then
                            S.g_vectorDelta[1 + index].delta = pieceDeltas[1 + i][1 + dir]
                            break
                        end
                        if i == S.pieceKing then break end
                        target = target + pieceDeltas[1 + i][1 + dir]
                    end
                    dir = dir + 1
                end
            end
            col = col + 1
        end
        row = row + 0x10
    end
end

--------------------------------------------------------------------------------
-- Attack detection
--------------------------------------------------------------------------------

local function IsSquareAttackableFrom(S, target, from)
    local index = from - target + 128
    local piece = S.g_board[1 + from]
    if bit.band(S.g_vectorDelta[1 + index].pieceMask[1 + bit.band(bit.rshift(piece, 3), 1)],
                bit.lshift(1, bit.band(piece, 0x7))) > 0 then
        local inc = S.g_vectorDelta[1 + index].delta
        local pos = from
        while true do
            pos = pos + inc
            if pos == target then return true end
            if S.g_board[1 + pos] ~= 0 then break end
        end
    end
    return false
end

function IsSquareAttackable(S, target, color)
    local inc = iif(color > 0, -16, 16)
    local pawn = bit.bor(iif(color > 0, S.colorWhite, S.colorBlack), 1)
    if S.g_board[1 + target - (inc - 1)] == pawn then return true end
    if S.g_board[1 + target - (inc + 1)] == pawn then return true end

    for i = 2, 6 do
        local idx = bit.lshift(bit.bor(color, i), 4)
        local square = S.g_pieceList[1 + idx]
        while square ~= 0 do
            if IsSquareAttackableFrom(S, target, square) then return true end
            idx = idx + 1
            square = S.g_pieceList[1 + idx]
        end
    end
    return false
end

local function ExposesCheck(S, from, kingPos)
    local index = kingPos - from + 128
    if bit.band(S.g_vectorDelta[1 + index].pieceMask[1], bit.lshift(1, S.pieceQueen)) ~= 0 then
        local delta = S.g_vectorDelta[1 + index].delta
        local pos = kingPos + delta
        while S.g_board[1 + pos] == 0 do
            pos = pos + delta
        end
        local piece = S.g_board[1 + pos]
        if bit.band(bit.band(piece, bit.bxor(S.g_board[1 + kingPos], 0x18)), 0x18) == 0 then
            return false
        end
        local backwardIndex = pos - kingPos + 128
        return bit.band(S.g_vectorDelta[1 + backwardIndex].pieceMask[1 + bit.band(bit.rshift(piece, 3), 1)],
                        bit.lshift(1, bit.band(piece, 0x7))) ~= 0
    end
    return false
end

--------------------------------------------------------------------------------
-- Move generation helpers
--------------------------------------------------------------------------------

local function GenerateMove(from, to)
    return bit.bor(from, bit.lshift(to, 8))
end

local function GenerateMove2(from, to, flags)
    return bit.bor(from, bit.bor(bit.lshift(to, 8), flags))
end

local function MSt(S, moveStack, usage, from, dt, enemy)
    local to = from + dt
    if usage == 1 then
        if (enemy == nil and S.g_board[1 + to] == 0) or (enemy ~= nil and bit.band(S.g_board[1 + to], enemy) > 0) then
            moveStack[#moveStack + 1] = GenerateMove(from, to)
        end
    elseif usage == 2 then
        while S.g_board[1 + to] == 0 do
            moveStack[#moveStack + 1] = GenerateMove(from, to)
            to = to + dt
        end
    elseif usage == 3 then
        while S.g_board[1 + to] == 0 do
            to = to + dt
        end
        if bit.band(S.g_board[1 + to], enemy) > 0 then
            moveStack[#moveStack + 1] = GenerateMove(from, to)
        end
    end
end

local function MovePawnTo(S, moveStack, start, square)
    local row = bit.band(square, 0xF0)
    if row == 0x90 or row == 0x20 then
        moveStack[#moveStack + 1] = GenerateMove2(start, square, bit.bor(S.moveflagPromotion, S.moveflagPromoteQueen))
        moveStack[#moveStack + 1] = GenerateMove2(start, square, bit.bor(S.moveflagPromotion, S.moveflagPromoteKnight))
        moveStack[#moveStack + 1] = GenerateMove2(start, square, bit.bor(S.moveflagPromotion, S.moveflagPromoteBishop))
        moveStack[#moveStack + 1] = GenerateMove2(start, square, S.moveflagPromotion)
    else
        moveStack[#moveStack + 1] = GenerateMove2(start, square, 0)
    end
end

local function GeneratePawnMoves(S, moveStack, from)
    local piece = S.g_board[1 + from]
    local color = bit.band(piece, S.colorWhite)
    local inc = iif(color == S.colorWhite, -16, 16)
    local to = from + inc
    if S.g_board[1 + to] == 0 then
        MovePawnTo(S, moveStack, from, to)
        if ((bit.band(from, 0xF0) == 0x30) and color ~= S.colorWhite) or
           ((bit.band(from, 0xF0) == 0x80) and color == S.colorWhite) then
            to = to + inc
            if S.g_board[1 + to] == 0 then
                moveStack[#moveStack + 1] = GenerateMove(from, to)
            end
        end
    end
end

local function GenerateCaptureMoves(S, moveStack)
    local inc = iif(S.g_toMove == 8, -16, 16)
    local enemy = iif(S.g_toMove == 8, 0x10, 0x8)

    -- Pawn captures
    local pieceIdx = bit.lshift(bit.bor(S.g_toMove, 1), 4)
    local from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        local to = from + inc - 1
        if bit.band(S.g_board[1 + to], enemy) > 0 then
            MovePawnTo(S, moveStack, from, to)
        end
        to = from + inc + 1
        if bit.band(S.g_board[1 + to], enemy) > 0 then
            MovePawnTo(S, moveStack, from, to)
        end
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- En passant
    if S.g_enPassentSquare ~= -1 then
        inc = iif(S.g_toMove == S.colorWhite, -16, 16)
        local pawn = bit.bor(S.g_toMove, S.piecePawn)
        from = S.g_enPassentSquare - (inc + 1)
        if bit.band(S.g_board[1 + from], 0xF) == pawn then
            moveStack[#moveStack + 1] = GenerateMove2(from, S.g_enPassentSquare, S.moveflagEPC)
        end
        from = S.g_enPassentSquare - (inc - 1)
        if bit.band(S.g_board[1 + from], 0xF) == pawn then
            moveStack[#moveStack + 1] = GenerateMove2(from, S.g_enPassentSquare, S.moveflagEPC)
        end
    end

    -- Knight captures
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 2), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 1, from, 31, enemy)
        MSt(S, moveStack, 1, from, 33, enemy)
        MSt(S, moveStack, 1, from, 14, enemy)
        MSt(S, moveStack, 1, from, -14, enemy)
        MSt(S, moveStack, 1, from, -31, enemy)
        MSt(S, moveStack, 1, from, -33, enemy)
        MSt(S, moveStack, 1, from, 18, enemy)
        MSt(S, moveStack, 1, from, -18, enemy)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- Bishop captures
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 3), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 3, from, -15, enemy)
        MSt(S, moveStack, 3, from, -17, enemy)
        MSt(S, moveStack, 3, from, 15, enemy)
        MSt(S, moveStack, 3, from, 17, enemy)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- Rook captures
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 4), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 3, from, -1, enemy)
        MSt(S, moveStack, 3, from, 1, enemy)
        MSt(S, moveStack, 3, from, -16, enemy)
        MSt(S, moveStack, 3, from, 16, enemy)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- Queen captures
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 5), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 3, from, -15, enemy)
        MSt(S, moveStack, 3, from, -17, enemy)
        MSt(S, moveStack, 3, from, 15, enemy)
        MSt(S, moveStack, 3, from, 17, enemy)
        MSt(S, moveStack, 3, from, -1, enemy)
        MSt(S, moveStack, 3, from, 1, enemy)
        MSt(S, moveStack, 3, from, -16, enemy)
        MSt(S, moveStack, 3, from, 16, enemy)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- King captures
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 6), 4)
    from = S.g_pieceList[1 + pieceIdx]
    MSt(S, moveStack, 1, from, -15, enemy)
    MSt(S, moveStack, 1, from, -17, enemy)
    MSt(S, moveStack, 1, from, 15, enemy)
    MSt(S, moveStack, 1, from, 17, enemy)
    MSt(S, moveStack, 1, from, -1, enemy)
    MSt(S, moveStack, 1, from, 1, enemy)
    MSt(S, moveStack, 1, from, -16, enemy)
    MSt(S, moveStack, 1, from, 16, enemy)
end

local function GenerateAllMoves(S, moveStack)
    -- Pawn quiet moves
    local pieceIdx = bit.lshift(bit.bor(S.g_toMove, 1), 4)
    local from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        GeneratePawnMoves(S, moveStack, from)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- Knight quiet moves
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 2), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 1, from, 31, nil)
        MSt(S, moveStack, 1, from, 33, nil)
        MSt(S, moveStack, 1, from, 14, nil)
        MSt(S, moveStack, 1, from, -14, nil)
        MSt(S, moveStack, 1, from, -31, nil)
        MSt(S, moveStack, 1, from, -33, nil)
        MSt(S, moveStack, 1, from, 18, nil)
        MSt(S, moveStack, 1, from, -18, nil)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- Bishop quiet moves
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 3), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 2, from, -15, nil)
        MSt(S, moveStack, 2, from, -17, nil)
        MSt(S, moveStack, 2, from, 15, nil)
        MSt(S, moveStack, 2, from, 17, nil)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- Rook quiet moves
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 4), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 2, from, -1, nil)
        MSt(S, moveStack, 2, from, 1, nil)
        MSt(S, moveStack, 2, from, 16, nil)
        MSt(S, moveStack, 2, from, -16, nil)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- Queen quiet moves
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 5), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        MSt(S, moveStack, 2, from, -15, nil)
        MSt(S, moveStack, 2, from, -17, nil)
        MSt(S, moveStack, 2, from, 15, nil)
        MSt(S, moveStack, 2, from, 17, nil)
        MSt(S, moveStack, 2, from, -1, nil)
        MSt(S, moveStack, 2, from, 1, nil)
        MSt(S, moveStack, 2, from, 16, nil)
        MSt(S, moveStack, 2, from, -16, nil)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end

    -- King quiet moves
    pieceIdx = bit.lshift(bit.bor(S.g_toMove, 6), 4)
    from = S.g_pieceList[1 + pieceIdx]
    MSt(S, moveStack, 1, from, -15, nil)
    MSt(S, moveStack, 1, from, -17, nil)
    MSt(S, moveStack, 1, from, 15, nil)
    MSt(S, moveStack, 1, from, 17, nil)
    MSt(S, moveStack, 1, from, -1, nil)
    MSt(S, moveStack, 1, from, 1, nil)
    MSt(S, moveStack, 1, from, -16, nil)
    MSt(S, moveStack, 1, from, 16, nil)

    if not S.g_inCheck then
        local castleRights = S.g_castleRights
        if S.g_toMove == 0 then
            castleRights = bit.rshift(castleRights, 2)
        end
        if bit.band(castleRights, 1) > 0 then
            if S.g_board[1 + (from + 1)] == S.pieceEmpty and S.g_board[1 + (from + 2)] == S.pieceEmpty then
                moveStack[#moveStack + 1] = GenerateMove2(from, from + 0x02, S.moveflagCastleKing)
            end
        end
        if bit.band(castleRights, 2) > 0 then
            if S.g_board[1 + (from - 1)] == S.pieceEmpty and S.g_board[1 + (from - 2)] == S.pieceEmpty and
               S.g_board[1 + (from - 3)] == S.pieceEmpty then
                moveStack[#moveStack + 1] = GenerateMove2(from, from - 0x02, S.moveflagCastleQueen)
            end
        end
    end
end

--------------------------------------------------------------------------------
-- Make/Unmake Move
--------------------------------------------------------------------------------

local function UndoHistory(ep, castleRights, inCheck, baseEval, hashKeyLow, hashKeyHigh, move50, captured)
    return {
        ep = ep,
        castleRights = castleRights,
        inCheck = inCheck,
        baseEval = baseEval,
        hashKeyLow = hashKeyLow,
        hashKeyHigh = hashKeyHigh,
        move50 = move50,
        captured = captured
    }
end

-- Forward declaration for mutual recursion
local UnmakeMove

local function MakeMove(S, move)
    local me = bit.rshift(S.g_toMove, 3)
    local otherColor = 8 - S.g_toMove
    local flags = bit.band(move, 0xFF0000)
    local to = bit.band(bit.rshift(move, 8), 0xFF)
    local from = bit.band(move, 0xFF)
    local diff = to - from
    local captured = S.g_board[1 + to]
    local piece = S.g_board[1 + from]
    local epcEnd = to

    S.g_finCnt = S.g_finCnt + 1

    if bit.band(flags, S.moveflagEPC) > 0 then
        epcEnd = iif(me > 0, (to + 0x10), (to - 0x10))
        captured = S.g_board[1 + epcEnd]
        S.g_board[1 + epcEnd] = S.pieceEmpty
    end

    S.g_moveUndoStack[1 + S.g_moveCount] = UndoHistory(
        S.g_enPassentSquare, S.g_castleRights, S.g_inCheck, S.g_baseEval,
        S.g_hashKeyLow, S.g_hashKeyHigh, S.g_move50, captured)
    S.g_moveCount = S.g_moveCount + 1

    S.g_enPassentSquare = -1

    if flags > 0 then
        if bit.band(flags, S.moveflagCastleKing) > 0 then
            if IsSquareAttackable(S, from + 1, otherColor) or IsSquareAttackable(S, from + 2, otherColor) then
                S.g_moveCount = S.g_moveCount - 1
                return false
            end
            local rook = S.g_board[1 + (to + 1)]
            S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + (to + 1)][1 + bit.band(rook, 0xF)])
            S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + (to + 1)][1 + bit.band(rook, 0xF)])
            S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + (to - 1)][1 + bit.band(rook, 0xF)])
            S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + (to - 1)][1 + bit.band(rook, 0xF)])
            S.g_board[1 + (to - 1)] = rook
            S.g_board[1 + (to + 1)] = S.pieceEmpty
            S.g_baseEval = S.g_baseEval - S.pieceSquareAdj[1 + bit.band(rook, 0x7)][1 + iif(me == 0, S.flipTable[1 + (to + 1)], (to + 1))]
            S.g_baseEval = S.g_baseEval + S.pieceSquareAdj[1 + bit.band(rook, 0x7)][1 + iif(me == 0, S.flipTable[1 + (to - 1)], (to - 1))]
            local rookIndex = S.g_pieceIndex[1 + (to + 1)]
            S.g_pieceIndex[1 + (to - 1)] = rookIndex
            S.g_pieceList[1 + bit.bor(bit.lshift(bit.band(rook, 0xF), 4), rookIndex)] = to - 1
        elseif bit.band(flags, S.moveflagCastleQueen) > 0 then
            if IsSquareAttackable(S, from - 1, otherColor) or IsSquareAttackable(S, from - 2, otherColor) then
                S.g_moveCount = S.g_moveCount - 1
                return false
            end
            local rook = S.g_board[1 + (to - 2)]
            S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + (to - 2)][1 + bit.band(rook, 0xF)])
            S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + (to - 2)][1 + bit.band(rook, 0xF)])
            S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + (to + 1)][1 + bit.band(rook, 0xF)])
            S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + (to + 1)][1 + bit.band(rook, 0xF)])
            S.g_board[1 + (to + 1)] = rook
            S.g_board[1 + (to - 2)] = S.pieceEmpty
            S.g_baseEval = S.g_baseEval - S.pieceSquareAdj[1 + bit.band(rook, 0x7)][1 + iif(me == 0, S.flipTable[1 + (to - 2)], (to - 2))]
            S.g_baseEval = S.g_baseEval + S.pieceSquareAdj[1 + bit.band(rook, 0x7)][1 + iif(me == 0, S.flipTable[1 + (to + 1)], (to + 1))]
            local rookIndex = S.g_pieceIndex[1 + (to - 2)]
            S.g_pieceIndex[1 + (to + 1)] = rookIndex
            S.g_pieceList[1 + bit.bor(bit.lshift(bit.band(rook, 0xF), 4), rookIndex)] = to + 1
        end
    end

    if captured > 0 then
        local capturedType = bit.band(captured, 0xF)
        S.g_pieceCount[1 + capturedType] = S.g_pieceCount[1 + capturedType] - 1
        local lastPieceSquare = S.g_pieceList[1 + bit.bor(bit.lshift(capturedType, 4), S.g_pieceCount[1 + capturedType])]
        S.g_pieceIndex[1 + lastPieceSquare] = S.g_pieceIndex[1 + epcEnd]
        S.g_pieceList[1 + bit.bor(bit.lshift(capturedType, 4), S.g_pieceIndex[1 + lastPieceSquare])] = lastPieceSquare
        S.g_pieceList[1 + bit.bor(bit.lshift(capturedType, 4), S.g_pieceCount[1 + capturedType])] = 0
        S.g_baseEval = S.g_baseEval + S.materialTable[1 + bit.band(captured, 0x7)]
        S.g_baseEval = S.g_baseEval + S.pieceSquareAdj[1 + bit.band(captured, 0x7)][1 + iif(me > 0, S.flipTable[1 + epcEnd], epcEnd)]
        S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + epcEnd][1 + capturedType])
        S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + epcEnd][1 + capturedType])
        S.g_move50 = 0
    else
        if bit.band(piece, 0x7) == S.piecePawn then
            if diff < 0 then diff = -diff end
            if diff > 16 then
                S.g_enPassentSquare = iif(me > 0, (to + 0x10), (to - 0x10))
            end
            S.g_move50 = 0
        end
    end

    S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + from][1 + bit.band(piece, 0xF)])
    S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + from][1 + bit.band(piece, 0xF)])
    S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + to][1 + bit.band(piece, 0xF)])
    S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + to][1 + bit.band(piece, 0xF)])
    S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristBlackLow)
    S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristBlackHigh)

    S.g_castleRights = bit.band(S.g_castleRights, bit.band(S.g_castleRightsMask[1 + from], S.g_castleRightsMask[1 + to]))
    S.g_baseEval = S.g_baseEval - S.pieceSquareAdj[1 + bit.band(piece, 0x7)][1 + iif(me == 0, S.flipTable[1 + from], from)]

    S.g_pieceIndex[1 + to] = S.g_pieceIndex[1 + from]
    S.g_pieceList[1 + bit.bor(bit.lshift(bit.band(piece, 0xF), 4), S.g_pieceIndex[1 + to])] = to

    if bit.band(flags, S.moveflagPromotion) > 0 then
        local newPiece = bit.band(piece, bit.bnot(0x7))
        if bit.band(flags, S.moveflagPromoteKnight) > 0 then
            newPiece = bit.bor(newPiece, S.pieceKnight)
        elseif bit.band(flags, S.moveflagPromoteQueen) > 0 then
            newPiece = bit.bor(newPiece, S.pieceQueen)
        elseif bit.band(flags, S.moveflagPromoteBishop) > 0 then
            newPiece = bit.bor(newPiece, S.pieceBishop)
        else
            newPiece = bit.bor(newPiece, S.pieceRook)
        end
        S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + to][1 + bit.band(piece, 0xF)])
        S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + to][1 + bit.band(piece, 0xF)])
        S.g_board[1 + to] = newPiece
        S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristLow[1 + to][1 + bit.band(newPiece, 0xF)])
        S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristHigh[1 + to][1 + bit.band(newPiece, 0xF)])
        S.g_baseEval = S.g_baseEval + S.pieceSquareAdj[1 + bit.band(newPiece, 0x7)][1 + iif(me == 0, S.flipTable[1 + to], to)]
        S.g_baseEval = S.g_baseEval - S.materialTable[1 + S.piecePawn]
        S.g_baseEval = S.g_baseEval + S.materialTable[1 + bit.band(newPiece, 0x7)]

        local pawnType = bit.band(piece, 0xF)
        local promoteType = bit.band(newPiece, 0xF)
        S.g_pieceCount[1 + pawnType] = S.g_pieceCount[1 + pawnType] - 1
        local lastPawnSquare = S.g_pieceList[1 + bit.bor(bit.lshift(pawnType, 4), S.g_pieceCount[1 + pawnType])]
        S.g_pieceIndex[1 + lastPawnSquare] = S.g_pieceIndex[1 + to]
        S.g_pieceList[1 + bit.bor(bit.lshift(pawnType, 4), S.g_pieceIndex[1 + lastPawnSquare])] = lastPawnSquare
        S.g_pieceList[1 + bit.bor(bit.lshift(pawnType, 4), S.g_pieceCount[1 + pawnType])] = 0
        S.g_pieceIndex[1 + to] = S.g_pieceCount[1 + promoteType]
        S.g_pieceList[1 + bit.bor(bit.lshift(promoteType, 4), S.g_pieceIndex[1 + to])] = to
        S.g_pieceCount[1 + promoteType] = S.g_pieceCount[1 + promoteType] + 1
    else
        S.g_board[1 + to] = S.g_board[1 + from]
        S.g_baseEval = S.g_baseEval + S.pieceSquareAdj[1 + bit.band(piece, 0x7)][1 + iif(me == 0, S.flipTable[1 + to], to)]
    end
    S.g_board[1 + from] = S.pieceEmpty

    S.g_toMove = otherColor
    S.g_baseEval = -S.g_baseEval

    if (bit.band(piece, 0x7) > 0) == ((S.pieceKing > 0) or S.g_inCheck) then
        if IsSquareAttackable(S, S.g_pieceList[1 + bit.lshift(bit.bor(S.pieceKing, (8 - S.g_toMove)), 4)], otherColor) then
            UnmakeMove(S, move)
            return false
        end
    else
        local kingPos = S.g_pieceList[1 + bit.lshift(bit.bor(S.pieceKing, (8 - S.g_toMove)), 4)]
        if ExposesCheck(S, from, kingPos) then
            UnmakeMove(S, move)
            return false
        end
        if epcEnd ~= to then
            if ExposesCheck(S, epcEnd, kingPos) then
                UnmakeMove(S, move)
                return false
            end
        end
    end

    S.g_inCheck = false
    if flags <= S.moveflagEPC then
        local theirKingPos = S.g_pieceList[1 + bit.lshift(bit.bor(S.pieceKing, S.g_toMove), 4)]
        S.g_inCheck = IsSquareAttackableFrom(S, theirKingPos, to)
        if not S.g_inCheck then
            S.g_inCheck = ExposesCheck(S, from, theirKingPos)
            if not S.g_inCheck then
                if epcEnd ~= to then
                    S.g_inCheck = ExposesCheck(S, epcEnd, theirKingPos)
                end
            end
        end
    else
        S.g_inCheck = IsSquareAttackable(S, S.g_pieceList[1 + bit.lshift(bit.bor(S.pieceKing, S.g_toMove), 4)], 8 - S.g_toMove)
    end

    S.g_repMoveStack[1 + (S.g_moveCount - 1)] = S.g_hashKeyLow
    S.g_move50 = S.g_move50 + 1

    return true
end

UnmakeMove = function(S, move)
    S.g_toMove = 8 - S.g_toMove
    S.g_baseEval = -S.g_baseEval
    S.g_moveCount = S.g_moveCount - 1

    local otherColor = 8 - S.g_toMove
    local me = bit.rshift(S.g_toMove, 3)
    local flags = bit.band(move, 0xFF0000)
    local captured = S.g_moveUndoStack[1 + S.g_moveCount].captured
    local to = bit.band(bit.rshift(move, 8), 0xFF)
    local from = bit.band(move, 0xFF)
    local piece = S.g_board[1 + to]

    S.g_enPassentSquare = S.g_moveUndoStack[1 + S.g_moveCount].ep
    S.g_castleRights = S.g_moveUndoStack[1 + S.g_moveCount].castleRights
    S.g_inCheck = S.g_moveUndoStack[1 + S.g_moveCount].inCheck
    S.g_baseEval = S.g_moveUndoStack[1 + S.g_moveCount].baseEval
    S.g_hashKeyLow = S.g_moveUndoStack[1 + S.g_moveCount].hashKeyLow
    S.g_hashKeyHigh = S.g_moveUndoStack[1 + S.g_moveCount].hashKeyHigh
    S.g_move50 = S.g_moveUndoStack[1 + S.g_moveCount].move50

    if flags > 0 then
        if bit.band(flags, S.moveflagCastleKing) > 0 then
            local rook = S.g_board[1 + (to - 1)]
            S.g_board[1 + (to + 1)] = rook
            S.g_board[1 + (to - 1)] = S.pieceEmpty
            local rookIndex = S.g_pieceIndex[1 + (to - 1)]
            S.g_pieceIndex[1 + (to + 1)] = rookIndex
            S.g_pieceList[1 + bit.bor(bit.lshift(bit.band(rook, 0xF), 4), rookIndex)] = to + 1
        elseif bit.band(flags, S.moveflagCastleQueen) > 0 then
            local rook = S.g_board[1 + (to + 1)]
            S.g_board[1 + (to - 2)] = rook
            S.g_board[1 + (to + 1)] = S.pieceEmpty
            local rookIndex = S.g_pieceIndex[1 + (to + 1)]
            S.g_pieceIndex[1 + (to - 2)] = rookIndex
            S.g_pieceList[1 + bit.bor(bit.lshift(bit.band(rook, 0xF), 4), rookIndex)] = to - 2
        end
    end

    if bit.band(flags, S.moveflagPromotion) > 0 then
        piece = bit.bor(bit.band(S.g_board[1 + to], bit.bnot(0x7)), S.piecePawn)
        S.g_board[1 + from] = piece

        local pawnType = bit.band(S.g_board[1 + from], 0xF)
        local promoteType = bit.band(S.g_board[1 + to], 0xF)

        S.g_pieceCount[1 + promoteType] = S.g_pieceCount[1 + promoteType] - 1
        local lastPromoteSquare = S.g_pieceList[1 + bit.bor(bit.lshift(promoteType, 4), S.g_pieceCount[1 + promoteType])]
        S.g_pieceIndex[1 + lastPromoteSquare] = S.g_pieceIndex[1 + to]
        S.g_pieceList[1 + bit.bor(bit.lshift(promoteType, 4), S.g_pieceIndex[1 + lastPromoteSquare])] = lastPromoteSquare
        S.g_pieceList[1 + bit.bor(bit.lshift(promoteType, 4), S.g_pieceCount[1 + promoteType])] = 0
        S.g_pieceIndex[1 + to] = S.g_pieceCount[1 + pawnType]
        S.g_pieceList[1 + bit.bor(bit.lshift(pawnType, 4), S.g_pieceIndex[1 + to])] = to
        S.g_pieceCount[1 + pawnType] = S.g_pieceCount[1 + pawnType] + 1
    else
        S.g_board[1 + from] = S.g_board[1 + to]
    end

    local epcEnd = to
    if bit.band(flags, S.moveflagEPC) > 0 then
        if S.g_toMove == S.colorWhite then
            epcEnd = to + 0x10
        else
            epcEnd = to - 0x10
        end
        S.g_board[1 + to] = S.pieceEmpty
    end

    S.g_board[1 + epcEnd] = captured

    S.g_pieceIndex[1 + from] = S.g_pieceIndex[1 + to]
    S.g_pieceList[1 + bit.bor(bit.lshift(bit.band(piece, 0xF), 4), S.g_pieceIndex[1 + from])] = from

    if captured > 0 then
        local captureType = bit.band(captured, 0xF)
        S.g_pieceIndex[1 + epcEnd] = S.g_pieceCount[1 + captureType]
        S.g_pieceList[1 + bit.bor(bit.lshift(captureType, 4), S.g_pieceCount[1 + captureType])] = epcEnd
        S.g_pieceCount[1 + captureType] = S.g_pieceCount[1 + captureType] + 1
    end
end

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

local function AdjMob(S, from, dto, mob, enemy)
    local to = from + dto
    local mb = mob
    while S.g_board[1 + to] == 0 do
        to = to + dto
        mb = mb + 1
    end
    if bit.band(S.g_board[1 + to], enemy) > 0 then
        mb = mb + 1
    end
    return mb
end

local function Mobility(S, color)
    local result = 0
    local enemy = iif(color == 8, 0x10, 0x8)
    local mobUnit = iif(color == 8, S.g_mobUnit[1], S.g_mobUnit[2])

    -- Knight mobility
    local mob = -3
    local pieceIdx = bit.lshift(bit.bor(color, 2), 4)
    local from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        mob = mob + (mobUnit[1 + S.g_board[1 + (from + 31)]] or 0)
        mob = mob + (mobUnit[1 + S.g_board[1 + (from + 33)]] or 0)
        mob = mob + (mobUnit[1 + S.g_board[1 + (from + 14)]] or 0)
        mob = mob + (mobUnit[1 + S.g_board[1 + (from - 14)]] or 0)
        mob = mob + (mobUnit[1 + S.g_board[1 + (from - 31)]] or 0)
        mob = mob + (mobUnit[1 + S.g_board[1 + (from - 33)]] or 0)
        mob = mob + (mobUnit[1 + S.g_board[1 + (from + 18)]] or 0)
        mob = mob + (mobUnit[1 + S.g_board[1 + (from - 18)]] or 0)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end
    result = result + (65 * mob)

    -- Bishop mobility
    mob = -4
    pieceIdx = bit.lshift(bit.bor(color, 3), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        mob = AdjMob(S, from, -15, mob, enemy)
        mob = AdjMob(S, from, -17, mob, enemy)
        mob = AdjMob(S, from, 15, mob, enemy)
        mob = AdjMob(S, from, 17, mob, enemy)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end
    result = result + (50 * mob)

    -- Rook mobility
    mob = -4
    pieceIdx = bit.lshift(bit.bor(color, 4), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        mob = AdjMob(S, from, -1, mob, enemy)
        mob = AdjMob(S, from, 1, mob, enemy)
        mob = AdjMob(S, from, -16, mob, enemy)
        mob = AdjMob(S, from, 16, mob, enemy)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end
    result = result + (25 * mob)

    -- Queen mobility
    mob = -2
    pieceIdx = bit.lshift(bit.bor(color, 5), 4)
    from = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while from ~= 0 do
        mob = AdjMob(S, from, -15, mob, enemy)
        mob = AdjMob(S, from, -17, mob, enemy)
        mob = AdjMob(S, from, 15, mob, enemy)
        mob = AdjMob(S, from, 17, mob, enemy)
        mob = AdjMob(S, from, -1, mob, enemy)
        mob = AdjMob(S, from, 1, mob, enemy)
        mob = AdjMob(S, from, -16, mob, enemy)
        mob = AdjMob(S, from, 16, mob, enemy)
        from = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end
    result = result + (22 * mob)

    return result
end

local function Evaluate(S)
    local curEval = S.g_baseEval
    local mobility = Mobility(S, 8) - Mobility(S, 0)

    local evalAdjust = 0
    if S.g_pieceList[1 + bit.lshift(S.pieceQueen, 4)] == 0 then
        evalAdjust = evalAdjust - S.pieceSquareAdj[1 + S.pieceKing][1 + S.g_pieceList[1 + bit.lshift(bit.bor(S.colorWhite, S.pieceKing), 4)]]
    end
    if S.g_pieceList[1 + bit.lshift(bit.bor(S.colorWhite, S.pieceQueen), 4)] == 0 then
        evalAdjust = evalAdjust + S.pieceSquareAdj[1 + S.pieceKing][1 + S.flipTable[1 + S.g_pieceList[1 + bit.lshift(S.pieceKing, 4)]]]
    end

    if S.g_pieceCount[1 + S.pieceBishop] >= 2 then
        evalAdjust = evalAdjust - 500
    end
    if S.g_pieceCount[1 + bit.bor(S.pieceBishop, S.colorWhite)] >= 2 then
        evalAdjust = evalAdjust + 500
    end

    if S.g_toMove == 0 then
        curEval = curEval - mobility
        curEval = curEval - evalAdjust
    else
        curEval = curEval + mobility
        curEval = curEval + evalAdjust
    end

    return curEval
end

--------------------------------------------------------------------------------
-- Move scoring and validation
--------------------------------------------------------------------------------

local function ScoreMove(S, move)
    local moveTo = bit.band(bit.rshift(move, 8), 0xFF)
    local captured = bit.band(S.g_board[1 + moveTo], 0x7)
    local piece = S.g_board[1 + bit.band(move, 0xFF)]
    if captured ~= 0 then
        return bit.lshift(captured, 5) - bit.band(piece, 0x7)
    else
        return S.historyTable[1 + bit.band(piece, 0xF)][1 + moveTo] or 0
    end
end

local function IsRepDraw(S)
    local i = S.g_moveCount - 5
    local stop = S.g_moveCount - 1 - S.g_move50
    if stop < 0 then stop = 0 end
    while i >= stop do
        if S.g_repMoveStack[1 + i] == S.g_hashKeyLow then
            return true
        end
        i = i - 2
    end
    return false
end

local function IsHashMoveValid(S, hashMove)
    local from = bit.band(hashMove, 0xFF)
    local to = bit.band(bit.rshift(hashMove, 8), 0xFF)
    local dir = to - from
    local ourPiece = S.g_board[1 + from]
    local pieceType = bit.band(ourPiece, 0x7)

    if pieceType < S.piecePawn or pieceType > S.pieceKing then return false end
    if S.g_toMove ~= bit.band(ourPiece, 0x8) then return false end
    if S.g_board[1 + to] ~= 0 and (S.g_toMove == bit.band(S.g_board[1 + to], 0x8)) then return false end

    if pieceType == S.piecePawn then
        if bit.band(hashMove, S.moveflagEPC) > 0 then return false end
        if (S.g_toMove == S.colorWhite) ~= (dir < 0) then return false end
        local row = bit.band(to, 0xF0)
        if ((row == 0x90 and S.g_toMove == 0) or (row == 0x20 and S.g_toMove > 0)) ~= (bit.band(hashMove, S.moveflagPromotion) > 0) then
            return false
        end
        if dir == -16 or dir == 16 then
            return S.g_board[1 + to] == 0
        elseif dir == -15 or dir == -17 or dir == 15 or dir == 17 then
            return S.g_board[1 + to] ~= 0
        elseif dir == -32 then
            if row ~= 0x60 then return false end
            if S.g_board[1 + to] ~= 0 then return false end
            if S.g_board[1 + (from - 16)] ~= 0 then return false end
        elseif dir == 32 then
            if row ~= 0x50 then return false end
            if S.g_board[1 + to] ~= 0 then return false end
            if S.g_board[1 + (from + 16)] ~= 0 then return false end
        else
            return false
        end
        return true
    else
        if bit.rshift(hashMove, 16) > 0 then return false end
        return IsSquareAttackableFrom(S, to, from)
    end
end

--------------------------------------------------------------------------------
-- SEE (Static Exchange Evaluation)
--------------------------------------------------------------------------------

local function IsSquareOnPieceLine(S, target, from)
    local index = from - target + 128
    local piece = S.g_board[1 + from]
    return bit.band(S.g_vectorDelta[1 + index].pieceMask[1 + bit.band(bit.rshift(piece, 3), 1)],
                    bit.lshift(1, bit.band(piece, 0x7))) > 0
end

local function SeeAddKnightAttacks(S, target, us, attacks)
    local pieceIdx = bit.lshift(bit.bor(us, S.pieceKnight), 4)
    local attackerSq = S.g_pieceList[1 + pieceIdx]
    pieceIdx = pieceIdx + 1
    while attackerSq ~= 0 do
        if IsSquareOnPieceLine(S, target, attackerSq) then
            attacks[#attacks + 1] = attackerSq
        end
        attackerSq = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end
end

local function SeeAddSliderAttacks(S, target, us, attacks, pieceType)
    local pieceIdx = bit.lshift(bit.bor(us, pieceType), 4)
    local attackerSq = S.g_pieceList[1 + pieceIdx]
    local hit = false
    pieceIdx = pieceIdx + 1
    while attackerSq ~= 0 do
        if IsSquareAttackableFrom(S, target, attackerSq) then
            attacks[#attacks + 1] = attackerSq
            hit = true
        end
        attackerSq = S.g_pieceList[1 + pieceIdx]
        pieceIdx = pieceIdx + 1
    end
    return hit
end

local function SeeAddXrayAttack(S, target, square, us, usAttacks, themAttacks)
    local index = square - target + 128
    local delta = -S.g_vectorDelta[1 + index].delta
    if delta == 0 then return end
    square = square + delta
    while S.g_board[1 + square] == 0 do
        square = square + delta
    end
    if bit.band(S.g_board[1 + square], 0x18) > 0 and IsSquareOnPieceLine(S, target, square) then
        if bit.band(S.g_board[1 + square], 8) == us then
            usAttacks[#usAttacks + 1] = square
        else
            themAttacks[#themAttacks + 1] = square
        end
    end
end

local function See(S, move)
    local from = bit.band(move, 0xFF)
    local to = bit.band(bit.rshift(move, 8), 0xFF)
    local fromPiece = S.g_board[1 + from]
    local us = iif(bit.band(fromPiece, S.colorWhite) > 0, S.colorWhite, 0)
    local them = 8 - us
    local themAttacks = {}
    local usAttacks = {}

    local fromValue = S.g_seeValues[1 + bit.band(fromPiece, 0xF)]
    local toValue = S.g_seeValues[1 + bit.band(S.g_board[1 + to], 0xF)]
    local seeValue = toValue - fromValue
    local inc = iif(bit.band(fromPiece, S.colorWhite) > 0, -16, 16)
    local captureDeficit = fromValue - toValue

    if fromValue <= toValue then return true end
    if bit.rshift(move, 16) > 0 then return true end

    if bit.band(S.g_board[1 + (to + inc + 1)], 0xF) == bit.bor(S.piecePawn, them) or
       bit.band(S.g_board[1 + (to + inc - 1)], 0xF) == bit.bor(S.piecePawn, them) then
        return false
    end

    SeeAddKnightAttacks(S, to, them, themAttacks)
    if #themAttacks ~= 0 and captureDeficit > S.g_seeValues[1 + S.pieceKnight] then
        return false
    end

    S.g_board[1 + from] = 0
    for pieceType = S.pieceBishop, S.pieceQueen do
        if SeeAddSliderAttacks(S, to, them, themAttacks, pieceType) then
            if captureDeficit > S.g_seeValues[1 + pieceType] then
                S.g_board[1 + from] = fromPiece
                return false
            end
        end
    end

    if bit.band(S.g_board[1 + (to - inc + 1)], 0xF) == bit.bor(S.piecePawn, us) or
       bit.band(S.g_board[1 + (to - inc - 1)], 0xF) == bit.bor(S.piecePawn, us) then
        S.g_board[1 + from] = fromPiece
        return true
    end

    SeeAddSliderAttacks(S, to, them, themAttacks, S.pieceKing)
    SeeAddKnightAttacks(S, to, us, usAttacks)
    for pieceType = S.pieceBishop, S.pieceKing do
        SeeAddSliderAttacks(S, to, us, usAttacks, pieceType)
    end

    S.g_board[1 + from] = fromPiece

    while true do
        local capturingPieceValue = 1000
        local capturingPieceIndex = -1

        for i = 1, #themAttacks do
            if themAttacks[i] ~= 0 then
                local pieceValue = S.g_seeValues[1 + bit.band(S.g_board[1 + themAttacks[i]], 0x7)]
                if pieceValue < capturingPieceValue then
                    capturingPieceValue = pieceValue
                    capturingPieceIndex = i
                end
            end
        end

        if capturingPieceIndex == -1 then return true end

        seeValue = seeValue + capturingPieceValue
        if seeValue < 0 then return false end

        local capturingPieceSquare = themAttacks[capturingPieceIndex]
        themAttacks[capturingPieceIndex] = 0
        SeeAddXrayAttack(S, to, capturingPieceSquare, us, usAttacks, themAttacks)

        capturingPieceValue = 1000
        capturingPieceIndex = -1

        for i = 1, #usAttacks do
            if usAttacks[i] ~= 0 then
                local pieceValue = S.g_seeValues[1 + bit.band(S.g_board[1 + usAttacks[i]], 0x7)]
                if pieceValue < capturingPieceValue then
                    capturingPieceValue = pieceValue
                    capturingPieceIndex = i
                end
            end
        end

        if capturingPieceIndex == -1 then return false end

        seeValue = seeValue - capturingPieceValue
        if seeValue >= 0 then return true end

        capturingPieceSquare = usAttacks[capturingPieceIndex]
        usAttacks[capturingPieceIndex] = 0
        SeeAddXrayAttack(S, to, capturingPieceSquare, us, usAttacks, themAttacks)
    end
end

--------------------------------------------------------------------------------
-- Move Picker
--------------------------------------------------------------------------------

local function MovePicker(S, mp, hashMove, depth, killer1, killer2)
    mp.hashMove = hashMove
    mp.depth = depth
    mp.killer1 = killer1
    mp.killer2 = killer2
    mp.moves = {}
    mp.losingCaptures = nil
    mp.moveCount = 0
    mp.atMove = -1
    mp.moveScores = nil
    mp.stage = 0
end

local function nextMove(S, mp)
    mp.atMove = mp.atMove + 1

    if mp.atMove == mp.moveCount then
        mp.stage = mp.stage + 1
        if mp.stage == 1 then
            if mp.hashMove ~= nil and IsHashMoveValid(S, mp.hashMove) then
                mp.moves[1] = mp.hashMove
                mp.moveCount = 1
            end
            if mp.moveCount ~= 1 then
                mp.hashMove = nil
                mp.stage = mp.stage + 1
            end
        end

        if mp.stage == 2 then
            GenerateCaptureMoves(S, mp.moves)
            mp.moveCount = #mp.moves
            mp.moveScores = {}
            for i = mp.atMove, mp.moveCount - 1 do
                local captured = bit.band(S.g_board[1 + bit.band(bit.rshift(mp.moves[1 + i], 8), 0xFF)], 0x7)
                local pieceType = bit.band(S.g_board[1 + bit.band(mp.moves[1 + i], 0xFF)], 0x7)
                mp.moveScores[1 + i] = bit.lshift(captured, 5) - pieceType
            end
            if mp.atMove == mp.moveCount then mp.stage = mp.stage + 1 end
        end

        if mp.stage == 3 then
            if IsHashMoveValid(S, mp.killer1) and mp.killer1 ~= mp.hashMove then
                mp.moves[#mp.moves + 1] = mp.killer1
                mp.moveCount = #mp.moves
            else
                mp.killer1 = 0
                mp.stage = mp.stage + 1
            end
        end

        if mp.stage == 4 then
            if IsHashMoveValid(S, mp.killer2) and mp.killer2 ~= mp.hashMove then
                mp.moves[#mp.moves + 1] = mp.killer2
                mp.moveCount = #mp.moves
            else
                mp.killer2 = 0
                mp.stage = mp.stage + 1
            end
        end

        if mp.stage == 5 then
            GenerateAllMoves(S, mp.moves)
            mp.moveCount = #mp.moves
            for i = mp.atMove, mp.moveCount - 1 do
                mp.moveScores[1 + i] = ScoreMove(S, mp.moves[1 + i])
            end
            if mp.atMove == mp.moveCount then mp.stage = mp.stage + 1 end
        end

        if mp.stage == 6 then
            if mp.losingCaptures ~= nil then
                for i = 1, #mp.losingCaptures do
                    mp.moves[#mp.moves + 1] = mp.losingCaptures[i]
                end
                for i = mp.atMove, mp.moveCount - 1 do
                    mp.moveScores[1 + i] = ScoreMove(S, mp.moves[1 + i])
                end
                mp.moveCount = #mp.moves
            end
            if mp.atMove == mp.moveCount then mp.stage = mp.stage + 1 end
        end

        if mp.stage == 7 then return 0 end
    end

    local bestMove = mp.atMove
    for j = mp.atMove + 1, mp.moveCount - 1 do
        if mp.moveScores[1 + j] == nil then break end
        if mp.moveScores[1 + j] > (mp.moveScores[1 + bestMove] or -999999) then
            bestMove = j
        end
    end

    if bestMove ~= mp.atMove then
        local tmpMove = mp.moves[1 + mp.atMove]
        mp.moves[1 + mp.atMove] = mp.moves[1 + bestMove]
        mp.moves[1 + bestMove] = tmpMove
        local tmpScore = mp.moveScores[1 + mp.atMove]
        mp.moveScores[1 + mp.atMove] = mp.moveScores[1 + bestMove]
        mp.moveScores[1 + bestMove] = tmpScore
    end

    local candidateMove = mp.moves[1 + mp.atMove]
    if (mp.stage > 1 and candidateMove == mp.hashMove) or
       (mp.stage > 3 and candidateMove == mp.killer1) or
       (mp.stage > 4 and candidateMove == mp.killer2) then
        return nextMove(S, mp)
    end

    if mp.stage == 2 and not See(S, candidateMove) then
        if mp.losingCaptures == nil then
            mp.losingCaptures = {}
        end
        mp.losingCaptures[#mp.losingCaptures + 1] = candidateMove
        return nextMove(S, mp)
    end

    return mp.moves[1 + mp.atMove]
end

--------------------------------------------------------------------------------
-- Quiescence Search
--------------------------------------------------------------------------------

local function QSearch(S, alpha, beta, ply)
    S.g_qNodeCount = S.g_qNodeCount + 1

    local realEval = iif(S.g_inCheck, (S.minEval + 1), Evaluate(S))

    if realEval >= beta then return realEval end
    if realEval > alpha then alpha = realEval end

    local moves = {}
    local moveScores = {}
    local wasInCheck = S.g_inCheck

    if wasInCheck then
        GenerateCaptureMoves(S, moves)
        GenerateAllMoves(S, moves)
        for i = 0, #moves - 1 do
            moveScores[1 + i] = ScoreMove(S, moves[1 + i])
        end
    else
        GenerateCaptureMoves(S, moves)
        for i = 0, #moves - 1 do
            local captured = bit.band(S.g_board[1 + bit.band(bit.rshift(moves[1 + i], 8), 0xFF)], 0x7)
            local pieceType = bit.band(S.g_board[1 + bit.band(moves[1 + i], 0xFF)], 0x7)
            moveScores[1 + i] = bit.lshift(captured, 5) - pieceType
        end
    end

    for i = 0, #moves - 1 do
        local bestMove = i
        for j = #moves - 1, i + 1, -1 do
            if moveScores[1 + j] > moveScores[1 + bestMove] then
                bestMove = j
            end
        end
        local tmpMove = moves[1 + i]
        moves[1 + i] = moves[1 + bestMove]
        moves[1 + bestMove] = tmpMove
        local tmpScore = moveScores[1 + i]
        moveScores[1 + i] = moveScores[1 + bestMove]
        moveScores[1 + bestMove] = tmpScore

        if (wasInCheck or See(S, moves[1 + i])) and MakeMove(S, moves[1 + i]) then
            local value = -QSearch(S, -beta, -alpha, ply - 1)
            UnmakeMove(S, moves[1 + i])
            if value > realEval then
                if value >= beta then return value end
                if value > alpha then alpha = value end
                realEval = value
            end
        end
    end

    if ply == 0 and not wasInCheck then
        moves = {}
        GenerateAllMoves(S, moves)
        for i = 0, #moves - 1 do
            moveScores[1 + i] = ScoreMove(S, moves[1 + i])
        end
        for i = 0, #moves - 1 do
            local bestMove = i
            for j = #moves - 1, i + 1, -1 do
                if moveScores[1 + j] > moveScores[1 + bestMove] then
                    bestMove = j
                end
            end
            local tmpMove = moves[1 + i]
            moves[1 + i] = moves[1 + bestMove]
            moves[1 + bestMove] = tmpMove
            local tmpScore = moveScores[1 + i]
            moveScores[1 + i] = moveScores[1 + bestMove]
            moveScores[1 + bestMove] = tmpScore

            local brk = false
            if not MakeMove(S, moves[1 + i]) then
                brk = true
            else
                local checking = S.g_inCheck
                UnmakeMove(S, moves[1 + i])
                if not checking then
                    brk = true
                elseif not See(S, moves[1 + i]) then
                    brk = true
                end
            end

            if not brk then
                MakeMove(S, moves[1 + i])
                local value = -QSearch(S, -beta, -alpha, ply - 1)
                UnmakeMove(S, moves[1 + i])
                if value > realEval then
                    if value >= beta then return value end
                    if value > alpha then alpha = value end
                    realEval = value
                end
            end
        end
    end

    return realEval
end

--------------------------------------------------------------------------------
-- Alpha-Beta Search
--------------------------------------------------------------------------------

local function AllCutNode(S, ply, depth, beta, allowNull)
    if ply <= 0 then
        return QSearch(S, beta - 1, beta, 0)
    end

    if S.g_timeout and (DeltaChess.Util.Clock() - S.g_startTime > S.g_timeout) then
        S.g_searchValid = false
        return beta - 1
    end

    if S.g_finCnt > S.g_maxfinCnt then
        S.g_searchValid = false
        return beta - 1
    end

    S.g_nodeCount = S.g_nodeCount + 1

    if IsRepDraw(S) then return 0 end

    if S.minEval + depth >= beta then return beta end
    if (S.maxEval - (depth + 1)) < beta then return (beta - 1) end

    local hashNode = S.g_hashTable[1 + bit.band(S.g_hashKeyLow, S.g_hashMask)]
    local hashMove = nil

    if hashNode ~= nil and hashNode.lock == S.g_hashKeyHigh then
        hashMove = hashNode.bestMove
        if hashNode.hashDepth >= ply then
            local hashValue = hashNode.value
            if hashValue >= S.maxMateBuffer then
                hashValue = hashValue - depth
            elseif hashValue <= S.minMateBuffer then
                hashValue = hashValue + depth
            end
            if hashNode.flags == S.hashflagExact then return hashValue end
            if hashNode.flags == S.hashflagAlpha and hashValue < beta then return hashValue end
            if hashNode.flags == S.hashflagBeta and hashValue >= beta then return hashValue end
        end
    end

    if not S.g_inCheck and allowNull and beta > S.minMateBuffer and beta < S.maxMateBuffer then
        local razorMargin = 2500 + 200 * ply
        if hashMove == nil and ply < 4 then
            if S.g_baseEval < beta - razorMargin then
                local razorBeta = beta - razorMargin
                local v = QSearch(S, razorBeta - 1, razorBeta, 0)
                if v < razorBeta then return v end
            end
        end

        if ply > 1 and S.g_baseEval >= beta - iif(ply >= 4, 2500, 0) and
           (S.g_pieceCount[1 + bit.bor(S.pieceBishop, S.g_toMove)] ~= 0 or
            S.g_pieceCount[1 + bit.bor(S.pieceKnight, S.g_toMove)] ~= 0 or
            S.g_pieceCount[1 + bit.bor(S.pieceRook, S.g_toMove)] ~= 0 or
            S.g_pieceCount[1 + bit.bor(S.pieceQueen, S.g_toMove)] ~= 0) then
            local r = 3 + iif(ply >= 5, 1, ply / 4)
            if S.g_baseEval - beta > 1500 then r = r + 1 end

            S.g_toMove = 8 - S.g_toMove
            S.g_baseEval = -S.g_baseEval
            S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristBlackLow)
            S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristBlackHigh)

            local value = -AllCutNode(S, ply - r, depth + 1, -(beta - 1), false)

            S.g_hashKeyLow = bit.bxor(S.g_hashKeyLow, S.g_zobristBlackLow)
            S.g_hashKeyHigh = bit.bxor(S.g_hashKeyHigh, S.g_zobristBlackHigh)
            S.g_toMove = 8 - S.g_toMove
            S.g_baseEval = -S.g_baseEval

            if value >= beta then return beta end
        end
    end

    local moveMade = false
    local realEval = S.minEval
    local mp = {}
    MovePicker(S, mp, hashMove, depth, S.g_killers[1 + depth][1], S.g_killers[1 + depth][2])

    while true do
        local currentMove = nextMove(S, mp)
        if currentMove == 0 then break end

        local plyToSearch = ply - 1

        if MakeMove(S, currentMove) then
            local doFullSearch = true

            if S.g_inCheck then
                plyToSearch = plyToSearch + 1
            else
                if mp.stage == 5 and mp.atMove > 5 and ply >= 3 then
                    local reduced = plyToSearch - iif(mp.atMove > 14, 2, 1)
                    local value = -AllCutNode(S, reduced, depth + 1, -(beta - 1), true)
                    doFullSearch = (value >= beta)
                end
            end

            local value
            if doFullSearch then
                value = -AllCutNode(S, plyToSearch, depth + 1, -(beta - 1), true)
            end

            moveMade = true
            UnmakeMove(S, currentMove)

            if not S.g_searchValid then return beta - 1 end

            if value and value > realEval then
                if value >= beta then
                    local histTo = bit.band(bit.rshift(currentMove, 8), 0xFF)
                    if S.g_board[1 + histTo] == 0 then
                        local histPiece = bit.band(S.g_board[1 + bit.band(currentMove, 0xFF)], 0xF)
                        local h = S.historyTable[1 + histPiece][1 + histTo]
                        h = h + (ply * ply)
                        if h > 32767 then h = bit.rshift(h, 1) end
                        S.historyTable[1 + histPiece][1 + histTo] = h

                        if S.g_killers[1 + depth][1] ~= currentMove then
                            S.g_killers[1 + depth][2] = S.g_killers[1 + depth][1]
                            S.g_killers[1 + depth][1] = currentMove
                        end
                    end
                    StoreHash(S, value, S.hashflagBeta, ply, currentMove, depth)
                    return value
                end
                realEval = value
                hashMove = currentMove
            end
        end
    end

    if not moveMade then
        if S.g_inCheck then
            return (S.minEval + depth)
        else
            return 0
        end
    end

    StoreHash(S, realEval, S.hashflagAlpha, ply, hashMove, depth)
    return realEval
end

local function AlphaBeta(S, ply, depth, alpha, beta)
    if ply <= 0 then
        return QSearch(S, alpha, beta, 0)
    end

    S.g_nodeCount = S.g_nodeCount + 1

    if depth > 0 and IsRepDraw(S) then return 0 end

    alpha = iif(alpha < S.minEval + depth, alpha, S.minEval + depth)
    beta = iif(beta > S.maxEval - (depth + 1), beta, S.maxEval - (depth + 1))
    if alpha >= beta then return alpha end

    local hashNode = S.g_hashTable[1 + bit.band(S.g_hashKeyLow, S.g_hashMask)]
    local hashMove = nil
    local hashFlag = S.hashflagAlpha

    if hashNode ~= nil and hashNode.lock == S.g_hashKeyHigh then
        hashMove = hashNode.bestMove
    end

    local moveMade = false
    local realEval = S.minEval
    local oldAlpha = alpha
    local mp = {}
    MovePicker(S, mp, hashMove, depth, S.g_killers[1 + depth][1], S.g_killers[1 + depth][2])

    while true do
        local currentMove = nextMove(S, mp)
        if currentMove == 0 then break end

        local plyToSearch = ply - 1

        if MakeMove(S, currentMove) then
            if S.g_inCheck then
                plyToSearch = plyToSearch + 1
            end

            local value
            if moveMade then
                value = -AllCutNode(S, plyToSearch, depth + 1, -alpha, true)
                if value > alpha then
                    value = -AlphaBeta(S, plyToSearch, depth + 1, -beta, -alpha)
                end
            else
                value = -AlphaBeta(S, plyToSearch, depth + 1, -beta, -alpha)
            end

            moveMade = true
            UnmakeMove(S, currentMove)

            if not S.g_searchValid then return alpha end

            if value > realEval then
                if value >= beta then
                    local histTo = bit.band(bit.rshift(currentMove, 8), 0xFF)
                    if S.g_board[1 + histTo] == 0 then
                        local histPiece = bit.band(S.g_board[1 + bit.band(currentMove, 0xFF)], 0xF)
                        local h = S.historyTable[1 + histPiece][1 + histTo]
                        h = h + (ply * ply)
                        if h > 32767 then h = bit.rshift(h, 1) end
                        S.historyTable[1 + histPiece][1 + histTo] = h

                        if S.g_killers[1 + depth][1] ~= currentMove then
                            S.g_killers[1 + depth][2] = S.g_killers[1 + depth][1]
                            S.g_killers[1 + depth][1] = currentMove
                        end
                    end
                    StoreHash(S, value, S.hashflagBeta, ply, currentMove, depth)
                    return value
                end

                if value > oldAlpha then
                    hashFlag = S.hashflagExact
                    alpha = value
                end

                realEval = value
                hashMove = currentMove
            end
        end
    end

    if not moveMade then
        if S.g_inCheck then
            return (S.minEval + depth)
        else
            return 0
        end
    end

    StoreHash(S, realEval, hashFlag, ply, hashMove, depth)
    return realEval
end

--------------------------------------------------------------------------------
-- Format move to UCI
--------------------------------------------------------------------------------

local function FormatMove(S, move)
    local result = FormatSquare(S, bit.band(move, 0xFF)) .. FormatSquare(S, bit.band(bit.rshift(move, 8), 0xFF))
    if bit.band(move, S.moveflagPromotion) > 0 then
        if bit.band(move, S.moveflagPromoteBishop) > 0 then
            result = result .. "b"
        elseif bit.band(move, S.moveflagPromoteKnight) > 0 then
            result = result .. "n"
        elseif bit.band(move, S.moveflagPromoteQueen) > 0 then
            result = result .. "q"
        else
            result = result .. "r"
        end
    end
    return result
end

--------------------------------------------------------------------------------
-- Search function (loopFn yields between plies to keep event loop responsive)
--------------------------------------------------------------------------------

local function Search(S, maxPly, onPlyCallback, onCompleteCallback, loopFn)
    loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
    local alpha = S.minEval
    local beta = S.maxEval
    local bestMove = 0
    local value = 0

    S.g_globalPly = S.g_globalPly + 1
    S.g_nodeCount = 0
    S.g_qNodeCount = 0
    S.g_searchValid = true
    S.g_foundmove = 0
    S.g_finCnt = 0
    S.g_startTime = DeltaChess.Util.Clock()

    -- Step-based iterative deepening.
    -- Each call to stepFn() runs one ply of search and returns
    -- true (continue) or false (done).
    local ply = 1
    local redoPly = false

    loopFn(function()
        -- stepFn: one ply per step
        if ply > maxPly or not S.g_searchValid then
            return false  -- done
        end

        local tmp = AlphaBeta(S, ply, 0, alpha, beta)
        if not S.g_searchValid then
            return false  -- done
        end

        value = tmp

        if value > alpha and value < beta then
            alpha = value - 500
            beta = value + 500
            if alpha < S.minEval then alpha = S.minEval end
            if beta > S.maxEval then beta = S.maxEval end
        elseif alpha ~= S.minEval then
            -- Redo same ply with full window
            alpha = S.minEval
            beta = S.maxEval
            redoPly = true
            return true  -- continue (will redo same ply)
        end

        if S.g_hashTable[1 + bit.band(S.g_hashKeyLow, S.g_hashMask)] ~= nil then
            bestMove = S.g_hashTable[1 + bit.band(S.g_hashKeyLow, S.g_hashMask)].bestMove
        end

        if onPlyCallback then
            onPlyCallback(bestMove, value, ply)
        end

        if redoPly then
            redoPly = false
            -- Don't advance ply, redo at same depth
        else
            ply = ply + 1
        end
        return true  -- continue to next ply
    end, function()
        -- doneFn
        if onCompleteCallback then
            onCompleteCallback(bestMove, value, ply - 1)
        end
    end)
end

--------------------------------------------------------------------------------
-- Engine Interface Implementation
--------------------------------------------------------------------------------

function M:GetEloRange()
    return { 1400, 2200 }
end

function M:GetAverageCpuTime(elo)
    -- Estimated CPU time in ms based on ELO (deeper search at higher ELO)
    -- Always faster than sunfish and fruit
    -- Faster than zugzug at depth 4+, slower at depths 1-3
    local range = self:GetEloRange()
    local t = (elo - range[1]) / math.max(1, range[2] - range[1])
    -- At low ELO (depth 1-3): ~200ms (slower than zugzug at depths 1-3)
    -- At high ELO (depth 4+): ~700ms (faster than zugzug at depth 4)
    -- Always faster than sunfish/fruit
    return 200 + t * 500  -- 200ms to 700ms
end

--- Map state.elo to search depth (weaker = shallower, stronger = deeper). Uses state.ply_limit when set.
function M:GetSearchDepth(state)
    if state.ply_limit ~= nil then
        return state.ply_limit
    end
    return DeltaChess.Util.EloToPly(state.elo, self:GetEloRange(), 1, 8)
end

-- Parse a UCI move string and return internal move representation
local function ParseUciMove(S, uciMove)
    if not uciMove or #uciMove < 4 then return nil end
    
    local from = deFormatSquare(string.sub(uciMove, 1, 2))
    local to = deFormatSquare(string.sub(uciMove, 3, 4))
    local promo = #uciMove >= 5 and string.sub(uciMove, 5, 5) or nil
    
    -- Generate all legal moves and find matching one
    local moves = {}
    GenerateCaptureMoves(S, moves)
    GenerateAllMoves(S, moves)
    
    for _, move in ipairs(moves) do
        local moveFrom = bit.band(move, 0xFF)
        local moveTo = bit.band(bit.rshift(move, 8), 0xFF)
        
        if moveFrom == from and moveTo == to then
            -- Check promotion match
            if promo then
                local flags = bit.band(move, 0xFF0000)
                if bit.band(flags, S.moveflagPromotion) > 0 then
                    local promoLower = string.lower(promo)
                    if promoLower == "q" and bit.band(flags, S.moveflagPromoteQueen) > 0 then
                        return move
                    elseif promoLower == "n" and bit.band(flags, S.moveflagPromoteKnight) > 0 then
                        return move
                    elseif promoLower == "b" and bit.band(flags, S.moveflagPromoteBishop) > 0 then
                        return move
                    elseif promoLower == "r" and bit.band(flags, S.moveflagPromotion) > 0 and 
                           bit.band(flags, S.moveflagPromoteQueen) == 0 and
                           bit.band(flags, S.moveflagPromoteKnight) == 0 and
                           bit.band(flags, S.moveflagPromoteBishop) == 0 then
                        return move
                    end
                end
            else
                -- Non-promotion move
                if bit.band(move, S.moveflagPromotion) == 0 then
                    return move
                end
            end
        end
    end
    
    return nil
end

function M:Calculate(state, loopFn, stepFn, onComplete)
    loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
    onComplete = onComplete or function() end

    if state.cancelled then
        onComplete(nil, "cancelled")
        return
    end

    -- Create fresh engine state for this calculation
    local S = createEngineState()
    InitializeEval(S)
    ResetGame(S)

    local moves = state.moves
    local usedMoveReplay = false

    -- Define runSearch so both replay path and direct path can call it
    local function runSearch()
        -- Fall back to FEN initialization if move replay wasn't used
        if not usedMoveReplay then
            local fen = state.fen
            if not fen or fen == "" then
                onComplete(nil, "invalid FEN")
                return
            end

            local ok, err = pcall(function() InitializeFromFen(S, fen) end)
            if not ok then
                onComplete(nil, "FEN parse error: " .. tostring(err))
                return
            end
        end

        -- Calculate search depth from ELO
        local maxDepth = self:GetSearchDepth(state)

        -- Set timeout only when time_limit_ms is provided; otherwise no time limit is checked
        if state.time_limit_ms then
            S.g_timeout = state.time_limit_ms / 1000
        else
            S.g_timeout = nil
        end

        if state.cancelled then
            onComplete(nil, "cancelled")
            return
        end

        -- Search uses loopFn to yield between plies
        Search(S, maxDepth, nil, function(bestMove, value, depth)
            if bestMove == 0 or bestMove == nil then
                onComplete(nil, "no legal moves")
                return
            end

            local uci = FormatMove(S, bestMove)
            onComplete({
                move = uci,
                san = uci,
                depth = depth or maxDepth,
                nodes = S.g_nodeCount + S.g_qNodeCount,
                score = value,
            }, nil)
        end, loopFn)
    end

    -- If moves are provided, replay them from start position to populate
    -- transposition tables and repetition history for better search
    if moves and #moves > 0 then
        -- Initialize from starting position
        local startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        local ok, err = pcall(function() InitializeFromFen(S, startFen) end)
        if ok then
            -- Replay moves step-by-step using loopFn
            local replayIdx = 1
            local replayFailed = false

            loopFn(function()
                -- stepFn: replay one move per step
                if state.cancelled then
                    return false
                end
                if replayIdx > #moves then
                    return false  -- done replaying
                end
                local uciMove = moves[replayIdx]
                local move = ParseUciMove(S, uciMove)
                if not move then
                    -- Replay failed; reset and fall back to FEN
                    S = createEngineState()
                    InitializeEval(S)
                    ResetGame(S)
                    replayFailed = true
                    return false
                end
                local success = MakeMove(S, move)
                if not success then
                    S = createEngineState()
                    InitializeEval(S)
                    ResetGame(S)
                    replayFailed = true
                    return false
                end
                replayIdx = replayIdx + 1
                return true  -- continue replaying
            end, function()
                -- doneFn: replay done, start search
                if state.cancelled then
                    onComplete(nil, "cancelled")
                    return
                end
                if not replayFailed then
                    usedMoveReplay = true
                end
                runSearch()
            end)
            return
        end
    end

    -- No move replay path: run search
    runSearch()
end

DeltaChess.Engines.Registry[M.id] = M
return M
