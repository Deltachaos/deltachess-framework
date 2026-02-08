--[[
  Sunfish engine: MTD-bi search with iterative deepening, transposition table.
  Lua port based on https://github.com/thomasahle/sunfish
  Original transpilation by Soumith Chintala (BSD License).
  Adapted to DeltaChess engine interface.
  Registers globally: DeltaChess.Engines.Registry.sunfish
]]

DeltaChess = DeltaChess or {}
DeltaChess.Engines = DeltaChess.Engines or {}
DeltaChess.Engines.Registry = DeltaChess.Engines.Registry or {}

-- Load MoveGen for legal move validation
local MoveGen = DeltaChess.MoveGen

local M = {
    id = "sunfish",
    name = "Sunfish.lua",
    description = "Minimalist MTD-bi based chess engine, featuring a compact search algorithm with simple evaluation",
    author = "Thomas Ahle",
    portedBy = "Soumith Chintala",
    url = "https://github.com/soumith/sunfish.lua",
    license = "GPL-3.0"
}

M.id = "sunfish"
M.name = "Sunfish"
M.description = "MTD-bi search with iterative deepening and transposition tables. Strong tactical play."

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------
local TABLE_SIZE = 1e6
local DEFAULT_NODES = 1e4
local MATE_VALUE = 30000

-- Board indices for 10x12 mailbox
local A1, H1, A8, H8 = 91, 98, 21, 28
local __1 = 1  -- 1-index correction

--------------------------------------------------------------------------------
-- Move directions
--------------------------------------------------------------------------------
local N, E, S, W = -10, 1, 10, -1
local directions = {
    P = {N, 2*N, N+W, N+E},
    N = {2*N+E, N+2*E, S+2*E, 2*S+E, 2*S+W, S+2*W, N+2*W, 2*N+W},
    B = {N+E, S+E, S+W, N+W},
    R = {N, E, S, W},
    Q = {N, E, S, W, N+E, S+E, S+W, N+W},
    K = {N, E, S, W, N+E, S+E, S+W, N+W}
}

--------------------------------------------------------------------------------
-- Piece-Square Tables
--------------------------------------------------------------------------------
local pst = {
    P = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 198, 198, 198, 198, 198, 198, 198, 198, 0,
        0, 178, 198, 198, 198, 198, 198, 198, 178, 0,
        0, 178, 198, 198, 198, 198, 198, 198, 178, 0,
        0, 178, 198, 208, 218, 218, 208, 198, 178, 0,
        0, 178, 198, 218, 238, 238, 218, 198, 178, 0,
        0, 178, 198, 208, 218, 218, 208, 198, 178, 0,
        0, 178, 198, 198, 198, 198, 198, 198, 178, 0,
        0, 198, 198, 198, 198, 198, 198, 198, 198, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    B = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 797, 824, 817, 808, 808, 817, 824, 797, 0,
        0, 814, 841, 834, 825, 825, 834, 841, 814, 0,
        0, 818, 845, 838, 829, 829, 838, 845, 818, 0,
        0, 824, 851, 844, 835, 835, 844, 851, 824, 0,
        0, 827, 854, 847, 838, 838, 847, 854, 827, 0,
        0, 826, 853, 846, 837, 837, 846, 853, 826, 0,
        0, 817, 844, 837, 828, 828, 837, 844, 817, 0,
        0, 792, 819, 812, 803, 803, 812, 819, 792, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    N = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 627, 762, 786, 798, 798, 786, 762, 627, 0,
        0, 763, 798, 822, 834, 834, 822, 798, 763, 0,
        0, 817, 852, 876, 888, 888, 876, 852, 817, 0,
        0, 797, 832, 856, 868, 868, 856, 832, 797, 0,
        0, 799, 834, 858, 870, 870, 858, 834, 799, 0,
        0, 758, 793, 817, 829, 829, 817, 793, 758, 0,
        0, 739, 774, 798, 810, 810, 798, 774, 739, 0,
        0, 683, 718, 742, 754, 754, 742, 718, 683, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    R = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    Q = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    K = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 60098, 60132, 60073, 60025, 60025, 60073, 60132, 60098, 0,
        0, 60119, 60153, 60094, 60046, 60046, 60094, 60153, 60119, 0,
        0, 60146, 60180, 60121, 60073, 60073, 60121, 60180, 60146, 0,
        0, 60173, 60207, 60148, 60100, 60100, 60148, 60207, 60173, 0,
        0, 60196, 60230, 60171, 60123, 60123, 60171, 60230, 60196, 0,
        0, 60224, 60258, 60199, 60151, 60151, 60199, 60258, 60224, 0,
        0, 60287, 60321, 60262, 60214, 60214, 60262, 60321, 60287, 0,
        0, 60298, 60332, 60273, 60225, 60225, 60273, 60332, 60298, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
}

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
local special = '. \n'

local function isspace(s)
    return s == ' ' or s == '\n'
end

local function isupper(s)
    if special:find(s, 1, true) then return false end
    return s:upper() == s
end

local function islower(s)
    if special:find(s, 1, true) then return false end
    return s:lower() == s
end

local function swapcase(s)
    local s2 = ''
    for i = 1, #s do
        local c = s:sub(i, i)
        if islower(c) then
            s2 = s2 .. c:upper()
        else
            s2 = s2 .. c:lower()
        end
    end
    return s2
end

--------------------------------------------------------------------------------
-- Position class
--------------------------------------------------------------------------------
local Position = {}

function Position.new(board, score, wc, bc, ep, kp, isRotated)
    local self = {}
    self.board = board
    self.score = score
    self.wc = wc
    self.bc = bc
    self.ep = ep
    self.kp = kp
    self.isRotated = isRotated or false  -- true when it's actually black's turn
    for k, v in pairs(Position) do self[k] = v end
    return self
end

--------------------------------------------------------------------------------
-- Convert Sunfish position back to FEN (for move validation)
--------------------------------------------------------------------------------
local function sunfishToFen(pos)
    -- If rotated, un-rotate the board first
    local board = pos.board
    local wc, bc = pos.wc, pos.bc
    local ep = pos.ep
    local stm = "w"
    
    if pos.isRotated then
        board = swapcase(board:reverse())
        wc, bc = pos.bc, pos.wc
        if ep ~= 0 then ep = 119 - ep end
        stm = "b"
    end
    
    -- Extract the 8x8 board from the 10x12 mailbox
    local rows = {}
    for rank = 8, 1, -1 do
        local rowStart = A1 - (rank - 1) * 10  -- A1=91, A2=81, etc.
        local rowStr = ""
        local emptyCount = 0
        for file = 0, 7 do
            local idx = rowStart + file
            local c = board:sub(idx + __1, idx + __1)
            if c == '.' then
                emptyCount = emptyCount + 1
            else
                if emptyCount > 0 then
                    rowStr = rowStr .. tostring(emptyCount)
                    emptyCount = 0
                end
                rowStr = rowStr .. c
            end
        end
        if emptyCount > 0 then
            rowStr = rowStr .. tostring(emptyCount)
        end
        table.insert(rows, rowStr)
    end
    
    local placement = table.concat(rows, "/")
    
    -- Castling rights
    local castling = ""
    if wc[2] then castling = castling .. "K" end
    if wc[1] then castling = castling .. "Q" end
    if bc[2] then castling = castling .. "k" end
    if bc[1] then castling = castling .. "q" end
    if castling == "" then castling = "-" end
    
    -- En passant square
    local epStr = "-"
    if ep ~= 0 then
        local epFile = (ep - A1) % 10
        local epRank = -math.floor((ep - A1) / 10) + 1
        if epFile >= 0 and epFile <= 7 and epRank >= 1 and epRank <= 8 then
            epStr = string.char(epFile + ("a"):byte(1)) .. tostring(epRank)
        end
    end
    
    return placement .. " " .. stm .. " " .. castling .. " " .. epStr .. " 0 1"
end

--------------------------------------------------------------------------------
-- In-engine attack detection (avoids calling MoveGen for every move in the tree)
--------------------------------------------------------------------------------
local function onBoard(sq)
    return sq >= 21 and sq <= 98 and (sq % 10) >= 1 and (sq % 10) <= 8
end

-- Returns true if square sq (0-based) is attacked by pieces of color attackerUpper (true = uppercase).
local function isSquareAttacked(board, sq, attackerUpper)
    local at = function(s) return board:sub(s + __1, s + __1) end
    if attackerUpper then
        -- White P attacks upward (toward lower idx); from sq the attacking P is at sq+S+W or sq+S+E
        for _, d in ipairs({S + W, S + E}) do
            local s = sq + d
            if onBoard(s) and at(s) == 'P' then return true end
        end
        for _, d in ipairs(directions.N) do
            local s = sq + d
            if onBoard(s) and at(s) == 'N' then return true end
        end
        for _, d in ipairs(directions.K) do
            local s = sq + d
            if onBoard(s) and at(s) == 'K' then return true end
        end
        for _, d in ipairs(directions.R) do
            local s = sq + d
            while onBoard(s) do
                local c = at(s)
                if c ~= '.' and c ~= ' ' and c ~= '\n' then
                    if c == 'R' or c == 'Q' then return true end
                    break
                end
                s = s + d
            end
        end
        for _, d in ipairs(directions.B) do
            local s = sq + d
            while onBoard(s) do
                local c = at(s)
                if c ~= '.' and c ~= ' ' and c ~= '\n' then
                    if c == 'B' or c == 'Q' then return true end
                    break
                end
                s = s + d
            end
        end
    else
        -- Black p attacks downward (toward higher idx); from sq the attacking p is at sq+N+W or sq+N+E
        for _, d in ipairs({N + W, N + E}) do
            local s = sq + d
            if onBoard(s) and at(s) == 'p' then return true end
        end
        for _, d in ipairs(directions.N) do
            local s = sq + d
            if onBoard(s) and at(s) == 'n' then return true end
        end
        for _, d in ipairs(directions.K) do
            local s = sq + d
            if onBoard(s) and at(s) == 'k' then return true end
        end
        for _, d in ipairs(directions.R) do
            local s = sq + d
            while onBoard(s) do
                local c = at(s)
                if c ~= '.' and c ~= ' ' and c ~= '\n' then
                    if c == 'r' or c == 'q' then return true end
                    break
                end
                s = s + d
            end
        end
        for _, d in ipairs(directions.B) do
            local s = sq + d
            while onBoard(s) do
                local c = at(s)
                if c ~= '.' and c ~= ' ' and c ~= '\n' then
                    if c == 'b' or c == 'q' then return true end
                    break
                end
                s = s + d
            end
        end
    end
    return false
end

-- Check if move is legal: after making it, our king must not be in check.
-- Position:move returns rotated child; our king (we just moved) is 'k' in child. Check if 'k' is attacked by uppercase.
local function isLegalMove(pos, move)
    local child = pos:move(move)
    local board = child.board
    local kingSq = nil
    for idx = 0, 119 do
        if onBoard(idx) and board:sub(idx + __1, idx + __1) == 'k' then
            kingSq = idx
            break
        end
    end
    if not kingSq then return true end
    return not isSquareAttacked(board, kingSq, true)
end

--------------------------------------------------------------------------------
-- Convert Sunfish move to UCI (for result validation and engine interface)
--------------------------------------------------------------------------------
local function sunfishMoveToUci(from, to, piece, pos)
    local actualFrom, actualTo = from, to
    
    if pos.isRotated then
        actualFrom = 119 - from
        actualTo = 119 - to
    end
    
    local function renderSq(i)
        local rank = math.floor((i - A1) / 10)
        local file = (i - A1) % 10
        return string.char(file + ("a"):byte(1)) .. tostring(-rank + 1)
    end
    
    local uci = renderSq(actualFrom) .. renderSq(actualTo)
    
    -- Check for pawn promotion
    local toRank = -math.floor((actualTo - A1) / 10) + 1
    if piece == 'P' and (toRank == 8 or toRank == 1) then
        uci = uci .. 'q'
    end
    
    return uci
end

function Position:genMoves()
    local pseudoMoves = {}
    for i = 0, #self.board - 1 do
        local p = self.board:sub(i + __1, i + __1)
        if isupper(p) and directions[p] then
            for _, d in ipairs(directions[p]) do
                local limit = (i + d) + (10000) * d
                for j = i + d, limit, d do
                    local q = self.board:sub(j + __1, j + __1)
                    if isspace(self.board:sub(j + __1, j + __1)) then break end
                    if i == A1 and q == 'K' and self.wc[0 + __1] then
                        table.insert(pseudoMoves, {j, j - 2, p})
                    end
                    if i == H1 and q == 'K' and self.wc[1 + __1] then
                        table.insert(pseudoMoves, {j, j + 2, p})
                    end
                    if isupper(q) then break end
                    if p == 'P' and (d == N + W or d == N + E) and q == '.' and j ~= self.ep and j ~= self.kp then
                        break
                    end
                    if p == 'P' and (d == N or d == 2 * N) and q ~= '.' then
                        break
                    end
                    if p == 'P' and d == 2 * N and (i < A1 + N or self.board:sub(i + N + __1, i + N + __1) ~= '.') then
                        break
                    end
                    table.insert(pseudoMoves, {i, j, p})
                    if p == 'P' or p == 'N' or p == 'K' then break end
                    if islower(q) then break end
                end
            end
        end
    end
    
    -- Filter to legal moves using in-engine check (no MoveGen per move)
    local legalMoves = {}
    for _, mv in ipairs(pseudoMoves) do
        if isLegalMove(self, mv) then
            table.insert(legalMoves, {mv[1], mv[2]})
        end
    end
    return legalMoves
end

function Position:rotate()
    return Position.new(
        swapcase(self.board:reverse()), -self.score,
        self.bc, self.wc, 119 - self.ep, 119 - self.kp, not self.isRotated)
end

function Position:move(move)
    assert(move)
    local i, j = move[0 + __1], move[1 + __1]
    local p, q = self.board:sub(i + __1, i + __1), self.board:sub(j + __1, j + __1)
    local function put(board, idx, piece)
        return board:sub(1, idx - 1) .. piece .. board:sub(idx + 1)
    end
    local board = self.board
    local wc, bc, ep, kp = self.wc, self.bc, 0, 0
    local score = self.score + self:value(move)
    board = put(board, j + __1, board:sub(i + __1, i + __1))
    board = put(board, i + __1, '.')
    if i == A1 then wc = {false, wc[0 + __1]} end
    if i == H1 then wc = {wc[0 + __1], false} end
    if j == A8 then bc = {bc[0 + __1], false} end
    if j == H8 then bc = {false, bc[1 + __1]} end
    if p == 'K' then
        wc = {false, false}
        if math.abs(j - i) == 2 then
            kp = math.floor((i + j) / 2)
            board = put(board, j < i and A1 + __1 or H1 + __1, '.')
            board = put(board, kp + __1, 'R')
        end
    end
    if p == 'P' then
        if A8 <= j and j <= H8 then
            board = put(board, j + __1, 'Q')
        end
        if j - i == 2 * N then
            ep = i + N
        end
        if ((j - i) == N + W or (j - i) == N + E) and q == '.' then
            board = put(board, j + S + __1, '.')
        end
    end
    return Position.new(board, score, wc, bc, ep, kp):rotate()
end

function Position:value(move)
    local i, j = move[0 + __1], move[1 + __1]
    local p, q = self.board:sub(i + __1, i + __1), self.board:sub(j + __1, j + __1)
    local score = pst[p][j + __1] - pst[p][i + __1]
    if islower(q) then
        score = score + pst[q:upper()][j + __1]
    end
    if math.abs(j - self.kp) < 2 then
        score = score + pst['K'][j + __1]
    end
    if p == 'K' and math.abs(i - j) == 2 then
        score = score + pst['R'][math.floor((i + j) / 2) + __1]
        score = score - pst['R'][j < i and A1 + __1 or H1 + __1]
    end
    if p == 'P' then
        if A8 <= j and j <= H8 then
            score = score + pst['Q'][j + __1] - pst['P'][j + __1]
        end
        if j == self.ep then
            score = score + pst['P'][j + S + __1]
        end
    end
    return score
end

--------------------------------------------------------------------------------
-- FEN to Sunfish board conversion
--------------------------------------------------------------------------------
local function fenToSunfish(fen)
    local parts = {}
    for p in (fen or ""):gmatch("%S+") do parts[#parts + 1] = p end
    if #parts < 1 then return nil, nil, "invalid fen" end

    local placement = parts[1]
    local stm = (#parts >= 2 and parts[2]) or "w"
    local castling = (#parts >= 3 and parts[3]) or "-"
    local epSquare = (#parts >= 4 and parts[4] ~= "-" and parts[4]) or nil

    -- Build the 120-character board
    local board = '         \n         \n'
    local rows = {}
    for row in placement:gmatch("[^/]+") do
        table.insert(rows, row)
    end

    for rank = 1, 8 do
        local rowStr = ' '
        local fenRow = rows[rank] or "8"
        for i = 1, #fenRow do
            local c = fenRow:sub(i, i)
            if c:match("%d") then
                rowStr = rowStr .. string.rep('.', tonumber(c))
            else
                rowStr = rowStr .. c
            end
        end
        while #rowStr < 9 do rowStr = rowStr .. '.' end
        board = board .. rowStr .. '\n'
    end
    board = board .. '         \n          '

    -- Calculate initial score based on piece positions
    local score = 0
    for i = 0, 119 do
        local c = board:sub(i + __1, i + __1)
        if isupper(c) and pst[c] then
            score = score + pst[c][i + __1]
        elseif islower(c) and pst[c:upper()] then
            score = score - pst[c:upper()][119 - i + __1]
        end
    end

    -- Parse castling rights
    local wc = {castling:find("Q") ~= nil, castling:find("K") ~= nil}
    local bc = {castling:find("q") ~= nil, castling:find("k") ~= nil}

    -- Parse en passant square
    local ep = 0
    if epSquare then
        local file = epSquare:byte(1) - ("a"):byte(1)
        local rank = tonumber(epSquare:sub(2, 2)) - 1
        ep = A1 + file - 10 * rank
    end

    local pos = Position.new(board, score, wc, bc, ep, 0)

    -- If black to move, rotate the position
    if stm == "b" then
        pos = pos:rotate()
    end

    return pos, stm, nil
end

--------------------------------------------------------------------------------
-- Convert sunfish move to UCI
--------------------------------------------------------------------------------
local function renderSquare(i)
    local rank = math.floor((i - A1) / 10)
    local file = (i - A1) % 10
    return string.char(file + ("a"):byte(1)) .. tostring(-rank + 1)
end

local function moveToUci(move, pos, isBlack)
    if not move then return nil end
    local from, to = move[1], move[2]

    -- If black was to move, we need to un-rotate the move coordinates
    if isBlack then
        from = 119 - from
        to = 119 - to
    end

    local uci = renderSquare(from) .. renderSquare(to)

    -- Check for pawn promotion (pawn reaching 8th rank)
    local piece = pos.board:sub(from + __1, from + __1)
    if isBlack then
        -- For black, check the rotated position's board
        piece = pos:rotate().board:sub((119 - from) + __1, (119 - from) + __1)
    end

    local toRank = -math.floor((to - A1) / 10) + 1
    if (piece == 'P' or piece == 'p') and (toRank == 8 or toRank == 1) then
        uci = uci .. 'q'
    end

    return uci
end

--------------------------------------------------------------------------------
-- Transposition table (instance-local for thread safety)
--------------------------------------------------------------------------------
local function createTranspositionTable()
    return {
        tp = {},
        tp_index = {},
        tp_count = 0
    }
end

local function tp_hash(pos)
    local b1 = pos.bc[1] and 'true' or 'false'
    local b2 = pos.bc[2] and 'true' or 'false'
    local w1 = pos.wc[1] and 'true' or 'false'
    local w2 = pos.wc[2] and 'true' or 'false'
    return pos.board .. ';' .. pos.score .. ';' .. w1 .. ';' .. w2 .. ';'
        .. b1 .. ';' .. b2 .. ';' .. pos.ep .. ';' .. pos.kp
end

local function tp_set(tt, pos, val)
    local hash = tp_hash(pos)
    tt.tp[hash] = val
    tt.tp_index[#tt.tp_index + 1] = hash
    tt.tp_count = tt.tp_count + 1
end

local function tp_get(tt, pos)
    return tt.tp[tp_hash(pos)]
end

local function tp_popitem(tt)
    if #tt.tp_index > 0 then
        tt.tp[tt.tp_index[#tt.tp_index]] = nil
        tt.tp_index[#tt.tp_index] = nil
        tt.tp_count = tt.tp_count - 1
    end
end

--------------------------------------------------------------------------------
-- Search logic
--------------------------------------------------------------------------------
local function bound(tt, pos, gamma, depth, nodeCount, state)
    nodeCount[1] = nodeCount[1] + 1

    -- Check for cancellation periodically
    if nodeCount[1] % 1000 == 0 and state.cancelled then
        return nil, nil
    end

    local entry = tp_get(tt, pos)
    if entry ~= nil and entry.depth >= depth and (
            (entry.score < entry.gamma and entry.score < gamma) or
            (entry.score >= entry.gamma and entry.score >= gamma)) then
        return entry.score, nil
    end

    if math.abs(pos.score) >= MATE_VALUE then
        return pos.score, nil
    end

    local nullscore = depth > 0 and -bound(tt, pos:rotate(), 1 - gamma, depth - 3, nodeCount, state) or pos.score
    if nullscore == nil then return nil, nil end
    if nullscore >= gamma then
        return nullscore, nil
    end

    local best, bmove = -3 * MATE_VALUE, nil
    local moves = pos:genMoves()
    local function sorter(a, b)
        local va = pos:value(a)
        local vb = pos:value(b)
        if va ~= vb then
            return va > vb
        else
            if a[1] == b[1] then
                return a[2] > b[2]
            else
                return a[1] < b[1]
            end
        end
    end
    table.sort(moves, sorter)

    for _, move in ipairs(moves) do
        if state.cancelled then return nil, nil end
        if depth <= 0 and pos:value(move) < 150 then
            break
        end
        local score = -bound(tt, pos:move(move), 1 - gamma, depth - 1, nodeCount, state)
        if score == nil then return nil, nil end
        if score > best then
            best = score
            bmove = move
        end
        if score >= gamma then
            break
        end
    end

    if depth <= 0 and best < nullscore then
        return nullscore, nil
    end
    if depth > 0 and (best <= -MATE_VALUE) and nullscore > -MATE_VALUE then
        best = 0
    end

    if entry == nil or (depth >= entry.depth and best >= gamma) then
        tp_set(tt, pos, {depth = depth, score = best, gamma = gamma, move = bmove})
        if tt.tp_count > TABLE_SIZE then
            tp_popitem(tt)
        end
    end
    return best, bmove
end

-- Runs search with loopFn yielding between depths and MTD iterations.
-- Calls onSearchDone(move, score, nodeCount) when finished.
-- Flattened into a single stepFn that tracks depth and gamma sub-state.
local function search(tt, pos, maxn, state, loopFn, onSearchDone)
    loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
    local nodeCount = {0}
    local score = 0
    local lastMove = nil

    -- State for the flattened step function
    local depth = 1
    local lower, upper
    local gammaCount = 0
    local needNewDepth = true  -- start by initializing a new depth
    local searchResult = nil   -- set when done; {move, score, nodes}

    loopFn(function()
        -- stepFn: each call does one unit of work (one gamma iteration or depth init)

        -- Start a new depth: check termination, init bounds
        if needNewDepth then
            if state.cancelled then
                searchResult = { lastMove, score, nodeCount[1] }
                return false
            end
            if depth > 98 or nodeCount[1] >= maxn then
                local entry = tp_get(tt, pos)
                local mv = (entry and entry.move) or lastMove
                searchResult = { mv, score, nodeCount[1] }
                return false
            end
            lower = -3 * MATE_VALUE
            upper = 3 * MATE_VALUE
            gammaCount = 0
            needNewDepth = false
            return true  -- yield before first gamma iteration of this depth
        end

        -- Gamma convergence check: if converged, advance depth
        if lower >= upper - 3 then
            local entry = tp_get(tt, pos)
            if entry and entry.move then
                lastMove = entry.move
            end
            if nodeCount[1] >= maxn or math.abs(score) >= MATE_VALUE then
                entry = tp_get(tt, pos)
                local mv = (entry and entry.move) or lastMove
                searchResult = { mv, score, nodeCount[1] }
                return false
            end
            depth = depth + 1
            needNewDepth = true
            return true  -- yield before next depth
        end

        -- Run one gamma iteration
        if state.cancelled then
            searchResult = { lastMove, score, nodeCount[1] }
            return false
        end

        local gamma = math.floor((lower + upper + 1) / 2)
        score = bound(tt, pos, gamma, depth, nodeCount, state)
        if score == nil or state.cancelled then
            searchResult = { lastMove, score, nodeCount[1] }
            return false
        end
        if score >= gamma then
            lower = score
        end
        if score < gamma then
            upper = score
        end
        gammaCount = gammaCount + 1
        -- Yield every 2 MTD iterations to balance responsiveness and throughput
        if gammaCount >= 2 then
            gammaCount = 0
            return true  -- yield
        end
        -- Run another gamma iteration without yielding (tail into next step inline)
        return true
    end, function()
        -- doneFn
        if searchResult then
            onSearchDone(searchResult[1], searchResult[2], searchResult[3])
        else
            onSearchDone(lastMove, score, nodeCount[1])
        end
    end)
end

--------------------------------------------------------------------------------
-- Engine interface
--------------------------------------------------------------------------------
function M:GetEloRange()
    return {600, 1500}
end

--- Node limit from state.node_limit when set, else from state.elo.
function M:GetNodeLimit(state)
    if state.node_limit ~= nil then
        return state.node_limit
    end
    return DeltaChess.Util.EloToNodes(state.elo, self:GetEloRange(), 500, 4000)
end

function M:GetAverageCpuTime(elo)
    -- Estimate based on node count scaling with ELO
    -- Faster than fruit until ~2000 nodes, then slower
    -- Always slower than garbochess
    local range = self:GetEloRange()
    local t = (elo - range[1]) / math.max(1, range[2] - range[1])
    -- Node count: 500 + t * 3500 = 500 to 4000
    -- At ~2000 nodes (t ~0.43, elo ~1543), fruit becomes faster
    -- Below 2000 nodes: sunfish faster (lower time)
    -- Above 2000 nodes: fruit faster (sunfish time increases more)
    local nodeCount = 500 + t * 3500
    if nodeCount < 2000 then
        -- Below 2000 nodes: sunfish is faster, so lower time
        return math.floor(400 + t * 250)  -- 250ms to 450ms
    else
        -- Above 2000 nodes: fruit is faster, so sunfish time increases more
        return math.floor(600 + (t - 0.43) * 1800)  -- 450ms to 1300ms
    end
end

function M:Calculate(state, loopFn, stepFn, onComplete)
    loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
    onComplete = onComplete or function() end

    if state.cancelled then
        onComplete(nil, "cancelled")
        return
    end

    -- Convert FEN to sunfish position
    local pos, stm, err = fenToSunfish(state.fen)
    if not pos then
        onComplete(nil, err or "invalid FEN")
        return
    end

    local isBlack = (stm == "b")

    local nodeLimit = self:GetNodeLimit(state)

    -- Create fresh transposition table for this search
    local tt = createTranspositionTable()

    -- Run the search (loopFn yields between depths and MTD iterations)
    search(tt, pos, nodeLimit, state, loopFn, function(move, score, nodes)
        if state.cancelled then
            onComplete(nil, "cancelled")
            return
        end

        if not move then
            -- No move found - use MoveGen to get a legal move
            local mgPos = MoveGen.ParseFen(state.fen)
            if mgPos then
                local legalMoves = MoveGen.LegalMoves(mgPos)
                if #legalMoves > 0 then
                    local uci = MoveGen.MoveToUci(legalMoves[1])
                    onComplete({
                        move = uci,
                        san = uci,
                        nodes = nodes or 0,
                        score = 0,
                        mate = nil,
                    }, nil)
                    return
                end
            end
            onComplete(nil, "no legal moves")
            return
        end

        -- Convert move to UCI format
        local uci = moveToUci(move, pos, isBlack)
        if not uci then
            onComplete(nil, "failed to convert move")
            return
        end

        -- Validate the move using MoveGen (Sunfish generates pseudo-legal moves)
        if not MoveGen.IsLegalMove(state.fen, uci) then
            -- Sunfish generated an invalid move, fall back to MoveGen's legal moves
            local mgPos = MoveGen.ParseFen(state.fen)
            if mgPos then
                local legalMoves = MoveGen.LegalMoves(mgPos)
                if #legalMoves > 0 then
                    -- Pick the first legal move as fallback
                    uci = MoveGen.MoveToUci(legalMoves[1])
                else
                    onComplete(nil, "no legal moves")
                    return
                end
            else
                onComplete(nil, "invalid FEN for move validation")
                return
            end
        end

        -- Calculate mate score if applicable
        local mate = nil
        if score and math.abs(score) >= MATE_VALUE - 100 then
            mate = score > 0 and 1 or -1
        end

        onComplete({
            move = uci,
            san = uci,
            nodes = nodes,
            score = score,
            mate = mate,
        }, nil)
    end)
end

--------------------------------------------------------------------------------
-- Register engine
--------------------------------------------------------------------------------
DeltaChess.Engines.Registry[M.id] = M
return M
