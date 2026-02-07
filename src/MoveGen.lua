--[[
  Minimal legal move generation.
  Supports standard chess: pieces, pawns, captures, en passant, castling, promotion.
  Board: 0..63, piece codes P,N,B,R,Q,K (white) p,n,b,r,q,k (black).
  Registers globally: DeltaChess.MoveGen
]]

DeltaChess = DeltaChess or {}
local M = {}

local EMPTY = "."
local RANKS = { "1", "2", "3", "4", "5", "6", "7", "8" }
local FILES = { "a", "b", "c", "d", "e", "f", "g", "h" }

function M.ParseFen(fen)
  local board = {}
  for i = 1, 64 do board[i] = EMPTY end
  local parts = {}
  for p in (fen or ""):gmatch("%S+") do parts[#parts + 1] = p end
  if #parts < 1 then return nil, "invalid fen" end
  local placement = parts[1]
  local stm = (#parts >= 2 and parts[2] == "b") and "b" or "w"
  local castling = #parts >= 3 and parts[3] or "-"
  local ep = #parts >= 4 and parts[4] ~= "-" and parts[4] or nil
  local half = #parts >= 5 and tonumber(parts[5]) or 0
  local full = #parts >= 6 and tonumber(parts[6]) or 1

  local row, col = 8, 1
  for i = 1, #placement do
    local c = placement:sub(i, i)
    if c == "/" then
      row = row - 1
      col = 1
    elseif c:match("%d") then
      col = col + tonumber(c)
    else
      local sq = (row - 1) * 8 + col
      if sq >= 1 and sq <= 64 then board[sq] = c end
      col = col + 1
    end
  end

  return {
    board = board,
    stm = stm,
    castling = castling,
    ep = ep,
    half = half,
    full = full,
  }
end

function M.SqToUci(sq)
  if type(sq) == "string" and #sq >= 4 then return sq end
  local f = (sq - 1) % 8 + 1
  local r = math.floor((sq - 1) / 8) + 1
  return FILES[f] .. RANKS[r]
end

function M.UciToSq(uci)
  if type(uci) ~= "string" or #uci < 2 then return nil end
  local f = uci:byte(1) - ("a"):byte(1) + 1
  local r = uci:byte(2) - ("1"):byte(1) + 1
  if f < 1 or f > 8 or r < 1 or r > 8 then return nil end
  return (r - 1) * 8 + f
end

local function CopyPos(pos)
  local b = {}
  for i = 1, 64 do b[i] = pos.board[i] end
  return {
    board = b,
    stm = pos.stm,
    castling = pos.castling,
    ep = pos.ep,
    half = pos.half,
    full = pos.full,
  }
end

local function IsWhite(p) return p:upper() == p and p ~= EMPTY end
local function IsBlack(p) return p:lower() == p and p ~= EMPTY end
local function SameColor(a, b)
  if a == EMPTY or b == EMPTY then return false end
  return IsWhite(a) == IsWhite(b)
end

local function FindKing(board, white)
  local k = white and "K" or "k"
  for sq = 1, 64 do
    if board[sq] == k then return sq end
  end
  return nil
end

local function RayClear(board, r, f, kr, kf)
  local dr = (kr > r and 1) or (kr < r and -1) or 0
  local df = (kf > f and 1) or (kf < f and -1) or 0
  local steps = math.max(math.abs(kr - r), math.abs(kf - f)) - 1
  for step = 1, steps do
    local nr, nf = r + step * dr, f + step * df
    if board[(nr - 1) * 8 + nf] ~= EMPTY then return false end
  end
  return true
end

local function InCheck(board, white)
  local ksq = FindKing(board, white)
  if not ksq then return true end
  local enemy = white and "pnbrqk" or "PNBRQK"
  local kr, kf = math.floor((ksq - 1) / 8) + 1, (ksq - 1) % 8 + 1
  for sq = 1, 64 do
    local p = board[sq]
    if p ~= EMPTY and enemy:find(p, 1, true) then
      local r, f = math.floor((sq - 1) / 8) + 1, (sq - 1) % 8 + 1
      local dr, df = kr - r, kf - f
      if p == "P" then
        -- White pawns attack diagonally upward (toward higher ranks)
        if math.abs(df) == 1 and (kr - r) == 1 then return true end
      elseif p == "p" then
        -- Black pawns attack diagonally downward (toward lower ranks)
        if math.abs(df) == 1 and (r - kr) == 1 then return true end
      elseif p == "N" or p == "n" then
        local adr, adf = math.abs(dr), math.abs(df)
        if (adr == 2 and adf == 1) or (adr == 1 and adf == 2) then return true end
      elseif p == "B" or p == "b" then
        if math.abs(dr) == math.abs(df) and dr ~= 0 and RayClear(board, r, f, kr, kf) then return true end
      elseif p == "R" or p == "r" then
        if (dr == 0 or df == 0) and (dr ~= 0 or df ~= 0) and RayClear(board, r, f, kr, kf) then return true end
      elseif p == "Q" or p == "q" then
        if (math.abs(dr) == math.abs(df) or dr == 0 or df == 0) and (dr ~= 0 or df ~= 0) and RayClear(board, r, f, kr, kf) then return true end
      elseif p == "K" or p == "k" then
        if math.abs(dr) <= 1 and math.abs(df) <= 1 then return true end
      end
    end
  end
  return false
end

-- Simplified: generate pseudo-legal moves then filter by "leave king in check"
local function GenMoves(pos)
  local moves = {}
  local board = pos.board
  local white = (pos.stm == "w")
  local pawn = white and "P" or "p"
  local knight = white and "N" or "n"
  local bishop = white and "B" or "b"
  local rook = white and "R" or "r"
  local queen = white and "Q" or "q"
  local king = white and "K" or "k"
  local push = white and 8 or -8  -- white moves toward row 8, black toward row 1
  local start_rank = white and 2 or 7
  local promo_rank = white and 8 or 1
  local second_rank = white and 7 or 2

  for sq = 1, 64 do
    local p = board[sq]
    if p ~= EMPTY and (white and p == p:upper() or not white and p == p:lower()) then
      local r, f = math.floor((sq - 1) / 8) + 1, (sq - 1) % 8 + 1
      if p == pawn then
        local to = sq + push
        if to >= 1 and to <= 64 and board[to] == EMPTY then
          local tr = math.floor((to - 1) / 8) + 1
          if (white and tr == 8) or (not white and tr == 1) then
            for _, prom in ipairs({ "q", "r", "b", "n" }) do
              table.insert(moves, { from = sq, to = to, prom = white and prom:upper() or prom })
            end
          else
            table.insert(moves, { from = sq, to = to })
          end
          if (white and r == 2) or (not white and r == 7) then
            local to2 = to + push
            if to2 >= 1 and to2 <= 64 and board[to2] == EMPTY then
              table.insert(moves, { from = sq, to = to2 })
            end
          end
        end
        for _, dfile in ipairs({ -1, 1 }) do
          local to = sq + push + dfile
          if to >= 1 and to <= 64 then
            local tr = math.floor((to - 1) / 8) + 1
            local tf = (to - 1) % 8 + 1
            if math.abs((to - 1) % 8 - (sq - 1) % 8) == 1 then
              if board[to] ~= EMPTY and (white and board[to] == board[to]:lower() or not white and board[to] == board[to]:upper()) then
                if (white and tr == 8) or (not white and tr == 1) then
                  for _, prom in ipairs({ "q", "r", "b", "n" }) do
                    table.insert(moves, { from = sq, to = to, prom = white and prom:upper() or prom })
                  end
                else
                  table.insert(moves, { from = sq, to = to })
                end
              elseif pos.ep and M.UciToSq(pos.ep) == to then
                table.insert(moves, { from = sq, to = to, ep = true })
              end
            end
          end
        end
      elseif p == knight or p == "N" or p == "n" then
        for _, delta in ipairs({ {-2,-1},{-2,1},{-1,-2},{-1,2},{1,-2},{1,2},{2,-1},{2,1} }) do
          local tr, tf = r + delta[1], f + delta[2]
          if tr >= 1 and tr <= 8 and tf >= 1 and tf <= 8 then
            local to = (tr - 1) * 8 + tf
            if board[to] == EMPTY or not SameColor(p, board[to]) then
              table.insert(moves, { from = sq, to = to })
            end
          end
        end
      elseif p == king or p == "K" or p == "k" then
        for dr = -1, 1 do for df = -1, 1 do
          if dr ~= 0 or df ~= 0 then
            local tr, tf = r + dr, f + df
            if tr >= 1 and tr <= 8 and tf >= 1 and tf <= 8 then
              local to = (tr - 1) * 8 + tf
              if board[to] == EMPTY or not SameColor(p, board[to]) then
                table.insert(moves, { from = sq, to = to })
              end
            end
          end
        end end
        -- Castling
        -- White: king on e1(5), kingside to g1(7), queenside to c1(3)
        if white and pos.castling:find("K") and board[6] == EMPTY and board[7] == EMPTY and not InCheck(board, true) then
          local b2 = {}
          for i = 1, 64 do b2[i] = board[i] end
          b2[6], b2[5] = b2[5], EMPTY  -- king e1->f1
          if not InCheck(b2, true) then b2[7], b2[6] = b2[6], EMPTY; if not InCheck(b2, true) then table.insert(moves, { from = 5, to = 7, castle = true }) end end
        end
        if white and pos.castling:find("Q") and board[4] == EMPTY and board[3] == EMPTY and board[2] == EMPTY and not InCheck(board, true) then
          local b2 = {}
          for i = 1, 64 do b2[i] = board[i] end
          b2[4], b2[5] = b2[5], EMPTY  -- king e1->d1
          if not InCheck(b2, true) then table.insert(moves, { from = 5, to = 3, castle = true }) end
        end
        -- Black: king on e8(61), kingside to g8(63), queenside to c8(59)
        if not white and pos.castling:find("k") and board[62] == EMPTY and board[63] == EMPTY and not InCheck(board, false) then
          local b2 = {}
          for i = 1, 64 do b2[i] = board[i] end
          b2[62], b2[61] = b2[61], EMPTY  -- king e8->f8
          if not InCheck(b2, false) then b2[63], b2[62] = b2[62], EMPTY; if not InCheck(b2, false) then table.insert(moves, { from = 61, to = 63, castle = true }) end end
        end
        if not white and pos.castling:find("q") and board[60] == EMPTY and board[59] == EMPTY and board[58] == EMPTY and not InCheck(board, false) then
          local b2 = {}
          for i = 1, 64 do b2[i] = board[i] end
          b2[60], b2[61] = b2[61], EMPTY  -- king e8->d8
          if not InCheck(b2, false) then table.insert(moves, { from = 61, to = 59, castle = true }) end
        end
      else
        -- Rook, Bishop, Queen: sliding
        local deltas = {}
        if p == rook or p == "R" or p == "r" then
          deltas = { {1,0}, {-1,0}, {0,1}, {0,-1} }
        elseif p == bishop or p == "B" or p == "b" then
          deltas = { {1,1}, {1,-1}, {-1,1}, {-1,-1} }
        else
          deltas = { {1,0}, {-1,0}, {0,1}, {0,-1}, {1,1}, {1,-1}, {-1,1}, {-1,-1} }
        end
        for _, d in ipairs(deltas) do
          for step = 1, 8 do
            local tr, tf = r + step * d[1], f + step * d[2]
            if tr < 1 or tr > 8 or tf < 1 or tf > 8 then break end
            local to = (tr - 1) * 8 + tf
            if board[to] == EMPTY then
              table.insert(moves, { from = sq, to = to })
            else
              if not SameColor(p, board[to]) then table.insert(moves, { from = sq, to = to }) end
              break
            end
          end
        end
      end
    end
  end

  -- Filter to legal only
  local legal = {}
  for _, mv in ipairs(moves) do
    local pos2 = CopyPos(pos)
    local b = pos2.board
    local from, to = mv.from, mv.to
    local piece = b[from]
    b[to] = piece
    b[from] = EMPTY
    if mv.ep then
      local epSq = M.UciToSq(pos.ep)
      if epSq then
        local epCapture = epSq + (white and -8 or 8)  -- captured pawn is one rank behind the ep square
        if epCapture >= 1 and epCapture <= 64 then b[epCapture] = EMPTY end
      end
    end
    if mv.castle then
      if to == 7 then b[6], b[8] = b[8], EMPTY        -- white kingside: rook h1(8)->f1(6)
      elseif to == 3 then b[4], b[1] = b[1], EMPTY    -- white queenside: rook a1(1)->d1(4)
      elseif to == 63 then b[62], b[64] = b[64], EMPTY -- black kingside: rook h8(64)->f8(62)
      elseif to == 59 then b[60], b[57] = b[57], EMPTY -- black queenside: rook a8(57)->d8(60)
      end
    end
    if mv.prom then b[to] = (white and mv.prom:upper() or mv.prom:lower()) end
    pos2.stm = white and "b" or "w"
    pos2.ep = nil
    if not InCheck(pos2.board, white) then
      table.insert(legal, mv)
    end
  end
  return legal
end

function M.LegalMoves(pos)
  return GenMoves(pos)
end

function M.InCheck(pos)
  return InCheck(pos.board, pos.stm == "w")
end

function M.MakeMove(pos, mv)
  local pos2 = CopyPos(pos)
  local b = pos2.board
  local white = (pos.stm == "w")
  local from, to = mv.from, mv.to
  local piece = b[from]
  local captured = b[to]
  b[to] = piece
  b[from] = EMPTY
  if mv.ep then
    local epSq = M.UciToSq(pos.ep)
    if epSq then
      local epCapture = epSq + (white and -8 or 8)
      if epCapture >= 1 and epCapture <= 64 then b[epCapture] = EMPTY end
    end
  end
  if mv.castle then
    if to == 7 then b[6], b[8] = b[8], EMPTY        -- white kingside: rook h1(8)->f1(6)
    elseif to == 3 then b[4], b[1] = b[1], EMPTY    -- white queenside: rook a1(1)->d1(4)
    elseif to == 63 then b[62], b[64] = b[64], EMPTY -- black kingside: rook h8(64)->f8(62)
    elseif to == 59 then b[60], b[57] = b[57], EMPTY -- black queenside: rook a8(57)->d8(60)
    end
  end
  if mv.prom then b[to] = (white and mv.prom:upper() or mv.prom:lower()) end
  pos2.stm = white and "b" or "w"
  pos2.ep = nil
  -- Update castling rights (moving piece or captured rook)
  local castling = pos2.castling
  if piece == "K" then castling = (castling or ""):gsub("[KQ]", "")
  elseif piece == "k" then castling = (castling or ""):gsub("[kq]", "")
  elseif piece == "R" and from == 1 then castling = (castling or ""):gsub("Q", "")  -- a1 rook
  elseif piece == "R" and from == 8 then castling = (castling or ""):gsub("K", "")  -- h1 rook
  elseif piece == "r" and from == 57 then castling = (castling or ""):gsub("q", "") -- a8 rook
  elseif piece == "r" and from == 64 then castling = (castling or ""):gsub("k", "") -- h8 rook
  -- Captured rook on its home square loses castling right
  elseif captured == "R" and to == 1 then castling = (castling or ""):gsub("Q", "")
  elseif captured == "R" and to == 8 then castling = (castling or ""):gsub("K", "")
  elseif captured == "r" and to == 57 then castling = (castling or ""):gsub("q", "")
  elseif captured == "r" and to == 64 then castling = (castling or ""):gsub("k", "")
  end
  pos2.castling = castling == "" and "-" or castling
  return pos2
end

--- Build UCI string from move table (e.g. e2e4, e7e8q).
function M.MoveToUci(mv)
  if not mv or not mv.from or not mv.to then return nil end
  local uci = M.SqToUci(mv.from) .. M.SqToUci(mv.to)
  if mv.prom then uci = uci .. (type(mv.prom) == "string" and mv.prom:lower() or "q") end
  return uci
end

--- Find the legal move that matches UCI string. Returns move table or nil.
function M.MoveFromUci(pos, uci)
  if type(uci) ~= "string" or #uci < 4 then return nil end
  local moves = M.LegalMoves(pos)
  local uciNorm = uci:lower()
  for _, mv in ipairs(moves) do
    if M.MoveToUci(mv) == uciNorm then return mv end
  end
  return nil
end

--- Convert a move to Standard Algebraic Notation (SAN). Requires position before the move.
function M.MoveToSan(pos, mv)
  if not pos or not mv or not pos.board then return nil end
  local b = pos.board
  local from, to = mv.from, mv.to
  local piece = b[from]
  if not piece or piece == EMPTY then return nil end
  local pieceUpper = piece:upper()
  local toUci = M.SqToUci(to)
  local fromUci = M.SqToUci(from)
  local fromFile, fromRank = fromUci:sub(1, 1), fromUci:sub(2, 2)
  local isCapture = (b[to] and b[to] ~= EMPTY) or mv.ep

  if mv.castle then
    return (to > from) and "O-O" or "O-O-O"
  end

  local san
  if pieceUpper == "P" then
    if isCapture then
      san = fromFile .. "x" .. toUci
    else
      san = toUci
    end
    if mv.prom then san = san .. "=" .. (mv.prom:upper()) end
  else
    local letter = pieceUpper == "K" and "K" or pieceUpper == "Q" and "Q" or pieceUpper == "R" and "R" or pieceUpper == "B" and "B" or "N"
    local samePieceMoves = {}
    local uciMv = M.MoveToUci(mv)
    for _, m in ipairs(M.LegalMoves(pos)) do
      if M.MoveToUci(m) ~= uciMv then
        local p = b[m.from]
        if p and p:upper() == pieceUpper and m.to == to then
          samePieceMoves[#samePieceMoves + 1] = m
        end
      end
    end
    -- PGN disambiguation: use file when pieces are on different files; use rank when on same file
    local needFile, needRank = false, false
    if #samePieceMoves > 0 then
      local filesSet = {}
      filesSet[fromFile] = true
      for _, m in ipairs(samePieceMoves) do
        filesSet[M.SqToUci(m.from):sub(1, 1)] = true
      end
      local numFiles = 0
      for _ in pairs(filesSet) do numFiles = numFiles + 1 end
      needFile = (numFiles > 1)
      needRank = (numFiles == 1)
    end
    local disamb = (needFile and needRank) and (fromFile .. fromRank) or needFile and fromFile or needRank and fromRank or ""
    san = letter .. disamb .. (isCapture and "x" or "") .. toUci
  end
  return san
end

--- Convert UCI string to SAN in the given position. Returns SAN string or nil.
function M.UciToSan(pos, uci)
  local mv = M.MoveFromUci(pos, uci)
  if not mv then return nil end
  return M.MoveToSan(pos, mv)
end

--- Convert SAN string to UCI in the given position. Strips trailing + and # from SAN.
-- @param pos position table (from ParseFen)
-- @param san string e.g. "e4", "Nf3", "O-O", "Qa5+"
-- @return string|nil UCI move (e.g. "e2e4") or nil if invalid/ambiguous
function M.SanToUci(pos, san)
  if not pos or not san or san == "" then return nil end
  local sanNorm = san:gsub("[%+#]$", "")
  local moves = M.LegalMoves(pos)
  for _, mv in ipairs(moves) do
    if M.MoveToSan(pos, mv) == sanNorm then
      return M.MoveToUci(mv)
    end
  end
  return nil
end

--- Return true if uci is a legal move in position fen.
function M.IsLegalMove(fen, uci)
  local pos, err = M.ParseFen(fen)
  if not pos then return false end
  return M.MoveFromUci(pos, uci) ~= nil
end

--- Serialize position to FEN string.
function M.PosToFen(pos)
  if not pos or not pos.board then return nil end
  local b = pos.board
  local rankParts = {}
  for row = 8, 1, -1 do
    local rowStr = ""
    local empty = 0
    for col = 1, 8 do
      local sq = (row - 1) * 8 + col
      local p = b[sq] or "."
      if p == "." then
        empty = empty + 1
      else
        if empty > 0 then rowStr = rowStr .. empty; empty = 0 end
        rowStr = rowStr .. p
      end
    end
    if empty > 0 then rowStr = rowStr .. empty end
    rankParts[#rankParts + 1] = rowStr
  end
  local placement = table.concat(rankParts, "/")
  return placement .. " " .. (pos.stm == "b" and "b" or "w") .. " " ..
      ((pos.castling and pos.castling ~= "") and pos.castling or "-") .. " " ..
      (pos.ep and pos.ep or "-") .. " " ..
      tostring(pos.half or 0) .. " " ..
      tostring(pos.full or 1)
end

local Decorated = DeltaChess.Cache.new(M)

DeltaChess.MoveGen = Decorated
return Decorated
