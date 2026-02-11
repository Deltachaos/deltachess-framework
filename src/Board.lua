--[[
  Board: Tracks game state including the 50-move rule.
  Wraps MoveGen position with proper half-move clock tracking.
  Supports captured pieces, move metadata, game metadata, and serialization.
  Registers globally: DeltaChess.Board
]]

DeltaChess = DeltaChess or {}
local MoveGen = DeltaChess.MoveGen
local Constants = DeltaChess.Constants
local BoardMove = DeltaChess.BoardMove
local Util = DeltaChess.Util

local M = {}

--------------------------------------------------------------------------------
-- Piece utilities
--------------------------------------------------------------------------------

-- Material values indexed by UCI piece character
M.PIECE_VALUES = {
  P = 1, p = 1,
  N = 3, n = 3,
  B = 3, b = 3,
  R = 5, r = 5,
  Q = 9, q = 9,
  K = 0, k = 0,
}

--- Get the material value of a piece by its UCI character.
-- @param pieceChar string Single character (P, N, B, R, Q, K or lowercase)
-- @return number Material value (0 for king, nil for invalid)
function M.GetPieceValue(pieceChar)
  return M.PIECE_VALUES[pieceChar]
end

--- Calculate material advantage from captured piece arrays.
-- Use when board state is not available (e.g. replay from moves).
-- @param capturedByWhite table Array of piece chars white captured (black pieces)
-- @param capturedByBlack table Array of piece chars black captured (white pieces)
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK (perspective to return)
-- @return number Positive = that color ahead
function M.CalculateMaterialAdvantageFromArrays(capturedByWhite, capturedByBlack, color)
  local whitePoints = 0
  local blackPoints = 0
  for _, pieceChar in ipairs(capturedByWhite or {}) do
    whitePoints = whitePoints + (M.PIECE_VALUES[pieceChar] or 0)
  end
  for _, pieceChar in ipairs(capturedByBlack or {}) do
    blackPoints = blackPoints + (M.PIECE_VALUES[pieceChar] or 0)
  end
  local whiteAdvantage = whitePoints - blackPoints
  return (color == Constants.COLOR.WHITE) and whiteAdvantage or -whiteAdvantage
end

--- Get the color of a piece by its UCI character.
-- @param pieceChar string Single character
-- @return string|nil "white" for uppercase, "black" for lowercase, nil for invalid
function M.GetPieceColor(pieceChar)
  if not pieceChar or pieceChar == "" then return nil end
  return pieceChar == pieceChar:upper() and DeltaChess.Constants.COLOR.WHITE or DeltaChess.Constants.COLOR.BLACK
end

--- Check if a piece belongs to a specific color.
-- @param pieceChar string Single character
-- @param color string COLOR.WHITE or COLOR.BLACK
-- @return boolean
function M.IsPieceColor(pieceChar, color)
  if not pieceChar or pieceChar == "" then return false end
  if color == DeltaChess.Constants.COLOR.WHITE then
    return pieceChar == pieceChar:upper()
  else
    return pieceChar == pieceChar:lower()
  end
end

--------------------------------------------------------------------------------
-- UCI square utilities
--------------------------------------------------------------------------------

-- File letters for UCI notation
M.FILES = {"a", "b", "c", "d", "e", "f", "g", "h"}

-- Reverse lookup: file letter to column number
M.FILE_TO_COL = {a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8}

--- Convert row and column to UCI square notation.
-- @param row number 1-8 (1 = white's back rank)
-- @param col number 1-8 (1 = a-file)
-- @return string UCI square (e.g., "e4")
function M.ToSquare(row, col)
  if row < 1 or row > 8 or col < 1 or col > 8 then return nil end
  return M.FILES[col] .. row
end

--- Convert UCI square notation to row and column.
-- @param square string UCI square (e.g., "e4")
-- @return number, number row (1-8), col (1-8), or nil if invalid
function M.FromSquare(square)
  if not square or #square ~= 2 then return nil, nil end
  local file = square:sub(1, 1):lower()
  local rank = tonumber(square:sub(2, 2))
  local col = M.FILE_TO_COL[file]
  if not col or not rank or rank < 1 or rank > 8 then return nil, nil end
  return rank, col
end

--- Get all 64 UCI squares in order (a1, b1, ..., h1, a2, ..., h8).
-- @return table Array of 64 square strings
function M.AllSquares()
  local squares = {}
  for row = 1, 8 do
    for col = 1, 8 do
      table.insert(squares, M.ToSquare(row, col))
    end
  end
  return squares
end

--------------------------------------------------------------------------------
-- Board object: wraps a position and tracks move history
--------------------------------------------------------------------------------

local Board = {}
Board.__index = Board

-- Internal: Initialize board state fields
local function initBoardState(board)
  board.moves = {}              -- Array of move objects with metadata
  board.gameMeta = {}           -- Arbitrary game-level metadata

  board._cachedStatus = nil     -- Cache for status calculation
  board._cachedResult = nil
  board._cachedReason = nil

  -- Native game state tracking fields (gameStatus/gameResult derived from these + _paused)
  board._startTime = nil        -- Game start timestamp (number)
  board._endTime = nil          -- Game end timestamp (number)
  board._paused = false         -- Game is paused (use PauseGame/ContinueGame)
  board._whitePlayer = nil      -- { name = string, class = string|nil } or string (name)
  board._blackPlayer = nil      -- { name = string, class = string|nil } or string (name)
  board._resignedColor = nil    -- COLOR.WHITE or COLOR.BLACK when game ended by resignation
  board._playerColor = nil      -- Constants.COLOR.WHITE or COLOR.BLACK (local player's side)

  -- Clocks: initial time per player (seconds) and optional increment per move (seconds)
  board._clockWhite = 0
  board._clockBlack = 0
  board._incrementWhite = 0
  board._incrementBlack = 0
  -- Pause history: list of { pauseTime = number, unpauseTime = number } for computing elapsed time
  board._pauseHistory = {}
  -- Position keys for threefold repetition: first 4 FEN fields (placement, stm, castling, ep) after each move
  board._positionKeys = {}
end

-- Internal: Get repetition key (first 4 FEN fields) for the current position. Used for threefold repetition.
-- FIDE: positions are the same if (1) same player has the move, (2) same pieces on same squares,
-- (3) possible moves of all pieces are the same. (3) fails if en passant or castling rights differ
-- between occurrences; the key uses placement, stm, castling, and ep so those are encoded.
local function getRepetitionKey(board)
  local fen = board:GetFen()
  local parts = {}
  for part in (fen or ""):gmatch("%S+") do
    parts[#parts + 1] = part
  end
  if #parts < 4 then
    return (fen or ""):match("^([^%s]+%s+[^%s]+%s+[^%s]+%s+[^%s]+)") or fen or ""
  end
  return parts[1] .. " " .. parts[2] .. " " .. (parts[3] or "-") .. " " .. (parts[4] or "-")
end

-- Internal: Rebuild _positionKeys from _startFen and moves (replay and collect keys).
local function rebuildPositionKeys(board)
  local startFen = board._startFen or Constants.START_FEN
  local temp = M.FromFen(startFen)
  for _, move in ipairs(board.moves) do
    temp:MakeMoveUci(move.uci)
  end
  board._positionKeys = {}
  for _, k in ipairs(temp._positionKeys) do
    table.insert(board._positionKeys, k)
  end
end

--- Create a new Board from FEN string (defaults to starting position).
-- @param fen (string|nil) FEN string to initialize from
-- @return Board object
function M.FromFen(fen)
  fen = fen or Constants.START_FEN
  local pos, err = MoveGen.ParseFen(fen)
  if not pos then
    return nil, err
  end
  
  local board = setmetatable({}, Board)
  board.pos = pos
  initBoardState(board)
  board._startFen = fen
  board._positionKeys = { getRepetitionKey(board) }
  return board
end

--- Create a new Board from the standard starting position.
-- @return Board object
function M.New()
  return M.FromFen(Constants.START_FEN)
end

--- Get the current position (MoveGen format).
-- @return position table
function Board:GetPos()
  return self.pos
end

--- Get the current FEN string.
-- @return string FEN
function Board:GetFen()
  return MoveGen.PosToFen(self.pos)
end

--- Get the piece at a square by row and column (1-8).
-- Used by UI rendering (e.g. RenderPieces) when passing a Board as board state.
-- @param row number 1-8 (1 = white's back rank)
-- @param col number 1-8 (1 = a-file)
-- @return string|nil Piece character (K, Q, R, B, N, P or lowercase) or nil if empty
function Board:GetPiece(row, col)
  if not self.pos or not self.pos.board then return nil end
  if row < 1 or row > 8 or col < 1 or col > 8 then return nil end
  local sq = (row - 1) * 8 + col
  local piece = self.pos.board[sq]
  if not piece or piece == "." then return nil end
  return piece
end

--- Get the move history as array of BoardMove objects.
-- Each move has accessor methods for uci, piece, captured, promotion, castle, check, meta.
-- @return table of BoardMove objects
function Board:GetMoveHistory()
  local result = {}
  for i, move in ipairs(self.moves) do
    result[i] = BoardMove.Wrap(move, self, i)
  end
  return result
end

--- Get the raw move history (internal use, for iteration without wrapping).
-- @return table of raw move tables
function Board:GetMoveHistoryRaw()
  return self.moves
end

--- Get the move history as array of UCI strings (for engine compatibility).
-- @return table of UCI strings
function Board:GetMoveHistoryUci()
  local uciList = {}
  for i, move in ipairs(self.moves) do
    uciList[i] = move.uci
  end
  return uciList
end

--- Get the number of half-moves played.
-- @return number
function Board:GetHalfMoveCount()
  return #self.moves
end

--- Get the last move played as a BoardMove.
-- @return BoardMove or nil if no moves played
function Board:GetLastMove()
  local move = self.moves[#self.moves]
  if not move then return nil end
  return BoardMove.Wrap(move, self, #self.moves)
end

--- Get a specific move by index (1-based) as a BoardMove.
-- @param index number (optional, defaults to last move)
-- @return BoardMove or nil
function Board:GetMove(index)
  index = index or #self.moves
  local move = self.moves[index]
  if not move then return nil end
  return BoardMove.Wrap(move, self, index)
end

--- Get moves in PGN notation (no clock/time annotations).
-- Uses SAN from each move; appends "+" for check except on the last move.
-- First line: full moves 1-8; second line: full moves 9 to end.
-- @return string PGN move text (e.g. "1. e4 e5 2. Nf3 Nc6 ...")
function Board:GetPGNMovesWithoutTime()
  local history = self:GetMoveHistory()
  if #history == 0 then return "" end
  local parts = {}
  local fullMove = 1
  for i = 1, #history, 2 do
    local wMove = history[i]
    local bMove = history[i + 1]
    local w = wMove and wMove:GetSan() or ""
    local b = bMove and bMove:GetSan() or ""
    if wMove and wMove:IsCheck() and i < #history then w = w .. "+" end
    if bMove and bMove:IsCheck() and (i + 1) < #history then b = b .. "+" end
    local line = fullMove .. ". " .. w
    if b ~= "" then line = line .. " " .. b end
    parts[#parts + 1] = line
    fullMove = fullMove + 1
  end
  local out = {}
  local firstChunk = {}
  for k = 1, math.min(8, #parts) do firstChunk[#firstChunk + 1] = parts[k] end
  out[#out + 1] = table.concat(firstChunk, " ")
  if #parts > 8 then
    local rest = {}
    for k = 9, #parts do rest[#rest + 1] = parts[k] end
    out[#out + 1] = table.concat(rest, " ")
  end
  return table.concat(out, "\n")
end

-- Escape a PGN tag value: " -> \", \ -> \\
local function pgnEscape(s)
  if s == nil or s == "" then return "" end
  return tostring(s):gsub("\\", "\\\\"):gsub('"', '\\"')
end

-- Serialize player for PGN White/Black tag: object -> JSON string, string -> as-is.
local function playerToPgnValue(player)
  if player == nil then return "?" end
  if type(player) == "string" then return player end
  if type(player) == "table" and Util and Util.HasJSON and Util.HasJSON() then
    local str, _ = Util.SerializeJSON(player)
    if str then return str end
  end
  return (player.name or player.class or tostring(player)) or "?"
end

-- Parse player from PGN White/Black tag value: JSON object -> table, else -> string.
local function playerFromPgnValue(val)
  if not val or val == "?" then return nil end
  if val:match("^%s*{") and Util and Util.DeserializeJSON then
    local t, _ = Util.DeserializeJSON(val)
    if type(t) == "table" then return t end
  end
  return val
end

-- Map game result to PGN Result tag (1-0, 0-1, 1/2-1/2, *)
local function resultToPgn(result)
  if result == Constants.COLOR.WHITE or result == "white" then return "1-0" end
  if result == Constants.COLOR.BLACK or result == "black" then return "0-1" end
  if result == "draw" then return "1/2-1/2" end
  return "*"
end

-- Format seconds as PGN %clk time: 0:MM:SS or H:MM:SS for hours
local function formatClkTime(seconds)
  if not seconds or seconds < 0 then return nil end
  local s = math.floor(seconds)
  local h = math.floor(s / 3600)
  local m = math.floor((s % 3600) / 60)
  local sec = s % 60
  if h > 0 then
    return ("%d:%02d:%02d"):format(h, m, sec)
  end
  return ("0:%02d:%02d"):format(m, sec)
end

--- Get full PGN string (tag pairs + movetext). Per https://en.wikipedia.org/wiki/Portable_Game_Notation
-- Seven Tag Roster: Event, Site, Date, Round, White, Black, Result.
-- Optional: FEN (if not start position), TimeControl. Non-standard data (e.g. clock after each move) in comments.
-- @return string PGN document
function Board:GetPGN()
  local tags = {}
  -- Date: from _startTime when set, else unknown
  if type(self._startTime) == "number" then
    tags.Date = Util.Date(self._startTime, "%Y.%m.%d")
  else
    tags.Date = "????.??.??"
  end
  tags.White = playerToPgnValue(self._whitePlayer)
  tags.Black = playerToPgnValue(self._blackPlayer)
  local result = self:GetGameResult()
  tags.Result = resultToPgn(result)

  -- PGN standard 9.5.1 Time (local), 9.5.2 UTCTime: game start time
  if type(self._startTime) == "number" then
    tags.Time = Util.Date(self._startTime, "%H:%M:%S")
    tags.UTCTime = Util.Date(self._startTime, "!%H:%M:%S")
  end
  -- Supplemental: end time (not in STR; allows restoring _endTime)
  if type(self._endTime) == "number" then
    tags.EndTime = Util.Date(self._endTime, "%H:%M:%S")
  end

  if self._startTime and (self:GetClock(Constants.COLOR.WHITE) or self:GetClock(Constants.COLOR.BLACK)) then
    local w = self:GetClock(Constants.COLOR.WHITE) or 0
    local b = self:GetClock(Constants.COLOR.BLACK) or 0
    local incW = self:GetIncrement(Constants.COLOR.WHITE) or 0
    local incB = self:GetIncrement(Constants.COLOR.BLACK) or 0
    if incW > 0 or incB > 0 then
      tags.TimeControl = ("%d+%d:%d+%d"):format(w, incW, b, incB)
    else
      tags.TimeControl = ("%d:%d"):format(w, b)
    end
  end

  local lines = {}
  local tagOrder = { "Date", "White", "Black", "Result", "Time", "UTCTime", "EndTime", "TimeControl" }
  local seen = {}
  for _, tagName in ipairs(tagOrder) do
    local tagVal = tags[tagName]
    if tagVal and not seen[tagName] then
      seen[tagName] = true
      table.insert(lines, ("[%s \"%s\"]"):format(tagName, pgnEscape(tagVal)))
    end
  end
  table.insert(lines, "")

  local history = self:GetMoveHistory()
  if #history == 0 then
    table.insert(lines, resultToPgn(result))
    return table.concat(lines, "\n")
  end

  local moveLines = {}
  local fullMove = 1
  for i = 1, #history, 2 do
    local wMove = history[i]
    local bMove = history[i + 1]
    local w = wMove and wMove:GetSan() or ""
    local b = bMove and bMove:GetSan() or ""
    if wMove and wMove:IsCheck() and i < #history then w = w .. "+" end
    if bMove and bMove:IsCheck() and (i + 1) < #history then b = b .. "+" end

    local part = fullMove .. ". " .. w
    if wMove and self._startTime and type(wMove:GetTimestamp()) == "number" then
      local boardAfter = self:GetBoardAtIndex(i)
      if boardAfter then
        local cw = boardAfter:GetRemainingTime(Constants.COLOR.WHITE, wMove:GetTimestamp())
        if cw ~= nil then
          local clk = formatClkTime(cw)
          if clk then part = part .. " {[%clk " .. clk .. "]}" end
        end
      end
    end
    if b ~= "" then
      part = part .. " " .. b
      if bMove and self._startTime and type(bMove:GetTimestamp()) == "number" then
        local boardAfter = self:GetBoardAtIndex(i + 1)
        if boardAfter then
          local cb = boardAfter:GetRemainingTime(Constants.COLOR.BLACK, bMove:GetTimestamp())
          if cb ~= nil then
            local clk = formatClkTime(cb)
            if clk then part = part .. " {[%clk " .. clk .. "]}" end
          end
        end
      end
    end
    table.insert(moveLines, part)
    fullMove = fullMove + 1
  end

  local movetext = table.concat(moveLines, " ")
  table.insert(lines, movetext)
  if tags.Result ~= "*" then
    table.insert(lines, tags.Result)
  end
  return table.concat(lines, "\n")
end

-- Internal: Compute captured pieces from move history.
local function computeCapturedPieces(self)
  local w, b = {}, {}
  for _, move in ipairs(self.moves) do
    if move.captured then
      local capturingSide = (move.piece == move.piece:upper()) and "w" or "b"
      local arr = (capturingSide == "w") and w or b
      table.insert(arr, move.captured)
    end
  end
  return { w = w, b = b }
end

--- Get captured pieces for a side.
-- @param side "w" or "b" (pieces captured BY this side)
-- @return table of piece characters
function Board:GetCapturedPieces(side)
  local captured = computeCapturedPieces(self)
  return captured[side] or {}
end

--- Get pieces captured by white (black pieces).
-- @return table of piece characters
function Board:GetCapturedPiecesWhite()
  return self:GetCapturedPieces("w")
end

--- Get pieces captured by black (white pieces).
-- @return table of piece characters
function Board:GetCapturedPiecesBlack()
  return self:GetCapturedPieces("b")
end

--- Get material value of pieces captured by white.
-- @return number
function Board:GetCapturedValueWhite()
  local sum = 0
  for _, pieceChar in ipairs(self:GetCapturedPiecesWhite()) do
    sum = sum + (M.PIECE_VALUES[pieceChar] or 0)
  end
  return sum
end

--- Get material value of pieces captured by black.
-- @return number
function Board:GetCapturedValueBlack()
  local sum = 0
  for _, pieceChar in ipairs(self:GetCapturedPiecesBlack()) do
    sum = sum + (M.PIECE_VALUES[pieceChar] or 0)
  end
  return sum
end

--- Get material value of pieces captured by the given color.
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @return number
function Board:GetCapturedValue(color)
  if color == Constants.COLOR.WHITE then
    return self:GetCapturedValueWhite()
  else
    return self:GetCapturedValueBlack()
  end
end

--- Calculate material advantage from the perspective of the given color.
-- Uses board's captured pieces; positive = that color is ahead.
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @return number Positive = that color ahead, negative = that color behind
function Board:CalculateMaterialAdvantage(color)
  local myValue = self:GetCapturedValue(color)
  local oppColor = (color == Constants.COLOR.WHITE) and Constants.COLOR.BLACK or Constants.COLOR.WHITE
  local oppValue = self:GetCapturedValue(oppColor)
  return myValue - oppValue
end

--- Get the half-move clock (for 50-move rule).
-- @return number
function Board:GetHalfMoveClock()
  return self.pos.half or 0
end

-- Internal: Return how many times the current position has occurred in the game (for repetition rules).
local function getRepetitionCount(board)
  local keys = board._positionKeys
  if not keys or #keys == 0 then return 0 end
  local currentKey = getRepetitionKey(board)
  local count = 0
  for i = 1, #keys do
    if keys[i] == currentKey then count = count + 1 end
  end
  return count
end

--- Check if a draw by threefold repetition is possible (same position occurred at least 3 times).
-- Intervening moves do not matter. The draw must be claimed by the player with the turn to move
-- (either before making the move that would repeat for the third time, or after the position
-- has just appeared for the third time). This method only reports whether the condition is met;
-- it does not end the game or enforce who may claim.
-- @return boolean true if the current position has occurred at least 3 times
function Board:IsThreefoldRepetitionDrawPossible()
  return getRepetitionCount(self) >= 3
end

--- Get the full-move number.
-- @return number
function Board:GetFullMoveNumber()
  return self.pos.full or 1
end

--- Get all legal moves in the current position.
-- @return table of move objects
function Board:LegalMoves()
  return MoveGen.LegalMoves(self.pos)
end

--- Check if the current side to move is in check.
-- @return boolean
function Board:InCheck()
  return MoveGen.InCheck(self.pos)
end

--- Get the side to move ("w" or "b").
-- @return string
function Board:SideToMove()
  return self.pos.stm
end

--- Check if it's white's turn.
-- @return boolean
function Board:IsWhiteToMove()
  return self.pos.stm == "w"
end

--- Get the current turn as color constant ("white" or "black").
-- @return string Constants.COLOR.WHITE or Constants.COLOR.BLACK
function Board:GetCurrentTurn()
  return self:IsWhiteToMove() and Constants.COLOR.WHITE or Constants.COLOR.BLACK
end

--------------------------------------------------------------------------------
-- Game Status API
--------------------------------------------------------------------------------

-- Internal: Check if clocks are configured (game started, at least one clock > 0).
-- Does not check _endTime or _paused, so it works for ended/deserialized games too.
local function hasClocks(self)
  return self._startTime
    and ((self._clockWhite or 0) > 0 or (self._clockBlack or 0) > 0)
end

-- Internal: Check if clocks are actively ticking (game started, not ended/paused, at least one clock > 0).
-- Used for the live re-check optimization when status is cached as RUNNING.
local function hasClocksActive(self)
  return hasClocks(self) and not self._endTime and not self._paused
end

-- Internal: Calculate and cache game status
local function calculateStatus(self)
  -- Skip if cached, UNLESS status is RUNNING with active clocks (time-dependent, needs re-check)
  if self._cachedStatus then
    if self._cachedStatus ~= Constants.RUNNING or not hasClocksActive(self) then
      return
    end
    -- Re-check timeout only (position-based checks are still valid)
    -- Check non-moving side first (definitive completed-move time)
    local stmColor = (self.pos.stm == "w") and Constants.COLOR.WHITE or Constants.COLOR.BLACK
    local otherColor = (stmColor == Constants.COLOR.WHITE) and Constants.COLOR.BLACK or Constants.COLOR.WHITE
    local otherRemain = self:GetRemainingTime(otherColor)
    if otherRemain ~= nil and otherRemain <= 0 then
      self._cachedStatus = Constants.ENDED
      self._cachedResult = (otherColor == Constants.COLOR.WHITE) and Constants.BLACK or Constants.WHITE
      self._cachedReason = Constants.REASON_TIMEOUT
    else
      local stmRemain = self:GetRemainingTime(stmColor)
      if stmRemain ~= nil and stmRemain <= 0 then
        self._cachedStatus = Constants.ENDED
        self._cachedResult = (stmColor == Constants.COLOR.WHITE) and Constants.BLACK or Constants.WHITE
        self._cachedReason = Constants.REASON_TIMEOUT
      end
    end
    return
  end
  
  -- Check resignation
  if self._resignedColor then
    self._cachedStatus = Constants.ENDED
    self._cachedResult = (self._resignedColor == Constants.COLOR.WHITE) and Constants.BLACK or Constants.WHITE
    self._cachedReason = Constants.REASON_RESIGNATION
    return
  end
  
  -- Check 50-move rule
  if (self.pos.half or 0) >= Constants.FIFTY_MOVE_LIMIT then
    self._cachedStatus = Constants.ENDED
    self._cachedResult = Constants.DRAWN
    self._cachedReason = Constants.REASON_FIFTY_MOVE
    return
  end

  -- Fivefold repetition (FIDE 2014): same position 5 times (consecutive or not) is an immediate draw by the arbiter
  if getRepetitionCount(self) >= 5 then
    self._cachedStatus = Constants.ENDED
    self._cachedResult = Constants.DRAWN
    self._cachedReason = Constants.REASON_FIVEFOLD_REPETITION
    return
  end
  
  -- Check for no legal moves
  local legal = self:LegalMoves()
  if #legal == 0 then
    self._cachedStatus = Constants.ENDED
    if self:InCheck() then
      -- Checkmate: the side NOT to move wins
      self._cachedResult = self.pos.stm == "w" and Constants.BLACK or Constants.WHITE
      self._cachedReason = Constants.REASON_CHECKMATE
    else
      -- Stalemate
      self._cachedResult = Constants.DRAWN
      self._cachedReason = Constants.REASON_STALEMATE
    end
    return
  end
  
  -- Check timeout (clock ran out)
  -- Uses hasClocks (not hasClocksActive) so timeout is detected even after the game
  -- has ended and been restored from history (where _endTime is already set).
  -- Check the non-moving side first: their time is definitively known from completed
  -- moves only, so if they exceeded their clock it should be detected before checking
  -- the side-to-move whose remaining time includes the live current segment.
  if hasClocks(self) then
    local stmColor = (self.pos.stm == "w") and Constants.COLOR.WHITE or Constants.COLOR.BLACK
    local otherColor = (stmColor == Constants.COLOR.WHITE) and Constants.COLOR.BLACK or Constants.COLOR.WHITE
    local otherRemain = self:GetRemainingTime(otherColor)
    if otherRemain ~= nil and otherRemain <= 0 then
      self._cachedStatus = Constants.ENDED
      self._cachedResult = (otherColor == Constants.COLOR.WHITE) and Constants.BLACK or Constants.WHITE
      self._cachedReason = Constants.REASON_TIMEOUT
      return
    end
    local stmRemain = self:GetRemainingTime(stmColor)
    if stmRemain ~= nil and stmRemain <= 0 then
      self._cachedStatus = Constants.ENDED
      self._cachedResult = (stmColor == Constants.COLOR.WHITE) and Constants.BLACK or Constants.WHITE
      self._cachedReason = Constants.REASON_TIMEOUT
      return
    end
  end
  
  -- If the game was explicitly ended but no specific reason found, it's a remis (draw by agreement)
  if self._endTime then
    self._cachedStatus = Constants.ENDED
    self._cachedResult = Constants.DRAWN
    self._cachedReason = Constants.REASON_REMIS
    return
  end
  
  -- Game is still running
  self._cachedStatus = Constants.RUNNING
  self._cachedResult = nil
  self._cachedReason = nil
end

-- Internal: Invalidate cached status (called after making a move)
local function invalidateCache(self)
  self._cachedStatus = nil
  self._cachedResult = nil
  self._cachedReason = nil
end

--- Get the game status.
-- @return string Constants.RUNNING or Constants.ENDED
function Board:GetStatus()
  if self._endTime then
    return Constants.ENDED
  end
  calculateStatus(self)
  return self._cachedStatus
end

--- Check if the game is still running.
-- @return boolean
function Board:IsRunning()
  return self:GetStatus() == Constants.RUNNING
end

--- Check if the game has ended.
-- @return boolean
function Board:IsEnded()
  return self:GetStatus() == Constants.ENDED
end

--- Get the game result (only valid when status is ENDED).
-- @return string|nil Constants.WHITE, Constants.BLACK, Constants.DRAWN, or nil if game is running
function Board:GetResult()
  calculateStatus(self)
  return self._cachedResult
end

--- Get the reason the game ended (only valid when status is ENDED).
-- @return string|nil One of Constants.REASON_* or nil if game is running
function Board:GetEndReason()
  calculateStatus(self)
  return self._cachedReason
end

--- Legacy compatibility: Check if game is over.
-- @return boolean, string|nil (isOver, reason)
function Board:IsGameOver()
  calculateStatus(self)
  if self._cachedStatus == Constants.ENDED then
    return true, self._cachedReason
  end
  return false, nil
end

--------------------------------------------------------------------------------
-- Move Making
--------------------------------------------------------------------------------

--- Make a move given a move object (from MoveGen).
-- Updates the half-move clock, full-move number, captured pieces, and move history.
-- @param mv move object from LegalMoves or MoveFromUci
-- @param moveMeta (optional) table of metadata to attach to this move
-- @return Board (self for chaining), or nil and error message
function Board:MakeMove(mv, moveMeta)
  if not mv then
    return nil, "invalid move"
  end
  
  local pos = self.pos
  local board = pos.board
  local white = (pos.stm == "w")
  local from, to = mv.from, mv.to
  local piece = board[from]
  local captured = board[to]
  
  -- Handle en passant capture
  local epCaptured = nil
  if mv.ep then
    -- En passant: the captured pawn is not on the 'to' square
    local epPawnSq = white and (to - 8) or (to + 8)
    epCaptured = board[epPawnSq]
  end
  
  -- Determine actual captured piece
  local capturedPiece = nil
  if mv.ep and epCaptured and epCaptured ~= "." then
    capturedPiece = epCaptured
  elseif captured and captured ~= "." then
    capturedPiece = captured
  end
  
  -- Check if this is a pawn move or capture (resets half-move clock)
  local isPawnMove = (piece == "P" or piece == "p")
  local isCapture = capturedPiece ~= nil
  local resetsHalfMove = isPawnMove or isCapture
  
  -- Detect castling
  local castleType = nil
  if mv.castle then
    if piece == "K" then
      castleType = (to > from) and "K" or "Q"  -- Kingside or Queenside for white
    else
      castleType = (to > from) and "k" or "q"  -- Kingside or Queenside for black
    end
  end
  
  -- Make the move using MoveGen
  local newPos = MoveGen.MakeMove(pos, mv)
  if not newPos then
    return nil, "failed to make move"
  end
  
  -- Update half-move clock
  if resetsHalfMove then
    newPos.half = 0
  else
    newPos.half = (pos.half or 0) + 1
  end
  
  -- Update full-move number (increments after black's move)
  if not white then
    newPos.full = (pos.full or 1) + 1
  else
    newPos.full = pos.full or 1
  end
  
  -- Update en passant square for two-square pawn push
  if isPawnMove then
    local fromRank = math.floor((from - 1) / 8) + 1
    local toRank = math.floor((to - 1) / 8) + 1
    if math.abs(toRank - fromRank) == 2 then
      -- Set en passant square (the square the pawn passed through)
      local epRank = white and 3 or 6
      local epFile = (from - 1) % 8 + 1
      local epSq = (epRank - 1) * 8 + epFile
      newPos.ep = MoveGen.SqToUci(epSq)
    end
  end
  
  -- Update position first so we can check for check
  self.pos = newPos
  invalidateCache(self)
  table.insert(self._positionKeys, getRepetitionKey(self))
  
  -- Check if move results in check
  local givesCheck = MoveGen.InCheck(newPos)
  
  -- Record the move with metadata (timestamp used for clock: when this move was made)
  local uci = MoveGen.MoveToUci(mv)
  local now = (moveMeta and type(moveMeta.timestamp) == "number" and moveMeta.timestamp) or Util.TimeNow()
  local moveRecord = {
    uci = uci,
    piece = piece,  -- Store the piece character (P, N, B, R, Q, K or lowercase)
    captured = capturedPiece,
    promotion = mv.prom,
    castle = castleType,
    check = givesCheck,
    ep = mv.ep or false,
    meta = moveMeta or {},
    timestamp = now
  }
  table.insert(self.moves, moveRecord)
  
  return self
end

--- Make a move given a UCI string.
-- @param uci UCI string (e.g. "e2e4", "e7e8q")
-- @param moveMeta (optional) table of metadata to attach to this move
-- @return Board (self for chaining), or nil and error message
function Board:MakeMoveUci(uci, moveMeta)
  local mv = MoveGen.MoveFromUci(self.pos, uci)
  if not mv then
    return nil, "illegal move: " .. tostring(uci)
  end
  return self:MakeMove(mv, moveMeta)
end

--- Create a copy of this board.
-- @return Board new board with same state
function Board:Copy()
  local copy = M.FromFen(self:GetFen())
  
  -- Deep copy moves with metadata and timestamp
  for _, move in ipairs(self.moves) do
    local moveCopy = {
      uci = move.uci,
      piece = move.piece,
      captured = move.captured,
      promotion = move.promotion,
      castle = move.castle,
      check = move.check,
      ep = move.ep,
      meta = {},
      timestamp = move.timestamp
    }
    -- Copy move meta
    for k, v in pairs(move.meta or {}) do
      moveCopy.meta[k] = v
    end
    table.insert(copy.moves, moveCopy)
  end

  -- Copy game meta
  for k, v in pairs(self.gameMeta) do
    copy.gameMeta[k] = v
  end

  copy._startFen = self._startFen
  rebuildPositionKeys(copy)

  -- Copy native game state fields (gameStatus/gameResult derived from times + _paused)
  copy._startTime = self._startTime
  copy._endTime = self._endTime
  copy._paused = self._paused
  copy._whitePlayer = self._whitePlayer
  copy._blackPlayer = self._blackPlayer
  copy._resignedColor = self._resignedColor
  copy._playerColor = self._playerColor
  -- Clocks and pause history
  copy._clockWhite = self._clockWhite
  copy._clockBlack = self._clockBlack
  copy._incrementWhite = self._incrementWhite
  copy._incrementBlack = self._incrementBlack
  copy._pauseHistory = {}
  for _, entry in ipairs(self._pauseHistory or {}) do
    table.insert(copy._pauseHistory, { pauseTime = entry.pauseTime, unpauseTime = entry.unpauseTime })
  end

  return copy
end

--- Return a board representing the game state at a given move index (for replay).
-- The returned board has the same opponents, startTime, and game meta as this board,
-- with only moves 1..index applied, and is in a paused state so it is suitable for
-- rendering the position at that point in the game.
-- @param index number 0-based or 1-based: 0 = initial position, 1 = after first move, etc.
-- @return Board new board with state up to that move (paused)
function Board:GetBoardAtIndex(index)
  index = (index and index >= 0) and index or 0
  local board = M.FromFen(Constants.START_FEN)
  if not board then return nil end

  -- Copy game metadata and player info (opponents, startTime, clocks, pause history, etc.)
  board._startTime = self._startTime
  board._endTime = nil
  board._paused = true
  board._whitePlayer = self._whitePlayer
  board._blackPlayer = self._blackPlayer
  board._resignedColor = nil
  board._playerColor = self._playerColor
  board._clockWhite = self._clockWhite
  board._clockBlack = self._clockBlack
  board._incrementWhite = self._incrementWhite
  board._incrementBlack = self._incrementBlack
  board._pauseHistory = {}
  for _, entry in ipairs(self._pauseHistory or {}) do
    table.insert(board._pauseHistory, { pauseTime = entry.pauseTime, unpauseTime = entry.unpauseTime })
  end
  for k, v in pairs(self.gameMeta) do
    board.gameMeta[k] = v
  end

  -- Apply moves 1..index (1-based), preserving timestamps for clock calculation
  for i = 1, index do
    local move = self.moves[i]
    if not move then break end
    local meta = {}
    for k, v in pairs(move.meta or {}) do meta[k] = v end
    if type(move.timestamp) == "number" then meta.timestamp = move.timestamp end
    local ok, err = board:MakeMoveUci(move.uci, meta)
    if not ok then
      break
    end
  end

  return board
end

--------------------------------------------------------------------------------
-- Metadata API
--------------------------------------------------------------------------------

--- Set metadata on the last move.
-- @param key string key name
-- @param value any value to store
function Board:SetMoveMeta(key, value)
  local lastMove = self.moves[#self.moves]
  if lastMove then
    lastMove.meta = lastMove.meta or {}
    lastMove.meta[key] = value
  end
end

--- Get metadata from a move at a specific index.
-- @param index number 1-based move index (nil or 0 means last move)
-- @param key string key name
-- @return any stored value or nil
function Board:GetMoveMeta(index, key)
  index = (index and index > 0) and index or #self.moves
  local move = self.moves[index]
  if move and move.meta then
    return move.meta[key]
  end
  return nil
end

--- Set game-level metadata.
-- @param key string key name
-- @param value any value to store
function Board:SetGameMeta(key, value)
  self.gameMeta[key] = value
end

--- Get game-level metadata.
-- @param key string key name
-- @return any stored value or nil
function Board:GetGameMeta(key)
  return self.gameMeta[key]
end

--------------------------------------------------------------------------------
-- Game State Tracking API
--------------------------------------------------------------------------------

--- Check if the game is actively being played (started, running, not paused).
-- @return boolean
function Board:IsActive()
  return self._startTime ~= nil and not self._paused and self:IsRunning()
end

--- Start the game: set start time and clear end time (and unpause).
-- @param timestamp number|nil Unix timestamp for start (default: current time)
function Board:StartGame(timestamp)
  local t = timestamp or Util.TimeNow()
  self._startTime = t
  self._endTime = nil
  self._paused = false
end

--- End the game: set end time (and unpause).
-- @param timestamp number|nil Unix timestamp for end (default: current time)
function Board:EndGame(timestamp)
  local t = timestamp or Util.TimeNow()
  self._endTime = t
  self._paused = false
  invalidateCache(self)
end

--- Pause the game (sets _paused; records pause timestamp in pause history).
function Board:PauseGame()
  local t = Util.TimeNow()
  self._pauseHistory = self._pauseHistory or {}
  table.insert(self._pauseHistory, { pauseTime = t, unpauseTime = nil })
  self._paused = true
end

--- Continue (unpause) the game (records unpause timestamp for the current pause entry).
function Board:ContinueGame()
  if self._pauseHistory and #self._pauseHistory > 0 then
    local last = self._pauseHistory[#self._pauseHistory]
    if last.unpauseTime == nil then
      last.unpauseTime = Util.TimeNow()
    end
  end
  self._paused = false
end

--- Whether the game is paused.
-- @return boolean
function Board:IsPaused()
  return self._paused == true
end

--- Get pause history (list of { pauseTime, unpauseTime }).
-- Used with move timestamps to compute time remaining on each clock.
-- @return table Array of { pauseTime = number, unpauseTime = number }
function Board:GetPauseHistory()
  return self._pauseHistory or {}
end

--- Set initial clock time for a player (seconds).
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @param seconds number Initial time in seconds
function Board:SetClock(color, seconds)
  local s = tonumber(seconds)
  if not s or s < 0 then return end
  if color == Constants.COLOR.WHITE then
    self._clockWhite = s
  else
    self._clockBlack = s
  end
end

--- Get initial clock time for a player (seconds).
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @return number
function Board:GetClock(color)
  if color == Constants.COLOR.WHITE then
    return self._clockWhite or 0
  else
    return self._clockBlack or 0
  end
end

--- Set increment per move for a player (seconds added after each move).
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @param seconds number Increment in seconds (0 to disable)
function Board:SetIncrement(color, seconds)
  local s = tonumber(seconds)
  if not s or s < 0 then return end
  if color == Constants.COLOR.WHITE then
    self._incrementWhite = s
  else
    self._incrementBlack = s
  end
end

--- Get increment per move for a player (seconds).
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @return number
function Board:GetIncrement(color)
  if color == Constants.COLOR.WHITE then
    return self._incrementWhite or 0
  else
    return self._incrementBlack or 0
  end
end

-- Internal: Paused duration in seconds between two timestamps (only counts closed pause intervals).
local function getPausedDurationBetween(pauseHistory, startTs, endTs)
  if not pauseHistory or startTs >= endTs then return 0 end
  local total = 0
  for _, entry in ipairs(pauseHistory) do
    local p, u = entry.pauseTime, entry.unpauseTime
    if type(p) == "number" and type(u) == "number" and u > p then
      local overlapStart = (p > startTs) and p or startTs
      local overlapEnd = (u < endTs) and u or endTs
      if overlapStart < overlapEnd then
        total = total + (overlapEnd - overlapStart)
      end
    end
  end
  return total
end

-- Internal: Total time (seconds) spent on the clock by the given color (completed turns only).
local function getTimeUsedByColor(self, color)
  local stmKey = (color == Constants.COLOR.WHITE) and "w" or "b"
  local history = self._pauseHistory or {}
  local used = 0
  local startTs = self._startTime
  if not startTs then return 0 end
  for i, move in ipairs(self.moves) do
    local moveColor = (move.piece == move.piece:upper()) and "w" or "b"
    if moveColor == stmKey then
      local endTs = type(move.timestamp) == "number" and move.timestamp or startTs
      local segment = endTs - startTs
      segment = segment - getPausedDurationBetween(history, startTs, endTs)
      if segment > 0 then used = used + segment end
    end
    startTs = type(move.timestamp) == "number" and move.timestamp or startTs
  end
  return used
end

--- Get remaining time on the clock for a player (seconds).
-- Uses initial clock, move timestamps, and pause history. If the game is active and it is
-- that player's turn, includes the current segment (from last move or start to currentTime).
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @param currentTime number|nil Optional "now" for active game (defaults to Util.TimeNow())
-- @return number|nil Remaining seconds, or nil if clocks not used (startTime not set)
function Board:GetRemainingTime(color, currentTime)
  if not self._startTime then return nil end
  local initial = self:GetClock(color)
  local increment = self:GetIncrement(color)
  local moves = self.moves or {}
  local stmKey = (color == Constants.COLOR.WHITE) and "w" or "b"
  local numMovesByColor = 0
  for _, move in ipairs(moves) do
    local moveColor = (move.piece == move.piece:upper()) and "w" or "b"
    if moveColor == stmKey then numMovesByColor = numMovesByColor + 1 end
  end
  local used = getTimeUsedByColor(self, color)
  -- Add current/final turn segment: for live games count up to now, for ended games count up to _endTime.
  -- This ensures timeout is still detectable after the game has ended and been restored from history.
  local now = currentTime
  if now == nil then now = Util.TimeNow() end
  if self._endTime and now > self._endTime then now = self._endTime end
  if not self._paused and self.pos and self.pos.stm == stmKey then
    local lastTs = (#moves > 0 and type(moves[#moves].timestamp) == "number") and moves[#moves].timestamp or self._startTime
    local segment = now - lastTs
    segment = segment - getPausedDurationBetween(self._pauseHistory or {}, lastTs, now)
    if segment > 0 then used = used + segment end
  end
  local remaining = initial + (numMovesByColor * increment) - used
  return remaining > 0 and remaining or 0
end

--- Time spent waiting for this side to move (seconds counted against their clock).
-- Sum of all this color's turn durations (from move timestamps and pause history),
-- including the current turn if it's their turn and the game is active.
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @param currentTime number|nil Optional "now" (defaults to Util.TimeNow())
-- @return number Seconds (0 if game not started or no clock)
function Board:TimeThinking(color, currentTime)
  if not self._startTime then return 0 end
  local used = getTimeUsedByColor(self, color)
  local moves = self.moves or {}
  local stmKey = (color == Constants.COLOR.WHITE) and "w" or "b"
  local now = currentTime
  if now == nil then now = Util.TimeNow() end
  if self._endTime and now > self._endTime then now = self._endTime end
  -- Add current/final turn segment (same logic as GetRemainingTime).
  if not self._paused and self.pos and self.pos.stm == stmKey then
    local lastTs = (#moves > 0 and type(moves[#moves].timestamp) == "number") and moves[#moves].timestamp or self._startTime
    local segment = now - lastTs
    segment = segment - getPausedDurationBetween(self._pauseHistory or {}, lastTs, now)
    if segment > 0 then used = used + segment end
  end
  return used
end

--- Time left on the clock for this side (seconds).
-- Uses TimeThinking as helper: initial + (moves by this color * increment) - TimeThinking.
-- Returns nil if no clock is configured (no start time or both clocks 0).
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
-- @param currentTime number|nil Optional "now"
-- @return number|nil Remaining seconds, or nil if no clock
function Board:TimeLeft(color, currentTime)
  if not self._startTime then return nil end
  local initial = self:GetClock(color)
  local increment = self:GetIncrement(color)
  if initial == 0 and increment == 0 then return nil end
  local moves = self.moves or {}
  local stmKey = (color == Constants.COLOR.WHITE) and "w" or "b"
  local numMovesByColor = 0
  for _, move in ipairs(moves) do
    local moveColor = (move.piece == move.piece:upper()) and "w" or "b"
    if moveColor == stmKey then numMovesByColor = numMovesByColor + 1 end
  end
  local thinking = self:TimeThinking(color, currentTime)
  local remaining = initial + (numMovesByColor * increment) - thinking
  return remaining > 0 and remaining or 0
end

--- Get the game start time.
-- @return number|nil Unix timestamp when the game started
function Board:GetStartTime()
  return self._startTime
end

--- Get the game start time as a formatted date string (e.g. "2025-02-04 14:30:00").
-- @return string|nil Formatted string or nil if no start time
function Board:GetStartDateString()
  local t = self:GetStartTime()
  if not t then return nil end
  return Util.Date(t, "%Y-%m-%d %H:%M:%S")
end

--- Set the game start time.
-- @param timestamp number|nil Unix timestamp
function Board:SetStartTime(timestamp)
  self._startTime = timestamp
end

--- Get the game end time.
-- @return number|nil Unix timestamp when the game ended
function Board:GetEndTime()
  return self._endTime
end

--- Set the game end time.
-- @param timestamp number|nil Unix timestamp
function Board:SetEndTime(timestamp)
  self._endTime = timestamp
end

--- Get the game duration in seconds.
-- Computed from startTime and endTime if both are set.
-- If the game is still running (no endTime), computes duration from startTime to now.
-- @param currentTime number|nil Optional current time for calculation (defaults to Util.TimeNow())
-- @return number|nil Duration in seconds, or nil if startTime is not set
function Board:GetDuration(currentTime)
  if not self._startTime then
    return nil
  end
  
  local endTs = self._endTime
  if not endTs then
    endTs = currentTime or Util.TimeNow()
  end
  
  return endTs - self._startTime
end

--- Get the game result (only when _endTime is set).
-- Derived from _resignedColor (winner) or rules-based GetResult() (white/black/draw).
-- @return string|nil "white", "black", or "draw"
function Board:GetGameResult()
  if not self._endTime then return nil end
  if self._resignedColor then
    return (self._resignedColor == Constants.COLOR.WHITE) and Constants.COLOR.BLACK or Constants.COLOR.WHITE
  end
  local r = self:GetResult()
  if not r then return nil end
  if r == Constants.WHITE then return Constants.COLOR.WHITE end
  if r == Constants.BLACK then return Constants.COLOR.BLACK end
  if r == Constants.DRAWN then return "draw" end
  return nil
end

--- Record resignation by the given color. Ends the game and stores resigning color (result is derived from _resignedColor).
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK (the side that resigned)
function Board:Resign(color)
  self._resignedColor = color
  self:EndGame()
end


--------------------------------------------------------------------------------
-- Player API (whitePlayer / blackPlayer)
--------------------------------------------------------------------------------

local function playerName(p)
  if not p then return nil end
  return type(p) == "string" and p or p.name
end

local function playerClass(p)
  if not p or type(p) == "string" then return nil end
  return p.class
end

local function playerEngine(p)
  if not p or type(p) == "string" then return nil end
  return p.engine
end

--- Get white player metadata (table with name, class or nil).
-- @return table|string|nil { name = string, class = string|nil } or legacy string name
function Board:GetWhitePlayer()
  return self._whitePlayer
end

--- Set white player. Pass string (name) or table { name = string, class = string|nil }.
-- @param player string|table
function Board:SetWhitePlayer(player)
  self._whitePlayer = player
end

--- Get black player metadata (table with name, class or nil).
-- @return table|string|nil { name = string, class = string|nil } or legacy string name
function Board:GetBlackPlayer()
  return self._blackPlayer
end

--- Set black player. Pass string (name) or table { name = string, class = string|nil }.
-- @param player string|table
function Board:SetBlackPlayer(player)
  self._blackPlayer = player
end

--- Get white player display name.
-- @return string|nil
function Board:GetWhitePlayerName()
  return playerName(self._whitePlayer)
end

--- Get black player display name.
-- @return string|nil
function Board:GetBlackPlayerName()
  return playerName(self._blackPlayer)
end

--- Get white player class (e.g. for color) or nil.
-- @return string|nil
function Board:GetWhitePlayerClass()
  return playerClass(self._whitePlayer)
end

--- Get black player class (e.g. for color) or nil.
-- @return string|nil
function Board:GetBlackPlayerClass()
  return playerClass(self._blackPlayer)
end

--- Get the name of the player who resigned, or nil if game did not end by resignation.
-- Derived from _resignedColor and white/black player names.
-- @return string|nil
function Board:GetResignedPlayer()
  if not self._resignedColor then return nil end
  local p = (self._resignedColor == Constants.COLOR.WHITE) and self._whitePlayer or self._blackPlayer
  return playerName(p)
end

--- Get the local player's color (which side they are playing).
-- @return string|nil Constants.COLOR.WHITE or Constants.COLOR.BLACK
function Board:GetPlayerColor()
  return self._playerColor or self.gameMeta["playerColor"]
end

--- Set the local player's color.
-- @param color string Constants.COLOR.WHITE or Constants.COLOR.BLACK
function Board:SetPlayerColor(color)
  self._playerColor = color
end

--- True if either white or black player has engine metadata (computer opponent).
-- @return boolean
function Board:OneOpponentIsEngine()
  if (playerEngine(self._whitePlayer) ~= nil) or (playerEngine(self._blackPlayer) ~= nil) then
    return true
  end
  return self.gameMeta["isVsComputer"] == true
end

--- Color of the engine side, or nil if neither side is an engine.
-- @return string|nil Constants.COLOR.WHITE or Constants.COLOR.BLACK
function Board:GetEnginePlayerColor()
  if playerEngine(self._whitePlayer) then return Constants.COLOR.WHITE end
  if playerEngine(self._blackPlayer) then return Constants.COLOR.BLACK end
  return self.gameMeta["computerColor"]
end

--- Engine id of the computer side (from player.engine.id), or nil.
-- @return string|nil
function Board:GetEngineId()
  local e = playerEngine(self._whitePlayer) or playerEngine(self._blackPlayer)
  if e and e.id then return e.id end
  return self.gameMeta["computerEngine"]
end

--- Engine ELO/difficulty of the computer side (from player.engine.elo), or nil.
-- @return number|nil
function Board:GetEngineElo()
  local e = playerEngine(self._whitePlayer) or playerEngine(self._blackPlayer)
  if e and e.elo ~= nil then return e.elo end
  return self.gameMeta["computerDifficulty"]
end

--------------------------------------------------------------------------------
-- PGN Import (FromPGN)
--------------------------------------------------------------------------------

-- Unescape PGN tag value: \" -> ", \\ -> \
local function pgnUnescape(s)
  if not s then return "" end
  local PLACEHOLDER = "\255\255"
  return s:gsub("\\\\", PLACEHOLDER):gsub('\\"', '"'):gsub(PLACEHOLDER, "\\")
end

-- Parse [Tag "value"] pairs from the tag section (lines before first blank line).
local function parsePgnTags(pgn)
  local tags = {}
  for line in (pgn or ""):gmatch("[^\r\n]+") do
    local trimmed = line:match("^%s*(.-)%s*$") or line
    if trimmed == "" then break end
    local name, val = trimmed:match('^%[(%w+)%s+"(.*)"%s*%]$')
    if name and val ~= nil then
      tags[name] = pgnUnescape(val)
    end
  end
  return tags
end

-- Extract movetext (after first blank line), strip semicolon comments.
local function getMovetextSection(pgn)
  pgn = (pgn or ""):gsub("\r\n", "\n"):gsub("\r", "\n")
  local s, e = pgn:find("\n%s*\n")
  if not s then return pgn:gsub(";[^\n]*", " "):match("^%s*(.-)%s*$") or pgn end
  return pgn:sub(e + 1):gsub(";[^\n]*", " "):match("^%s*(.-)%s*$") or pgn:sub(e + 1)
end

-- Extract brace comments in order; return list of comment bodies and movetext with comments removed.
local function extractComments(movetext)
  local comments = {}
  local count = 0
  local clean = (movetext or ""):gsub("%b{}", function(inner)
    count = count + 1
    comments[count] = inner:sub(2, -2)
    return " "
  end)
  return comments, clean
end

-- Parse clock from comment. Supports:
-- [%clk H:MM:SS] or [%clk 0:MM:SS] (standard; moveIndex 1=white, 2=black -> return that side only)
-- [%clockW n][%clockB n] (legacy; return both)
-- moveIndex: 1-based SAN index (odd=white, even=black). Used for %clk. Optional for legacy.
local function parseClockComment(comment, moveIndex)
  if not comment then return nil, nil end
  local cw = comment:match("%[%%clockW%s+(%d+)%]")
  local cb = comment:match("%[%%clockB%s+(%d+)%]")
  if cw or cb then
    return tonumber(cw), tonumber(cb)
  end
  -- Standard [%clk H:MM:SS] or [%clk 0:MM:SS]
  local h, m, s = comment:match("%[%%clk%s+(%d+):(%d%d):(%d%d)%]")
  if h and m and s then
    local sec = tonumber(h) * 3600 + tonumber(m) * 60 + tonumber(s)
    if moveIndex and sec then
      if moveIndex % 2 == 1 then
        return sec, nil
      else
        return nil, sec
      end
    end
    return nil, nil
  end
  return nil, nil
end

-- Tokenize movetext into list of SANs and list of comments (by move index).
local function tokenizeMovetext(movetext)
  local comments, clean = extractComments(movetext)
  local sans = {}
  for token in clean:gmatch("%S+") do
    if token:match("^%d+%.+$") then
      -- move number (e.g. 1. or 2. or 3...)
    elseif token == "1-0" or token == "0-1" or token == "1/2-1/2" or token == "*" then
      break
    else
      table.insert(sans, token)
    end
  end
  return sans, comments
end

--- Create a board from a PGN string. Restores position, moves, tags, and optional clock from comments.
-- Clock in comments: {[%clk 0:MM:SS]} (standard, one per move) or legacy {[%clockW n][%clockB n]} (seconds).
-- @param pgn string PGN document (tag pairs + blank line + movetext)
-- @return Board|nil, error message
function M.FromPGN(pgn)
  if not pgn or pgn == "" then
    return nil, "empty PGN"
  end
  local tags = parsePgnTags(pgn)
  local movetext = getMovetextSection(pgn)
  local sans, comments = tokenizeMovetext(movetext)

  local startFen = Constants.START_FEN
  if tags.FEN and tags.SetUp == "1" then
    startFen = tags.FEN
  end
  local board, err = M.FromFen(startFen)
  if not board then
    return nil, err or "invalid FEN in PGN"
  end

  board.gameMeta["event"] = tags.Event
  board.gameMeta["site"] = tags.Site
  board.gameMeta["round"] = tags.Round
  if tags.White and tags.White ~= "?" then
    board:SetWhitePlayer(playerFromPgnValue(tags.White))
  end
  if tags.Black and tags.Black ~= "?" then
    board:SetBlackPlayer(playerFromPgnValue(tags.Black))
  end

  -- PGN standard 9.5.1 Time (local), 9.5.2 UTCTime: restore _startTime from Date + Time or UTCTime
  local dateStr = tags.Date or ""
  local timeStr = tags.Time or tags.UTCTime
  if timeStr and dateStr:match("^(%d%d%d%d)%.(%d%d)%.(%d%d)$") then
    local y, m, d = dateStr:match("^(%d%d%d%d)%.(%d%d)%.(%d%d)$")
    local hour, min, sec = timeStr:match("^(%d%d):(%d%d):(%d%d)$")
    if y and m and d and hour and min and sec then
      local t = {
        year = tonumber(y), month = tonumber(m), day = tonumber(d),
        hour = tonumber(hour), min = tonumber(min), sec = tonumber(sec),
      }
      local ts = Util.TimeFromTable(t)
      if ts then
        if tags.UTCTime == timeStr and _G.os and _G.os.time and _G.os.date then
          -- os.time(t) interprets t as local; convert UTC components to real timestamp
          local now = _G.os.time()
          ts = ts - _G.os.time(_G.os.date("!*t", now)) + now
        end
        board._startTime = ts
      end
    end
  end
  -- Supplemental EndTime: restore _endTime (same calendar date as start)
  if tags.EndTime and board._startTime then
    local hour, min, sec = (tags.EndTime or ""):match("^(%d%d):(%d%d):(%d%d)$")
    if hour and min and sec then
      local startDate = Util.DateTable(board._startTime)
      local endT = {
        year = startDate.year, month = startDate.month, day = startDate.day,
        hour = tonumber(hour), min = tonumber(min), sec = tonumber(sec),
      }
      local endTs = Util.TimeFromTable(endT)
      if endTs then board._endTime = endTs end
    end
  end

  local pos = MoveGen.ParseFen(startFen)
  if not pos then
    return nil, "ParseFen failed"
  end

  for i, san in ipairs(sans) do
    local uci = MoveGen.SanToUci(pos, san)
    if not uci then
      return nil, ("invalid or ambiguous SAN at move %d: %s"):format(i, tostring(san))
    end
    local ok, makeErr = board:MakeMoveUci(uci)
    if not ok then
      return nil, ("move %d %s: %s"):format(i, san, tostring(makeErr))
    end
    pos = MoveGen.ParseFen(board:GetFen())
    if not pos then
      return nil, "position after move " .. i .. " invalid"
    end
    local cw, cb = parseClockComment(comments[i], i)
    if cw ~= nil then
      board:SetMoveMeta("clockWhite", cw)
    end
    if cb ~= nil then
      board:SetMoveMeta("clockBlack", cb)
    end
  end

  if tags.Result and tags.Result ~= "*" then
    board.gameMeta["pgnResult"] = tags.Result
    board:EndGame()
  end

  if tags.TimeControl and tags.TimeControl ~= "" then
    local w, b = tags.TimeControl:match("^(%d+):(%d+)$")
    if w and b then
      board:SetClock(Constants.COLOR.WHITE, tonumber(w))
      board:SetClock(Constants.COLOR.BLACK, tonumber(b))
    end
  end

  return board
end

--------------------------------------------------------------------------------
-- Serialization API
--------------------------------------------------------------------------------

--- Serialize the entire board state to a table.
-- @return table serializable representation of full game state
function Board:Serialize()
  -- Deep copy moves (include timestamp for clock)
  local movesCopy = {}
  for _, move in ipairs(self.moves) do
    local moveCopy = {
      uci = move.uci,
      piece = move.piece,
      captured = move.captured,
      promotion = move.promotion,
      castle = move.castle,
      check = move.check,
      ep = move.ep,
      meta = {},
      timestamp = move.timestamp
    }
    for k, v in pairs(move.meta or {}) do
      moveCopy.meta[k] = v
    end
    table.insert(movesCopy, moveCopy)
  end

  -- Deep copy game meta
  local gameMetaCopy = {}
  for k, v in pairs(self.gameMeta) do
    gameMetaCopy[k] = v
  end

  -- Deep copy pause history
  local pauseHistoryCopy = {}
  for _, entry in ipairs(self._pauseHistory or {}) do
    table.insert(pauseHistoryCopy, { pauseTime = entry.pauseTime, unpauseTime = entry.unpauseTime })
  end

  return {
    fen = self:GetFen(),
    moves = movesCopy,
    gameMeta = gameMetaCopy,
    startFen = self._startFen or Constants.START_FEN,
    -- Native game state fields
    gameStatus = self._endTime and Constants.STATUS_ENDED
      or self._paused and Constants.STATUS_PAUSED
      or self._startTime and Constants.STATUS_ACTIVE
      or nil,
    startTime = self._startTime,
    endTime = self._endTime,
    paused = self._paused,
    gameResult = self:GetGameResult(),
    whitePlayer = self._whitePlayer,
    blackPlayer = self._blackPlayer,
    resignedColor = self._resignedColor,
    playerColor = self._playerColor,
    clockWhite = self._clockWhite,
    clockBlack = self._clockBlack,
    incrementWhite = self._incrementWhite,
    incrementBlack = self._incrementBlack,
    pauseHistory = pauseHistoryCopy
  }
end

--- Restore a board from serialized data.
-- @param data table from Board:Serialize()
-- @return Board object, or nil and error message
function M.Deserialize(data)
  if not data or not data.fen then
    return nil, "invalid serialized data: missing fen"
  end
  
  local board, err = M.FromFen(data.fen)
  if not board then
    return nil, err
  end
  
  -- Restore moves (captured pieces and timestamps for clock)
  if data.moves then
    for _, move in ipairs(data.moves) do
      local moveCopy = {
        uci = move.uci,
        piece = move.piece,
        captured = move.captured,
        promotion = move.promotion,
        castle = move.castle,
        check = move.check,
        ep = move.ep,
        meta = {},
        timestamp = move.timestamp
      }
      for k, v in pairs(move.meta or {}) do
        moveCopy.meta[k] = v
      end
      table.insert(board.moves, moveCopy)
    end
  end

  -- Restore game meta (exclude gameStatus and pgnResult; game state comes from _paused and timestamps only)
  if data.gameMeta then
    for k, v in pairs(data.gameMeta) do
      board.gameMeta[k] = v
    end
  end

  board._startFen = data.startFen or Constants.START_FEN
  rebuildPositionKeys(board)

  -- Restore native game state fields
  board._startTime = data.startTime
  board._endTime = data.endTime
  board._paused = (data.paused == true) or (data.gameStatus == Constants.STATUS_PAUSED)
  board._whitePlayer = data.whitePlayer
  board._blackPlayer = data.blackPlayer
  board._resignedColor = data.resignedColor
  board._playerColor = data.playerColor
  board._clockWhite = data.clockWhite
  board._clockBlack = data.clockBlack
  board._incrementWhite = data.incrementWhite
  board._incrementBlack = data.incrementBlack
  board._pauseHistory = {}
  if data.pauseHistory then
    for _, entry in ipairs(data.pauseHistory) do
      table.insert(board._pauseHistory, { pauseTime = entry.pauseTime, unpauseTime = entry.unpauseTime })
    end
  end

  return board
end

-- Export the Board prototype for restoring saved games
M.Prototype = Board

DeltaChess.Board = M
return M
