--------------------------------------------------------------------------------
-- BoardMove: Encapsulates a single move with accessor methods
-- Part of EngineFramework
--------------------------------------------------------------------------------

local M = {}

local BoardMove = {}
BoardMove.__index = BoardMove

-- Export the prototype for metatable restoration
M.Prototype = BoardMove

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

local COLOR = DeltaChess.Constants.COLOR

--- Get color from piece character (uppercase = white, lowercase = black)
local function pieceColor(char)
    if not char then return nil end
    return char:upper() == char and COLOR.WHITE or COLOR.BLACK
end

--------------------------------------------------------------------------------
-- Constructor
--------------------------------------------------------------------------------

--- Create a new BoardMove from raw move data.
-- @param data table with move properties (uci, piece, captured, etc.)
-- @param board (optional) the Board this move belongs to
-- @param index (optional) the 1-based index of this move in the board's history
-- @return BoardMove
function M.New(data, board, index)
    local move = setmetatable({}, BoardMove)
    
    -- Store raw data (timestamp can be on move or in meta for clock calculation)
    move._uci = data.uci or ""
    move._piece = data.piece
    move._captured = data.captured
    move._promotion = data.promotion
    move._castle = data.castle
    move._check = data.check or false
    move._ep = data.ep or false
    move._meta = data.meta or {}
    move._timestamp = data.timestamp
    
    -- Board reference (optional)
    move._board = board
    move._index = index
    
    -- Parse UCI for coordinates (cached)
    move._fromFile = nil
    move._fromRank = nil
    move._toFile = nil
    move._toRank = nil
    if move._uci and #move._uci >= 4 then
        move._fromFile = move._uci:sub(1, 1)
        move._fromRank = tonumber(move._uci:sub(2, 2))
        move._toFile = move._uci:sub(3, 3)
        move._toRank = tonumber(move._uci:sub(4, 4))
    end
    
    return move
end

--- Wrap an existing plain move table as a BoardMove.
-- If already a BoardMove, returns as-is.
-- @param moveData table or BoardMove
-- @param board (optional) the Board this move belongs to
-- @param index (optional) the 1-based index
-- @return BoardMove
function M.Wrap(moveData, board, index)
    if not moveData then return nil end
    if getmetatable(moveData) == BoardMove then
        -- Already a BoardMove, just update board/index if provided
        if board then moveData._board = board end
        if index then moveData._index = index end
        return moveData
    end
    return M.New(moveData, board, index)
end

--------------------------------------------------------------------------------
-- Core Getters
--------------------------------------------------------------------------------

--- Get the UCI string for this move (e.g., "e2e4", "e7e8q").
-- @return string
function BoardMove:GetUci()
    return self._uci
end

--- Get the move in Standard Algebraic Notation (e.g., "e4", "Nf3", "O-O").
-- Requires this move to be associated with a board and index so the position before the move can be determined.
-- @return string|nil SAN string, or nil if board/index missing or conversion fails
function BoardMove:GetSan()
    if not self._uci or self._uci == "" then return nil end
    local MoveGen = DeltaChess.MoveGen
    if not MoveGen or not MoveGen.UciToSan then return nil end
    if not self._board or not self._index then return nil end
    local boardBefore = self._board:GetBoardAtIndex(self._index - 1)
    if not boardBefore then return nil end
    local fen = boardBefore:GetFen()
    if not fen then return nil end
    local pos = MoveGen.ParseFen(fen)
    if not pos then return nil end
    return MoveGen.UciToSan(pos, self._uci)
end

--- Get the piece character that moved (e.g., "P", "N", "k").
-- @return string|nil
function BoardMove:GetPiece()
    return self._piece
end

--- Get the piece type name (e.g., "p", "n", "k").
-- @return string
function BoardMove:GetPieceType()
    return self._piece and self._piece:lower() or "p"
end

--- Get the color of the piece that moved.
-- @return string "white" or "black"
function BoardMove:GetPieceColor()
    return pieceColor(self._piece) or COLOR.WHITE
end

--- Get the captured piece character, or nil if no capture.
-- @return string|nil
function BoardMove:GetCaptured()
    return self._captured
end

--- Get the captured piece type name, or nil if no capture.
-- @return string|nil
function BoardMove:GetCapturedType()
    return self._captured and self._captured:lower()
end

--- Get the captured piece color, or nil if no capture.
-- @return string|nil
function BoardMove:GetCapturedColor()
    return pieceColor(self._captured)
end

--- Check if this move was a capture.
-- @return boolean
function BoardMove:IsCapture()
    return self._captured ~= nil
end

--- Get the promotion piece character (e.g., "q", "r"), or nil if not a promotion.
-- @return string|nil
function BoardMove:GetPromotion()
    return self._promotion
end

--- Get the promotion piece type name, or nil if not a promotion.
-- @return string|nil
function BoardMove:GetPromotionType()
    return self._promotion  -- already lowercase UCI notation
end

--- Check if this move was a promotion.
-- @return boolean
function BoardMove:IsPromotion()
    return self._promotion ~= nil
end

--- Get the castle type ("K", "Q", "k", "q"), or nil if not castling.
-- @return string|nil
function BoardMove:GetCastle()
    return self._castle
end

--- Check if this move was castling.
-- @return boolean
function BoardMove:IsCastle()
    return self._castle ~= nil
end

--- Check if this move was kingside castling.
-- @return boolean
function BoardMove:IsKingsideCastle()
    return self._castle == "K" or self._castle == "k"
end

--- Check if this move was queenside castling.
-- @return boolean
function BoardMove:IsQueensideCastle()
    return self._castle == "Q" or self._castle == "q"
end

--- Check if this move gives check.
-- @return boolean
function BoardMove:IsCheck()
    return self._check
end

--- Check if this move was en passant.
-- @return boolean
function BoardMove:IsEnPassant()
    return self._ep
end

--------------------------------------------------------------------------------
-- Coordinate Getters
--------------------------------------------------------------------------------

--- Get from square as "e2" style string.
-- @return string
function BoardMove:GetFromSquare()
    if self._fromFile and self._fromRank then
        return self._fromFile .. self._fromRank
    end
    return ""
end

--- Get to square as "e4" style string.
-- @return string
function BoardMove:GetToSquare()
    if self._toFile and self._toRank then
        return self._toFile .. self._toRank
    end
    return ""
end

--------------------------------------------------------------------------------
-- Metadata Getters
--------------------------------------------------------------------------------

--- Get metadata value by key.
-- @param key string
-- @return any
function BoardMove:GetMeta(key)
    return self._meta[key]
end

--- Set metadata value.
-- @param key string
-- @param value any
function BoardMove:SetMeta(key, value)
    self._meta[key] = value
end

--- Get the raw metadata table.
-- @return table
function BoardMove:GetMetaTable()
    return self._meta
end

--- Get the timestamp when this move was made.
-- @return number|nil
function BoardMove:GetTimestamp()
    return self._timestamp
end

--- Get the think time for this move in seconds.
-- @return number|nil
function BoardMove:GetThinkTime()
    return self._meta.thinkTime
end

--- Get the color that made this move (from meta, or inferred from index).
-- @return string "white" or "black"
function BoardMove:GetColor()
    if self._meta.color then
        return self._meta.color
    end
    -- Infer from index: odd = white, even = black
    if self._index then
        return (self._index % 2 == 1) and COLOR.WHITE or COLOR.BLACK
    end
    -- Fallback to piece color
    return self:GetPieceColor()
end

--------------------------------------------------------------------------------
-- Board Reference
--------------------------------------------------------------------------------

--- Get the board this move belongs to.
-- @return Board|nil
function BoardMove:GetBoard()
    return self._board
end

--- Get the index of this move in the board's history (1-based).
-- @return number|nil
function BoardMove:GetIndex()
    return self._index
end

--- Get the full move number (1 for moves 1-2, 2 for moves 3-4, etc.).
-- @return number
function BoardMove:GetFullMoveNumber()
    if self._index then
        return math.ceil(self._index / 2)
    end
    return 1
end

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

--- Convert to a plain table for serialization.
-- @return table
function BoardMove:ToTable()
    return {
        uci = self._uci,
        piece = self._piece,
        captured = self._captured,
        promotion = self._promotion,
        castle = self._castle,
        check = self._check,
        ep = self._ep,
        meta = self._meta
    }
end

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

--- Get a string representation for debugging.
-- @return string
function BoardMove:ToString()
    local str = self._uci or "????"
    if self._check then str = str .. "+" end
    if self._captured then str = str .. " (capture)" end
    if self._promotion then str = str .. "=" .. self._promotion end
    if self._castle then str = str .. " (castle)" end
    return str
end

-- Allow tostring() to work
BoardMove.__tostring = BoardMove.ToString

-- Register globally for WoW addon access
DeltaChess = DeltaChess or {}
DeltaChess.BoardMove = M

return M
