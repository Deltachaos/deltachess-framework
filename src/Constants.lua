--[[
  Constants: Shared constants for DeltaChess.
  Registers globally: DeltaChess.Constants
]]

DeltaChess = DeltaChess or {}

local M = {}

--------------------------------------------------------------------------------
-- Piece colors
--------------------------------------------------------------------------------
M.COLOR = {
    WHITE = "white",
    BLACK = "black"
}

--------------------------------------------------------------------------------
-- Game Results
--------------------------------------------------------------------------------

-- Game result constants
M.WHITE = "WHITE"     -- White wins (checkmate)
M.BLACK = "BLACK"     -- Black wins (checkmate)
M.DRAWN = "DRAWN"     -- Draw (stalemate, 50-move rule, insufficient material, etc.)
M.TIMEOUT = "TIMEOUT" -- Max half-moves reached (game limit, not a chess rule)
M.ERROR = "ERROR"     -- Engine error or invalid move

--------------------------------------------------------------------------------
-- Game Status (rules-based, from Board:GetStatus())
--------------------------------------------------------------------------------

-- Rules-based status (checkmate, stalemate detection)
M.RUNNING = "running"  -- Game is still in progress (rules-based)
M.ENDED = "ended"      -- Game has ended by rules (check GetResult for outcome)

--------------------------------------------------------------------------------
-- Application-level Game Status (used in serialization and UI layer)
--------------------------------------------------------------------------------

M.STATUS_ACTIVE = "active"        -- Game is actively being played (see Board:IsActive())
M.STATUS_PAUSED = "paused"        -- Game is paused (see Board:IsPaused())
M.STATUS_ENDED = "ended"          -- Game has ended (see Board:IsEnded())

--------------------------------------------------------------------------------
-- Draw Reasons
--------------------------------------------------------------------------------

M.DRAW_STALEMATE = "stalemate"
M.DRAW_FIFTY_MOVE = "fifty_move_rule"
M.DRAW_INSUFFICIENT = "insufficient_material"
M.DRAW_REPETITION = "threefold_repetition"

--------------------------------------------------------------------------------
-- Game End Reasons
--------------------------------------------------------------------------------

M.REASON_CHECKMATE = "checkmate"
M.REASON_STALEMATE = "stalemate"
M.REASON_FIFTY_MOVE = "fifty_move_rule"
M.REASON_FIVEFOLD_REPETITION = "fivefold_repetition"
M.REASON_TIMEOUT = "timeout"
M.REASON_ENGINE_ERROR = "engine_error"
M.REASON_INVALID_MOVE = "invalid_move"
M.REASON_RESIGNATION = "resignation"
M.REASON_REMIS = "remis"

--------------------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------------------

M.START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
M.DEFAULT_MAX_HALF_MOVES = 500
M.FIFTY_MOVE_LIMIT = 100  -- 50 full moves = 100 half-moves

DeltaChess.Constants = M
return M
