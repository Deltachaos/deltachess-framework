--[[
  MoveGen integration tests.
  Tests the move generation library for correctness.
]]

local Test = require("test")
local M = DeltaChess.MoveGen

Test.suite("MoveGen")

-- Helper: check if a move list contains a specific UCI move
local function containsMove(moves, uci)
  for _, mv in ipairs(moves) do
    if M.MoveToUci(mv) == uci then return true end
  end
  return false
end

-- Helper: get list of UCI moves from a position
local function getUciMoves(fen)
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  local uciList = {}
  for _, mv in ipairs(moves) do
    table.insert(uciList, M.MoveToUci(mv))
  end
  return uciList
end

--------------------------------------------------------------------------------
-- FEN Parsing Tests
--------------------------------------------------------------------------------

Test.test("ParseFen: starting position", function()
  local pos = M.ParseFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  Test.assertEq(pos.stm, "w")
  Test.assertEq(pos.board[1], "R")  -- a1
  Test.assertEq(pos.board[5], "K")  -- e1
  Test.assertEq(pos.board[57], "r") -- a8
  Test.assertEq(pos.board[61], "k") -- e8
end)

Test.test("SqToUci and UciToSq roundtrip", function()
  for sq = 1, 64 do
    local uci = M.SqToUci(sq)
    local sq2 = M.UciToSq(uci)
    Test.assertEq(sq2, sq, "roundtrip failed for sq " .. sq)
  end
end)

--------------------------------------------------------------------------------
-- Move Count Tests
--------------------------------------------------------------------------------

Test.test("Starting position has 20 legal moves", function()
  local pos = M.ParseFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  local moves = M.LegalMoves(pos)
  Test.assertEq(#moves, 20, "starting position should have 20 moves")
end)

--------------------------------------------------------------------------------
-- Pawn Attack Detection Tests
--------------------------------------------------------------------------------

Test.test("Pawn attack: Black pawn attacks diagonally downward", function()
  -- Position: r3k3/ppp5/2n4p/3K1P2/2P3P1/P7/4P3/qN3B2 w q - 0 22
  -- King on d5, black pawns on a7, b7, c7
  -- Move d5c6 would capture knight but leave king in check from b7 pawn
  local fen = "r3k3/ppp5/2n4p/3K1P2/2P3P1/P7/4P3/qN3B2 w q - 0 22"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  
  -- d5c6 should NOT be in legal moves because b7 pawn attacks c6
  Test.assertFalse(containsMove(moves, "d5c6"), "d5c6 should be illegal (king would be in check from b7 pawn)")
end)

Test.test("Pawn attack: White pawn attacks diagonally upward", function()
  -- White pawn on e4 should attack d5 and f5
  -- Place black king on d5 with white pawn on e4 - king should be in check
  local fen = "8/8/8/3k4/4P3/8/8/4K3 b - - 0 1"
  local pos = M.ParseFen(fen)
  Test.assertTrue(M.InCheck(pos), "black king on d5 should be in check from white pawn on e4")
end)

Test.test("Pawn attack: White pawn does not attack backward", function()
  -- White pawn on e4, black king on d3 - king should NOT be in check
  local fen = "8/8/8/8/4P3/3k4/8/4K3 b - - 0 1"
  local pos = M.ParseFen(fen)
  Test.assertFalse(M.InCheck(pos), "black king on d3 should NOT be in check from white pawn on e4")
end)

Test.test("Pawn attack: Black pawn does not attack backward", function()
  -- Black pawn on e5, white king on d6 - king should NOT be in check
  local fen = "8/8/3K4/4p3/8/8/8/4k3 w - - 0 1"
  local pos = M.ParseFen(fen)
  Test.assertFalse(M.InCheck(pos), "white king on d6 should NOT be in check from black pawn on e5")
end)

Test.test("Pawn attack: Black pawn attacks diagonally downward 2", function()
  -- Black pawn on e5, white king on d4 - king SHOULD be in check
  local fen = "8/8/8/4p3/3K4/8/8/4k3 w - - 0 1"
  local pos = M.ParseFen(fen)
  Test.assertTrue(M.InCheck(pos), "white king on d4 should be in check from black pawn on e5")
end)

Test.test("Pawn attack: King cannot move into pawn attack", function()
  -- White king on e4, black pawn on e6
  -- King should not be able to move to d5 (attacked diagonally by e6 pawn)
  local fen = "8/8/4p3/8/4K3/8/8/4k3 w - - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e4d5"), "e4d5 should be illegal (would be in check from e6 pawn)")
end)

--------------------------------------------------------------------------------
-- En Passant Tests
--------------------------------------------------------------------------------

Test.test("En passant is legal", function()
  local fen = "8/8/8/3pP3/8/8/8/4K2k w - d6 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e5d6"), "en passant e5d6 should be legal")
end)

--------------------------------------------------------------------------------
-- Castling Tests
--------------------------------------------------------------------------------

Test.test("Castling kingside", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e1g1"), "white kingside castling should be legal")
end)

Test.test("Castling queenside", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e1c1"), "white queenside castling should be legal")
end)

Test.test("Cannot castle through check", function()
  -- Rook on d8 attacks d1, so queenside castling through d1 is illegal
  local fen = "3r4/8/8/8/8/8/8/R3K2R w KQ - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1c1"), "cannot castle queenside through d1 (attacked)")
end)

Test.test("Match: e1g1 not valid after given game", function()
  -- Play a full game and verify white kingside castling (e1g1) is not legal at the end.
  -- The white king or rook has moved (or castling rights were lost), so e1g1 must not be valid.
  local moves = {
    "e2e4", "g8f6", "b1c3", "d7d5", "e4e5", "f6g4", "d2d4", "b8c6", "c1g5", "f7f6",
    "g5f4", "f6e5", "d4e5", "c6e5", "c3d5", "c7c6", "d5c3", "d8b6", "g1h3", "b6b2",
    "f4d2", "c8f5", "a1b1", "b2c2", "d1c2", "f5c2", "b1b7", "c2f5", "h3f4", "e8c8",
    "b7a7", "c8b8", "a7a5", "g7g5", "h2h3", "g4f2", "a5e5", "f2h1", "e5f5", "g5f4",
    "d2f4", "b8b7", "f1c4", "f8g7", "c3e4", "d8d4", "e4c5", "b7b6", "c4e2", "d4f4",
    "c5d7", "b6c7", "f5f4", "c7d7"
  }
  local startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  local pos = M.ParseFen(startFen)
  Test.assertNotNil(pos, "ParseFen should succeed")
  for _, uci in ipairs(moves) do
    local mv = M.MoveFromUci(pos, uci)
    Test.assertNotNil(mv, "MoveFromUci should succeed for " .. uci)
    pos = M.MakeMove(pos, mv)
    Test.assertNotNil(pos, "MakeMove should succeed for " .. uci)
  end
  -- After c7d7 it is white to move; e1g1 (kingside castling) must not be legal
  local legal = M.LegalMoves(pos)
  Test.assertFalse(containsMove(legal, "e1g1"), "e1g1 should not be a valid move in this position")
end)

-- King moved but returned to e1/e8: castling rights lost
Test.test("Castling: white king moved and back, O-O-O not legal", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w - kq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1c1"), "O-O-O not legal after king moved")
  Test.assertFalse(containsMove(moves, "e1g1"), "O-O not legal after king moved")
end)
Test.test("Castling: black king moved and back, O-O-O not legal", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQ - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e8c8"), "O-O-O not legal after king moved")
  Test.assertFalse(containsMove(moves, "e8g8"), "O-O not legal after king moved")
end)

-- Left rook (a1/a8) moved but returned: queenside castling lost
Test.test("Castling: white left rook moved and back, O-O-O not legal", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w Kkq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1c1"), "O-O-O not legal after a1 rook moved")
  Test.assertTrue(containsMove(moves, "e1g1"), "O-O remains legal")
end)
Test.test("Castling: black left rook moved and back, O-O-O not legal", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQk - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e8c8"), "O-O-O not legal after a8 rook moved")
  Test.assertTrue(containsMove(moves, "e8g8"), "O-O remains legal")
end)

-- Right rook (h1/h8) moved but returned: kingside castling lost
Test.test("Castling: white right rook moved and back, O-O not legal", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w Qkq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1g1"), "O-O not legal after h1 rook moved")
  Test.assertTrue(containsMove(moves, "e1c1"), "O-O-O remains legal")
end)
Test.test("Castling: black right rook moved and back, O-O not legal", function()
  local fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e8g8"), "O-O not legal after h8 rook moved")
  Test.assertTrue(containsMove(moves, "e8c8"), "O-O-O remains legal")
end)

-- Original rook captured, promoted rook on corner: castling not allowed
Test.test("Castling: white left rook captured and replaced by promoted rook, O-O-O not legal", function()
  local fen = "8/8/8/8/8/8/8/R3K2R w K - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1c1"), "O-O-O not legal when a1 rook is promoted")
  Test.assertTrue(containsMove(moves, "e1g1"), "O-O remains legal")
end)
Test.test("Castling: white right rook captured and replaced by promoted rook, O-O not legal", function()
  local fen = "8/8/8/8/8/8/8/R3K2R w Q - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1g1"), "O-O not legal when h1 rook is promoted")
  Test.assertTrue(containsMove(moves, "e1c1"), "O-O-O remains legal")
end)
Test.test("Castling: black left rook captured and replaced by promoted rook, O-O-O not legal", function()
  local fen = "r3k2r/8/8/8/8/8/8/8 b k - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e8c8"), "O-O-O not legal when a8 rook is promoted")
  Test.assertTrue(containsMove(moves, "e8g8"), "O-O remains legal")
end)
Test.test("Castling: black right rook captured and replaced by promoted rook, O-O not legal", function()
  local fen = "r3k2r/8/8/8/8/8/8/8 b q - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e8g8"), "O-O not legal when h8 rook is promoted")
  Test.assertTrue(containsMove(moves, "e8c8"), "O-O-O remains legal")
end)

-- b1 or b8 threatened: queenside castling allowed (king does not pass through b1/b8)
Test.test("Castling: b1 threatened, white O-O-O legal", function()
  local fen = "8/8/8/8/8/8/1r6/R3K2R w KQ - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e1c1"), "O-O-O legal when only b1 is threatened")
end)
Test.test("Castling: b8 threatened, black O-O-O legal", function()
  local fen = "r3k2r/1R6/8/8/8/8/8/8 b kq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e8c8"), "O-O-O legal when only b8 is threatened")
end)

-- c1,d1,e1 or c8,d8,e8 threatened: queenside castling not allowed
Test.test("Castling: d1 threatened, white O-O-O not legal", function()
  local fen = "8/8/8/8/8/8/3r4/R3K2R w KQ - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1c1"), "O-O-O not legal when d1 is threatened")
end)
Test.test("Castling: d8 threatened, black O-O-O not legal", function()
  local fen = "r3k2r/3R4/8/8/8/8/8/8 b kq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e8c8"), "O-O-O not legal when d8 is threatened")
end)

-- h1 or h8 threatened: kingside castling allowed (king does not pass through h1/h8)
Test.test("Castling: h1 threatened, white O-O legal", function()
  local fen = "8/8/8/8/8/8/7r/R3K2R w KQ - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e1g1"), "O-O legal when only h1 is threatened")
end)
Test.test("Castling: h8 threatened, black O-O legal", function()
  local fen = "r3k2r/7R/8/8/8/8/8/8 b kq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e8g8"), "O-O legal when only h8 is threatened")
end)

-- e1,f1,g1 or e8,f8,g8 threatened: kingside castling not allowed
Test.test("Castling: f1 threatened, white O-O not legal", function()
  local fen = "8/8/8/8/8/8/5r2/R3K2R w KQ - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e1g1"), "O-O not legal when f1 is threatened")
end)
Test.test("Castling: f8 threatened, black O-O not legal", function()
  local fen = "r3k2r/5R2/8/8/8/8/8/8 b kq - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertFalse(containsMove(moves, "e8g8"), "O-O not legal when f8 is threatened")
end)

--------------------------------------------------------------------------------
-- Piece Movement Tests
--------------------------------------------------------------------------------

Test.test("Knight moves", function()
  local fen = "8/8/8/4N3/8/8/8/4K2k w - - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e5f7"), "knight e5f7 should be legal")
  Test.assertTrue(containsMove(moves, "e5g4"), "knight e5g4 should be legal")
  Test.assertTrue(containsMove(moves, "e5c6"), "knight e5c6 should be legal")
end)

--------------------------------------------------------------------------------
-- Pawn Promotion Tests
--------------------------------------------------------------------------------

Test.test("Pawn promotion", function()
  local fen = "8/4P3/8/8/8/8/8/4K2k w - - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  Test.assertTrue(containsMove(moves, "e7e8q"), "pawn promotion to queen should be legal")
  Test.assertTrue(containsMove(moves, "e7e8r"), "pawn promotion to rook should be legal")
  Test.assertTrue(containsMove(moves, "e7e8b"), "pawn promotion to bishop should be legal")
  Test.assertTrue(containsMove(moves, "e7e8n"), "pawn promotion to knight should be legal")
end)

--------------------------------------------------------------------------------
-- Check Evasion Tests
--------------------------------------------------------------------------------

Test.test("Must block or capture when in check", function()
  -- White king on e1 in check from black queen on e8
  local fen = "4q3/8/8/8/8/8/8/4K3 w - - 0 1"
  local pos = M.ParseFen(fen)
  local moves = M.LegalMoves(pos)
  -- All legal moves must either move the king or block
  for _, mv in ipairs(moves) do
    local uci = M.MoveToUci(mv)
    -- King must move (starts with e1)
    Test.assertTrue(uci:sub(1,2) == "e1", "in check, only king moves should be legal: " .. uci)
  end
end)
