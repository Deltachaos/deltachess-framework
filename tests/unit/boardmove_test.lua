--[[
  BoardMove unit tests.
  Tests the BoardMove module and all accessor methods.
]]

local Test = require("test")

local BoardMove = DeltaChess.BoardMove
local Board = DeltaChess.Board
local Constants = DeltaChess.Constants

Test.suite("BoardMove")

--------------------------------------------------------------------------------
-- Constructor: New, Wrap
--------------------------------------------------------------------------------

Test.test("New: creates BoardMove from data", function()
  local data = { uci = "e2e4", piece = "P", captured = nil, promotion = nil, castle = nil, check = false, ep = false, meta = {} }
  local move = BoardMove.New(data)
  Test.assertNotNil(move, "New returns BoardMove")
  Test.assertEq(move:GetUci(), "e2e4", "UCI")
  Test.assertEq(move:GetPiece(), "P", "piece")
end)

Test.test("Wrap: wraps plain table as BoardMove", function()
  local raw = { uci = "e7e5", piece = "p" }
  local move = BoardMove.Wrap(raw)
  Test.assertNotNil(move, "Wrap returns BoardMove")
  Test.assertEq(move:GetUci(), "e7e5", "UCI from raw")
  Test.assertEq(move:GetPiece(), "p", "piece")
end)

Test.test("Wrap: returns nil for nil", function()
  Test.assertNil(BoardMove.Wrap(nil), "Wrap(nil) is nil")
end)

Test.test("Wrap: returns same object if already BoardMove", function()
  local data = { uci = "g1f3", piece = "N" }
  local move1 = BoardMove.New(data)
  local move2 = BoardMove.Wrap(move1)
  Test.assertEq(move1, move2, "same instance")
end)

--------------------------------------------------------------------------------
-- Core getters: UCI, piece, piece type, piece color
--------------------------------------------------------------------------------

Test.test("GetUci: returns UCI string", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P" })
  Test.assertEq(move:GetUci(), "e2e4", "GetUci")
end)

Test.test("GetPiece: returns piece character", function()
  Test.assertEq(BoardMove.New({ uci = "e2e4", piece = "P" }):GetPiece(), "P", "white pawn")
  Test.assertEq(BoardMove.New({ uci = "e7e5", piece = "p" }):GetPiece(), "p", "black pawn")
  Test.assertNil(BoardMove.New({ uci = "????" }):GetPiece(), "no piece")
end)

Test.test("GetPieceType: returns lowercase type", function()
  Test.assertEq(BoardMove.New({ piece = "Q" }):GetPieceType(), "q", "queen")
  Test.assertEq(BoardMove.New({ piece = "n" }):GetPieceType(), "n", "knight")
  Test.assertEq(BoardMove.New({ piece = nil }):GetPieceType(), "p", "nil defaults to p")
end)

Test.test("GetPieceColor: white for uppercase, black for lowercase", function()
  Test.assertEq(BoardMove.New({ piece = "K" }):GetPieceColor(), Constants.COLOR.WHITE, "K white")
  Test.assertEq(BoardMove.New({ piece = "k" }):GetPieceColor(), Constants.COLOR.BLACK, "k black")
end)

--------------------------------------------------------------------------------
-- Capture getters
--------------------------------------------------------------------------------

Test.test("GetCaptured, GetCapturedType, GetCapturedColor, IsCapture", function()
  local capture = BoardMove.New({ uci = "e4d5", piece = "P", captured = "p" })
  Test.assertEq(capture:GetCaptured(), "p", "GetCaptured")
  Test.assertEq(capture:GetCapturedType(), "p", "GetCapturedType")
  Test.assertEq(capture:GetCapturedColor(), Constants.COLOR.BLACK, "GetCapturedColor")
  Test.assertTrue(capture:IsCapture(), "IsCapture true")

  local nonCapture = BoardMove.New({ uci = "e2e4", piece = "P" })
  Test.assertNil(nonCapture:GetCaptured(), "no captured")
  Test.assertNil(nonCapture:GetCapturedType(), "GetCapturedType nil")
  Test.assertNil(nonCapture:GetCapturedColor(), "GetCapturedColor nil")
  Test.assertFalse(nonCapture:IsCapture(), "IsCapture false")
end)

--------------------------------------------------------------------------------
-- Promotion getters
--------------------------------------------------------------------------------

Test.test("GetPromotion, GetPromotionType, IsPromotion", function()
  local prom = BoardMove.New({ uci = "e7e8q", piece = "p", promotion = "q" })
  Test.assertEq(prom:GetPromotion(), "q", "GetPromotion")
  Test.assertEq(prom:GetPromotionType(), "q", "GetPromotionType")
  Test.assertTrue(prom:IsPromotion(), "IsPromotion true")

  local nonProm = BoardMove.New({ uci = "e2e4", piece = "P" })
  Test.assertNil(nonProm:GetPromotion(), "no promotion")
  Test.assertFalse(nonProm:IsPromotion(), "IsPromotion false")
end)

--------------------------------------------------------------------------------
-- Castle getters
--------------------------------------------------------------------------------

Test.test("GetCastle, IsCastle, IsKingsideCastle, IsQueensideCastle", function()
  local kingside = BoardMove.New({ uci = "e1g1", piece = "K", castle = "K" })
  Test.assertEq(kingside:GetCastle(), "K", "GetCastle")
  Test.assertTrue(kingside:IsCastle(), "IsCastle")
  Test.assertTrue(kingside:IsKingsideCastle(), "IsKingsideCastle")
  Test.assertFalse(kingside:IsQueensideCastle(), "IsQueensideCastle false")

  local queenside = BoardMove.New({ uci = "e8c8", piece = "k", castle = "q" })
  Test.assertEq(queenside:GetCastle(), "q", "GetCastle q")
  Test.assertTrue(queenside:IsQueensideCastle(), "IsQueensideCastle")
  Test.assertFalse(queenside:IsKingsideCastle(), "IsKingsideCastle false")

  local nonCastle = BoardMove.New({ uci = "e2e4", piece = "P" })
  Test.assertNil(nonCastle:GetCastle(), "no castle")
  Test.assertFalse(nonCastle:IsCastle(), "IsCastle false")
end)

--------------------------------------------------------------------------------
-- Check and en passant
--------------------------------------------------------------------------------

Test.test("IsCheck: returns check flag", function()
  local withCheck = BoardMove.New({ uci = "d1h5", piece = "Q", check = true })
  Test.assertTrue(withCheck:IsCheck(), "IsCheck true")
  local noCheck = BoardMove.New({ uci = "e2e4", piece = "P", check = false })
  Test.assertFalse(noCheck:IsCheck(), "IsCheck false")
end)

Test.test("IsEnPassant: returns ep flag", function()
  local ep = BoardMove.New({ uci = "d5e6", piece = "P", ep = true, captured = "p" })
  Test.assertTrue(ep:IsEnPassant(), "IsEnPassant true")
  Test.assertFalse(BoardMove.New({ uci = "e2e4", piece = "P" }):IsEnPassant(), "IsEnPassant false")
end)

--------------------------------------------------------------------------------
-- Coordinate getters (from UCI)
--------------------------------------------------------------------------------

Test.test("GetFromSquare, GetToSquare: parse UCI", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P" })
  Test.assertEq(move:GetFromSquare(), "e2", "GetFromSquare")
  Test.assertEq(move:GetToSquare(), "e4", "GetToSquare")

  local short = BoardMove.New({ uci = "ab", piece = "P" })
  Test.assertEq(short:GetFromSquare(), "", "short UCI from empty")
  Test.assertEq(short:GetToSquare(), "", "short UCI to empty")
end)

--------------------------------------------------------------------------------
-- Metadata getters and setters
--------------------------------------------------------------------------------

Test.test("GetMeta, SetMeta, GetMetaTable", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P", meta = { x = 1 } })
  Test.assertEq(move:GetMeta("x"), 1, "GetMeta")
  move:SetMeta("y", "hello")
  Test.assertEq(move:GetMeta("y"), "hello", "SetMeta then GetMeta")
  local meta = move:GetMetaTable()
  Test.assertEq(meta.x, 1, "GetMetaTable has x")
  Test.assertEq(meta.y, "hello", "GetMetaTable has y")
end)

Test.test("GetTimestamp: returns timestamp", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P", timestamp = 12345 })
  Test.assertEq(move:GetTimestamp(), 12345, "GetTimestamp")
  Test.assertNil(BoardMove.New({ uci = "e2e4" }):GetTimestamp(), "no timestamp")
end)

Test.test("GetThinkTime: from meta", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P", meta = { thinkTime = 2.5 } })
  Test.assertEq(move:GetThinkTime(), 2.5, "GetThinkTime")
  Test.assertNil(BoardMove.New({ uci = "e2e4" }):GetThinkTime(), "no thinkTime")
end)

Test.test("GetColor: from meta or index", function()
  local moveWithMeta = BoardMove.New({ uci = "e7e5", piece = "p", meta = { color = Constants.COLOR.BLACK } })
  Test.assertEq(moveWithMeta:GetColor(), Constants.COLOR.BLACK, "from meta")

  local moveOdd = BoardMove.New({ uci = "e2e4", piece = "P" }, nil, 1)
  Test.assertEq(moveOdd:GetColor(), Constants.COLOR.WHITE, "index 1 = white")

  local moveEven = BoardMove.New({ uci = "e7e5", piece = "p" }, nil, 2)
  Test.assertEq(moveEven:GetColor(), Constants.COLOR.BLACK, "index 2 = black")
end)

--------------------------------------------------------------------------------
-- Board reference and index
--------------------------------------------------------------------------------

Test.test("GetBoard, GetIndex: when wrapped with board", function()
  local board = Board.New()
  board:MakeMoveUci("e2e4")
  local history = board:GetMoveHistory()
  local move = history[1]
  Test.assertEq(move:GetBoard(), board, "GetBoard")
  Test.assertEq(move:GetIndex(), 1, "GetIndex")
end)

Test.test("GetFullMoveNumber: from index", function()
  Test.assertEq(BoardMove.New({ uci = "e2e4" }, nil, 1):GetFullMoveNumber(), 1, "index 1 -> full move 1")
  Test.assertEq(BoardMove.New({ uci = "e7e5" }, nil, 2):GetFullMoveNumber(), 1, "index 2 -> full move 1")
  Test.assertEq(BoardMove.New({ uci = "g1f3" }, nil, 3):GetFullMoveNumber(), 2, "index 3 -> full move 2")
  Test.assertEq(BoardMove.New({ uci = "????" }, nil, nil):GetFullMoveNumber(), 1, "no index -> 1")
end)

--------------------------------------------------------------------------------
-- Serialization and string
--------------------------------------------------------------------------------

Test.test("ToTable: plain table for serialization", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P", check = true, meta = { a = 1 } })
  local t = move:ToTable()
  Test.assertEq(t.uci, "e2e4", "uci")
  Test.assertEq(t.piece, "P", "piece")
  Test.assertEq(t.check, true, "check")
  Test.assertEq(t.meta.a, 1, "meta")
end)

Test.test("ToString: debug string", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P", check = true })
  local str = move:ToString()
  Test.assertTrue(str:find("e2e4") ~= nil, "contains UCI")
  Test.assertTrue(str:find("%+") ~= nil, "contains + for check")
end)

Test.test("ToString: with capture and promotion", function()
  local move = BoardMove.New({ uci = "e7e8q", piece = "p", promotion = "q", captured = nil })
  local str = move:ToString()
  Test.assertTrue(str:find("e7e8q") ~= nil, "contains UCI")
  Test.assertTrue(str:find("=") ~= nil and str:find("q") ~= nil, "promotion in string")
end)

Test.test("__tostring: same as ToString", function()
  local move = BoardMove.New({ uci = "e2e4", piece = "P" })
  Test.assertEq(tostring(move), move:ToString(), "tostring equals ToString")
end)

--------------------------------------------------------------------------------
-- GetSan: UCI to SAN / PGN (full game)
--------------------------------------------------------------------------------

Test.test("GetSan: full game UCI to PGN matches expected", function()
  local uciGame = "e2e4 d7d5 b1c3 d5d4 c3d5 c7c6 d5f4 e7e5 f4h5 c8e6 g1f3 g7g6 h5g3 b8d7 b2b3 h7h5 f1c4 h5h4 g3e2 e6c4 b3c4 g8f6 d2d3 b7b5 c2c3 b5c4 c3d4 c4d3 d1d3 d8a5 e2c3 f6e4 c1d2 e4c3 d2c3 f8b4 c3b4 a5b4 e1f1 b4d6 a1b1 d6d5 f1g1 e8g8 h2h3 e5e4 d3b3 a8b8 b3c2 e4f3 b1b8 f8b8 g2f3 d5f3 a2a3 b8b3 a3a4 b3d3 g1h2 f3h3 h2g1 h3g4 g1f1 d3d1 c2d1 g4d1 f1g2 d1g4 g2h2 g4f3 h1f1 h4h3 f1g1 f3f2 h2h1 d7f6 g1g5 f2f3 h1h2 f6g4 g5g4 f3g4 a4a5 g4g2"
  local expectedPgn = "1. e4 d5 2. Nc3 d4 3. Nd5 c6 4. Nf4 e5 5. Nh5 Be6 6. Nf3 g6 7. Ng3 Nd7 8. b3 h5\n" ..
    "9. Bc4 h4 10. Ne2 Bxc4 11. bxc4 Ngf6 12. d3 b5 13. c3 bxc4 14. cxd4 cxd3 15. Qxd3 Qa5+ 16. Nc3 Nxe4 17. Bd2 Nxc3 18. Bxc3 Bb4 19. Bxb4 Qxb4+ 20. Kf1 Qd6 21. Rb1 Qd5 22. Kg1 O-O 23. h3 e4 24. Qb3 Rab8 25. Qc2 exf3 26. Rxb8 Rxb8 27. gxf3 Qxf3 28. a3 Rb3 29. a4 Rd3 30. Kh2 Qxh3+ 31. Kg1 Qg4+ 32. Kf1 Rd1+ 33. Qxd1 Qxd1+ 34. Kg2 Qg4+ 35. Kh2 Qf3 36. Rf1 h3 37. Rg1 Qxf2+ 38. Kh1 Nf6 39. Rg5 Qf3+ 40. Kh2 Ng4+ 41. Rxg4 Qxg4 42. a5 Qg2"

  local board = Board.New()
  for uci in (uciGame:gmatch("%S+")) do
    local ok, err = board:MakeMoveUci(uci)
    Test.assertTrue(ok, "MakeMoveUci(" .. uci .. "): " .. tostring(err))
  end

  local history = board:GetMoveHistory()
  Test.assertEq(#history, 84, "84 half-moves played")  -- 42 full moves

  -- Move-by-move SAN comparison
  local expectedSans = {}
  for token in (expectedPgn:gsub("\n", " ")):gmatch("%S+") do
    if not token:match("^%d+%.?$") then
      expectedSans[#expectedSans + 1] = token:gsub("[%+#]$", "")
    end
  end
  for i = 1, #history do
    local san = history[i]:GetSan()
    Test.assertNotNil(san, "GetSan for move " .. i .. " " .. (history[i]:GetUci() or ""))
    Test.assertEq(san, expectedSans[i], "move " .. i .. " (" .. (history[i]:GetUci() or "") .. "): expected " .. tostring(expectedSans[i]) .. ", got " .. tostring(san))
  end

  Test.assertEq(board:GetPGNMovesWithoutTime(), expectedPgn, "GetPGNMovesWithoutTime produces expected PGN")
end)
