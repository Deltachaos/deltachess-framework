--[[
  Util unit tests.
  Tests utility functions including bit operations polyfill, FEN display, and value dumping.
]]

local Test = require("test")

local Util = DeltaChess.Util

Test.suite("Util")

--------------------------------------------------------------------------------
-- bitPolyfill.band Tests
--------------------------------------------------------------------------------

Test.test("bitPolyfill.band: basic AND operations", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.band(0xFF, 0x0F), 0x0F, "0xFF AND 0x0F should be 0x0F")
  Test.assertEq(b.band(0xFF, 0xF0), 0xF0, "0xFF AND 0xF0 should be 0xF0")
  Test.assertEq(b.band(0xAA, 0x55), 0x00, "0xAA AND 0x55 should be 0x00")
  Test.assertEq(b.band(0xAA, 0xAA), 0xAA, "0xAA AND 0xAA should be 0xAA")
end)

Test.test("bitPolyfill.band: with zero", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.band(0xFF, 0), 0, "anything AND 0 should be 0")
  Test.assertEq(b.band(0, 0xFF), 0, "0 AND anything should be 0")
  Test.assertEq(b.band(0, 0), 0, "0 AND 0 should be 0")
end)

Test.test("bitPolyfill.band: with all ones", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.band(0xFF, 0xFFFFFFFF), 0xFF, "0xFF AND all ones should be 0xFF")
  Test.assertEq(b.band(0x12345678, 0xFFFFFFFF), 0x12345678, "value AND all ones should be value")
end)

Test.test("bitPolyfill.band: large values", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.band(0x12345678, 0xFF000000), 0x12000000, "mask high byte")
  Test.assertEq(b.band(0x12345678, 0x0000FF00), 0x00005600, "mask third byte")
end)

--------------------------------------------------------------------------------
-- bitPolyfill.bor Tests
--------------------------------------------------------------------------------

Test.test("bitPolyfill.bor: basic OR operations", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bor(0xF0, 0x0F), 0xFF, "0xF0 OR 0x0F should be 0xFF")
  Test.assertEq(b.bor(0xAA, 0x55), 0xFF, "0xAA OR 0x55 should be 0xFF")
  Test.assertEq(b.bor(0xAA, 0xAA), 0xAA, "0xAA OR 0xAA should be 0xAA")
end)

Test.test("bitPolyfill.bor: with zero", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bor(0xFF, 0), 0xFF, "value OR 0 should be value")
  Test.assertEq(b.bor(0, 0xFF), 0xFF, "0 OR value should be value")
  Test.assertEq(b.bor(0, 0), 0, "0 OR 0 should be 0")
end)

Test.test("bitPolyfill.bor: combining different bits", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bor(0x01, 0x02), 0x03, "0x01 OR 0x02 should be 0x03")
  Test.assertEq(b.bor(0x10, 0x01), 0x11, "0x10 OR 0x01 should be 0x11")
  Test.assertEq(b.bor(0x1000, 0x0100), 0x1100, "combine separate bytes")
end)

Test.test("bitPolyfill.bor: large values", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bor(0x12000000, 0x00345678), 0x12345678, "combine high and low parts")
end)

--------------------------------------------------------------------------------
-- bitPolyfill.bxor Tests
--------------------------------------------------------------------------------

Test.test("bitPolyfill.bxor: basic XOR operations", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bxor(0xFF, 0x00), 0xFF, "0xFF XOR 0x00 should be 0xFF")
  Test.assertEq(b.bxor(0xFF, 0xFF), 0x00, "0xFF XOR 0xFF should be 0x00")
  Test.assertEq(b.bxor(0xAA, 0x55), 0xFF, "0xAA XOR 0x55 should be 0xFF")
  Test.assertEq(b.bxor(0xAA, 0xAA), 0x00, "0xAA XOR 0xAA should be 0x00")
end)

Test.test("bitPolyfill.bxor: with zero", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bxor(0xFF, 0), 0xFF, "value XOR 0 should be value")
  Test.assertEq(b.bxor(0, 0xFF), 0xFF, "0 XOR value should be value")
  Test.assertEq(b.bxor(0, 0), 0, "0 XOR 0 should be 0")
end)

Test.test("bitPolyfill.bxor: toggle bits", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bxor(0x0F, 0xFF), 0xF0, "toggle lower nibble")
  Test.assertEq(b.bxor(0xF0, 0xFF), 0x0F, "toggle upper nibble")
end)

Test.test("bitPolyfill.bxor: self-inverse property", function()
  local b = DeltaChess.LibBitPolyfill
  local original = 0x12345678
  local mask = 0xDEADBEEF
  local xored = b.bxor(original, mask)
  local restored = b.bxor(xored, mask)
  Test.assertEq(restored, original, "XOR should be self-inverse")
end)

--------------------------------------------------------------------------------
-- bitPolyfill.bnot Tests
--------------------------------------------------------------------------------

Test.test("bitPolyfill.bnot: basic NOT operations", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bnot(0), 0xFFFFFFFF, "NOT 0 should be all ones")
  Test.assertEq(b.bnot(0xFFFFFFFF), 0, "NOT all ones should be 0")
end)

Test.test("bitPolyfill.bnot: partial values", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.bnot(0x0000FFFF), 0xFFFF0000, "NOT lower half")
  Test.assertEq(b.bnot(0xFFFF0000), 0x0000FFFF, "NOT upper half")
end)

Test.test("bitPolyfill.bnot: double NOT is identity", function()
  local b = DeltaChess.LibBitPolyfill
  local original = 0x12345678
  Test.assertEq(b.bnot(b.bnot(original)), original, "double NOT should return original")
end)

--------------------------------------------------------------------------------
-- bitPolyfill.lshift Tests
--------------------------------------------------------------------------------

Test.test("bitPolyfill.lshift: basic left shifts", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.lshift(1, 0), 1, "shift by 0")
  Test.assertEq(b.lshift(1, 1), 2, "shift 1 left by 1")
  Test.assertEq(b.lshift(1, 4), 16, "shift 1 left by 4")
  Test.assertEq(b.lshift(1, 8), 256, "shift 1 left by 8")
end)

Test.test("bitPolyfill.lshift: multi-bit values", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.lshift(0x0F, 4), 0xF0, "shift 0x0F left by 4")
  Test.assertEq(b.lshift(0xFF, 8), 0xFF00, "shift 0xFF left by 8")
  Test.assertEq(b.lshift(0xFF, 16), 0xFF0000, "shift 0xFF left by 16")
end)

Test.test("bitPolyfill.lshift: overflow wraps at 32 bits", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.lshift(0x80000000, 1), 0, "shift out of 32-bit range")
  Test.assertEq(b.lshift(0xFFFFFFFF, 1), 0xFFFFFFFE, "shift all ones left")
end)

Test.test("bitPolyfill.lshift: shift by zero is identity", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.lshift(0x12345678, 0), 0x12345678, "shift by 0 is identity")
end)

--------------------------------------------------------------------------------
-- bitPolyfill.rshift Tests
--------------------------------------------------------------------------------

Test.test("bitPolyfill.rshift: basic right shifts", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.rshift(1, 0), 1, "shift by 0")
  Test.assertEq(b.rshift(2, 1), 1, "shift 2 right by 1")
  Test.assertEq(b.rshift(16, 4), 1, "shift 16 right by 4")
  Test.assertEq(b.rshift(256, 8), 1, "shift 256 right by 8")
end)

Test.test("bitPolyfill.rshift: multi-bit values", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.rshift(0xF0, 4), 0x0F, "shift 0xF0 right by 4")
  Test.assertEq(b.rshift(0xFF00, 8), 0xFF, "shift 0xFF00 right by 8")
  Test.assertEq(b.rshift(0xFF0000, 16), 0xFF, "shift 0xFF0000 right by 16")
end)

Test.test("bitPolyfill.rshift: shifts in zeros", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.rshift(0x80000000, 31), 1, "shift highest bit to lowest")
  Test.assertEq(b.rshift(0xFFFFFFFF, 16), 0xFFFF, "shift all ones right by 16")
end)

Test.test("bitPolyfill.rshift: shift by zero is identity", function()
  local b = DeltaChess.LibBitPolyfill
  Test.assertEq(b.rshift(0x12345678, 0), 0x12345678, "shift by 0 is identity")
end)

--------------------------------------------------------------------------------
-- bitPolyfill: Combined Operations Tests
--------------------------------------------------------------------------------

Test.test("bitPolyfill: mask and shift pattern", function()
  local b = DeltaChess.LibBitPolyfill
  -- Extract byte 2 (bits 8-15) from 0x12345678
  local value = 0x12345678
  local byte2 = b.band(b.rshift(value, 8), 0xFF)
  Test.assertEq(byte2, 0x56, "extract second byte")
end)

Test.test("bitPolyfill: set bit pattern", function()
  local b = DeltaChess.LibBitPolyfill
  -- Set bit 5 in value 0
  local value = 0
  local withBit5 = b.bor(value, b.lshift(1, 5))
  Test.assertEq(withBit5, 32, "set bit 5")
end)

Test.test("bitPolyfill: clear bit pattern", function()
  local b = DeltaChess.LibBitPolyfill
  -- Clear bit 3 in value 0xFF
  local value = 0xFF
  local cleared = b.band(value, b.bnot(b.lshift(1, 3)))
  Test.assertEq(cleared, 0xF7, "clear bit 3")
end)

Test.test("bitPolyfill: toggle bit pattern", function()
  local b = DeltaChess.LibBitPolyfill
  -- Toggle bit 4
  local value = 0x10
  local toggled = b.bxor(value, b.lshift(1, 4))
  Test.assertEq(toggled, 0, "toggle bit 4 off")
  local toggledBack = b.bxor(toggled, b.lshift(1, 4))
  Test.assertEq(toggledBack, 0x10, "toggle bit 4 back on")
end)

--------------------------------------------------------------------------------
-- Util.bit Tests (uses native or polyfill)
--------------------------------------------------------------------------------

Test.test("Util.bit: exists and has required functions", function()
  Test.assertNotNil(Util.bit, "Util.bit should exist")
  Test.assertNotNil(Util.bit.band, "band should exist")
  Test.assertNotNil(Util.bit.bor, "bor should exist")
  Test.assertNotNil(Util.bit.bxor, "bxor should exist")
  Test.assertNotNil(Util.bit.bnot, "bnot should exist")
  Test.assertNotNil(Util.bit.lshift, "lshift should exist")
  Test.assertNotNil(Util.bit.rshift, "rshift should exist")
end)

Test.test("Util.bit: basic operations work", function()
  local b = Util.bit
  Test.assertEq(b.band(0xFF, 0x0F), 0x0F, "band works")
  Test.assertEq(b.bor(0xF0, 0x0F), 0xFF, "bor works")
  Test.assertEq(b.bxor(0xFF, 0xFF), 0x00, "bxor works")
  Test.assertEq(b.lshift(1, 4), 16, "lshift works")
  Test.assertEq(b.rshift(16, 4), 1, "rshift works")
end)

--------------------------------------------------------------------------------
-- FenToBoard Tests
--------------------------------------------------------------------------------

Test.test("FenToBoard: starting position has correct structure", function()
  local board = Util.FenToBoard("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  Test.assertNotNil(board, "Should return a board string")
  
  -- Check that it has 8 ranks plus file labels = 9 lines
  local lines = {}
  for line in board:gmatch("[^\n]+") do
    table.insert(lines, line)
  end
  Test.assertEq(#lines, 9, "Should have 9 lines (8 ranks + file labels)")
end)

Test.test("FenToBoard: rank labels are correct", function()
  local board = Util.FenToBoard("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
  
  local lines = {}
  for line in board:gmatch("[^\n]+") do
    table.insert(lines, line)
  end
  
  -- First 8 lines should start with rank numbers 8,7,6,5,4,3,2,1
  for i = 1, 8 do
    local expectedRank = 9 - i
    Test.assertTrue(lines[i]:match("^" .. expectedRank), "Line " .. i .. " should start with rank " .. expectedRank)
  end
end)

Test.test("FenToBoard: file labels at bottom", function()
  local board = Util.FenToBoard("8/8/8/8/8/8/8/8")
  
  local lines = {}
  for line in board:gmatch("[^\n]+") do
    table.insert(lines, line)
  end
  
  Test.assertTrue(lines[#lines]:find("a b c d e f g h"), "Last line should have file labels")
end)

Test.test("FenToBoard: empty board shows dots", function()
  local board = Util.FenToBoard("8/8/8/8/8/8/8/8", { labels = false })
  -- Should contain the middle dot character
  Test.assertTrue(board:find("\194\183"), "Empty squares should show middle dot")
end)

Test.test("FenToBoard: without labels option", function()
  local board = Util.FenToBoard("8/8/8/8/8/8/8/8", { labels = false })
  
  local lines = {}
  for line in board:gmatch("[^\n]+") do
    table.insert(lines, line)
  end
  
  Test.assertEq(#lines, 8, "Without labels should have 8 lines")
  Test.assertFalse(lines[1]:match("^%d"), "Should not start with rank number")
end)

Test.test("FenToBoard: handles partial FEN (placement only)", function()
  local board = Util.FenToBoard("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
  Test.assertNotNil(board, "Should handle placement-only FEN")
  Test.assertTrue(#board > 0, "Should produce non-empty output")
end)

Test.test("FenToBoard: empty string returns empty", function()
  local board = Util.FenToBoard("")
  Test.assertEq(board, "", "Empty FEN should return empty string")
end)

Test.test("FenToBoard: nil returns empty", function()
  local board = Util.FenToBoard(nil)
  Test.assertEq(board, "", "nil FEN should return empty string")
end)

Test.test("FenToBoard: pieces are converted to unicode", function()
  local board = Util.FenToBoard("K7/8/8/8/8/8/8/k7", { labels = false })
  -- White king ♔ = \226\153\148, Black king ♚ = \226\153\154
  Test.assertTrue(board:find("\226\153\148"), "Should contain white king unicode")
  Test.assertTrue(board:find("\226\153\154"), "Should contain black king unicode")
end)

--------------------------------------------------------------------------------
-- Dump Tests
--------------------------------------------------------------------------------

Test.test("Dump: string values are quoted", function()
  local result = Util.Dump("hello")
  Test.assertEq(result, '"hello"', "String should be quoted")
end)

Test.test("Dump: string with special characters", function()
  local result = Util.Dump("hello\nworld")
  -- Lua's %q escapes newlines as backslash-newline continuation
  Test.assertTrue(result:find("hello") and result:find("world"), "String content should be preserved")
  Test.assertTrue(result:match('^"') and result:match('"$'), "Should be quoted")
end)

Test.test("Dump: number values", function()
  Test.assertEq(Util.Dump(42), "42", "Integer")
  Test.assertEq(Util.Dump(3.14), "3.14", "Float")
  Test.assertEq(Util.Dump(-17), "-17", "Negative")
  Test.assertEq(Util.Dump(0), "0", "Zero")
end)

Test.test("Dump: boolean values", function()
  Test.assertEq(Util.Dump(true), "true", "true")
  Test.assertEq(Util.Dump(false), "false", "false")
end)

Test.test("Dump: nil value", function()
  Test.assertEq(Util.Dump(nil), "nil", "nil")
end)

Test.test("Dump: empty table", function()
  Test.assertEq(Util.Dump({}), "{}", "Empty table")
end)

Test.test("Dump: simple table", function()
  local result = Util.Dump({ a = 1, b = 2 })
  Test.assertTrue(result:find("a = 1"), "Should contain a = 1")
  Test.assertTrue(result:find("b = 2"), "Should contain b = 2")
  Test.assertTrue(result:find("{"), "Should have opening brace")
  Test.assertTrue(result:find("}"), "Should have closing brace")
end)

Test.test("Dump: table with string values", function()
  local result = Util.Dump({ name = "test" })
  Test.assertTrue(result:find('name = "test"'), "Should contain name = \"test\"")
end)

Test.test("Dump: table with numeric keys", function()
  local result = Util.Dump({ [1] = "a", [2] = "b" })
  Test.assertTrue(result:find('%[1%] = "a"'), "Should contain [1] = \"a\"")
  Test.assertTrue(result:find('%[2%] = "b"'), "Should contain [2] = \"b\"")
end)

Test.test("Dump: nested table", function()
  local result = Util.Dump({ outer = { inner = 42 } })
  Test.assertTrue(result:find("outer = {"), "Should have outer table")
  Test.assertTrue(result:find("inner = 42"), "Should have inner value")
end)

Test.test("Dump: function value", function()
  local result = Util.Dump(function() end)
  Test.assertTrue(result:find("function"), "Should represent function")
end)

Test.test("Dump: mixed table", function()
  local result = Util.Dump({
    num = 123,
    str = "hello",
    bool = true,
    tbl = { nested = true }
  })
  Test.assertTrue(result:find("num = 123"), "Should have number")
  Test.assertTrue(result:find('str = "hello"'), "Should have string")
  Test.assertTrue(result:find("bool = true"), "Should have boolean")
  Test.assertTrue(result:find("nested = true"), "Should have nested table")
end)

--------------------------------------------------------------------------------
-- EloToPly / EloToNodes Tests
--------------------------------------------------------------------------------

Test.test("EloToPly: nil elo uses midpoint", function()
  local range = { 800, 1600 }
  local depth = Util.EloToPly(nil, range, 2, 6)
  Test.assertEq(depth, 4, "Midpoint ELO 1200 should give depth 4")
end)

Test.test("EloToPly: min elo gives depthMin", function()
  local range = { 800, 1600 }
  Test.assertEq(Util.EloToPly(800, range, 2, 6), 2, "Min ELO gives depth 2")
end)

Test.test("EloToPly: max elo gives depthMax", function()
  local range = { 800, 1600 }
  Test.assertEq(Util.EloToPly(1600, range, 2, 6), 6, "Max ELO gives depth 6")
end)

Test.test("EloToPly: elo clamped to range", function()
  local range = { 500, 2200 }
  Test.assertEq(Util.EloToPly(100, range, 1, 8), 1, "Below range clamped to depthMin")
  Test.assertEq(Util.EloToPly(3000, range, 1, 8), 8, "Above range clamped to depthMax")
end)

Test.test("EloToNodes: nil elo uses midpoint", function()
  local range = { 1200, 2000 }
  local nodes = Util.EloToNodes(nil, range, 2000, 20000)
  Test.assertEq(nodes, 11000, "Midpoint ELO 1600 should give 11000 nodes")
end)

Test.test("EloToNodes: min/max elo give bounds", function()
  local range = { 1200, 2000 }
  Test.assertEq(Util.EloToNodes(1200, range, 2000, 20000), 2000, "Min ELO gives 2000 nodes")
  Test.assertEq(Util.EloToNodes(2000, range, 2000, 20000), 20000, "Max ELO gives 20000 nodes")
end)
