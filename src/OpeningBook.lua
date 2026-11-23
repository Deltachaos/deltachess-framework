--[[
  Opening-book lookup for the engine runner.
  Talks to LibChessOpeningBook-1.0 when that library is loaded.
  Registers: DeltaChess.OpeningBook
]]

local DeltaChess = LibStub("DeltachessFramework-1.0")
local M = {}

local LIB_NAME = "LibChessOpeningBook-1.0"
local ELO_RANGE = { 100, 2600 }
local DEPTH_MIN = 1
local DEPTH_MAX = 16

local function getLib()
  return LibStub(LIB_NAME, true)
end

function M.IsAvailable()
  return getLib() ~= nil
end

function M.GetBookList()
  local lib = getLib()
  if not lib then
    return {}
  end
  return lib:GetBookList()
end

function M.GetBooksForElo(elo)
  local lib = getLib()
  if not lib then
    return {}
  end
  return lib:GetBooksForElo(elo)
end

--- Map engine ELO to a book ply limit (weaker = fewer book moves).
function M.DepthForElo(elo)
  local Util = DeltaChess.Util
  if Util and Util.EloToPly then
    return Util.EloToPly(elo, ELO_RANGE, DEPTH_MIN, DEPTH_MAX)
  end
  local e = tonumber(elo) or math.floor((ELO_RANGE[1] + ELO_RANGE[2]) / 2)
  e = math.max(ELO_RANGE[1], math.min(ELO_RANGE[2], e))
  local t = (e - ELO_RANGE[1]) / math.max(1, ELO_RANGE[2] - ELO_RANGE[1])
  return math.max(DEPTH_MIN, math.min(DEPTH_MAX, math.floor(DEPTH_MIN + t * (DEPTH_MAX - DEPTH_MIN) + 0.5)))
end

local function normalizeBookIds(bookIds)
  if bookIds == nil then return nil end
  if type(bookIds) == "string" then
    return { bookIds }
  end
  return bookIds
end

--- Weighted random among legal book moves for fen.
-- depth: optional ply limit; current ply (from FEN) must be < depth.
-- @return { move, san, weight, bookId } or nil
function M.SelectMove(fen, bookIds, depth)
  local lib = getLib()
  if not lib or not fen then
    return nil
  end
  local candidates = lib:Lookup(fen, bookIds, depth)
  if not candidates or #candidates == 0 then
    return nil
  end

  local MoveGen = DeltaChess.MoveGen
  local legal = {}
  local total = 0
  for i = 1, #candidates do
    local c = candidates[i]
    local weight = tonumber(c.weight) or 0
    if weight > 0 and MoveGen and MoveGen.IsLegalMove(fen, c.move) then
      legal[#legal + 1] = c
      total = total + weight
    end
  end
  if total <= 0 or #legal == 0 then
    return nil
  end

  local pick = math.random(1, total)
  local acc = 0
  local chosen = legal[1]
  for i = 1, #legal do
    acc = acc + (tonumber(legal[i].weight) or 0)
    if pick <= acc then
      chosen = legal[i]
      break
    end
  end

  local san = chosen.move
  if MoveGen and MoveGen.ParseFen and MoveGen.UciToSan then
    local pos = MoveGen.ParseFen(fen)
    if pos then
      san = MoveGen.UciToSan(pos, chosen.move) or chosen.move
    end
  end

  return {
    move = chosen.move,
    san = san,
    weight = chosen.weight,
    bookId = chosen.bookId,
  }
end

--- True if every id in bookIds is registered. Nil bookIds is always valid.
function M.UnknownBookIds(bookIds)
  local lib = getLib()
  if not lib then
    return normalizeBookIds(bookIds) or {}
  end
  local ids = normalizeBookIds(bookIds)
  if not ids then
    return {}
  end
  local missing = {}
  for i = 1, #ids do
    if not lib:GetBook(ids[i]) then
      missing[#missing + 1] = ids[i]
    end
  end
  return missing
end

DeltaChess.OpeningBook = M
return M
