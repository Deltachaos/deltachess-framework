--[[
  Utility helpers. FEN to Unicode board display, bitop compatibility.
  Registers globally: DeltaChess.Util
]]

DeltaChess = DeltaChess or {}
local M = {}

--------------------------------------------------------------------------------
-- Bit operations compatibility layer
-- Works with Lua 5.1 bit, Lua 5.2+ bit32, LuaJIT bit, or pure-Lua fallback
-- Registered under DeltaChess.Util.bit for use by engines
--------------------------------------------------------------------------------
M.bit = DeltaChess.LibBit -- TODO replace all usages with DeltaChess.LibBit

-- Unicode chess symbols (white: U+2654–2659, black: U+265A–265F)
local PIECE_TO_UNICODE = {
    K = "\226\153\148",   -- ♔
    Q = "\226\153\149",   -- ♕
    R = "\226\153\150",   -- ♖
    B = "\226\153\151",   -- ♗
    N = "\226\153\152",   -- ♘
    P = "\226\153\153",   -- ♙
    k = "\226\153\154",   -- ♚
    q = "\226\153\155",   -- ♛
    r = "\226\153\156",   -- ♜
    b = "\226\153\157",   -- ♝
    n = "\226\153\158",   -- ♞
    p = "\226\153\159",   -- ♟
}
local EMPTY_SQ = "\194\183"  -- · (U+00B7)

--------------------------------------------------------------------------------
-- Convert FEN piece placement to a board string using Unicode piece symbols.
-- fen       (string)  Full FEN or just the placement part (first token used).
-- options   (table|nil)  Optional: { labels = true } to add rank/file labels.
-- Returns   (string)  Multi-line string, rank 8 at top, rank 1 at bottom.
--------------------------------------------------------------------------------
function M.FenToBoard(fen, options)
    options = options or {}
    local useLabels = options.labels ~= false  -- default true for readability
    fen = (fen or ""):match("%S+") or ""
    if fen == "" then return "" end

    local ranks = {}
    for part in fen:gmatch("[^/]+") do
        ranks[#ranks + 1] = part
    end
    -- FEN gives rank 8 first, then 7..1
    local lines = {}
    local fileLabels = "  a b c d e f g h"
    for r = 1, #ranks do
        local rankStr = {}
        local rankNum = 9 - r  -- 8,7,...,1
        for i = 1, #ranks[r] do
            local c = ranks[r]:sub(i, i)
            if c:match("%d") then
                for _ = 1, tonumber(c) do rankStr[#rankStr + 1] = EMPTY_SQ end
            else
                rankStr[#rankStr + 1] = PIECE_TO_UNICODE[c] or c
            end
        end
        local line = table.concat(rankStr, " ")
        if useLabels then
            line = rankNum .. " " .. line
        end
        lines[#lines + 1] = line
    end
    if useLabels then
        table.insert(lines, fileLabels)
    end
    return table.concat(lines, "\n")
end

--------------------------------------------------------------------------------
-- ELO scaling helpers for engines (weaker ELO = lower depth/nodes, stronger = higher).
-- eloRange: { min, max } from engine:GetEloRange(). elo: number or nil (use midpoint).
--------------------------------------------------------------------------------

--- Normalize ELO to [0,1] within range; if elo is nil, use midpoint of eloRange.
local function eloToT(elo, eloRange)
    local eloMin = (eloRange and eloRange[1]) or 0
    local eloMax = (eloRange and eloRange[2]) or 2000
    local e = elo
    if e == nil then
        e = math.floor((eloMin + eloMax) / 2)
    end
    e = math.max(eloMin, math.min(eloMax, e))
    return (e - eloMin) / math.max(1, eloMax - eloMin)
end

--- Map ELO to search depth (ply). Lower ELO = depthMin, higher ELO = depthMax.
-- eloRange: { min, max }. depthMin, depthMax: integers.
-- Returns integer depth in [depthMin, depthMax].
function M.EloToPly(elo, eloRange, depthMin, depthMax)
    local t = eloToT(elo, eloRange)
    return math.max(depthMin, math.min(depthMax, math.floor(depthMin + t * (depthMax - depthMin) + 0.5)))
end

--- Map ELO to node count. Lower ELO = nodesMin, higher ELO = nodesMax.
-- eloRange: { min, max }. nodesMin, nodesMax: integers.
-- Returns integer node limit in [nodesMin, nodesMax].
function M.EloToNodes(elo, eloRange, nodesMin, nodesMax)
    local t = eloToT(elo, eloRange)
    return math.floor(nodesMin + t * (nodesMax - nodesMin))
end

--------------------------------------------------------------------------------
-- Clock: CPU/wall time in seconds. os.clock() in Lua; WoW polyfill uses GetTime().
-- Returns seconds (suitable for elapsed-time measurements and benchmarks).
--------------------------------------------------------------------------------
do
  local os_clock = _G.os and _G.os.clock
  local wow_get_time = _G.GetTime  -- seconds since UI load (WoW API)

  function M.Clock()
    if type(os_clock) == "function" then
      local ok, v = pcall(os_clock)
      if ok and v then return v end
    end
    if type(wow_get_time) == "function" then
      local ok, v = pcall(wow_get_time)
      if ok and v then return v end  -- GetTime() already returns seconds in WoW
    end
    return 0
  end
end

--------------------------------------------------------------------------------
-- Time: wrappers for os.time / os.date (Lua) or time() / date() (WoW global).
-- TimeNow() -> Unix seconds. Date(ts, format) -> string. DateTable(ts) -> table.
-- TimeFromTable(t) -> Unix seconds. In WoW, date() is the global mirror of os.date().
--------------------------------------------------------------------------------
do
  local os_time = _G.os and _G.os.time
  local os_date = _G.os and _G.os.date
  local wow_time = _G.time
  local wow_date = _G.date

  function M.TimeNow()
    if os_time then local ok, v = pcall(os_time); if ok and v then return v end end
    if type(wow_time) == "function" then local ok, v = pcall(wow_time); if ok and v then return v end end
    return 0
  end

  function M.DateTable(ts)
    if os_date and ts then local ok, t = pcall(os_date, "*t", ts); if ok and t then return t end end
    if wow_date and ts then local ok, t = pcall(wow_date, "*t", ts); if ok and t then return t end end
    return {}
  end

  function M.Date(ts, format)
    if os_date and ts and format then local ok, out = pcall(os_date, format, ts); if ok and out then return out end end
    if wow_date and ts and format then local ok, out = pcall(wow_date, format, ts); if ok and out then return out end end
    return tostring(ts or "")
  end

  -- Unix timestamp from date table. WoW has no os.time; keep polyfill for table -> seconds.
  local function timeFromTablePolyfill(t)
    if not t then return 0 end
    local y, mo, d = t.year or 1970, t.month or 1, t.day or 1
    local h, min, s = t.hour or 0, t.min or 0, t.sec or 0
    local dayno = 0
    for year = 1970, y - 1 do
      dayno = dayno + (365 + ((year % 4 == 0 and (year % 100 ~= 0 or year % 400 == 0)) and 1 or 0))
    end
    local month_days = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
    if y % 4 == 0 and (y % 100 ~= 0 or y % 400 == 0) then month_days[2] = 29 end
    for i = 1, mo - 1 do dayno = dayno + month_days[i] end
    dayno = dayno + (d - 1)
    return dayno * 86400 + h * 3600 + min * 60 + s
  end

  function M.TimeFromTable(t)
    if os_time and t then
      local ok, out = pcall(os_time, t)
      if ok and out then return out end
    end
    return timeFromTablePolyfill(t)
  end
end

--------------------------------------------------------------------------------
-- JSON: use WoW C_EncodingUtil when available, else lib/json.lua polyfill.
-- SerializeJSON(value [, options]) -> output string or nil, err
-- DeserializeJSON(source) -> value or nil, err
--------------------------------------------------------------------------------
function M.SerializeJSON(value, options)
    if _G.C_EncodingUtil and type(_G.C_EncodingUtil.SerializeJSON) == "function" then
        local ok, out = pcall(_G.C_EncodingUtil.SerializeJSON, value, options)
        if not ok then return nil, out end
        return out
    else
        local ok, out = pcall(DeltaChess.LibJSON.encode, value)
        if not ok then return nil, out end
        return out
    end
end

function M.DeserializeJSON(source)   
    if type(source) ~= "string" then return nil, "source must be string" end
    if _G.C_EncodingUtil and type(_G.C_EncodingUtil.DeserializeJSON) == "function" then
        local ok, out = pcall(_G.C_EncodingUtil.DeserializeJSON, source)
        if not ok then return nil, out end
        return out
    else
        local ok, out = pcall(DeltaChess.LibJSON.decode, source)
        if not ok then return nil, out end
        return out
    end
end


--------------------------------------------------------------------------------
-- Dump a value to a string. Tables are serialized recursively.
-- val       (any)      Value to dump.
-- indent    (string|nil) Internal: current indentation.
-- Returns   (string)   Human-readable representation.
--------------------------------------------------------------------------------
function M.Dump(val, indent)
    indent = indent or ""
    local t = type(val)
    if t == "string" then
        return string.format("%q", val)
    elseif t == "number" or t == "boolean" or t == "nil" then
        return tostring(val)
    elseif t == "table" then
        local parts = {}
        local nextIndent = indent .. "  "
        for k, v in pairs(val) do
            local keyStr = type(k) == "string" and k or ("[" .. tostring(k) .. "]")
            parts[#parts + 1] = nextIndent .. keyStr .. " = " .. M.Dump(v, nextIndent)
        end
        if #parts == 0 then
            return "{}"
        end
        return "{\n" .. table.concat(parts, ",\n") .. "\n" .. indent .. "}"
    else
        return tostring(val)
    end
end

DeltaChess.Util = M
return M
