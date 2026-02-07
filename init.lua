--[[
  DeltaChess Init - Single entry point that loads all modules and registers engines.
  Compatible with World of Warcraft addons: set DeltaChess.AddonPath to the addon
  directory (e.g. "Interface\\AddOns\\DeltaChess\\") and list only this file in the TOC;
  it will load the rest via loadfile. If AddonPath is not set, assumes modules were
  already loaded by the TOC in order (or via require).
]]

DeltaChess = DeltaChess or {}
DeltaChess.AddonPath = DeltaChess.AddonPath or ""

require("json")
require("bit")
require("fruit21")
require("Constants")
require("Cache")
require("Util")
require("MoveGen")
require("BoardMove")
require("Board")
require("EngineInterface")
require("EngineRunner")
require("EngineDuel")
require("Engines/DumbGoblin")
require("Engines/GarboChess")
require("Engines/Sunfish")
require("Engines/Fruit21")