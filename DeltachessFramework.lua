--[[
DeltachessFramework-1.0
Stateless chess engines and move generation for WoW addons.

    local DeltaChess = LibStub("DeltachessFramework-1.0")
    local board = DeltaChess.Board.New()
    DeltaChess.EngineRunner.Create("dumbgoblin")
        :Fen(board:GetFen())
        :OnComplete(function(result, err) end)
        :Run()
]]

local MAJOR, MINOR = "DeltachessFramework-1.0", 1
local Lib = LibStub:NewLibrary(MAJOR, MINOR)
if not Lib then return end
