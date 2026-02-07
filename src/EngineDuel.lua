--[[
  PlayGame: run a game between two engines. No output; optional onState callback.
  Depends on DeltaChess.Engines, DeltaChess.EngineRunner, DeltaChess.Board, DeltaChess.Constants.
  Registers: DeltaChess.EngineDuel
]]

DeltaChess = DeltaChess or {}
local Engines = DeltaChess.Engines
local EngineRunner = DeltaChess.EngineRunner
local Board = DeltaChess.Board
local Constants = DeltaChess.Constants

local M = {}

-- Re-export result constants for convenience
M.WHITE = Constants.WHITE
M.BLACK = Constants.BLACK
M.DRAWN = Constants.DRAWN
M.TIMEOUT = Constants.TIMEOUT
M.ERROR = Constants.ERROR

-- Re-export defaults
M.START_FEN = Constants.START_FEN
M.DEFAULT_MAX_HALF_MOVES = Constants.DEFAULT_MAX_HALF_MOVES

--------------------------------------------------------------------------------
-- Play a game: whiteEngine vs blackEngine with optional ELOs. Async, no output.
-- Intended for benchmarking engines against each other.
--
-- PlayGame(whiteEngineId, whiteElo, blackEngineId, blackElo, maxHalfMoves, options, onDone)
--
--   whiteEngineId, blackEngineId  (string)  Engine ids (must be registered).
--   whiteElo, blackElo            (number|nil)  Optional ELO for each side.
--   maxHalfMoves                 (number)  Game ends with TIMEOUT after this many half-moves.
--   options                      (table|nil)  Optional: { onState = function(state) end }.
--   onDone                       (function)  Called once with (result, moves, errorMsg).
--       result    EngineDuel.WHITE | EngineDuel.BLACK | EngineDuel.DRAWN | EngineDuel.TIMEOUT | EngineDuel.ERROR
--       moves     Array of UCI strings (e.g. {"e2e4","e7e5",...}).
--       errorMsg  (string|nil) Error message when result is ERROR, nil otherwise.
--
--   options.onState(state)  Optional. Called after each move and once when game ends.
--       state.fen         (string)  Current position FEN.
--       state.moves       (table)  UCI strings played so far.
--       state.halfMoveNum (number) Number of half-moves played.
--       state.lastMove    (string) UCI of the move just played, or nil.
--       state.lastMoveSan (string) SAN of the move just played (e.g. "e4", "Nf3"), or nil.
--       state.result      (string|nil) WHITE|BLACK|DRAWN|TIMEOUT|ERROR when game over, else nil.
--       state.endReason   (string|nil) Reason game ended (checkmate, stalemate, fifty_move_rule, etc.)
--       state.errorMsg    (string|nil) Error message when result is ERROR, nil otherwise.
--
-- On engine error the game ends as ERROR; on max half-moves as TIMEOUT.
--------------------------------------------------------------------------------
function M.PlayGame(whiteEngineId, whiteElo, blackEngineId, blackElo, maxHalfMoves, options, onDone)
    maxHalfMoves = maxHalfMoves or Constants.DEFAULT_MAX_HALF_MOVES
    options = options or {}
    local onState = options.onState

    local function notify(state)
        if type(onState) == "function" then
            onState(state)
        end
    end

    local result = nil
    local errorMsg = nil
    local endReason = nil
    
    -- Use Board to track game state
    local board = Board.New()

    local function getEngineAndElo(isWhite)
        if isWhite then
            return whiteEngineId, whiteElo
        else
            return blackEngineId, blackElo
        end
    end

    -- Use a trampoline pattern to avoid stack overflow on long games
    local pendingNextMove = nil
    local trampolineRunning = false
    
    local function scheduleNext(fn)
        pendingNextMove = fn
    end
    
    local function runPending()
        if trampolineRunning then return end
        trampolineRunning = true
        while pendingNextMove do
            local fn = pendingNextMove
            pendingNextMove = nil
            fn()
        end
        trampolineRunning = false
    end

    local function doHalfMove(halfMoveNum, doneCb)
        local currentFen = board:GetFen()
        local moves = board:GetMoveHistory()
        local isWhite = board:IsWhiteToMove()
        
        -- Check if board reports game has ended (checkmate, stalemate, 50-move rule)
        if board:IsEnded() then
            result = board:GetResult()
            endReason = board:GetEndReason()
            notify({ 
                fen = currentFen, 
                moves = moves, 
                halfMoveNum = #moves, 
                lastMove = (moves[#moves] and moves[#moves]:GetUci()) or nil, 
                lastMoveSan = (moves[#moves] and moves[#moves]:GetSan()) or nil,
                result = result,
                endReason = endReason
            })
            if doneCb then doneCb() end
            return
        end

        -- Check for timeout (max half-moves reached)
        if halfMoveNum > maxHalfMoves then
            result = Constants.TIMEOUT
            endReason = Constants.REASON_TIMEOUT
            notify({ 
                fen = currentFen, 
                moves = moves, 
                halfMoveNum = #moves, 
                lastMove = (moves[#moves] and moves[#moves]:GetUci()) or nil, 
                lastMoveSan = (moves[#moves] and moves[#moves]:GetSan()) or nil,
                result = result,
                endReason = endReason
            })
            if doneCb then doneCb() end
            return
        end

        local engineId, elo = getEngineAndElo(isWhite)
        local builder = EngineRunner.Create(engineId)
            :Fen(currentFen)
            :Moves(moves)
            :OnComplete(function(res, err)
                if not res and err then
                    -- Engine error
                    result = Constants.ERROR
                    errorMsg = err
                    endReason = Constants.REASON_ENGINE_ERROR
                    notify({ 
                        fen = currentFen, 
                        moves = moves, 
                        halfMoveNum = #moves, 
                        lastMove = (moves[#moves] and moves[#moves]:GetUci()) or nil, 
                        lastMoveSan = (moves[#moves] and moves[#moves]:GetSan()) or nil,
                        result = result, 
                        endReason = endReason,
                        errorMsg = errorMsg 
                    })
                    if doneCb then doneCb() end
                    return
                end
                if not res or not res.move then
                    -- No valid move returned (but no error message) - treat as draw
                    result = Constants.DRAWN
                    endReason = Constants.REASON_RESIGNATION
                    notify({ 
                        fen = currentFen, 
                        moves = moves, 
                        halfMoveNum = #moves, 
                        lastMove = (moves[#moves] and moves[#moves]:GetUci()) or nil, 
                        lastMoveSan = (moves[#moves] and moves[#moves]:GetSan()) or nil,
                        result = result,
                        endReason = endReason
                    })
                    if doneCb then doneCb() end
                    return
                end
                
                local uci = res.move
                local boardResult, boardErr = board:MakeMoveUci(uci)
                if not boardResult then
                    result = Constants.ERROR
                    errorMsg = { message = "invalid move from engine", move = uci, detail = boardErr }
                    endReason = Constants.REASON_INVALID_MOVE
                    notify({ 
                        fen = currentFen, 
                        moves = moves, 
                        halfMoveNum = #moves, 
                        lastMove = (moves[#moves] and moves[#moves]:GetUci()) or nil, 
                        lastMoveSan = (moves[#moves] and moves[#moves]:GetSan()) or nil,
                        result = result, 
                        endReason = endReason,
                        errorMsg = errorMsg 
                    })
                    if doneCb then doneCb() end
                    return
                end
                
                local newMoves = board:GetMoveHistory()
                local lastMoveObj = newMoves[#newMoves]
                notify({ 
                    fen = board:GetFen(), 
                    moves = newMoves, 
                    halfMoveNum = #newMoves, 
                    lastMove = uci, 
                    lastMoveSan = (lastMoveObj and lastMoveObj:GetSan()) or nil,
                    result = nil 
                })
                
                -- Schedule next move
                scheduleNext(function()
                    doHalfMove(halfMoveNum + 1, doneCb)
                end)
                runPending()
            end)
            :Scheduler(function(next) next() end)
        if elo then builder:Elo(elo) end
        builder:Run()
    end

    doHalfMove(1, function()
        onDone(result, board:GetMoveHistoryUci(), errorMsg)
    end)
end

DeltaChess.EngineDuel = M
return M
