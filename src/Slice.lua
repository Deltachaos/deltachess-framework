--[[
  Time-sliced search helper.
  Search work runs inside a coroutine. Poll() yields when the current
  resume's millisecond budget is spent, and sets a stop flag on cancel
  or when accumulated compute time exceeds the search time limit.
  Registers: DeltaChess.Slice
]]

local DeltaChess = LibStub("DeltachessFramework-1.0")
local M = {}

-- Default: check the clock every N Poll() calls. Small Compute budgets (WoW
-- per-frame slices) check every call so a single expensive node can still yield.
local POLL_INTERVAL = 32
local SMALL_SLICE_MS = 16
local sessions = {}
local pendingBudget = math.huge

-- WoW's GetTime() is frozen for the whole script burst; debugprofilestop
-- keeps ticking, which is what lets Compute(ms) actually yield mid-search.
local function clockMs()
    local stop = _G.debugprofilestop
    if type(stop) == "function" then
        local ok, v = pcall(stop)
        if ok and type(v) == "number" then
            return v
        end
    end
    local Clock = DeltaChess.Util and DeltaChess.Util.Clock
    if type(Clock) == "function" then
        local ok, v = pcall(Clock)
        if ok and type(v) == "number" then
            return v * 1000
        end
    end
    return 0
end

local function currentSession()
    local co = coroutine.running()
    if not co then
        return nil
    end
    return sessions[co]
end

--- Budget in milliseconds for the next step()/Compute() resume.
-- nil or a negative value means unlimited (run until stop or completion).
function M.SetBudget(ms)
    if ms == nil or ms < 0 then
        pendingBudget = math.huge
    else
        pendingBudget = ms
    end
end

--- True while running inside a Slice.Run coroutine.
function M.Active()
    return currentSession() ~= nil
end

--- True once cancel or (time limit + AllowStop) has requested an unwind.
function M.Stopped()
    local sess = currentSession()
    return sess ~= nil and sess.stop == true
end

--- Permit a time-limit stop. Call after a legal move is stored.
-- Cancel always stops, even when this is false.
function M.AllowStop(on)
    local sess = currentSession()
    if sess then
        sess.canStop = not not on
    end
end

--- Heartbeat. Call from inner search loops (or pass true from an already-throttled checker).
-- Yields when the current slice budget is spent. Sets stop on cancel or time limit.
function M.Poll(force)
    local sess = currentSession()
    if not sess then
        return
    end

    if sess.state and sess.state.cancelled then
        sess.stop = true
        return
    end

    sess.nodes = (sess.nodes or 0) + 1
    local interval = POLL_INTERVAL
    if sess.sliceBudget ~= math.huge and sess.sliceBudget <= SMALL_SLICE_MS then
        interval = 1
    end
    if not force and sess.nodes % interval ~= 0 then
        return
    end

    local unlimitedSlice = sess.sliceBudget == math.huge
    local noTimeLimit = not sess.timeLimitMs
    if unlimitedSlice and noTimeLimit then
        return
    end

    local now = clockMs()
    local spent = now - sess.sliceStart

    if sess.timeLimitMs and sess.canStop and (sess.cpuUsed + spent) >= sess.timeLimitMs then
        sess.stop = true
        return
    end

    if not unlimitedSlice and spent >= sess.sliceBudget then
        coroutine.yield()
        if sess.state and sess.state.cancelled then
            sess.stop = true
        end
    end
end

--- Drive `work` through `loopFn(step, done)`.
-- work() -> result, err and may yield via Poll().
-- done() calls onComplete(result, err) once after the last step returns false.
function M.Run(loopFn, work, onComplete, state)
    loopFn = loopFn or function(step, done)
        while step() ~= false do end
        done()
    end
    onComplete = onComplete or function() end

    local sess = {
        state = state,
        cpuUsed = 0,
        sliceBudget = math.huge,
        sliceStart = 0,
        nodes = 0,
        stop = false,
        canStop = false,
        result = nil,
        err = nil,
        -- 0/false/nil => no limit. (Lua treats 0 as truthy, so callers using
        -- TimeLimitMs(0) must not be treated as an already-expired budget.)
        timeLimitMs = (state and type(state.time_limit_ms) == "number" and state.time_limit_ms > 0)
            and state.time_limit_ms
            or nil,
    }

    local co = coroutine.create(function()
        local result, err = work()
        sess.result = result
        sess.err = err
    end)
    sess.co = co
    sessions[co] = sess

    local finished = false

    local function step()
        if finished then
            return false
        end
        sess.sliceBudget = pendingBudget
        sess.sliceStart = clockMs()
        local ok, err = coroutine.resume(co)
        local spent = clockMs() - sess.sliceStart
        if spent < 0 then
            spent = 0
        end
        sess.cpuUsed = sess.cpuUsed + spent

        if not ok then
            sessions[co] = nil
            finished = true
            sess.err = err
            return false
        end
        if coroutine.status(co) == "dead" then
            sessions[co] = nil
            finished = true
            return false
        end
        return true
    end

    local function done()
        onComplete(sess.result, sess.err)
    end

    loopFn(step, done)
end

DeltaChess.Slice = M
return M
