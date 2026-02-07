--[[
  File and directory operations for tests.
  Implemented using os.execute (and io.popen where output must be captured) so that
  environments without standard io (e.g. WoW addon) can replace with a different implementation.
]]

local sep = package.config:sub(1, 1)
local isWindows = (sep == "\\")

local M = {}

--- Read entire file at path. Returns content string or nil, err.
function M.read(path)
  if not path or path == "" then return nil, "empty path" end
  local cmd
  if isWindows then
    cmd = 'type "' .. path:gsub('"', '""') .. '" 2>nul'
  else
    cmd = 'cat "' .. path:gsub('"', '\\"') .. '" 2>/dev/null'
  end
  local h = io.popen(cmd)
  if not h then return nil, "popen failed" end
  local content = h:read("a")
  h:close()
  if content == nil then return nil, "read failed" end
  return content
end

--- Write content to path. Returns true or nil, err.
function M.write(path, content)
  if not path or path == "" then return nil, "empty path" end
  content = content or ""
  local f = io.open(path, "w")
  if not f then return nil, "open for write failed" end
  f:write(content)
  f:close()
  return true
end

--- Return whether path exists (file or directory).
function M.exists(path)
  if not path or path == "" then return false end
  local pathEsc = path:gsub('"', '""')
  local cmd
  if isWindows then
    -- dir /b path returns 0 if path exists (file or dir)
    cmd = 'dir /b "' .. pathEsc .. '" >nul 2>nul'
  else
    cmd = 'test -e "' .. path:gsub('"', '\\"') .. '"'
  end
  local ok = os.execute(cmd)
  return ok == true or ok == 0
end

--- Create directory (and parents on Unix). Returns true or nil, err.
function M.mkdir(path)
  if not path or path == "" then return nil, "empty path" end
  local pathNorm = path:gsub("/", sep):gsub(sep .. sep .. "+", sep)
  local cmd
  if isWindows then
    cmd = 'mkdir "' .. pathNorm:gsub('"', '""') .. '" 2>nul'
  else
    cmd = 'mkdir -p "' .. pathNorm:gsub('"', '\\"') .. '" 2>/dev/null'
  end
  local ok = os.execute(cmd)
  return (ok == true or ok == 0) and true or nil
end

--- List entries in directory. Returns array of names or nil, err.
function M.listDir(path)
  if not path or path == "" then return nil, "empty path" end
  local pathNorm = path:gsub("/", sep)
  local cmd
  if isWindows then
    cmd = 'dir /b "' .. pathNorm:gsub('"', '""') .. '" 2>nul'
  else
    cmd = 'ls -1 "' .. pathNorm:gsub('"', '\\"') .. '" 2>/dev/null'
  end
  local h = io.popen(cmd)
  if not h then return nil, "popen failed" end
  local list = {}
  for line in h:lines() do
    local trimmed = line:gsub("^%s+", ""):gsub("%s+$", ""):gsub("\r", "")
    if trimmed ~= "" then
      list[#list + 1] = trimmed
    end
  end
  h:close()
  return list
end

return M
