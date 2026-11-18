
local bitPolyfill =(function()
    local floor = math.floor
    local b = {}
    function b.band(a, n)
      local r, p = 0, 1
      for i = 0, 31 do
        local ba, bn = a % 2, n % 2
        if ba == 1 and bn == 1 then r = r + p end
        a, n = floor(a / 2), floor(n / 2)
        p = p * 2
      end
      return r
    end
    function b.bor(a, n)
      local r, p = 0, 1
      for i = 0, 31 do
        local ba, bn = a % 2, n % 2
        if ba == 1 or bn == 1 then r = r + p end
        a, n = floor(a / 2), floor(n / 2)
        p = p * 2
      end
      return r
    end
    function b.bxor(a, n)
      local r, p = 0, 1
      for i = 0, 31 do
        local ba, bn = a % 2, n % 2
        if ba ~= bn then r = r + p end
        a, n = floor(a / 2), floor(n / 2)
        p = p * 2
      end
      return r
    end
    function b.bnot(n)
      return 0xFFFFFFFF - n
    end
    function b.lshift(n, bits)
      return floor(n *(2 ^ bits)) % 0x100000000
    end
    function b.rshift(n, bits)
      return floor(n /(2 ^ bits)) % 0x100000000
    end
    return b
end)()

local libBit = _G.bit or bit32 or bit or bitPolyfill

DeltaChess = DeltaChess or {}
DeltaChess.LibBitPolyfill = bitPolyfill
DeltaChess.LibBit = libBit

return libBit