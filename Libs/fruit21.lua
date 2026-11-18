--
-- Fruit 2.1 chess engine by Fabien Letouzey, 2004-2005.
-- At http://www.fruitchess.com,  http://wbec-ridderkerk.nl/
--
-- Port to Lua language by http://chessforeva.blogspot.com, 2011
-- 
-- Lua is a scripting language, this means: not a strong chess
-- But this is a smart AI anyway.
--
-- There is no opening book. lua library for chess openings is available at
-- Chessforeva's site. Or develop it.
--
-- Samples and all the usage is obvious down at the end.
-- Free usage and much thanks to Fabien!
--

--------------------------------------------------------------------------------
-- Bit operations compatibility layer
-- Works with Lua 5.1 bit, Lua 5.2+ bit32, LuaJIT bit, or pure-Lua fallback
-- Registered under DeltaChess.Util.bit for use by engines
--------------------------------------------------------------------------------

-- colour.h

local function CreateEngine()

local bit = DeltaChess.LibBit


M = {}
M.__index = M

function M:CreateInstance(clockPolyfill)
    local self = setmetatable({}, M)

function self:clock()
    return clockPolyfill()
end

function self:time()
    return clockPolyfill()
end

function self:log(msg)
end

-- Lua 5.1+ compatibility: table.getn was removed in modern Lua
function self:getn(t)
    return #t
end

-- Lua 5.1+ compatibility: table.getn was removed in modern Lua
function self:formatd(t)
    return string.format("%d", math.floor(t))
end


-- constants

 self.UseMcache = false;      -- to use files for faster performance(saves some memory variables),
                        -- set "false" for normal lua, needed for very slow lua case
 self.TRUE = 1;
 self.FALSE = 0;

 self.UseTable = true;   -- const bool
 self.MaterialTableSize = 64 * 1024;         -- const size of self.Material hashing(array elements to use)
 self.PawnTableSize = 64 * 1024;             -- const size of self.Pawn hashing(array elements to use)

 self.UseTrans = true;   -- const bool
 self.TransSize = 64 * 1024;                 -- const size of transp-table(array elements to use)
   -- it is not a memory hash size, because there is no memory allocation at all

 self.bestmv = "";      -- string contains best move
 self.bestmv2 = "";     -- string contains pgn-format of the move

 self.ShowInfo = false;  -- set true to show thinking!

 self.iDbg01 = false;  -- internal for debugging

 self.ColourNone = -1; -- const int
 self.White = 0;       -- const int
 self.Black = 1;       -- const int
 self.ColourNb = 2;    -- const int

 self.WhiteFlag = bit.lshift(1,self.White);   -- const int
 self.BlackFlag = bit.lshift(1,self.Black);   -- const int
 self.WxorB = bit.bxor(self.White,self.Black);
 self.bnot1 = bit.bnot(1);
 self.bnot3 = bit.bnot(3);
 self.bnotF = bit.bnot(0xF);

 self.V07777 = 4095;                     -- const int
 self.bnotV07777 = bit.bnot( self.V07777 );   -- const int
 self.bnotV77 = bit.bnot(0x77);          -- const int

-- should be true, or error otherwise

function self:ASSERT(id,logic)
 if( not logic ) then
   self:log("ASSERT FAIL on id=" .. self:formatd(id));
 end
end


--(if ? then : else) substitute

function self:iif(ask, ontrue, onfalse)
 if( ask ) then
  return ontrue;
 end
 return onfalse;
end

-- convert bytes(little endian) to a 32-bit two's complement integer(not used)
function self:bytes_to_int(b1, b2, b3, b4)
 if(b3==nil) then
   b3 = 0;
 end
 if(b4==nil) then
   b4 = 0;
 end
 local n = b1 +(b2*256) +(b3*65536) +(b4*16777216);
 if(n > 2147483647) then
   return(n - 4294967296);
 end
 return n;
end


-- constructor to provide string as a parm
function self:string_t()
 local b = {};
 b.v = "";
 return b;
end
-- constructor to provide int as a parm
function self:int_t()
  local b = {};
  b.v = 0;
  return b;
end

-- self:my_fatal()

function self:my_fatal( errmess )  -- for error case
   -- suppressed: self:log( "my-error: "..errmess );
end

function self:COLOUR_IS_OK(colour)
 return(bit.band(colour,self.bnot1)==0);
end

function self:COLOUR_IS_WHITE(colour)
 return(colour==self.White);
end

function self:COLOUR_IS_BLACK(colour)
 return(colour~=self.White);
end

function self:COLOUR_FLAG(colour)
 return(colour+1);
end

function self:COLOUR_IS(piece,colour)
 return(self:FLAG_IS(piece,colour+1));
end

function self:FLAG_IS(piece,flag)
 return(bit.band(piece,flag)~=0);
end

function self:COLOUR_OPP(colour)
 return bit.bxor(colour,self.WxorB);
end

-- end of colour.h


-- piece.h

-- constants

 self.WhitePawnFlag = bit.lshift(1,2);   -- const int
 self.BlackPawnFlag = bit.lshift(1,3);   -- const int
 self.KnightFlag    = bit.lshift(1,4);   -- const int
 self.BishopFlag    = bit.lshift(1,5);   -- const int
 self.RookFlag      = bit.lshift(1,6);   -- const int
 self.KingFlag      = bit.lshift(1,7);   -- const int

 self.PawnFlags  = bit.bor(self.WhitePawnFlag, self.BlackPawnFlag);   -- const int
 self.QueenFlags = bit.bor(self.BishopFlag, self.RookFlag);          -- const int

 self.PieceNone64 = 0; -- const int
 self.WhitePawn64 = self.WhitePawnFlag;  -- const int
 self.BlackPawn64 = self.BlackPawnFlag;  -- const int
 self.Knight64    = self.KnightFlag;     -- const int
 self.Bishop64    = self.BishopFlag;     -- const int
 self.Rook64      = self.RookFlag;       -- const int
 self.Queen64     = self.QueenFlags;     -- const int
 self.King64      = self.KingFlag;       -- const int

 self.PieceNone256   = 0; -- const int
 self.WhitePawn256   =  bit.bor(self.WhitePawn64,self.WhiteFlag);  -- const int
 self.BlackPawn256   =  bit.bor(self.BlackPawn64,self.BlackFlag);  -- const int
 self.WhiteKnight256 =  bit.bor(self.Knight64,self.WhiteFlag);     -- const int
 self.BlackKnight256 =  bit.bor(self.Knight64,self.BlackFlag);     -- const int
 self.WhiteBishop256 =  bit.bor(self.Bishop64,self.WhiteFlag);     -- const int
 self.BlackBishop256 =  bit.bor(self.Bishop64,self.BlackFlag);     -- const int
 self.WhiteRook256   =  bit.bor(self.Rook64,self.WhiteFlag);       -- const int
 self.BlackRook256   =  bit.bor(self.Rook64,self.BlackFlag);       -- const int
 self.WhiteQueen256  =  bit.bor(self.Queen64,self.WhiteFlag);      -- const int
 self.BlackQueen256  =  bit.bor(self.Queen64,self.BlackFlag);      -- const int
 self.WhiteKing256   =  bit.bor(self.King64,self.WhiteFlag);       -- const int
 self.BlackKing256   =  bit.bor(self.King64,self.BlackFlag);       -- const int
 self.PieceNb        = 256; -- const int

 self.WhitePawn12   =  0; -- const int
 self.BlackPawn12   =  1; -- const int
 self.WhiteKnight12 =  2; -- const int
 self.BlackKnight12 =  3; -- const int
 self.WhiteBishop12 =  4; -- const int
 self.BlackBishop12 =  5; -- const int
 self.WhiteRook12   =  6; -- const int
 self.BlackRook12   =  7; -- const int
 self.WhiteQueen12  =  8; -- const int
 self.BlackQueen12  =  9; -- const int
 self.WhiteKing12   = 10; -- const int
 self.BlackKing12   = 11; -- const int

-- "constants"

 self.PawnMake = { self.WhitePawn256, self.BlackPawn256 };   -- const int[self.ColourNb]

 self.PieceFrom12 = {  self.WhitePawn256, self.BlackPawn256, self.WhiteKnight256, self.BlackKnight256,
   self.WhiteBishop256, self.BlackBishop256, self.WhiteRook256, self.BlackRook256,
   self.WhiteQueen256,  self.BlackQueen256, self.WhiteKing256, self.BlackKing256 };  -- const int[12]

 self.PieceString = "PpNnBbRrQqKk";   -- const char[12+1]

 self.PawnMoveInc = { 16, -16 };  -- const int[self.ColourNb]

 self.KnightInc = { -33, -31, -18, -14, 14, 18, 31, 33, 0 };  -- const int[8+1]

 self.BishopInc = { -17, -15, 15, 17, 0 };  -- const int[4+1]

 self.RookInc = { -16, -1, 1, 16, 0 };  -- const int[4+1]

 self.QueenInc = { -17, -16, -15, -1, 1, 15, 16, 17, 0 };  -- const[8+1]

 self.KingInc = { -17, -16, -15, -1, 1, 15, 16, 17, 0 };  -- const[8+1]


-- variables

 self.PieceTo12 = {};    -- int[self.PieceNb]
 self.PieceOrder = {};   -- int[self.PieceNb]
 self.PieceInc = {};     -- const

-- macros

function self:PAWN_OPP(pawn)
 return bit.bxor(pawn,bit.bxor(self.WhitePawn256,self.BlackPawn256));
end

function self:PIECE_COLOUR(piece)
 return(bit.band(piece,3)-1);
end

function self:PIECE_TYPE(piece)
 return bit.band(piece,self.bnot3);
end

function self:PIECE_IS_PAWN(piece)
 return(bit.band(piece,self.PawnFlags)~=0);
end

function self:PIECE_IS_KNIGHT(piece)
 return(bit.band(piece,self.KnightFlag)~=0);
end

function self:PIECE_IS_BISHOP(piece)
 return(bit.band(piece,self.QueenFlags)==self.BishopFlag);
end

function self:PIECE_IS_ROOK(piece)
 return(bit.band(piece,self.QueenFlags)==self.RookFlag);
end

function self:PIECE_IS_QUEEN(piece)
 return(bit.band(piece,self.QueenFlags)==self.QueenFlags);
end

function self:PIECE_IS_KING(piece)
 return(bit.band(piece,self.KingFlag)~=0);
end

function self:PIECE_IS_SLIDER(piece)
 return(bit.band(piece,self.QueenFlags)~=0);
end


-- end of piece.h



-- square.h


-- constants

 self.FileNb = 16;   -- const int
 self.RankNb = 16;   -- const int

 self.SquareNb = self.FileNb*self.RankNb;   -- const int

 self.FileInc = 1;   -- const int
 self.RankInc = 16;  -- const int

 self.FileNone = 0;   -- const int

 self.FileA = 0x4;   -- const int
 self.FileB = 0x5;   -- const int
 self.FileC = 0x6;   -- const int
 self.FileD = 0x7;   -- const int
 self.FileE = 0x8;   -- const int
 self.FileF = 0x9;   -- const int
 self.FileG = 0xA;   -- const int
 self.FileH = 0xB;   -- const int

 self.RankNone = 0;   -- const int

 self.Rank1 = 0x4;   -- const int
 self.Rank2 = 0x5;   -- const int
 self.Rank3 = 0x6;   -- const int
 self.Rank4 = 0x7;   -- const int
 self.Rank5 = 0x8;   -- const int
 self.Rank6 = 0x9;   -- const int
 self.Rank7 = 0xA;   -- const int
 self.Rank8 = 0xB;   -- const int

 self.SquareNone = 0;   -- const int

 self.A1=0x44; self.B1=0x45; self.C1=0x46; self.D1=0x47; self.E1=0x48; self.F1=0x49; self.G1=0x4A; self.H1=0x4B;   -- const int
 self.A2=0x54; self.B2=0x55; self.C2=0x56; self.D2=0x57; self.E2=0x58; self.F2=0x59; self.G2=0x5A; self.H2=0x5B;   -- const int
 self.A3=0x64; self.B3=0x65; self.C3=0x66; self.D3=0x67; self.E3=0x68; self.F3=0x69; self.G3=0x6A; self.H3=0x6B;   -- const int
 self.A4=0x74; self.B4=0x75; self.C4=0x76; self.D4=0x77; self.E4=0x78; self.F4=0x79; self.G4=0x7A; self.H4=0x7B;   -- const int
 self.A5=0x84; self.B5=0x85; self.C5=0x86; self.D5=0x87; self.E5=0x88; self.F5=0x89; self.G5=0x8A; self.H5=0x8B;   -- const int
 self.A6=0x94; self.B6=0x95; self.C6=0x96; self.D6=0x97; self.E6=0x98; self.F6=0x99; self.G6=0x9A; self.H6=0x9B;   -- const int
 self.A7=0xA4; self.B7=0xA5; self.C7=0xA6; self.D7=0xA7; self.E7=0xA8; self.F7=0xA9; self.G7=0xAA; self.H7=0xAB;   -- const int
 self.A8=0xB4; self.B8=0xB5; self.C8=0xB6; self.D8=0xB7; self.E8=0xB8; self.F8=0xB9; self.G8=0xBA; self.H8=0xBB;   -- const int

 self.Dark  = 0;   -- const int
 self.Light = 1;   -- const int

-- variables

 self.SquareTo64 = {};        -- int[self.SquareNb]
 self.SquareIsPromote = {};   -- bool[self.SquareNb]


-- "constants"

  self.SquareFrom64 = {
   self.A1, self.B1, self.C1, self.D1, self.E1, self.F1, self.G1, self.H1,
   self.A2, self.B2, self.C2, self.D2, self.E2, self.F2, self.G2, self.H2,
   self.A3, self.B3, self.C3, self.D3, self.E3, self.F3, self.G3, self.H3,
   self.A4, self.B4, self.C4, self.D4, self.E4, self.F4, self.G4, self.H4,
   self.A5, self.B5, self.C5, self.D5, self.E5, self.F5, self.G5, self.H5,
   self.A6, self.B6, self.C6, self.D6, self.E6, self.F6, self.G6, self.H6,
   self.A7, self.B7, self.C7, self.D7, self.E7, self.F7, self.G7, self.H7,
   self.A8, self.B8, self.C8, self.D8, self.E8, self.F8, self.G8, self.H8,
};   -- const int[64]

  self.RankMask = { 0, 0xF };          -- const int[self.ColourNb]
  self.PromoteRank = { 0xB0, 0x40 };   -- const int[self.ColourNb]

-- macros

function self:SQUARE_IS_OK(square)
 return(bit.band(square-0x44,self.bnotV77)==0);
end

function self:SQUARE_MAKE(file,rank)
 return bit.bor(bit.lshift(rank,4),file);
end

function self:SQUARE_FILE(square)
 return bit.band(square,0xF);
end

function self:SQUARE_RANK(square)
 return bit.rshift(square,4);
end

function self:SQUARE_EP_DUAL(square)
 return bit.bxor(square,16);
end

function self:SQUARE_COLOUR(square)
 return bit.band( bit.bxor(square,bit.rshift(square,4)),1);
end

function self:SQUARE_FILE_MIRROR(square)
 return bit.bxor(square,0x0F);
end

function self:SQUARE_RANK_MIRROR(square)
 return bit.bxor(square,0xF0);
end

function self:FILE_OPP(file)
 return bit.bxor(file,0xF);
end

function self:RANK_OPP(rank)
 return bit.bxor(rank,0xF);
end

function self:PAWN_RANK(square,colour)
 return bit.bxor(self:SQUARE_RANK(square),self.RankMask[1+colour]);
end

function self:PAWN_PROMOTE(square,colour)
 return bit.bor(self.PromoteRank[1+colour],bit.band(square,0xF));
end


-- end of square.h


-- board.h

-- constants

 self.Empty = 0;       -- const int
 self.Edge = self.Knight64; -- const int   HACK: uncoloured knight

 self.WP = self.WhitePawn256;   -- const int
 self.WN = self.WhiteKnight256; -- const int
 self.WB = self.WhiteBishop256; -- const int
 self.WR = self.WhiteRook256;   -- const int
 self.WQ = self.WhiteQueen256;  -- const int
 self.WK = self.WhiteKing256;   -- const int

 self.BP = self.BlackPawn256;   -- const int
 self.BN = self.BlackKnight256; -- const int
 self.BB = self.BlackBishop256; -- const int
 self.BR = self.BlackRook256;   -- const int
 self.BQ = self.BlackQueen256;  -- const int
 self.BK = self.BlackKing256;   -- const int

 self.FlagsNone = 0;   -- const int
 self.FlagsWhiteKingCastle  = bit.lshift(1,0) ;   -- const int
 self.FlagsWhiteQueenCastle = bit.lshift(1,1) ;   -- const int
 self.FlagsBlackKingCastle  = bit.lshift(1,2) ;   -- const int
 self.FlagsBlackQueenCastle = bit.lshift(1,3) ;   -- const int

 self.StackSize = 4096; -- const int

-- macros

function self:KING_POS(board,colour)
 return board.piece[1+colour][1+0];
end

-- types

function self:board_t()
   local b = {};
   b.square = { 0, 0 };  -- int[self.SquareNb]
   b.pos = { 0, 0 };     -- int[self.SquareNb]

   b.piece = {};   -- int[self.ColourNb][32] only 17 are needed
   b.piece[1+0] = {0};
   b.piece[1+1] = {0};

   b.piece_size = { 0, 0 };  -- int[self.ColourNb]

   b.pawn = {};       -- int[self.ColourNb][16] only 9 are needed
   b.pawn[1+0] = {0};
   b.pawn[1+1] = {0};

   b.pawn_size = { 0, 0 };  -- int[self.ColourNb]

   b.piece_nb = 0;   -- int
   b.number = {};    -- int[16] only 12 are needed

   b.pawn_file = {}; -- int[self.ColourNb][self.FileNb];
   b.pawn_file[1+0] = {0};
   b.pawn_file[1+1] = {0};

   b.turn = 0;       -- int
   b.flags = 0;      -- int
   b.ep_square = 0;  -- int
   b.ply_nb = 0;     -- int
   b.sp = 0;         -- int  TODO: MOVE ME?

   b.cap_sq = 0;     -- int

   b.opening = 0;    -- int
   b.endgame = 0;    -- int

   b.key = 0;           -- uint64
   b.pawn_key = 0;      -- uint64
   b.material_key = 0;  -- uint64

   b.stack = {0};        -- uint64[self.StackSize];
   b.movenumb = 0;     -- int
   return b;
end


-- end of board.h


-- move.h

-- constants

 self.MoveNone = 0;  -- const int   HACK: a1a1 cannot be a legal move
 self.Movenil = 11; -- const int   HACK: a1d2 cannot be a legal move

 self.MoveNormal    =  bit.lshift(0,14);   -- const int
 self.MoveCastle    =  bit.lshift(1,14);   -- const int
 self.MovePromote   =  bit.lshift(2,14);   -- const int
 self.MoveEnPassant =  bit.lshift(3,14);   -- const int
 self.MoveFlags     =  bit.lshift(3,14);   -- const int

 self.MovePromoteKnight =  bit.bor(self.MovePromote,bit.lshift(0,12));   -- const int
 self.MovePromoteBishop =  bit.bor(self.MovePromote,bit.lshift(1,12));   -- const int
 self.MovePromoteRook   =  bit.bor(self.MovePromote,bit.lshift(2,12));   -- const int
 self.MovePromoteQueen  =  bit.bor(self.MovePromote,bit.lshift(3,12));   -- const int

 self.MoveAllFlags = bit.lshift(0xF,12);   -- const int

 self.nilMoveString = "nil"; -- const char[] "0000" in UCI

 self.PromotePiece = { self.Knight64, self.Bishop64, self.Rook64, self.Queen64 };   -- int[4]

-- macros

function self:MOVE_MAKE(from,to)
 return bit.bor( bit.lshift(self.SquareTo64[1+from],6) , self.SquareTo64[1+to]);
end

function self:MOVE_MAKE_FLAGS(from,to,flags)  return bit.bor( bit.lshift(self.SquareTo64[1+from],6), bit.bor(self.SquareTo64[1+to],flags));
end

function self:MOVE_FROM(move)
 return self.SquareFrom64[1+ bit.band(bit.rshift(move,6), 63)];
end

function self:MOVE_TO(move)
 return self.SquareFrom64[1+ bit.band(move, 63)];
end

function self:MOVE_IS_SPECIAL(move)
 return( bit.band(move,self.MoveFlags)~=self.MoveNormal );
end

function self:MOVE_IS_PROMOTE(move)
 return( bit.band(move,self.MoveFlags)==self.MovePromote );
end

function self:MOVE_IS_EN_PASSANT(move)
 return( bit.band(move,self.MoveFlags)==self.MoveEnPassant );
end

function self:MOVE_IS_CASTLE(move)
 return( bit.band(move,self.MoveFlags)==self.MoveCastle );
end

function self:MOVE_PIECE(move,board)
 return((board).square[1+self:MOVE_FROM(move)]);
end


-- end of move.h



-- attack.h

-- types
function self:attack_t()
   local b = {};
   b.dn = 0;   -- int
   b.ds = {};  -- int[2+1]
   b.di = {};  -- int[2+1]
   return b;
end


-- variables

  self.DeltaIncLine = {};      -- int[self.DeltaNb]
  self.DeltaIncAll = {};       -- int[self.DeltaNb]

  self.DeltaMask = {};         -- int[self.DeltaNb]
  self.IncMask = {};           -- int[self.IncNb]

  self.PieceCode = {};         -- int[self.PieceNb]
  self.PieceDeltaSize = {};    -- int[4][256]      4kB
  self.PieceDeltaDelta = {};   -- int[4][256][4]  16kB


-- macros

function self:IS_IN_CHECK(board,colour)
 return self:is_attacked(board,self:KING_POS(board,colour),self:COLOUR_OPP(colour));
end

function self:DELTA_INC_LINE(delta)
 return self.DeltaIncLine[1+self.DeltaOffset+delta];
end

function self:DELTA_INC_ALL(delta)
 return self.DeltaIncAll[1+self.DeltaOffset+delta];
end

function self:DELTA_MASK(delta)
 return self.DeltaMask[1+self.DeltaOffset+delta];
end

function self:INC_MASK(inc)
 return self.IncMask[1+self.IncOffset+inc];
end

function self:PSEUDO_ATTACK(piece,delta)
 return(bit.band(piece,self:DELTA_MASK(delta))~=0);
end

function self:PIECE_ATTACK(board,piece,from,to)
 return self:PSEUDO_ATTACK(piece,to-from) and self:line_is_empty(board,from,to);
end


function self:SLIDER_ATTACK(piece,inc)
 return(bit.band(piece,self:INC_MASK(inc))~=0);
end

function self:ATTACK_IN_CHECK(attack)
 return(attack.dn~=0);
end


-- end of attack.h


-- trans.h

-- constants

  self.UseModulo = false;        -- const bool
  self.DateSize = 16;            -- const int
  self.DepthNone = -128;         -- const int
  self.ClusterSize = 4;          -- const int, not a hash size

-- types

function self:entry_t()
   local b = {};
   b.lock = 0;        -- uint32
   b.move = 0;        -- uint16
   b.depth = 0;       -- sint8
   b.date = 0;        -- uint8
   b.move_depth = 0;  -- sint8
   b.flags = 0;       -- uint8
   b.min_depth = 0;   -- sint8
   b.max_depth = 0;   -- sint8
   b.min_value = 0;   -- sint16
   b.max_value = 0;   -- sint16
   return b;
end


function self:trans_t()
   local b = {};
   b.table = {};           -- entry_t*
   b.size = 0;             -- uint32
   b.mask = 0;             -- uint32
   b.date = 0;             -- int
   b.age = {};             -- int[self.DateSize]
   b.used = 0;             -- uint32
   b.read_nb = 0;          -- sint64
   b.read_hit = 0;         -- sint64
   b.write_nb = 0;         -- sint64
   b.write_hit = 0;        -- sint64
   b.write_collision = 0;  -- sint64
   return b;
end


function self:trans_rtrv()
   local b = {};
   b.trans_move = 0;        -- int
   b.trans_min_depth = 0;   -- int
   b.trans_max_depth = 0;   -- int
   b.trans_min_value = 0;   -- int
   b.trans_max_value = 0;   -- int
   return b;
end

-- variables

 self.Trans = self:trans_t();      -- trans_t [1]
 self.TransRv = self:trans_rtrv();  -- retriever

-- end of trans.h




-- hash.h

-- macros

function self:uint32(i)
 return bit.band(i, 0xFFFFFFFF);
end

function self:KEY_INDEX(key)
 return self:uint32(key);
end

function self:KEY_LOCK(key)          -- no 64 bits, so, we use the original key
 return key;                    -- self:uint32(bit.rshift(key,32));
end

-- constants

 self.RandomPiece     =   0; -- 12 * 64   const int
 self.RandomCastle    = 768; -- 4         const int
 self.RandomEnPassant = 772; -- 8         const int
 self.RandomTurn      = 780; -- 1         const int


-- end of hash.h


-- list.h

-- constants

 self.ListSize = 256;   -- const int

 self.UseStrict = true;   -- const bool

-- types

function self:list_t()
   local b = {};
   b.size = 0;    -- int
   b.move = {};	  -- int[self.ListSize]
   b.value = {};  -- short int[self.ListSize]
   return b;
end

function self:alist_t()
   local b = {};
   b.size = 0;       -- int
   b.square = {};    -- int[15]
   return b;
end

function self:alists_t()
   local b = {};
   b.alist = {};             --alist_t [self.ColourNb][1]
   b.alist[1+0] = self:alist_t();
   b.alist[1+1] = self:alist_t();
   return b;
end


-- macros

function self:LIST_ADD(list,mv)
 list.move[1+list.size]=mv;
 list.size = list.size + 1;
end

function self:LIST_CLEAR(list)
 list.move = {};
 list.size = 0;
end


-- end of list.h


-- material.h

-- constants

   self.MAT_NONE = 0; self.MAT_KK = 1; self.MAT_KBK = 2; self.MAT_KKB = 3; self.MAT_KNK = 4; self.MAT_KKN = 5;
   self.MAT_KPK = 6; self.MAT_KKP = 7; self.MAT_KQKQ = 8; self.MAT_KQKP = 9; self.MAT_KPKQ = 10;
   self.MAT_KRKR = 11; self.MAT_KRKP = 12; self.MAT_KPKR = 13; self.MAT_KBKB = 14; self.MAT_KBKP = 15;
   self.MAT_KPKB = 16; self.MAT_KBPK = 17; self.MAT_KKBP = 18; self.MAT_KNKN = 19; self.MAT_KNKP = 20;
   self.MAT_KPKN = 21; self.MAT_KNPK = 22; self.MAT_KKNP = 23; self.MAT_KRPKR = 24; self.MAT_KRKRP = 25;
   self.MAT_KBPKB = 26; self.MAT_KBKBP = 27; self.MAT_NB = 28;

 self.DrawNodeFlag    =  bit.lshift(1,0);  -- const int
 self.DrawBishopFlag  =  bit.lshift(1,1);  -- const int
 self.MatRookPawnFlag =  bit.lshift(1,0);  -- const int
 self.MatBishopFlag   =  bit.lshift(1,1);  -- const int
 self.MatKnightFlag   =  bit.lshift(1,2);  -- const int
 self.MatKingFlag     =  bit.lshift(1,3);  -- const int


-- constants

  self.PawnPhase   = 0;   -- const int
  self.KnightPhase = 1;   -- const int
  self.BishopPhase = 1;   -- const int
  self.RookPhase   = 2;   -- const int
  self.QueenPhase  = 4;   -- const int
  self.TotalPhase =(self.PawnPhase * 16) +(self.KnightPhase * 4) +(self.BishopPhase * 4) + self.RookPhase * 4 +(self.QueenPhase * 2);   -- const int

-- constants and variables

  self.MaterialWeight = 256; -- 100% const int

  self.PawnOpening   = 80;    -- was 100 const int
  self.PawnEndgame   = 90;    -- was 100 const int
  self.KnightOpening = 325;   -- const int
  self.KnightEndgame = 325;   -- const int
  self.BishopOpening = 325;   -- const int
  self.BishopEndgame = 325;   -- const int
  self.RookOpening   = 500;   -- const int
  self.RookEndgame   = 500;   -- const int
  self.QueenOpening  = 1000;  -- const int
  self.QueenEndgame  = 1000;  -- const int

  self.BishopPairOpening = 50;   -- const int
  self.BishopPairEndgame = 50;   -- const int

-- types

function self:material_info_t()
   local b = {};
   b.lock = 0;	        -- uint32
   b.recog = 0;         -- uint8
   b.flags = 0;         -- uint8
   b.cflags = { 0, 0 }; -- uint8[self.ColourNb]
   b.mul = { 0, 0 };    -- uint8[self.ColourNb]
   b.phase = 0;         -- sint16
   b.opening = 0;       -- sint16
   b.endgame = 0;       -- sint16
   return b;
end

function self:material_t()
   local b = {};
   b.table = {};         -- entry_t*
   b.size = 0;           -- uint32
   b.mask = 0;           -- uint32
   b.used = 0;           -- uint32

   b.read_nb = 0;          -- sint64
   b.read_hit = 0;         -- sint64
   b.write_nb = 0;         -- sint64
   b.write_collision = 0;  -- sint64
   return b;
end

-- variables

  self.Material = self:material_t();   -- material_t[1]

-- self:material_info_copy()

function self:material_info_copy( dst, src )

   dst.lock = src.lock;
   dst.recog = src.recog;

   dst.cflags[1+0] = src.cflags[1+0];
   dst.cflags[1+1] = src.cflags[1+1];

   dst.mul[1+0] = src.mul[1+0];
   dst.mul[1+1] = src.mul[1+1];

   dst.phase = src.phase;
   dst.opening = src.opening;
   dst.endgame = src.endgame;

   dst.flags = src.flags;

end


-- end of material.h



-- move_do.h


-- types

function self:undo_t()
   local b = {};
   b.capture = false;   -- bool

   b.capture_square = 0;  -- int
   b.capture_piece = 0;   -- int
   b.capture_pos = 0;     -- int

   b.pawn_pos = 0;        -- int

   b.turn = 0;      -- int
   b.flags = 0;     -- int
   b.ep_square = 0; -- int
   b.ply_nb = 0;    -- int

   b.cap_sq = 0;    -- int

   b.opening = 0;   -- int
   b.endgame = 0;   -- int

   b.key = 0;           -- uint64
   b.pawn_key = 0;      -- uint64
   b.material_key = 0;  -- uint64

   return b;
end

-- variables

  self.CastleMask = {};   -- int[self.SquareNb]

-- end of move_do.h



-- pawn.h


-- constants

  self.BackRankFlag =  bit.lshift(1,0);   -- const int

-- types


function self:pawn_t()
   local b = {};
   b.table = {};        -- entry_t*
   b.size = 0;           -- uint32
   b.mask = 0;           -- uint32
   b.used = 0;           -- uint32

   b.read_nb = 0;          -- sint64
   b.read_hit = 0;         -- sint64
   b.write_nb = 0;         -- sint64
   b.write_collision = 0;  -- sint64
   return b;
end

function self:pawn_info_t()
   local b = {};
   b.lock = 0;                -- uint32
   b.opening = 0;             -- sint16
   b.endgame = 0;             -- sint16
   b.flags = { 0, 0 };        -- uint8[self.ColourNb]
   b.passed_bits = { 0, 0 };  -- uint8[self.ColourNb]
   b.single_file = { 0, 0 };  -- uint8[self.ColourNb]
   b.pad = 0;                 -- uint16
   return b;
end

-- self:pawn_info_copy()

function self:pawn_info_copy( dst, src )
   dst.lock = src.lock;
   dst.opening = src.opening;
   dst.endgame = src.endgame;
   dst.flags[1+0] = src.flags[1+0];
   dst.flags[1+1] = src.flags[1+1];
   dst.passed_bits[1+0] = src.passed_bits[1+0];
   dst.passed_bits[1+1] = src.passed_bits[1+1];
   dst.single_file[1+0] = src.single_file[1+0];
   dst.single_file[1+1] = src.single_file[1+1];
   dst.pad  = src.pad ;
end

-- constants and variables

  self.Pawn = self:pawn_t();           -- pawn_t[1]

  self.DoubledOpening = 10;       -- const int
  self.DoubledEndgame = 20;       -- const int

  self.IsolatedOpening = 10;      -- const int
  self.IsolatedOpeningOpen = 20;  -- const int
  self.IsolatedEndgame = 20;      -- const int

  self.BackwardOpening = 8;       -- const int
  self.BackwardOpeningOpen = 16;  -- const int
  self.BackwardEndgame = 10;      -- const int

  self.CandidateOpeningMin = 5;   -- const int
  self.CandidateOpeningMax = 55;  -- const int
  self.CandidateEndgameMin = 10;  -- const int
  self.CandidateEndgameMax = 110; -- const int

  self.Bonus = {};   -- int[self.RankNb]

-- variables

  self.BitEQ = {};   -- int[16]
  self.BitLT = {};   -- int[16]
  self.BitLE = {};   -- int[16]
  self.BitGT = {};   -- int[16]
  self.BitGE = {};   -- int[16]

  self.BitFirst = {};  -- int[0x100]
  self.BitLast = {};   -- int[0x100]
  self.BitCount = {};  -- int[0x100]
  self.BitRev = {};    -- int[0x100]


  self.BitRank1 = {};  -- int[self.RankNb]
  self.BitRank2 = {};  -- int[self.RankNb]
  self.BitRank3 = {};  -- int[self.RankNb]


-- end of pawn.h


-- pst.h

-- constants

  self.Opening = 0;   -- const int
  self.Endgame = 1;   -- const int
  self.StageNb = 2;   -- const int

-- constants

 self.pA1= 0; self.pB1= 1; self.pC1= 2; self.pD1= 3; self.pE1= 4; self.pF1= 5; self.pG1= 6; self.pH1= 7;     -- const int
 self.pA2= 8; self.pB2= 9; self.pC2=10; self.pD2=11; self.pE2=12; self.pF2=13; self.pG2=14; self.pH2=15;     -- const int
 self.pA3=16; self.pB3=17; self.pC3=18; self.pD3=19; self.pE3=20; self.pF3=21; self.pG3=22; self.pH3=23;     -- const int
 self.pA4=24; self.pB4=25; self.pC4=26; self.pD4=27; self.pE4=28; self.pF4=29; self.pG4=30; self.pH4=31;     -- const int
 self.pA5=32; self.pB5=33; self.pC5=34; self.pD5=35; self.pE5=36; self.pF5=37; self.pG5=38; self.pH5=39;     -- const int
 self.pA6=40; self.pB6=41; self.pC6=42; self.pD6=43; self.pE6=44; self.pF6=45; self.pG6=46; self.pH6=47;     -- const int
 self.pA7=48; self.pB7=49; self.pC7=50; self.pD7=51; self.pE7=52; self.pF7=53; self.pG7=54; self.pH7=55;     -- const int
 self.pA8=56; self.pB8=57; self.pC8=58; self.pD8=59; self.pE8=60; self.pF8=61; self.pG8=62; self.pH8=63;     -- const int

-- constants and variables

 self.PieceActivityWeight = 256; -- 100%   const int
 self.KingSafetyWeight = 256;    -- 100%  const int
 self.PawnStructureWeight = 256; -- 100%  const int
 self.PassedPawnWeight = 256;    -- 100%  const int

 self.PawnFileOpening = 5;        -- const int
 self.KnightCentreOpening = 5;    -- const int
 self.KnightCentreEndgame = 5;    -- const int
 self.KnightRankOpening = 5;      -- const int
 self.KnightBackRankOpening = 0;  -- const int
 self.KnightTrapped = 100;        -- const int
 self.BishopCentreOpening = 2;    -- const int
 self.BishopCentreEndgame = 3;    -- const int
 self.BishopBackRankOpening = 10; -- const int
 self.BishopDiagonalOpening = 4;  -- const int
 self.RookFileOpening = 3;        -- const int
 self.QueenCentreOpening = 0;     -- const int
 self.QueenCentreEndgame = 4;     -- const int
 self.QueenBackRankOpening = 5;   -- const int
 self.KingCentreEndgame = 12;     -- const int
 self.KingFileOpening = 10;       -- const int
 self.KingRankOpening = 10;       -- const int

-- "constants"

 self.PawnFile = { -3, -1, 0, 1, 1, 0, -1, -3 };      -- const int[8]

 self.KnightLine = { -4, -2, 0, 1, 1, 0, -2, -4 };    -- const int[8]

 self.KnightRank = { -2, -1, 0, 1, 2, 3, 2, 1 };    -- const int[8]

 self.BishopLine = { -3, -1, 0, 1, 1, 0, -1, -3 };    -- const int[8]

 self.RookFile = { -2, -1, 0, 1, 1, 0, -1, -2 };      -- const int[8]

 self.QueenLine = { -3, -1, 0, 1, 1, 0, -1, -3 };     -- const int[8]

 self.KingLine = { -3, -1, 0, 1, 1, 0, -1, -3 };      -- const int[8]

 self.KingFile = { 3, 4, 2, 0, 0, 2, 4, 3 };      -- const int[8]

 self.KingRank = { 1, 0, -2, -3, -4, -5, -6, -7 };      -- const int[8]

-- variables

 self.Pst = {};      -- sint16 [12][64][self.StageNb]


-- end of pst.h




-- random.h

-- "constants"

  self.Random64 = {};    -- uint64[self.RandomNb]  array of const fixed randoms
  self.R64_i = 0;        -- length
  self.RandomNb = 781;   -- max size

-- end of random.h




-- search.h

-- types

function self:my_timer_t()
   local b = {};
   b.start_real = 0.0;	 -- double
   b.elapsed_real = 0.0; -- double
   b.running = false;    -- bool
   return b;
end

function self:search_input_t()
   local b = {};
   b.board = self:board_t();          -- board_t[1]
   b.list = self:list_t();            -- list_t[1]
   b.infinite = false;           -- bool
   b.depth_is_limited = false;   -- bool
   b.depth_limit = 0;            -- int
   b.time_is_limited = false;    -- bool
   b.time_limit_1 = 0.0;         -- double
   b.time_limit_2 = 0.0;         -- double
   return b;
end

function self:search_info_t()
   local b = {};
   b.can_stop = false;   -- bool
   b.stop = false;       -- bool
   b.check_nb = 0;       -- int
   b.check_inc = 0;      -- int
   b.last_time = 0.0;    -- double
   return b;
end

function self:search_root_t()
   local b = {};
   b.list = self:list_t();  -- list_t[1]
   b.depth = 0;      -- int
   b.move = 0;       -- int
   b.move_pos = 0;   -- int
   b.move_nb = 0;    -- int
   b.last_value = 0; -- int
   b.bad_1 = false;  -- bool
   b.bad_2 = false;  -- bool
   b.change = false; -- bool
   b.easy = false;   -- bool
   b.flag = false;   -- bool
   return b;
end

function self:search_best_t()
   local b = {};
   b.move = 0;    -- int
   b.value = 0;   -- int
   b.flags = 0;   -- int
   b.depth = 0;   -- int
   b.pv = {};     -- int[self.HeightMax];
   return b;
end

function self:search_current_t()
   local b = {};
   b.board = self:board_t();       -- board_t[1]
   b.timer = self:my_timer_t();    -- my_timer_t[1]
   b.mate = 0;         -- int
   b.depth = 0;        -- int
   b.max_depth = 0;    -- int
   b.node_nb = 0;      -- sint64
   b.time = 0.0;       -- double
   b.speed = 0.0;      -- double
   return b;
end

-- variables

  self.setjmp = false;        -- c++ has self.setjmp-longjmp feature

-- constants

  self.DepthMax = 64;     -- const int
  self.HeightMax = 256;   -- const int

  self.SearchNormal = 0;  -- const int
  self.SearchShort  = 1;  -- const int

  self.SearchUnknown = 0; -- const int
  self.SearchUpper   = 1; -- const int
  self.SearchLower   = 2; -- const int
  self.SearchExact   = 3; -- const int

  self.UseShortSearch = true;    -- const bool
  self.ShortSearchDepth = 1;     -- const int

  self.DispBest = true;          -- const bool
  self.DispDepthStart = true;    -- const bool
  self.DispDepthEnd = true;      -- const bool
  self.DispRoot = true;          -- const bool
  self.DispStat = true;          -- const bool

  self.UseEasy = true;           -- const bool  singular move
  self.EasyThreshold = 150;      -- const int
  self.EasyRatio = 0.20;         -- const

  self.UseEarly = true;          -- const bool  early iteration end
  self.EarlyRatio = 0.60;        -- const

  self.UseBad = true;            -- const bool
  self.BadThreshold = 50;        -- const int
  self.UseExtension = true;      -- const bool

-- variables

  self.SearchInput = self:search_input_t();      -- search_input_t[1]
  self.SearchInfo = self:search_info_t();        -- search_info_t[1]
  self.SearchRoot = self:search_root_t();        -- search_root_t[1]
  self.SearchCurrent = self:search_current_t();  -- search_current_t[1]
  self.SearchBest = self:search_best_t();        -- search_best_t[1]



-- constants and variables

-- main search

  self.UseDistancePruning = true;   -- const bool

-- transposition table

  self.TransDepth = 1;    -- const int

  self.UseMateValues = true; -- use mate values from shallower searches?   -- const bool

-- nil move

  self.Usenil = true;     -- const bool
  self.UsenilEval = true; -- const bool
  self.nilDepth = 2;      -- const int
  self.nilReduction = 3;  -- const int

  self.UseVer = true;         -- const bool
  self.UseVerEndgame = true;  -- const bool
  self.VerReduction = 5;      -- const int   was 3

-- move ordering

  self.UseIID = true;      -- const bool
  self.IIDDepth = 3;       -- const int
  self.IIDReduction = 2;   -- const int

-- extensions

  self.ExtendSingleReply = true;   -- const bool

-- history pruning

  self.UseHistory = true;       -- const bool
  self.HistoryDepth = 3;        -- const int
  self.HistoryMoveNb = 3;       -- const int
  self.HistoryValue = 9830;     -- const int 60%
  self.HistoryReSearch = true;  -- const bool

-- futility pruning

  self.UseFutility = false;     -- const bool
  self.FutilityMargin = 100;    -- const int

-- quiescence search

  self.UseDelta = false;        -- const bool
  self.DeltaMargin = 50;        -- const int

  self.CheckNb = 1;             -- const int
  self.CheckDepth = 0;          -- const int   1 - self.CheckNb

-- misc

  self.NodeAll = -1;   -- const int
  self.NodePV  =  0;   -- const int
  self.NodeCut = 1;   -- const int


-- end of search.h



-- fen.h

-- "constants"

 self.StartFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";   -- const char

-- variables

 self.Strict = false;   -- const bool

-- end of fen.h



-- protocol.h

-- constants

 self.VERSION = "Fruit 2.1 by Fabien Letouzey, port to Lua by Chessforeva";
 self.NormalRatio = 1.0;   -- const
 self.PonderRatio = 1.25;   -- const

-- variables

 self.Init = false;       -- bool

-- end of protocol.h



-- sort.h

-- types

function self:sort_t()
   local b = {};
   b.depth = 0;         -- int
   b.height = 0;        -- int
   b.trans_killer = 0;  -- int
   b.killer_1 = 0;      -- int
   b.killer_2 = 0;      -- int
   b.gen = 0;           -- int
   b.test = 0;          -- int
   b.pos = 0;           -- int
   b.value = 0;         -- int
   b.board = {};        -- board_t *
   b.attack = {};       -- const attack_t *
   b.list = self:list_t();   -- list_t[1]
   b.bad = self:list_t();    -- list_t[1]
   return b;
end


-- constants

  self.KillerNb = 2;   -- const int

  self.HistorySize = 12 * 64;   -- const int
  self.HistoryMax = 16384;      -- const int

  self.TransScore   = 32766;   -- const int
  self.GoodScore    =  4000;   -- const int
  self.KillerScore  =     4;   -- const int
  self.HistoryScore = -24000;   -- const int
  self.BadScore     = -28000;   -- const int

  self.CODE_SIZE = 256;         -- const int


   self.GEN_ERROR = 0;
   self.GEN_LEGAL_EVASION = 1;
   self.GEN_TRANS = 2;
   self.GEN_GOOD_CAPTURE = 3;
   self.GEN_BAD_CAPTURE = 4;
   self.GEN_KILLER = 5;
   self.GEN_QUIET = 6;
   self.GEN_EVASION_QS = 7;
   self.GEN_CAPTURE_QS = 8;
   self.GEN_CHECK_QS = 9;
   self.GEN_END = 10;

   self.TEST_ERROR = 0;
   self.TEST_NONE = 1;
   self.TEST_LEGAL = 2;
   self.TEST_TRANS_KILLER = 3;
   self.TEST_GOOD_CAPTURE = 4;
   self.TEST_BAD_CAPTURE = 5;
   self.TEST_KILLER = 6;
   self.TEST_QUIET = 7;
   self.TEST_CAPTURE_QS = 8;
   self.TEST_CHECK_QS = 9;


-- variables

 self.PosLegalEvasion = 0;   -- int
 self.PosSEE = 0;            -- int

 self.PosEvasionQS = 0;      -- int
 self.PosCheckQS = 0;        -- int
 self.PosCaptureQS = 0;      -- int

 self.Code = {};             -- int[self.CODE_SIZE]

 self.Killer = {};           -- uint16[self.HeightMax][self.KillerNb]

 self.History = {};          -- uint16[self.HistorySize]
 self.HistHit = {};          -- uint16[self.HistorySize]
 self.HistTot = {};          -- uint16[self.HistorySize]



-- end of sort.h



-- value.h

-- variables

  self.ValuePiece = { 0, 0 };   -- int[self.PieceNb]

-- constants

 self.ValuePawn   = 100;   -- was 100   const int
 self.ValueKnight = 325;   -- was 300   const int
 self.ValueBishop = 325;   -- was 300   const int
 self.ValueRook   = 500;   -- was 500   const int
 self.ValueQueen  = 1000;  -- was 900   const int
 self.ValueKing   = 10000; -- was 10000 const int

 self.ValueNone    = -32767;          -- const int
 self.ValueDraw    = 0;               -- const int
 self.ValueMate    = 30000;           -- const int
 self.ValueInf     = self.ValueMate;       -- const int
 self.ValueEvalInf = self.ValueMate - 256; -- const int handle mates upto 255 plies


-- end of value.h


-- eval.h

-- constants and variables

 self.KnightUnit = 4;   -- const int
 self.BishopUnit = 6;   -- const int
 self.RookUnit = 7;     -- const int
 self.QueenUnit = 13;   -- const int

 self.MobMove = 1;      -- const int
 self.MobAttack = 1;    -- const int
 self.MobDefense = 0;   -- const int

 self.KnightMobOpening = 4; -- const int
 self.KnightMobEndgame = 4; -- const int
 self.BishopMobOpening = 5; -- const int
 self.BishopMobEndgame = 5; -- const int
 self.RookMobOpening = 2;   -- const int
 self.RookMobEndgame = 4;   -- const int
 self.QueenMobOpening = 1;  -- const int
 self.QueenMobEndgame = 2;  -- const int
 self.KingMobOpening = 0;   -- const int
 self.KingMobEndgame = 0;   -- const int

 self.UseOpenFile = true;   -- const bool
 self.RookSemiOpenFileOpening = 10;  -- const int
 self.RookSemiOpenFileEndgame = 10;  -- const int
 self.RookOpenFileOpening = 20;      -- const int
 self.RookOpenFileEndgame = 20;      -- const int
 self.RookSemiKingFileOpening = 10;  -- const int
 self.RookKingFileOpening = 20;      -- const int

 self.UseKingAttack = true;     -- const bool
 self.KingAttackOpening = 20;   -- const int

 self.UseShelter = true;    -- const bool
 self.ShelterOpening = 256; -- 100%  const int
 self.UseStorm = true;      -- const bool
 self.StormOpening = 10;    -- const int

 self.Rook7thOpening = 20;   -- const int
 self.Rook7thEndgame = 40;   -- const int
 self.Queen7thOpening = 10;  -- const int
 self.Queen7thEndgame = 20;  -- const int

 self.TrappedBishop = 100;   -- const int

 self.BlockedBishop = 50;   -- const int
 self.BlockedRook = 50;     -- const int

 self.PassedOpeningMin = 10;   -- const int
 self.PassedOpeningMax = 70;   -- const int
 self.PassedEndgameMin = 20;   -- const int
 self.PassedEndgameMax = 140;  -- const int

 self.UnstoppablePasser = 800; -- const int
 self.FreePasser = 60;         -- const int

 self.AttackerDistance = 5;    -- const int
 self.DefenderDistance = 20;   -- const int

-- "constants"

 self.KingAttackWeight = { 0, 0, 128, 192, 224, 240, 248, 252, 254, 255, 256, 256 ,256, 256, 256, 256 };  -- const int[16]

-- variables

 self.MobUnit = {};        -- int[self.ColourNb][self.PieceNb]
 self.MobUnit[1+0] = {};
 self.MobUnit[1+1] = {};

 self.KingAttackUnit = {}  -- int[self.PieceNb]

-- macros

function self:THROUGH(piece)
 return(piece==self.Empty);
end

-- end of eval.h



-- hash.h

-- variables

 self.Castle64 = {};   -- int[16]

-- end of hash.h




-- vector.h

-- "constants"

 self.IncNone = 0;          -- const int
 self.IncNb =(2*17) + 1;   -- const int
 self.IncOffset = 17;       -- const int

 self.DeltaNone = 0;           -- const int
 self.DeltaNb =(2*119) + 1;   -- const int
 self.DeltaOffset = 119;       -- const int

-- variables

 self.Distance = {};   -- int[self.DeltaNb]

-- macros

function self:DISTANCE(square_1,square_2)
 return self.Distance[1+self.DeltaOffset+(square_2-square_1)];
end

-- end of vector.h


-- option.h

-- types

function self:opt_t_def( var, declare, init, type, extra, val )
   local b = {};
   b.var = var;          -- string
   b.declare = declare;  -- bool
   b.init = init;        -- string
   b.type = type;        -- string
   b.extra = extra;      -- string
   b.val = val;          -- string
   return b;
end

-- variables

 self.Option = {};

-- end of option.h


--
-- Programs C
--


-- attack.cpp

-- functions

-- self:attack_init()

function self:attack_init()  -- void

   local delta = 0;   -- int
   local inc = 0;     -- int
   local piece = 0;   -- int
   local dir = 0;     -- int
   local dist = 0;    -- int
   local size = 0;    -- int
   local king = 0;    -- int
   local from = 0;    -- int
   local to = 0;      -- int
   local pos = 0;     -- int
   local k = 0;
   local mcache = true;
   local mfile = nil;


   -- clear

   for delta = 0, self.DeltaNb-1, 1 do
      self.DeltaIncLine[1+delta] = self.IncNone;
      self.DeltaIncAll[1+delta] = self.IncNone;
      self.DeltaMask[1+delta] = 0;
   end

   for inc = 0, self.IncNb-1, 1 do
      self.IncMask[1+inc] = 0;
   end

   -- pawn attacks

   self.DeltaMask[1+self.DeltaOffset-17] = bit.bor( self.DeltaMask[1+self.DeltaOffset-17], self.BlackPawnFlag );
   self.DeltaMask[1+self.DeltaOffset-15] = bit.bor( self.DeltaMask[1+self.DeltaOffset-15], self.BlackPawnFlag );

   self.DeltaMask[1+self.DeltaOffset+15] = bit.bor( self.DeltaMask[1+self.DeltaOffset+15], self.WhitePawnFlag );
   self.DeltaMask[1+self.DeltaOffset+17] = bit.bor( self.DeltaMask[1+self.DeltaOffset+17], self.WhitePawnFlag );

   -- knight attacks

   for dir = 0, 7, 1 do

      delta = self.KnightInc[1+dir];
      ----self:ASSERT(3, self:delta_is_ok(delta));

      ----self:ASSERT(4, self.DeltaIncAll[1+self.DeltaOffset+delta]==self.IncNone);
      self.DeltaIncAll[1+self.DeltaOffset+delta] = delta;
      self.DeltaMask[1+self.DeltaOffset+delta] = bit.bor( self.DeltaMask[1+self.DeltaOffset+delta], self.KnightFlag );
   end

   -- bishop/queen attacks

   for dir = 0, 3, 1 do

      inc = self.BishopInc[1+dir];
      ----self:ASSERT(5, inc~=self.IncNone);

      self.IncMask[1+self.IncOffset+inc] = bit.bor( self.IncMask[1+self.IncOffset+inc], self.BishopFlag );

      for dist = 1, 7, 1 do

         delta = inc*dist;
         ----self:ASSERT(6, self:delta_is_ok(delta));

         ----self:ASSERT(7, self.DeltaIncLine[1+self.DeltaOffset+delta]==self.IncNone);
         self.DeltaIncLine[1+self.DeltaOffset+delta] = inc;
         ----self:ASSERT(8, self.DeltaIncAll[1+self.DeltaOffset+delta]==self.IncNone);
         self.DeltaIncAll[1+self.DeltaOffset+delta] = inc;
         self.DeltaMask[1+self.DeltaOffset+delta] = bit.bor( self.DeltaMask[1+self.DeltaOffset+delta], self.BishopFlag );
      end
   end

   -- rook/queen attacks

   for dir = 0, 3, 1 do

      inc = self.RookInc[1+dir];
      ----self:ASSERT(9, inc~=self.IncNone);

      self.IncMask[1+self.IncOffset+inc] = bit.bor( self.IncMask[1+self.IncOffset+inc], self.RookFlag );

      for dist = 1, 7, 1 do

         delta = inc*dist;
         ----self:ASSERT(10, self:delta_is_ok(delta));

         ----self:ASSERT(11, self.DeltaIncLine[1+self.DeltaOffset+delta]==self.IncNone);
         self.DeltaIncLine[1+self.DeltaOffset+delta] = inc;
         ----self:ASSERT(12, self.DeltaIncAll[1+self.DeltaOffset+delta]==self.IncNone);
         self.DeltaIncAll[1+self.DeltaOffset+delta] = inc;
         self.DeltaMask[1+self.DeltaOffset+delta] = bit.bor( self.DeltaMask[1+self.DeltaOffset+delta], self.RookFlag );
      end
   end

   -- king attacks

   for dir = 0, 7, 1 do

      delta = self.KingInc[1+dir];
      ----self:ASSERT(13, self:delta_is_ok(delta));

      self.DeltaMask[1+self.DeltaOffset+delta] = bit.bor( self.DeltaMask[1+self.DeltaOffset+delta], self.KingFlag );
   end

   -- self.PieceCode[]

   for piece = 0, self.PieceNb-1, 1 do
      self.PieceCode[1+piece] = -1;
   end

   self.PieceCode[1+self.WN] = 0;
   self.PieceCode[1+self.WB] = 1;
   self.PieceCode[1+self.WR] = 2;
   self.PieceCode[1+self.WQ] = 3;

   self.PieceCode[1+self.BN] = 0;
   self.PieceCode[1+self.BB] = 1;
   self.PieceCode[1+self.BR] = 2;
   self.PieceCode[1+self.BQ] = 3;

   -- self.PieceDeltaSize[][] & self.PieceDeltaDelta[][][]

   for piece = 0, 3, 1 do

      self.PieceDeltaSize[1+piece] = {};
      self.PieceDeltaDelta[1+piece] = {}

      for delta = 0, 255, 1 do

         self.PieceDeltaSize[1+piece][1+delta] = 0;
         self.PieceDeltaDelta[1+piece][1+delta] = {};

      end
   end

   if((not self.UseMcache) or(not mcache)) then

    for king = 0, self.SquareNb-1, 1 do

      if(self:SQUARE_IS_OK(king)) then

         for from = 0, self.SquareNb-1, 1 do

            if(self:SQUARE_IS_OK(from)) then

               -- knight
               pos = 0;
               while(true) do
                  inc=self.KnightInc[1+pos];
                  if(inc == self.IncNone) then
                    break;
                  end
                  to = from + inc;
                  if(self:SQUARE_IS_OK(to)  and  self:DISTANCE(to,king) == 1) then
                     self:add_attack(0,king-from,to-from);
                  end
                  pos = pos + 1;
               end

               -- bishop
               pos = 0;
               while(true) do
                  inc=self.BishopInc[1+pos];
                  if(inc == self.IncNone) then
                    break;
                  end
                  to = from+inc;
                  while( self:SQUARE_IS_OK(to) ) do
                     if(self:DISTANCE(to,king) == 1) then
                        self:add_attack(1,king-from,to-from);
                        break;
                     end
                     to = to + inc;
                  end
                  pos = pos + 1;
               end

               -- rook
               pos = 0;
               while(true) do
                  inc=self.RookInc[1+pos];
                  if(inc == self.IncNone) then
                    break;
                  end
                  to = from+inc;
                  while( self:SQUARE_IS_OK(to) ) do
                     if(self:DISTANCE(to,king) == 1) then
                        self:add_attack(2,king-from,to-from);
                        break;
                     end
                     to = to + inc;
                  end
                  pos = pos + 1;
               end

               -- queen
               pos = 0;
               while(true) do
                  inc=self.QueenInc[1+pos];
                  if(inc == self.IncNone) then
                    break;
                  end
                  to = from+inc;
                  while( self:SQUARE_IS_OK(to) ) do
                     if(self:DISTANCE(to,king) == 1) then
                        self:add_attack(3,king-from,to-from);
                        break;
                     end
                     to = to + inc;
                  end
                  pos = pos + 1;
               end
            end
         end
      end
    end

    for piece = 0, 3, 1 do
      for delta = 0, 255, 1 do
         size = self.PieceDeltaSize[1+piece][1+delta];
         ----self:ASSERT(14, size>=0 and size<3);
         self.PieceDeltaDelta[1+piece][1+delta][1+size] = self.DeltaNone;
      end
    end

   end --

   if(mfile~=nil) then
     mfile:close();
   end

end

-- self:add_attack()

function self:add_attack(piece, king, target)  -- void

   local size = 0;   -- int
   local i = 0;      -- int


   ----self:ASSERT(15, piece>=0 and piece<4);
   ----self:ASSERT(16, self:delta_is_ok(king));
   ----self:ASSERT(17, self:delta_is_ok(target));

   size = self.PieceDeltaSize[1+piece][1+self.DeltaOffset+king];
   ----self:ASSERT(18, size>=0 and size<3);

   for i = 0, size-1, 1 do
      if(self.PieceDeltaDelta[1+piece][1+self.DeltaOffset+king][1+i] == target) then
        return;    -- already in the table
      end
   end

   if(size < 2)  then
      self.PieceDeltaDelta[1+piece][1+self.DeltaOffset+king][1+size] = target;
      size = size + 1;
      self.PieceDeltaSize[1+piece][1+self.DeltaOffset+king] = size;
   end
end

-- self:is_attacked()

function self:is_attacked( board, to, colour)  -- bool

   local inc = 0;    -- int
   local pawn = 0;   -- int
   local ptr = 0;    -- int
   local from = 0;   -- int
   local piece = 0;  -- int
   local delta = 0;  -- int
   local sq = 0;     -- int

   ----self:ASSERT(19, board.sp~=nil);
   ----self:ASSERT(20, self:SQUARE_IS_OK(to));
   ----self:ASSERT(21, self:COLOUR_IS_OK(colour));

   -- pawn attack

   inc = self.PawnMoveInc[1+colour];
   pawn = self.PawnMake[1+colour];

   if(board.square[1+to-(inc-1)] == pawn) then
     return true;
   end
   if(board.square[1+to-(inc+1)] == pawn) then
     return true;
   end

   -- piece attack

   ptr = 0;
   while(true) do
      from = board.piece[1+colour][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];
      delta = to - from;

      if(self:PSEUDO_ATTACK(piece,delta)) then

         inc = self:DELTA_INC_ALL(delta);
         ----self:ASSERT(22, inc~=self.IncNone);

         sq = from;
         while(true) do

           sq = sq + inc;
           if(sq == to) then
             return true;
           end
           if(board.square[1+sq] ~= self.Empty) then
             break;
           end
         end
      end
      ptr = ptr + 1;
   end

   return false;

end

-- self:line_is_empty()

function self:line_is_empty( board, from, to )  -- bool

   local delta = 0;  -- int
   local inc = 0;    -- int
   local sq = 0;     -- int

   ----self:ASSERT(23, board.sp~=nil);
   ----self:ASSERT(24, self:SQUARE_IS_OK(from));
   ----self:ASSERT(25, self:SQUARE_IS_OK(to));

   delta = to - from;
   ----self:ASSERT(26, self:delta_is_ok(delta));

   inc = self:DELTA_INC_ALL(delta);
   ----self:ASSERT(27, inc~=self.IncNone);

   sq = from;
   while(true) do

     sq = sq + inc;
     if(sq == to) then
       return true;
     end
     if(board.square[1+sq] ~= self.Empty) then
       break;
     end
   end

   return false;  -- blocker
end

-- self:is_pinned()

function self:is_pinned( board, square, colour)  -- bool

   local from = 0;  -- int
   local to = 0;    -- int
   local inc = 0;   -- int
   local sq = 0;    -- int
   local piece = 0; -- int

   ----self:ASSERT(28, board.sp~=nil);
   ----self:ASSERT(29, self:SQUARE_IS_OK(square));
   ----self:ASSERT(30, self:COLOUR_IS_OK(colour));

   from = square;
   to = self:KING_POS(board,colour);

   inc = self:DELTA_INC_LINE(to-from);
   if(inc == self.IncNone) then
     return false;  -- not a line
   end

   sq = from;
   while(true) do
     sq = sq + inc;
     if(board.square[1+sq] ~= self.Empty) then
       break;
     end
   end

   if(sq ~= to) then
     return false; -- blocker
   end

   sq = from;
   while(true) do
     sq = sq - inc;
     piece = board.square[1+sq];
     if( piece~= self.Empty) then
       break;
     end
   end

   return self:COLOUR_IS(piece,self:COLOUR_OPP(colour)) and self:SLIDER_ATTACK(piece,inc);
end

-- self:attack_is_ok()

function self:attack_is_ok( attack )  -- bool

   local i = 0;   -- int
   local sq = 0;  -- int
   local inc = 0; -- int

   if(attack.dn == nil) then
     return false;
   end

   -- checks

   if(attack.dn < 0 or attack.dn > 2) then
     return false;
   end

   for i = 0, attack.dn-1, 1 do
      sq = attack.ds[1+i];
      if(not self:SQUARE_IS_OK(sq)) then
        return false;
      end
      inc = attack.di[1+i];
      if(inc ~= self.IncNone  and(not self:inc_is_ok(inc))) then
        return false;
      end
   end

   if(attack.ds[1+attack.dn] ~= self.SquareNone) then
     return false;
   end
   if(attack.di[1+attack.dn] ~= self.IncNone) then
     return false;
   end

   return true;
end

-- self:attack_set()

function self:attack_set( attack, board )  -- void

   local me = 0;    -- int
   local opp = 0;   -- int
   local ptr = 0;   -- int
   local from = 0;  -- int
   local to = 0;    -- int
   local inc = 0;   -- int
   local pawn = 0;  -- int
   local delta = 0; -- int
   local piece = 0; -- int
   local sq = 0;    -- int
   local cont = false;

   ----self:ASSERT(31, attack.dn~=nil);
   ----self:ASSERT(32, board.sp~=nil);

   -- init

   attack.dn = 0;

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   to = self:KING_POS(board,me);

   -- pawn attacks

   inc = self.PawnMoveInc[1+opp];
   pawn = self.PawnMake[1+opp];

   from = to -(inc-1);
   if(board.square[1+from] == pawn) then
      attack.ds[1+attack.dn] = from;
      attack.di[1+attack.dn] = self.IncNone;
      attack.dn = attack.dn + 1;
   end

   from = to -(inc+1);
   if(board.square[1+from] == pawn) then
      attack.ds[1+attack.dn] = from;
      attack.di[1+attack.dn] = self.IncNone;
      attack.dn = attack.dn + 1;
   end

   -- piece attacks

   ptr = 1;	-- HACK: no king
   while(true) do
      from = board.piece[1+opp][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      delta = to - from;
      ----self:ASSERT(33, self:delta_is_ok(delta));

      if(self:PSEUDO_ATTACK(piece,delta)) then

         inc = self.IncNone;

         if(self:PIECE_IS_SLIDER(piece)) then

            -- check for blockers

            inc = self:DELTA_INC_LINE(delta);
            ----self:ASSERT(34, inc~=self.IncNone);

            sq = from;
            while(true) do
              sq = sq + inc;
              if(board.square[1+sq] ~= self.Empty) then
                break;
              end
            end

            if(sq ~= to) then
              cont = true;     -- blocker => next attacker
            end
         end

         if(cont) then
           cont = false;
         else
           attack.ds[1+attack.dn] = from;
           attack.di[1+attack.dn] = -inc; -- HACK
           attack.dn = attack.dn + 1;
         end
      end
      ptr = ptr + 1;
   end

   attack.ds[1+attack.dn] = self.SquareNone;
   attack.di[1+attack.dn] = self.IncNone;

   -- debug

   ----self:ASSERT(35, self:attack_is_ok(attack));
end

-- self:piece_attack_king()

function self:piece_attack_king( board, piece, from, king )  -- bool

   local code = 0;      -- int
   local delta_ptr = 0; -- int
   local delta = 0;     -- int
   local inc = 0;       -- int
   local to = 0;        -- int
   local sq = 0;        -- int

   --self:ASSERT(36, board.sp~=nil);
   --self:ASSERT(37, self:piece_is_ok(piece));
   --self:ASSERT(38, self:SQUARE_IS_OK(from));
   --self:ASSERT(39, self:SQUARE_IS_OK(king));

   code = self.PieceCode[1+piece];
   --self:ASSERT(40, code>=0 and code<4);

   if(self:PIECE_IS_SLIDER(piece)) then

      delta_ptr = 0;
      while(true) do

         delta = self.PieceDeltaDelta[1+code][1+self.DeltaOffset+(king-from)][1+delta_ptr];
         if(delta==self.DeltaNone) then
           break;
         end

         --self:ASSERT(41, self:delta_is_ok(delta));

         inc = self.DeltaIncLine[1+self.DeltaOffset+delta];
         --self:ASSERT(42, inc~=self.IncNone);

         to = from + delta;

         sq = from;
         while(true) do
           sq = sq + inc;

           if(sq == to and self:SQUARE_IS_OK(to)) then
              --self:ASSERT(43, self:DISTANCE(to,king)==1);
              return true;
           end

           if(board.square[1+sq] ~= self.Empty) then
             break;
           end
         end

         delta_ptr = delta_ptr + 1;
      end

   else -- non-slider

      delta_ptr = 0;
      while(true) do

         delta = self.PieceDeltaDelta[1+code][1+self.DeltaOffset+(king-from)][1+delta_ptr];
         if(delta==self.DeltaNone) then
           break;
         end

         --self:ASSERT(44, self:delta_is_ok(delta));

         to = from + delta;

         if(self:SQUARE_IS_OK(to)) then
            --self:ASSERT(45, self:DISTANCE(to,king)==1);
            return true;
         end

         delta_ptr = delta_ptr + 1;
      end
   end

   return false;
end

-- end of attack.cpp



-- board.cpp

-- functions

-- self:board_is_ok()

function self:board_is_ok( board ) -- const bool

   local sq = 0;     -- int
   local piece = 0;  -- int
   local colour = 0; -- int
   local size = 0;   -- int
   local pos = 0;    -- int

   if(board.sp == nil) then
     return false;
   end

   -- squares

   for sq = 0, self.SquareNb-1, 1 do

      piece = board.square[1+sq];
      pos = board.pos[1+sq];

      if(self:SQUARE_IS_OK(sq)) then

         -- inside square

         if(piece == self.Empty) then

            if(pos ~= -1) then
              return false;
            end
         else

            if(not self:piece_is_ok(piece)) then
              return false;
            end

            if(not self:PIECE_IS_PAWN(piece)) then
               colour = self:PIECE_COLOUR(piece);
               if(pos < 0  or  pos >= board.piece_size[1+colour]) then
                 return false;
               end
               if(board.piece[1+colour][1+pos] ~= sq) then
                 return false;
               end
            else -- pawn
               if(self.SquareIsPromote[1+sq]) then
                return false;
               end
               colour = self:PIECE_COLOUR(piece);
               if(pos < 0  or  pos >= board.pawn_size[1+colour]) then
                return false;
               end
               if(board.pawn[1+colour][1+pos] ~= sq) then
                return false;
               end
            end
         end

      else

         -- edge square

         if(piece ~= self.Edge) then
           return false;
         end
         if(pos ~= -1) then
           return false;
         end
      end
   end

   -- piece lists

   for colour = 0, 1, 1 do

      -- piece list

      size = board.piece_size[1+colour];
      if(size < 1  or  size > 16) then
        return false;
      end

      for pos = 0, size-1, 1 do

         sq = board.piece[1+colour][1+pos];
         if(not self:SQUARE_IS_OK(sq)) then
           return false;
         end
         if(board.pos[1+sq] ~= pos) then
           return false;
         end
         piece = board.square[1+sq];
         if(not self:COLOUR_IS(piece,colour)) then
           return false;
         end
         if(pos == 0  and  not self:PIECE_IS_KING(piece)) then
           return false;
         end
         if(pos ~= 0  and  self:PIECE_IS_KING(piece)) then
           return false;
         end
         if(pos ~= 0  and  self.PieceOrder[1+piece] > self.PieceOrder[1+board.square[1+board.piece[1+colour][1+pos-1]]]) then
            return false;
         end
      end

      sq = board.piece[1+colour][1+size];
      if(sq ~= self.SquareNone) then
        return false;
      end

      -- pawn list

      size = board.pawn_size[1+colour];
      if(size < 0  or  size > 8) then
        return false;
      end

      for pos = 0, size-1, 1 do

         sq = board.pawn[1+colour][1+pos];
         if(not self:SQUARE_IS_OK(sq)) then
           return false;
         end
         if(self.SquareIsPromote[1+sq]) then
           return false;
         end
         if(board.pos[1+sq] ~= pos) then
           return false;
         end
         piece = board.square[1+sq];
         if(not self:COLOUR_IS(piece,colour)) then
           return false;
         end
         if(not self:PIECE_IS_PAWN(piece)) then
           return false;
         end
      end

      sq = board.pawn[1+colour][1+size];
      if(sq ~= self.SquareNone) then
        return false;
      end

      -- piece total

      if(board.piece_size[1+colour] + board.pawn_size[1+colour] > 16) then
        return false;
      end
   end

   -- material

   if(board.piece_nb ~= board.piece_size[1+self.White] + board.pawn_size[1+self.White]
                        + board.piece_size[1+self.Black] + board.pawn_size[1+self.Black]) then
      return false;
   end

   if(board.number[1+self.WhitePawn12] ~= board.pawn_size[1+self.White]) then
     return false;
   end
   if(board.number[1+self.BlackPawn12] ~= board.pawn_size[1+self.Black]) then
     return false;
   end
   if(board.number[1+self.WhiteKing12] ~= 1) then
     return false;
   end
   if(board.number[1+self.BlackKing12] ~= 1) then
     return false;
   end

   -- misc

   if(not self:COLOUR_IS_OK(board.turn)) then
     return false;
   end

   if(board.ply_nb < 0) then
     return false;
   end

   if(board.sp < board.ply_nb) then
     return false;
   end

   if(board.cap_sq ~= self.SquareNone  and(not self:SQUARE_IS_OK(board.cap_sq))) then
     return false;
   end

   if(board.opening ~= self:board_opening(board)) then
     return false;
   end
   if(board.endgame ~= self:board_endgame(board)) then
     return false;
   end

   -- we can not guarantee that the key is the same, it is just a random number
   --
   --if(board.key ~= self:hash_key(board)) then
   --  return false;
   --end
   --if(board.pawn_key ~= self:hash_pawn_key(board)) then
   --  return false;
   --end
   --if(board.material_key ~= self:hash_material_key(board)) then
   --  return false;
   --end

   return true;
end

-- self:board_clear()

function self:board_clear( board ) -- void

   local sq = 0;     -- int
   local sq_64 = 0;  -- int

   --self:ASSERT(46, board.sp~=nil);

   -- edge squares

   for sq = 0, self.SquareNb-1, 1 do
      board.square[1+sq] = self.Edge;
   end

   -- empty squares

   for sq_64 = 0, 63, 1 do
      sq = self.SquareFrom64[1+sq_64];
      board.square[1+sq] = self.Empty;
   end

   -- misc

   board.turn = self.ColourNone;
   board.flags = self.FlagsNone;
   board.ep_square = self.SquareNone;
   board.ply_nb = 0;
end

-- self:board_copy()

function self:board_copy( dst, src ) -- void

   local i = 0;  -- int

   --self:ASSERT(47, dst.sp~=nil );
   --self:ASSERT(48, self:board_is_ok(src));

   dst.square = {};
   for i = 0, self:getn(src.square)-1, 1 do
     dst.square[1+i] = src.square[1+i];
   end
   
   dst.pos = {};
   for i = 0, self:getn(src.pos)-1, 1 do
     dst.pos[1+i] = src.pos[1+i];
   end
   
   dst.piece = {};
   dst.piece[1+0] = {};
   dst.piece[1+1] = {};

   for i = 0, self:getn(src.piece[1+0])-1, 1 do
     dst.piece[1+0][1+i] = src.piece[1+0][1+i];
   end
   for i = 0, self:getn(src.piece[1+1])-1, 1 do
     dst.piece[1+1][1+i] = src.piece[1+1][1+i];
   end

   dst.piece_size = {};
   for i = 0, self:getn(src.piece_size)-1, 1 do
     dst.piece_size[1+i] = src.piece_size[1+i];
   end

   dst.pawn = {};
   dst.pawn[1+0] = {};
   dst.pawn[1+1] = {};

   for i = 0, self:getn(src.pawn[1+0])-1, 1 do
     dst.pawn[1+0][1+i] = src.pawn[1+0][1+i];
   end
   for i = 0, self:getn(src.pawn[1+1])-1, 1 do
     dst.pawn[1+1][1+i] = src.pawn[1+1][1+i];
   end

   dst.pawn_size = {};
   for i = 0, self:getn(src.pawn_size)-1, 1 do
     dst.pawn_size[1+i] = src.pawn_size[1+i];
   end

   dst.piece_nb = src.piece_nb;
   dst.number = {};
   for i = 0, self:getn(src.number)-1, 1 do
     dst.number[1+i] = src.number[1+i];
   end

   dst.pawn_file = {};
   dst.pawn_file[1+0] = {};
   dst.pawn_file[1+1] = {};

   for i = 0, self:getn(src.pawn_file[1+0])-1, 1 do
     dst.pawn_file[1+0][1+i] = src.pawn_file[1+0][1+i];
   end
   for i = 0, self:getn(src.pawn_file[1+1])-1, 1 do
     dst.pawn_file[1+1][1+i] = src.pawn_file[1+1][1+i];
   end


   dst.turn = src.turn;
   dst.flags = src.flags;
   dst.ep_square = src.ep_square
   dst.ply_nb = src.ply_nb;
   dst.sp = src.sp;

   dst.cap_sq = src.cap_sq;

   dst.opening = src.opening;
   dst.endgame = src.endgame;

   dst.key = src.key;
   dst.pawn_key = src.pawn_key;
   dst.material_key = src.material_key;

   dst.stack = {};
   for i = 0, self:getn(src.stack)-1, 1 do
     dst.stack[1+i] = src.stack[1+i];
   end

end


-- self:board_init_list()

function self:board_init_list( board ) -- void

   local sq_64 = 0;   -- int
   local sq = 0;      -- int
   local piece = 0;   -- int
   local colour = 0;  -- int
   local pos = 0;     -- int
   local i = 0;       -- int
   local size = 0;    -- int
   local square = 0;  -- int
   local order = 0;   -- int
   local file = 0;    -- int

   --self:ASSERT(49, board.sp~=nil);

   -- init

   for sq = 0, self.SquareNb-1, 1 do
      board.pos[1+sq] = -1;
   end

   board.piece_nb = 0;
   for piece = 0, 11, 1 do
     board.number[1+piece] = 0;
   end

   -- piece lists

   for colour = 0, 1, 1 do

      -- piece list

      pos = 0;

      for sq_64 = 0, 63, 1 do

         sq = self.SquareFrom64[1+sq_64];
         piece = board.square[1+sq];
         if(piece ~= self.Empty  and(not self:piece_is_ok(piece))) then
           self:my_fatal("self:board_init_list(): illegal position\n");
         end

         if(self:COLOUR_IS(piece,colour)  and(not self:PIECE_IS_PAWN(piece))) then

            if(pos >= 16) then
              self:my_fatal("self:board_init_list(): illegal position\n");
            end
            --self:ASSERT(50, pos>=0 and pos<16);

            board.pos[1+sq] = pos;
            board.piece[1+colour][1+pos] = sq;
            pos = pos + 1;

            board.piece_nb = board.piece_nb + 1;
            board.number[1+self.PieceTo12[1+piece]] = board.number[1+self.PieceTo12[1+piece]] + 1;
         end
      end

      if( board.number[1+self:iif( self:COLOUR_IS_WHITE(colour), self.WhiteKing12, self.BlackKing12 ) ] ~= 1) then
        self:my_fatal("self:board_init_list(): illegal position\n");
      end

      --self:ASSERT(51, pos>=1 and pos<=16);
      board.piece[1+colour][1+pos] = self.SquareNone;
      board.piece_size[1+colour] = pos;

      -- MV sort

      size = board.piece_size[1+colour];

      for i = 1, size-1, 1 do

         square = board.piece[1+colour][1+i];
         piece = board.square[1+square];
         order = self.PieceOrder[1+piece];
         pos = i;
         while( pos > 0 ) do
            sq=board.piece[1+colour][1+pos-1];
            if( order <= self.PieceOrder[1+board.square[1+sq]] ) then
              break;
            end
            --self:ASSERT(52, pos>0 and pos<size);
            board.piece[1+colour][1+pos] = sq;
            --self:ASSERT(53, board.pos[1+sq]==pos-1);
            board.pos[1+sq] = pos;
            pos = pos - 1;
         end

         --self:ASSERT(54, pos>=0 and pos<size);
         board.piece[1+colour][1+pos] = square;
         --self:ASSERT(55, board.pos[1+square]==i);
         board.pos[1+square] = pos;
      end

      -- debug

      if(self.iDbg01) then

         for i = 0, board.piece_size[1+colour]-1, 1 do

            sq = board.piece[1+colour][1+i];
            --self:ASSERT(56, board.pos[1+sq]==i);

            if(i == 0) then  -- king
               --self:ASSERT(57, self:PIECE_IS_KING(board.square[1+sq]));
            else
               --self:ASSERT(58, not self:PIECE_IS_KING(board.square[1+sq]));
               --self:ASSERT(59, self.PieceOrder[1+board.square[1+board.piece[1+colour][1+i]]] <=                                                    self.PieceOrder[1+board.square[1+board.piece[1+colour][1+i-1]]]);
            end
         end
      end

      -- pawn list

      for file = 0, self.FileNb-1, 1 do
         board.pawn_file[1+colour][1+file] = 0;
      end

      pos = 0;

      for sq_64 = 0, 63, 1 do

         sq = self.SquareFrom64[1+sq_64];
         piece = board.square[1+sq];

         if(self:COLOUR_IS(piece,colour)  and  self:PIECE_IS_PAWN(piece)) then

            if(pos >= 8  or  self.SquareIsPromote[1+sq]) then
              self:my_fatal("self:board_init_list(): illegal position\n");
            end
            --self:ASSERT(60, pos>=0 and pos<8);

            board.pos[1+sq] = pos;
            board.pawn[1+colour][1+pos] = sq;
            pos = pos + 1;

            board.piece_nb = board.piece_nb + 1;
            board.number[1+self.PieceTo12[1+piece]] = board.number[1+self.PieceTo12[1+piece]] + 1;
            board.pawn_file[1+colour][1+self:SQUARE_FILE(sq)] =
              bit.bor( board.pawn_file[1+colour][1+self:SQUARE_FILE(sq)], self.BitEQ[1+self:PAWN_RANK(sq,colour)]);
         end
      end

      --self:ASSERT(61, pos>=0 and pos<=8);
      board.pawn[1+colour][1+pos] = self.SquareNone;
      board.pawn_size[1+colour] = pos;

      if(board.piece_size[1+colour] + board.pawn_size[1+colour] > 16) then
        self:my_fatal("self:board_init_list(): illegal position\n");
      end
   end

   -- last square

   board.cap_sq = self.SquareNone;

   -- PST

   board.opening = self:board_opening(board);
   board.endgame = self:board_endgame(board);

   -- hash key

   for i = 0, board.ply_nb-1, 1 do
     board.stack[1+i] = 0; -- HACK
   end
   board.sp = board.ply_nb;

   board.key = self:hash_key(board);
   board.pawn_key = self:hash_pawn_key(board);
   board.material_key = self:hash_material_key(board);

   -- legality

   if(not self:board_is_legal(board)) then
     self:my_fatal("self:board_init_list(): illegal position\n");
   end

   -- debug

   --self:ASSERT(62, self:board_is_ok(board));
end

-- self:board_is_legal()

function self:board_is_legal( board ) -- bool

   --self:ASSERT(63, board.sp~=nil);

   return(not self:IS_IN_CHECK(board,self:COLOUR_OPP(board.turn)));
end

-- self:board_is_check()

function self:board_is_check( board ) -- bool

   --self:ASSERT(64, board.sp~=nil);

   return self:IS_IN_CHECK(board,board.turn);
end

-- self:board_is_mate()

function self:board_is_mate( board ) -- bool

   local attack = self:attack_t();   -- attack_t[1]

   --self:ASSERT(65, board.sp~=nil);

   self:attack_set(attack,board);

   if(not self:ATTACK_IN_CHECK(attack)) then
     return false; -- not in check => not mate
   end

   if(self:legal_evasion_exist(board,attack)) then
     return false; -- legal move => not mate
   end

   return true; -- in check and no legal move => mate
end

-- self:board_is_stalemate()

function self:board_is_stalemate( board )  -- bool

   local list = self:list_t();   -- list_t[1];
   local i = 0;      -- int
   local move = 0;   -- int

   --self:ASSERT(66, board.sp~=nil);

   -- init

   if(self:IS_IN_CHECK(board,board.turn)) then
     return false; -- in check => not stalemate
   end

   -- move loop

   self:gen_moves(list,board);

   for i = 0, list.size-1, 1 do
      move = list.move[1+i];
      if(self:pseudo_is_legal(move,board)) then
        return false; -- legal move => not stalemate
      end
   end

   return true; -- in check and no legal move => mate
end

-- self:board_is_repetition()

function self:board_is_repetition( board )  -- bool

   local i = 0;   -- int

   --self:ASSERT(67, board.sp~=nil);

   -- 50-move rule

   if(board.ply_nb >= 100) then -- potential draw

      if(board.ply_nb > 100) then
        return true;
      end

      --self:ASSERT(68, board.ply_nb==100);
      return(not self:board_is_mate(board));
   end

   -- position repetition

   --self:ASSERT(69, board.sp>=board.ply_nb);

   for i = 4, board.ply_nb-1, 2 do
      if(board.stack[1+board.sp-i] == board.key) then
        return true;
      end
   end

   return false;
end

-- self:board_opening()

function self:board_opening( board )  -- int

   local opening = 0;   -- int
   local colour = 0;    -- int
   local ptr = 0;       -- int
   local sq = 0;        -- int
   local piece = 0;     -- int

   --self:ASSERT(70, board.sp~=nil);

   opening = 0;
   for colour = 0, 1, 1 do

      ptr = 0;
      while(true) do
        sq = board.piece[1+colour][1+ptr];
        if(sq==self.SquareNone) then
          break;
        end
        piece = board.square[1+sq];
        opening = opening + self:Pget( self.PieceTo12[1+piece], self.SquareTo64[1+sq], self.Opening );
        ptr = ptr + 1;
      end

      ptr = 0;
      while(true) do
        sq = board.pawn[1+colour][1+ptr];
        if(sq==self.SquareNone) then
          break;
        end
        piece = board.square[1+sq];
        opening = opening + self:Pget( self.PieceTo12[1+piece], self.SquareTo64[1+sq], self.Opening );
        ptr = ptr + 1;
      end

   end

   return opening;
end

-- self:board_endgame()

function self:board_endgame( board )  -- int

   local endgame = 0;   -- int
   local colour = 0;    -- int
   local ptr = 0;       -- int
   local sq = 0;        -- int
   local piece = 0;     -- int

   --self:ASSERT(71, board.sp~=nil);

   endgame = 0;
   for colour = 0, 1, 1 do

      ptr = 0;
      while(true) do
        sq = board.piece[1+colour][1+ptr];
        if(sq==self.SquareNone) then
          break;
        end
        piece = board.square[1+sq];
        endgame = endgame + self:Pget( self.PieceTo12[1+piece], self.SquareTo64[1+sq], self.Endgame );
        ptr = ptr + 1;
      end

      ptr = 0;
      while(true) do
        sq = board.pawn[1+colour][1+ptr];
        if(sq==self.SquareNone) then
          break;
        end
        piece = board.square[1+sq];
        endgame = endgame + self:Pget( self.PieceTo12[1+piece], self.SquareTo64[1+sq], self.Endgame );
        ptr = ptr + 1;
      end

   end

   return endgame;
end

-- end of board.cpp




-- eval.cpp

-- functions

-- self:eval_init()

function self:eval_init()

   local colour = 0;   -- int
   local piece = 0;    -- int

   -- UCI options

   self.PieceActivityWeight =(self:option_get_int("Piece Activity") * 256 + 50) / 100;
   self.KingSafetyWeight    =(self:option_get_int("King Safety")    * 256 + 50) / 100;
   self.PassedPawnWeight    =(self:option_get_int("Passed Pawns")   * 256 + 50) / 100;

   -- mobility table

   for colour = 0, 1, 1 do
      self.MobUnit[1+colour] = {};
      for piece = 0, self.PieceNb-1, 1 do
         self.MobUnit[1+colour][1+piece] = 0;
      end
   end

   self.MobUnit[1+self.White][1+self.Empty] = self.MobMove;

   self.MobUnit[1+self.White][1+self.BP] = self.MobAttack;
   self.MobUnit[1+self.White][1+self.BN] = self.MobAttack;
   self.MobUnit[1+self.White][1+self.BB] = self.MobAttack;
   self.MobUnit[1+self.White][1+self.BR] = self.MobAttack;
   self.MobUnit[1+self.White][1+self.BQ] = self.MobAttack;
   self.MobUnit[1+self.White][1+self.BK] = self.MobAttack;

   self.MobUnit[1+self.White][1+self.WP] = self.MobDefense;
   self.MobUnit[1+self.White][1+self.WN] = self.MobDefense;
   self.MobUnit[1+self.White][1+self.WB] = self.MobDefense;
   self.MobUnit[1+self.White][1+self.WR] = self.MobDefense;
   self.MobUnit[1+self.White][1+self.WQ] = self.MobDefense;
   self.MobUnit[1+self.White][1+self.WK] = self.MobDefense;

   self.MobUnit[1+self.Black][1+self.Empty] = self.MobMove;

   self.MobUnit[1+self.Black][1+self.WP] = self.MobAttack;
   self.MobUnit[1+self.Black][1+self.WN] = self.MobAttack;
   self.MobUnit[1+self.Black][1+self.WB] = self.MobAttack;
   self.MobUnit[1+self.Black][1+self.WR] = self.MobAttack;
   self.MobUnit[1+self.Black][1+self.WQ] = self.MobAttack;
   self.MobUnit[1+self.Black][1+self.WK] = self.MobAttack;

   self.MobUnit[1+self.Black][1+self.BP] = self.MobDefense;
   self.MobUnit[1+self.Black][1+self.BN] = self.MobDefense;
   self.MobUnit[1+self.Black][1+self.BB] = self.MobDefense;
   self.MobUnit[1+self.Black][1+self.BR] = self.MobDefense;
   self.MobUnit[1+self.Black][1+self.BQ] = self.MobDefense;
   self.MobUnit[1+self.Black][1+self.BK] = self.MobDefense;

   -- self.KingAttackUnit[]

   for piece = 0, self.PieceNb-1, 1 do
      self.KingAttackUnit[1+piece] = 0;
   end

   self.KingAttackUnit[1+self.WN] = 1;
   self.KingAttackUnit[1+self.WB] = 1;
   self.KingAttackUnit[1+self.WR] = 2;
   self.KingAttackUnit[1+self.WQ] = 4;

   self.KingAttackUnit[1+self.BN] = 1;
   self.KingAttackUnit[1+self.BB] = 1;
   self.KingAttackUnit[1+self.BR] = 2;
   self.KingAttackUnit[1+self.BQ] = 4;
end

function self:opening_t()
  local b = {};
  b.v = 0;
  return b;
end

function self:endgame_t()
  local b = {};
  b.v = 0;
  return b;
end

-- self:evalpos()

function self:evalpos( board )  -- int

   local opening = self:opening_t();   -- int
   local endgame = self:endgame_t();   -- int
   local mat_info = self:material_info_t();  -- material_info_t[1]
   local pawn_info = self:pawn_info_t();     -- pawn_info_t[1]
   local mul = { 0, 0 };   -- int[self.ColourNb]
   local phase = 0;  -- int
   local eval1 = 0;   -- int
   local wb = 0;     -- int
   local bb = 0;     -- int

   --self:ASSERT(84, board.sp~=nil);

   --self:ASSERT(85, self:board_is_legal(board));
   --self:ASSERT(86, not self:board_is_check(board)); -- exceptions are extremely rare

   -- material

   self:material_get_info(mat_info,board);

   opening.v = opening.v + mat_info.opening;
   endgame.v = endgame.v + mat_info.endgame;

   mul[1+self.White] = mat_info.mul[1+self.White];
   mul[1+self.Black] = mat_info.mul[1+self.Black];

   -- PST

   opening.v = opening.v + board.opening;
   endgame.v = endgame.v + board.endgame;

   -- pawns

   self:pawn_get_info(pawn_info,board);

   opening.v = opening.v + pawn_info.opening;
   endgame.v = endgame.v + pawn_info.endgame;

   -- draw

   self:eval_draw(board,mat_info,pawn_info,mul);

   if(mat_info.mul[1+self.White] < mul[1+self.White]) then
     mul[1+self.White] = mat_info.mul[1+self.White];
   end
   if(mat_info.mul[1+self.Black] < mul[1+self.Black]) then
     mul[1+self.Black] = mat_info.mul[1+self.Black];
   end

   if(mul[1+self.White] == 0  and  mul[1+self.Black] == 0) then
     return self.ValueDraw;
   end

   -- eval

   self:eval_piece(board,mat_info,pawn_info,opening,endgame);
   self:eval_king(board,mat_info,opening,endgame);
   self:eval_passer(board,pawn_info,opening,endgame);
   self:eval_pattern(board,opening,endgame);

   -- phase mix

   phase = mat_info.phase;
   eval1 =((opening.v *(256 - phase)) +(endgame.v * phase)) / 256;

   -- drawish bishop endgames

   if( bit.band( mat_info.flags, self.DrawBishopFlag ) ~= 0) then

      wb = board.piece[1+self.White][1+1];
      --self:ASSERT(87, self:PIECE_IS_BISHOP(board.square[1+wb]));

      bb = board.piece[1+self.Black][1+1];
      --self:ASSERT(88, self:PIECE_IS_BISHOP(board.square[1+bb]));

      if(self:SQUARE_COLOUR(wb) ~= self:SQUARE_COLOUR(bb)) then
         if(mul[1+self.White] == 16) then
           mul[1+self.White] = 8; -- 1/2
         end
         if(mul[1+self.Black] == 16) then
           mul[1+self.Black] = 8; -- 1/2
         end
      end
   end

   -- draw bound

   if(eval1 > self.ValueDraw) then
      eval1 =(eval1 * mul[1+self.White]) / 16;
   else
      if(eval1 < self.ValueDraw) then
        eval1 =(eval1 * mul[1+self.Black]) / 16;
      end
   end

   -- value range

   if(eval1 < -self.ValueEvalInf) then
     eval1 = -self.ValueEvalInf;
   end
   if(eval1 > self.ValueEvalInf) then
     eval1 = self.ValueEvalInf;
   end

   --self:ASSERT(89, eval1>=-self.ValueEvalInf and eval1<=self.ValueEvalInf);

   -- turn

   if(self:COLOUR_IS_BLACK(board.turn)) then
     eval1 = -eval1;
   end

   --self:ASSERT(90, not self:value_is_mate(eval1));

   return eval1;
end

-- self:eval_draw()

function self:eval_draw( board, mat_info, pawn_info, mul ) -- int

   local colour = 0;    -- int
   local me = 0;        -- int
   local opp = 0;       -- int
   local pawn = 0;      -- int
   local king = 0;      -- int
   local pawn_file = 0; -- int
   local prom = 0;      -- int
   local list = {};     -- int list[7+1]
   local ifelse = false;

   --self:ASSERT(91, board.sp~=nil);
   --self:ASSERT(92, mat_info.lock~=nil);
   --self:ASSERT(93, pawn_info.lock~=nil);
   --self:ASSERT(94, mul[1+0]~=nil);

   -- draw patterns

   for colour = 0, 1, 1 do

      me = colour;
      opp = self:COLOUR_OPP(me);

      -- KB*P+K* draw

      if( bit.band( mat_info.cflags[1+me], self.MatRookPawnFlag ) ~= 0 ) then

         pawn = pawn_info.single_file[1+me];

         if(pawn ~= self.SquareNone) then   -- all pawns on one file

            pawn_file = self:SQUARE_FILE(pawn);

            if(pawn_file == self.FileA  or  pawn_file == self.FileH) then

               king = self:KING_POS(board,opp);
               prom = self:PAWN_PROMOTE(pawn,me);

               if(self:DISTANCE(king,prom) <= 1  and( not self:bishop_can_attack(board,prom,me))) then
                  mul[1+me] = 0;
               end
            end
         end
      end

      -- K(B)P+K+ draw

      if( bit.band( mat_info.cflags[1+me], self.MatBishopFlag ) ~= 0) then

         pawn = pawn_info.single_file[1+me];

         if(pawn ~= self.SquareNone) then   -- all pawns on one file

            king = self:KING_POS(board,opp);

            if(self:SQUARE_FILE(king)  == self:SQUARE_FILE(pawn)
              and  self:PAWN_RANK(king,me) >  self:PAWN_RANK(pawn,me)
              and(not self:bishop_can_attack(board,king,me))) then
               mul[1+me] = 1;  -- 1/16
            end
         end
      end

      -- KNPK* draw

      if( bit.band( mat_info.cflags[1+me], self.MatKnightFlag ) ~= 0 ) then

         pawn = board.pawn[1+me][1+0];
         king = self:KING_POS(board,opp);

         if(self:SQUARE_FILE(king)  == self:SQUARE_FILE(pawn)
           and  self:PAWN_RANK(king,me) >  self:PAWN_RANK(pawn,me)
           and  self:PAWN_RANK(pawn,me) <= self.Rank6) then
            mul[1+me] = 1;  -- 1/16
         end
      end
   end

   -- recognisers, only heuristic draws herenot

   ifelse = true;

   if(ifelse and mat_info.recog == self.MAT_KPKQ) then

      -- KPKQ(white)

      self:draw_init_list(list,board,self.White);

      if(self:draw_kpkq(list,board.turn)) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KQKP) then

      -- KPKQ(black)

      self:draw_init_list(list,board,self.Black);

      if(self:draw_kpkq(list,self:COLOUR_OPP(board.turn))) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KPKR) then

      -- KPKR(white)

      self:draw_init_list(list,board,self.White);

      if(self:draw_kpkr(list,board.turn)) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KRKP) then

      -- KPKR(black)

      self:draw_init_list(list,board,self.Black);

      if(self:draw_kpkr(list,self:COLOUR_OPP(board.turn))) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KPKB) then

      -- KPKB(white)

      self:draw_init_list(list,board,self.White);

      if(self:draw_kpkb(list,board.turn)) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KBKP) then

      -- KPKB(black)

      self:draw_init_list(list,board,self.Black);

      if(self:draw_kpkb(list,self:COLOUR_OPP(board.turn))) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KPKN) then

      -- KPKN(white)

      self:draw_init_list(list,board,self.White);

      if(self:draw_kpkn(list,board.turn)) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KNKP) then

      -- KPKN(black)

      self:draw_init_list(list,board,self.Black);

      if(self:draw_kpkn(list,self:COLOUR_OPP(board.turn))) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KNPK) then

      -- KNPK(white)

      self:draw_init_list(list,board,self.White);

      if(self:draw_knpk(list,board.turn)) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KKNP) then

      -- KNPK(black)

      self:draw_init_list(list,board,self.Black);

      if(self:draw_knpk(list,self:COLOUR_OPP(board.turn))) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KRPKR) then

      -- KRPKR(white)

      self:draw_init_list(list,board,self.White);

      if(self:draw_krpkr(list,board.turn)) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KRKRP) then

      -- KRPKR(black)

      self:draw_init_list(list,board,self.Black);

      if(self:draw_krpkr(list,self:COLOUR_OPP(board.turn))) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KBPKB) then

      -- KBPKB(white)

      self:draw_init_list(list,board,self.White);

      if(self:draw_kbpkb(list,board.turn)) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KBKBP) then

      -- KBPKB(black)

      self:draw_init_list(list,board,self.Black);

      if(self:draw_kbpkb(list,self:COLOUR_OPP(board.turn))) then
         mul[1+self.White] = 1; -- 1/16;
         mul[1+self.Black] = 1; -- 1/16;
      end

      ifelse = false;
   end
end

--
function self:add_line( board, me, from, dx )

 local to = from + dx;
 local capture = 0;
 local mob = 0;

 while(true) do
   capture=board.square[1+to];
   if(capture~=self.Empty) then
     break;
   end
   mob = mob + self.MobMove;
   to = to + dx;
 end

 mob = mob + self.MobUnit[1+me][1+capture];

 return mob;
end


-- self:eval_piece()

function self:eval_piece( board, mat_info, pawn_info, opening, endgame )  -- int

   local colour = 0;   -- int
   local op = { 0, 0 };      -- int[self.ColourNb]
   local eg = { 0, 0 };      -- int[self.ColourNb]
   local me = 0;       -- int
   local opp = 0;      -- int
   local opp_flag = 0; -- int
   local ptr = 0;      -- int
   local from = 0;     -- int
   local to = 0;       -- int
   local piece = 0;    -- int
   local mob = 0;      -- int
   local capture = 0;  -- int
   local unit = {};     -- int[]
   local rook_file = 0;  -- int
   local king_file = 0;  -- int
   local king = 0;     -- int
   local delta = 0;    -- int
   local ptype = 0;

   --self:ASSERT(95, board.sp~=nil);
   --self:ASSERT(96, mat_info.lock~=nil);
   --self:ASSERT(97, pawn_info.lock~=nil);
   --self:ASSERT(98, opening.v~=nil);
   --self:ASSERT(99, endgame~=nil);

   -- eval

   for colour = 0, 1, 1 do

      me = colour;
      opp = self:COLOUR_OPP(me);

      opp_flag = self:COLOUR_FLAG(opp);

      unit = self.MobUnit[1+me];

      -- piece loop

      ptr = 1;            -- HACK: no king
      while(true) do
         from = board.piece[1+me][1+ptr];
         if(from==self.SquareNone) then
           break;
         end

         piece = board.square[1+from];

         ptype = self:PIECE_TYPE(piece);

         if(ptype == self.Knight64) then

            -- mobility

            mob = -self.KnightUnit;

            mob = mob + unit[1+board.square[1+from-33]];
            mob = mob + unit[1+board.square[1+from-31]];
            mob = mob + unit[1+board.square[1+from-18]];
            mob = mob + unit[1+board.square[1+from-14]];
            mob = mob + unit[1+board.square[1+from+14]];
            mob = mob + unit[1+board.square[1+from+18]];
            mob = mob + unit[1+board.square[1+from+31]];
            mob = mob + unit[1+board.square[1+from+33]];

            op[1+me] = op[1+me] +(mob * self.KnightMobOpening);
            eg[1+me] = eg[1+me] +(mob * self.KnightMobEndgame);

         end

         if(ptype == self.Bishop64) then

            -- mobility

            mob = -self.BishopUnit;

            mob = mob + self:add_line( board, me, from, -17 );
            mob = mob + self:add_line( board, me, from, -15 );
            mob = mob + self:add_line( board, me, from, 15 );
            mob = mob + self:add_line( board, me, from, 17 );

            op[1+me] = op[1+me] +(mob * self.BishopMobOpening);
            eg[1+me] = eg[1+me] +(mob * self.BishopMobEndgame);

         end

         if(ptype == self.Rook64) then

            -- mobility

            mob = -self.RookUnit;

            mob = mob + self:add_line( board, me, from, -16 );
            mob = mob + self:add_line( board, me, from, -1 );
            mob = mob + self:add_line( board, me, from, 1 );
            mob = mob + self:add_line( board, me, from, 16 );

            op[1+me] = op[1+me] +(mob * self.RookMobOpening);
            eg[1+me] = eg[1+me] +(mob * self.RookMobEndgame);

            -- open file

            if(self.UseOpenFile) then

               op[1+me] = op[1+me] -(self.RookOpenFileOpening / 2);
               eg[1+me] = eg[1+me] -(self.RookOpenFileEndgame / 2);


               rook_file = self:SQUARE_FILE(from);

               if(board.pawn_file[1+me][1+rook_file] == 0) then   -- no friendly pawn

                  op[1+me] = op[1+me] + self.RookSemiOpenFileOpening;
                  eg[1+me] = eg[1+me] + self.RookSemiOpenFileEndgame;

                  if(board.pawn_file[1+opp][1+rook_file] == 0) then  -- no enemy pawn

                     op[1+me] = op[1+me] + self.RookOpenFileOpening - self.RookSemiOpenFileOpening;
                     eg[1+me] = eg[1+me] + self.RookOpenFileEndgame - self.RookSemiOpenFileEndgame;

                  end

                  if( bit.band( mat_info.cflags[1+opp] , self.MatKingFlag ) ~= 0) then

                     king = self:KING_POS(board,opp);
                     king_file = self:SQUARE_FILE(king);

                     delta = math.abs(rook_file-king_file); -- file distance

                     if(delta <= 1) then
                        op[1+me] = op[1+me] + self.RookSemiKingFileOpening;
                        if(delta == 0) then
                          op[1+me] = op[1+me] + self.RookKingFileOpening - self.RookSemiKingFileOpening;
                        end
                     end
                  end
               end
            end

            -- 7th rank

            if(self:PAWN_RANK(from,me) == self.Rank7) then
                  -- if opponent pawn on 7th rank...
               if( bit.band( pawn_info.flags[1+opp], self.BackRankFlag ) ~= 0 or
                     self:PAWN_RANK(self:KING_POS(board,opp),me) == self.Rank8) then
                  op[1+me] = op[1+me] + self.Rook7thOpening;
                  eg[1+me] = eg[1+me] + self.Rook7thEndgame;
               end
            end

         end

         if(ptype == self.Queen64) then

            -- mobility

            mob = -self.QueenUnit;

            mob = mob + self:add_line( board, me, from, -17 );
            mob = mob + self:add_line( board, me, from, -16 );
            mob = mob + self:add_line( board, me, from, -15 );
            mob = mob + self:add_line( board, me, from, -1 );
            mob = mob + self:add_line( board, me, from, 1 );
            mob = mob + self:add_line( board, me, from, 15 );
            mob = mob + self:add_line( board, me, from, 16 );
            mob = mob + self:add_line( board, me, from, 17 );


            op[1+me] = op[1+me] +(mob * self.QueenMobOpening);
            eg[1+me] = eg[1+me] +(mob * self.QueenMobEndgame);

            -- 7th rank

            if(self:PAWN_RANK(from,me) == self.Rank7) then
                  -- if opponent pawn on 7th rank...
               if( bit.band( pawn_info.flags[1+opp], self.BackRankFlag ) ~= 0 or
                     self:PAWN_RANK(self:KING_POS(board,opp),me) == self.Rank8) then
                  op[1+me] = op[1+me] + self.Queen7thOpening;
                  eg[1+me] = eg[1+me] + self.Queen7thEndgame;
               end
            end

         end

        ptr = ptr + 1;
      end
   end

   -- update

   opening.v = opening.v +((op[1+self.White] - op[1+self.Black]) * self.PieceActivityWeight) / 256;
   endgame.v = endgame.v +((eg[1+self.White] - eg[1+self.Black]) * self.PieceActivityWeight) / 256;
end

-- self:eval_king()

function self:eval_king( board, mat_info, opening, endgame)  -- int

   local colour = 0;   -- int
   local op = { 0, 0 }; -- int[self.ColourNb]
   local eg = { 0, 0 }; -- int[self.ColourNb]
   local me = 0;       -- int
   local opp = 0;      -- int
   local from = 0;     -- int

   local penalty_1 = 0;   -- int
   local penalty_2 = 0;   -- int

   local tmp = 0;       -- int
   local penalty = 0;   -- int

   local king = 0;       -- int
   local ptr = 0;        -- int
   local piece = 0;      -- int
   local attack_tot = 0; -- int
   local piece_nb = 0;   -- int

   --self:ASSERT(100, board.sp~=nil);
   --self:ASSERT(101, mat_info.lock~=nil);
   --self:ASSERT(102, opening.v~=nil);
   --self:ASSERT(103, endgame.v~=nil);

   -- king attacks

   if(self.UseKingAttack) then

      for colour = 0, 1, 1 do

         if( bit.band( mat_info.cflags[1+colour], self.MatKingFlag ) ~= 0) then

            me = colour;
            opp = self:COLOUR_OPP(me);

            king = self:KING_POS(board,me);

            -- piece attacks

            attack_tot = 0;
            piece_nb = 0;

            ptr = 1;        -- HACK: no king
            while(true) do
               from=board.piece[1+opp][1+ptr];
               if(from==self.SquareNone) then
                 break;
               end

               piece = board.square[1+from];

               if(self:piece_attack_king(board,piece,from,king)) then
                  piece_nb = piece_nb + 1;
                  attack_tot = attack_tot + self.KingAttackUnit[1+piece];
               end
               ptr = ptr + 1;
            end

            -- scoring

            --self:ASSERT(104, piece_nb>=0 and piece_nb<16);
            op[1+colour] = op[1+colour] -(attack_tot * self.KingAttackOpening * self.KingAttackWeight[1+piece_nb]) / 256;
         end
      end
   end

   -- white pawn shelter

   if(self.UseShelter  and  bit.band( mat_info.cflags[1+self.White], self.MatKingFlag ) ~= 0) then

      me = self.White;

      -- king

      penalty_1 = self:shelter_square(board,self:KING_POS(board,me),me);

      -- castling

      penalty_2 = penalty_1;

      if( bit.band( board.flags, self.FlagsWhiteKingCastle ) ~= 0) then
         tmp = self:shelter_square(board,self.G1,me);
         if(tmp < penalty_2) then
           penalty_2 = tmp;
         end
      end

      if( bit.band( board.flags, self.FlagsWhiteQueenCastle ) ~= 0) then
         tmp = self:shelter_square(board,self.B1,me);
         if(tmp < penalty_2) then
           penalty_2 = tmp;
         end
      end

      --self:ASSERT(105, penalty_2>=0 and penalty_2<=penalty_1);

      -- penalty

      penalty =(penalty_1 + penalty_2) / 2;
      --self:ASSERT(106, penalty>=0);

      op[1+me] = op[1+me] -(penalty * self.ShelterOpening) / 256;
   end

   -- black pawn shelter

   if(self.UseShelter  and  bit.band( mat_info.cflags[1+self.Black], self.MatKingFlag ) ~= 0) then

      me = self.Black;

      -- king

      penalty_1 = self:shelter_square(board,self:KING_POS(board,me),me);

      -- castling

      penalty_2 = penalty_1;

      if( bit.band( board.flags, self.FlagsBlackKingCastle ) ~= 0) then
         tmp = self:shelter_square(board,self.G8,me);
         if(tmp < penalty_2) then
           penalty_2 = tmp;
         end
      end

      if( bit.band( board.flags, self.FlagsBlackQueenCastle ) ~= 0) then
         tmp = self:shelter_square(board,self.B8,me);
         if(tmp < penalty_2) then
           penalty_2 = tmp;
         end
      end

      --self:ASSERT(107, penalty_2>=0 and penalty_2<=penalty_1);

      -- penalty

      penalty =(penalty_1 + penalty_2) / 2;
      --self:ASSERT(108, penalty>=0);

      op[1+me] = op[1+me] -(penalty * self.ShelterOpening) / 256;
   end

   -- update

   opening.v = opening.v +((op[1+self.White] - op[1+self.Black]) * self.KingSafetyWeight) / 256;
   endgame.v = endgame.v +((eg[1+self.White] - eg[1+self.Black]) * self.KingSafetyWeight) / 256;
end

-- self:eval_passer()

function self:eval_passer( board, pawn_info, opening, endgame )  -- int


   local colour = 0;   -- int
   local op = { 0, 0 }; -- int[self.ColourNb]
   local eg = { 0, 0 }; -- int[self.ColourNb]
   local att = 0;      -- int
   local def = 0;      -- int
   local bits = 0;     -- int
   local file = 0;     -- int
   local rank = 0;     -- int
   local sq = 0;       -- int
   local min = 0;      -- int
   local max = 0;      -- int
   local delta = 0     -- int

   --self:ASSERT(109, board.sp~=nil);
   --self:ASSERT(110, pawn_info.lock~=nil);
   --self:ASSERT(111, opening~=nil);
   --self:ASSERT(112, endgame~=nil);


   -- passed pawns

   for colour = 0, 1, 1 do

      att = colour;
      def = self:COLOUR_OPP(att);
      bits = pawn_info.passed_bits[1+att];
      while(true) do
         if(bits == 0) then
           break;
         end

         file = self.BitFirst[1+bits];
         --self:ASSERT(113, file>=self.FileA and file<=self.FileH);

         rank = self.BitLast[1+board.pawn_file[1+att][1+file] ];
         --self:ASSERT(114, rank>=self.Rank2 and rank<=self.Rank7);

         sq = self:SQUARE_MAKE(file,rank);
         if(self:COLOUR_IS_BLACK(att)) then
           sq = self:SQUARE_RANK_MIRROR(sq);
         end

         --self:ASSERT(115, self:PIECE_IS_PAWN(board.square[1+sq]));
         --self:ASSERT(116, self:COLOUR_IS(board.square[1+sq],att));

         -- opening scoring

         op[1+att] = op[1+att] + self:quad(self.PassedOpeningMin,self.PassedOpeningMax,rank);

         -- endgame scoring init

         min = self.PassedEndgameMin;
         max = self.PassedEndgameMax;

         delta = max - min;
         --self:ASSERT(117, delta>0);

         -- "dangerous" bonus

           -- defender has no piece
         if(board.piece_size[1+def] <= 1
           and(self:unstoppable_passer(board,sq,att)  or  self:king_passer(board,sq,att))) then
            delta = delta + self.UnstoppablePasser;
         else
           if(self:free_passer(board,sq,att)) then
            delta = delta + self.FreePasser;
           end
         end

         -- king-distance bonus

         delta = delta -(self:pawn_att_dist(sq,self:KING_POS(board,att),att) * self.AttackerDistance);
         delta = delta +(self:pawn_def_dist(sq,self:KING_POS(board,def),att) * self.DefenderDistance);

         -- endgame scoring

         eg[1+att] = eg[1+att] + min;
         if(delta > 0) then
           eg[1+att] = eg[1+att] + self:quad(0,delta,rank);
         end

         bits = bit.band(bits, bits-1);
      end
   end

   -- update

   opening.v = opening.v +((op[1+self.White] - op[1+self.Black]) * self.PassedPawnWeight) / 256;
   endgame.v = endgame.v +((eg[1+self.White] - eg[1+self.Black]) * self.PassedPawnWeight) / 256;
end

-- self:eval_pattern()

function self:eval_pattern( board, opening, endgame )  -- int

   --self:ASSERT(118, board.sp~=nil);
   --self:ASSERT(119, opening.v~=nil);
   --self:ASSERT(120, endgame.v~=nil);

   -- trapped bishop(7th rank)

   if((board.square[1+self.A7] == self.WB  and  board.square[1+self.B6] == self.BP)
     or(board.square[1+self.B8] == self.WB  and  board.square[1+self.C7] == self.BP)) then
      opening.v = opening.v - self.TrappedBishop;
      endgame.v = endgame.v - self.TrappedBishop;
   end

   if((board.square[1+self.H7] == self.WB  and  board.square[1+self.G6] == self.BP)
     or(board.square[1+self.G8] == self.WB  and  board.square[1+self.F7] == self.BP)) then
      opening.v = opening.v - self.TrappedBishop;
      endgame.v = endgame.v - self.TrappedBishop;
   end

   if((board.square[1+self.A2] == self.BB  and  board.square[1+self.B3] == self.WP)
     or(board.square[1+self.B1] == self.BB  and  board.square[1+self.C2] == self.WP)) then
      opening.v = opening.v + self.TrappedBishop;
      endgame.v = endgame.v + self.TrappedBishop;
   end

   if((board.square[1+self.H2] == self.BB  and  board.square[1+self.G3] == self.WP)
     or(board.square[1+self.G1] == self.BB  and  board.square[1+self.F2] == self.WP)) then
      opening.v = opening.v + self.TrappedBishop;
      endgame.v = endgame.v + self.TrappedBishop;
   end

   -- trapped bishop(6th rank)

   if(board.square[1+self.A6] == self.WB  and  board.square[1+self.B5] == self.BP) then
      opening.v = opening.v -(self.TrappedBishop / 2);
      endgame.v = endgame.v -(self.TrappedBishop / 2);
   end

   if(board.square[1+self.H6] == self.WB  and  board.square[1+self.G5] == self.BP) then
      opening.v = opening.v -(self.TrappedBishop / 2);
      endgame.v = endgame.v -(self.TrappedBishop / 2);
   end

   if(board.square[1+self.A3] == self.BB  and  board.square[1+self.B4] == self.WP) then
      opening.v = opening.v +(self.TrappedBishop / 2);
      endgame.v = endgame.v +(self.TrappedBishop / 2);
   end

   if(board.square[1+self.H3] == self.BB  and  board.square[1+self.G4] == self.WP) then
      opening.v = opening.v +(self.TrappedBishop / 2);
      endgame.v = endgame.v +(self.TrappedBishop / 2);
   end

   -- blocked bishop

   if(board.square[1+self.D2] == self.WP  and  board.square[1+self.D3] ~= self.Empty  and  board.square[1+self.C1] == self.WB) then
      opening.v = opening.v - self.BlockedBishop;
   end

   if(board.square[1+self.E2] == self.WP  and  board.square[1+self.E3] ~= self.Empty  and  board.square[1+self.F1] == self.WB) then
      opening.v = opening.v - self.BlockedBishop;
   end

   if(board.square[1+self.D7] == self.BP  and  board.square[1+self.D6] ~= self.Empty  and  board.square[1+self.C8] == self.BB) then
      opening.v = opening.v + self.BlockedBishop;
   end

   if(board.square[1+self.E7] == self.BP  and  board.square[1+self.E6] ~= self.Empty  and  board.square[1+self.F8] == self.BB) then
      opening.v = opening.v + self.BlockedBishop;
   end

   -- blocked rook

   if((board.square[1+self.C1] == self.WK  or  board.square[1+self.B1] == self.WK)
     and(board.square[1+self.A1] == self.WR  or  board.square[1+self.A2] == self.WR  or  board.square[1+self.B1] == self.WR)) then
      opening.v = opening.v - self.BlockedRook;
   end

   if((board.square[1+self.F1] == self.WK  or  board.square[1+self.G1] == self.WK)
     and(board.square[1+self.H1] == self.WR  or  board.square[1+self.H2] == self.WR  or  board.square[1+self.G1] == self.WR)) then
      opening.v = opening.v - self.BlockedRook;
   end

   if((board.square[1+self.C8] == self.BK  or  board.square[1+self.B8] == self.BK)
     and(board.square[1+self.A8] == self.BR  or  board.square[1+self.A7] == self.BR  or  board.square[1+self.B8] == self.BR)) then
      opening.v = opening.v + self.BlockedRook;
   end

   if((board.square[1+self.F8] == self.BK  or  board.square[1+self.G8] == self.BK)
     and(board.square[1+self.H8] == self.BR  or  board.square[1+self.H7] == self.BR  or  board.square[1+self.G8] == self.BR)) then
      opening.v = opening.v + self.BlockedRook;
   end
end

-- self:unstoppable_passer()

function self:unstoppable_passer( board, pawn, colour )  -- bool

   local me = 0;     -- int
   local opp = 0;    -- int
   local file = 0;   -- int
   local rank = 0;   -- int
   local king = 0;   -- int
   local prom = 0;   -- int
   local ptr = 0;    -- int
   local sq = 0;     -- int
   local dist = 0;   -- int

   --self:ASSERT(121, board.sp~=nil);
   --self:ASSERT(122, self:SQUARE_IS_OK(pawn));
   --self:ASSERT(123, self:COLOUR_IS_OK(colour));

   me = colour;
   opp = self:COLOUR_OPP(me);

   file = self:SQUARE_FILE(pawn);
   rank = self:PAWN_RANK(pawn,me);

   king = self:KING_POS(board,opp);

   -- clear promotion path?


   ptr = 0;
   while(true) do
      sq=board.piece[1+me][1+ptr];
      if(sq==self.SquareNone) then
        break;
      end

      if(self:SQUARE_FILE(sq) == file  and  self:PAWN_RANK(sq,me) > rank) then
         return false; -- "friendly" blocker
      end
      ptr = ptr + 1;
   end


   -- init

   if(rank == self.Rank2) then
      pawn = pawn + self.PawnMoveInc[1+me];
      rank = rank + 1;
      --self:ASSERT(124, rank==self:PAWN_RANK(pawn,me));
   end

   --self:ASSERT(125, rank>=self.Rank3 and rank<=self.Rank7);

   prom = self:PAWN_PROMOTE(pawn,me);

   dist = self:DISTANCE(pawn,prom);
   --self:ASSERT(126, dist==self.Rank8-rank);
   if(board.turn == opp) then
     dist = dist + 1;
   end

   if(self:DISTANCE(king,prom) > dist) then
     return true; -- not in the square
   end

   return false;
end

-- self:king_passer()

function self:king_passer( board, pawn, colour )  -- bool

   local me = 0;     -- int
   local king = 0;   -- int
   local file = 0;   -- int
   local prom = 0;   -- int

   --self:ASSERT(127, board.sp~=nil);
   --self:ASSERT(128, self:SQUARE_IS_OK(pawn));
   --self:ASSERT(129, self:COLOUR_IS_OK(colour));

   me = colour;

   king = self:KING_POS(board,me);
   file = self:SQUARE_FILE(pawn);
   prom = self:PAWN_PROMOTE(pawn,me);

   if(self:DISTANCE(king,prom) <= 1
     and  self:DISTANCE(king,pawn) <= 1
     and(self:SQUARE_FILE(king) ~= file
      or(file ~= self.FileA  and  file ~= self.FileH))) then
      return true;
   end

   return false;
end

-- self:free_passer()

function self:free_passer( board, pawn, colour )  -- bool

   local me = 0;    -- int
   local opp = 0;   -- int
   local inc = 0;   -- int
   local sq = 0;    -- int
   local move = 0;  -- int

   --self:ASSERT(130, board.sp~=nil);
   --self:ASSERT(131, self:SQUARE_IS_OK(pawn));
   --self:ASSERT(132, self:COLOUR_IS_OK(colour));

   me = colour;
   opp = self:COLOUR_OPP(me);

   inc = self.PawnMoveInc[1+me];
   sq = pawn + inc;
   --self:ASSERT(133, self:SQUARE_IS_OK(sq));

   if(board.square[1+sq] ~= self.Empty) then
     return false;
   end

   move = self:MOVE_MAKE(pawn,sq);
   if(self:see_move(move,board) < 0) then
     return false;
   end

   return true;
end

-- self:pawn_att_dist()

function self:pawn_att_dist( pawn, king, colour )  -- int

   local me = 0;      -- int
   local inc = 0;     -- int
   local target = 0;  -- int

   --self:ASSERT(134, self:SQUARE_IS_OK(pawn));
   --self:ASSERT(135, self:SQUARE_IS_OK(king));
   --self:ASSERT(136, self:COLOUR_IS_OK(colour));

   me = colour;
   inc = self.PawnMoveInc[1+me];

   target = pawn + inc;

   return self:DISTANCE(king,target);
end

-- self:pawn_def_dist()

function self:pawn_def_dist( pawn, king, colour )  -- int

   local me = 0;      -- int
   local inc = 0;     -- int
   local target = 0;  -- int

   --self:ASSERT(137, self:SQUARE_IS_OK(pawn));
   --self:ASSERT(138, self:SQUARE_IS_OK(king));
   --self:ASSERT(139, self:COLOUR_IS_OK(colour));

   me = colour;
   inc = self.PawnMoveInc[1+me];

   target = pawn + inc;

   return self:DISTANCE(king,target);
end

-- self:draw_init_list()

function self:draw_init_list( list, board, pawn_colour )  -- int

   local pos = 0;   -- int
   local att = 0;   -- int
   local def = 0;   -- int
   local ptr = 0;   -- int
   local sq = 0;    -- int
   local pawn = 0;  -- int
   local i = 0;     -- int

   --self:ASSERT(141, board.sp~=nil);
   --self:ASSERT(142, self:COLOUR_IS_OK(pawn_colour));

   -- init

   pos = 0;

   att = pawn_colour;
   def = self:COLOUR_OPP(att);

   --self:ASSERT(143, board.pawn_size[1+att]==1);
   --self:ASSERT(144, board.pawn_size[1+def]==0);

   -- att

   ptr = 0;
   while(true) do
      sq=board.piece[1+att][1+ptr];
      if(sq==self.SquareNone) then
        break;
      end
      list[1+pos] = sq;
      pos = pos + 1;
      ptr = ptr + 1;
   end

   ptr = 0;
   while(true) do
      sq=board.pawn[1+att][1+ptr];
      if(sq==self.SquareNone) then
        break;
      end
      list[1+pos] = sq;
      pos = pos + 1;
      ptr = ptr + 1;
   end

   -- def

   ptr = 0;
   while(true) do
      sq=board.piece[1+def][1+ptr];
      if(sq==self.SquareNone) then
        break;
      end
      list[1+pos] = sq;
      pos = pos + 1;
      ptr = ptr + 1;
   end

   ptr = 0;
   while(true) do
      sq=board.pawn[1+def][1+ptr];
      if(sq==self.SquareNone) then
        break;
      end
      list[1+pos] = sq;
      pos = pos + 1;
      ptr = ptr + 1;
   end


   -- end marker

   --self:ASSERT(145, pos==board.piece_nb);

   list[1+pos] = self.SquareNone;

   -- file flip?

   pawn = board.pawn[1+att][1+0];

   if(self:SQUARE_FILE(pawn) >= self.FileE) then
      for i = 0, pos-1, 1 do
         list[1+i] = self:SQUARE_FILE_MIRROR(list[1+i]);
      end
   end

   -- rank flip?

   if(self:COLOUR_IS_BLACK(pawn_colour)) then
      for i = 0, pos-1, 1 do
         list[1+i] = self:SQUARE_RANK_MIRROR(list[1+i]);
      end
   end
end

-- self:draw_kpkq()

function self:draw_kpkq( list, turn )  -- bool

   local wk = 0;       -- int
   local wp = 0;       -- int
   local bk = 0;       -- int
   local bq = 0;       -- int
   local prom = 0;     -- int
   local dist = 0;     -- int
   local wp_file = 0;  -- int
   local wp_rank = 0;  -- int
   local ifelse = false;

   --self:ASSERT(146, list[1+0]~=nil);
   --self:ASSERT(147, self:COLOUR_IS_OK(turn));

   -- load

   wk = list[1+0];
   --self:ASSERT(148, self:SQUARE_IS_OK(wk));

   wp = list[1+1];
   --self:ASSERT(149, self:SQUARE_IS_OK(wp));
   --self:ASSERT(150, self:SQUARE_FILE(wp)<=self.FileD);

   bk =  list[1+2];
   --self:ASSERT(151, self:SQUARE_IS_OK(bk));

   bq =  list[1+3];
   --self:ASSERT(152, self:SQUARE_IS_OK(bq));

   --self:ASSERT(153, list[1+4]==self.SquareNone);

   -- test

   if(wp == self.A7) then

      prom = self.A8;
      dist = 4;

      if(wk == self.B7  or  wk == self.B8) then  -- best case
         if(self:COLOUR_IS_WHITE(turn)) then
           dist = dist - 1;
         end
      else
       if(wk == self.A8  or((wk == self.C7  or  wk == self.C8)  and  bq ~= self.A8)) then    -- white loses a tempo
         if(self:COLOUR_IS_BLACK(turn)  and  self:SQUARE_FILE(bq) ~= self.FileB) then
           return false;
         end
       else
         return false;
       end
      end

      --self:ASSERT(154, bq~=prom);
      if(self:DISTANCE(bk,prom) > dist) then
        return true;
      end
   else
    if(wp == self.C7) then

      prom = self.C8;
      dist = 4;

      ifelse = true;
      if(ifelse and wk == self.C8) then     -- dist = 0

         dist = dist + 1; -- self-blocking penalty
         if(self:COLOUR_IS_WHITE(turn)) then
           dist = dist - 1; -- right-to-move bonus
         end

         ifelse = false;
      end
      if(ifelse and(wk == self.B7  or  wk == self.B8)) then -- dist = 1, right side

         dist = dist - 1; -- right-side bonus
         if(self:DELTA_INC_LINE(wp-bq) == wk-wp) then
           dist = dist + 1; -- pinned-pawn penalty
         end
         if(self:COLOUR_IS_WHITE(turn)) then
           dist = dist - 1; -- right-to-move bonus
         end

         ifelse = false;
      end

      if(ifelse and(wk == self.D7  or  wk == self.D8)) then -- dist = 1, wrong side

         if(self:DELTA_INC_LINE(wp-bq) == wk-wp) then
           dist = dist + 1; -- pinned-pawn penalty
         end
         if(self:COLOUR_IS_WHITE(turn)) then
           dist = dist - 1; -- right-to-move bonus
         end

         ifelse = false;
      end

      if(ifelse and((wk == self.A7  or  wk == self.A8)  and  bq ~= self.C8)) then  -- dist = 2, right side

         if(self:COLOUR_IS_BLACK(turn)  and  self:SQUARE_FILE(bq) ~= self.FileB) then
           return false;
         end

         dist = dist - 1; -- right-side bonus

         ifelse = false;
      end

      if(ifelse and((wk == self.E7  or  wk == self.E8)  and  bq ~= self.C8)) then -- dist = 2, wrong side

         if(self:COLOUR_IS_BLACK(turn)  and  self:SQUARE_FILE(bq) ~= self.FileD) then
           return false;
         end

         ifelse = false;
      end
      if(ifelse) then
         return false;
      end

      --self:ASSERT(155, bq~=prom);
      if(self:DISTANCE(bk,prom) > dist) then
         return true;
      end
   end
  end

   return false;
end

-- self:draw_kpkr()

function self:draw_kpkr( list, turn )  -- bool

   local wk = 0;       -- int
   local wp = 0;       -- int
   local bk = 0;       -- int
   local br = 0;       -- int
   local inc = 0;      -- int
   local prom = 0;     -- int
   local dist = 0;     -- int
   local wk_file = 0;  -- int
   local wk_rank = 0;  -- int
   local wp_file = 0;  -- int
   local wp_rank = 0;  -- int
   local br_file = 0;  -- int
   local br_rank = 0;  -- int


   --self:ASSERT(156, list[1+0]~=nil);
   --self:ASSERT(157, self:COLOUR_IS_OK(turn));

   -- load

   wk = list[1+0];
   --self:ASSERT(158, self:SQUARE_IS_OK(wk));

   wp = list[1+1];
   --self:ASSERT(159, self:SQUARE_IS_OK(wp));
   --self:ASSERT(160, self:SQUARE_FILE(wp)<=self.FileD);

   bk = list[1+2];
   --self:ASSERT(161, self:SQUARE_IS_OK(bk));

   br = list[1+3];
   --self:ASSERT(162, self:SQUARE_IS_OK(br));

   --self:ASSERT(163, list[1+4]==self.SquareNone);

   -- init

   wk_file = self:SQUARE_FILE(wk);
   wk_rank = self:SQUARE_RANK(wk);

   wp_file = self:SQUARE_FILE(wp);
   wp_rank = self:SQUARE_RANK(wp);

   br_file = self:SQUARE_FILE(br);
   br_rank = self:SQUARE_RANK(br);

   inc = self.PawnMoveInc[1+self.White];
   prom = self:PAWN_PROMOTE(wp,self.White);

   -- conditions

   if(self:DISTANCE(wk,wp) == 1) then

      --self:ASSERT(164, math.abs(wk_file-wp_file)<=1);
      --self:ASSERT(165, math.abs(wk_rank-wp_rank)<=1);

      -- no-op

   else
    if(self:DISTANCE(wk,wp) == 2  and  math.abs(wk_rank-wp_rank) <= 1) then

      --self:ASSERT(166, math.abs(wk_file-wp_file)==2);
      --self:ASSERT(167, math.abs(wk_rank-wp_rank)<=1);

      if(self:COLOUR_IS_BLACK(turn)  and  br_file ~=(wk_file + wp_file) / 2) then
        return false;
      end
    else
      return false;
    end
   end

   -- white features

   dist = self:DISTANCE(wk,prom) + self:DISTANCE(wp,prom);
   if(wk == prom) then
     dist = dist + 1;
   end

   if(wk == wp+inc) then  -- king on pawn's "front square"
      if(wp_file == self.FileA) then
        return false;
      end
      dist = dist + 1; -- self-blocking penalty
   end

   -- black features

   if(br_file ~= wp_file  and  br_rank ~= self.Rank8) then
      dist = dist - 1; -- misplaced-rook bonus
   end

   -- test

   if(self:COLOUR_IS_WHITE(turn)) then
      dist = dist - 1; -- right-to-move bonus
   end

   if(self:DISTANCE(bk,prom) > dist) then
      return true;
   end

   return false;
end

-- self:draw_kpkb()

function self:draw_kpkb( list, turn )  -- bool

   local wk = 0;       -- int
   local wp = 0;       -- int
   local bk = 0;       -- int
   local bb = 0;       -- int
   local inc = 0;      -- int
   local en2 = 0;      -- int
   local to = 0;       -- int
   local delta = 0;    -- int
   local inc_2 = 0;    -- int
   local sq = 0;       -- int


   --self:ASSERT(168, list[1+0]~=nil);
   --self:ASSERT(169, self:COLOUR_IS_OK(turn));

   -- load

   wk = list[1+0];
   --self:ASSERT(170, self:SQUARE_IS_OK(wk));

   wp = list[1+1];
   --self:ASSERT(171, self:SQUARE_IS_OK(wp));
   --self:ASSERT(172, self:SQUARE_FILE(wp)<=self.FileD);

   bk = list[1+2];
   --self:ASSERT(173, self:SQUARE_IS_OK(bk));

   bb = list[1+3];
   --self:ASSERT(174, self:SQUARE_IS_OK(bb));

   --self:ASSERT(175, list[1+4]==self.SquareNone);

   -- blocked pawn?

   inc = self.PawnMoveInc[1+self.White];
   en2 = self:PAWN_PROMOTE(wp,self.White) + inc;

   to = wp+inc;
   while(to ~= en2) do

      --self:ASSERT(176, self:SQUARE_IS_OK(to));

      if(to == bb) then
        return true; -- direct blockade
      end

      delta = to - bb;
      --self:ASSERT(177, self:delta_is_ok(delta));

      if(self:PSEUDO_ATTACK(self.BB,delta)) then

         inc_2 = self:DELTA_INC_ALL(delta);
         --self:ASSERT(178, inc_2~=self.IncNone);

         sq = bb;
         while(true) do

            sq = sq + inc_2;
            --self:ASSERT(179, self:SQUARE_IS_OK(sq));
            --self:ASSERT(180, sq~=wk);
            --self:ASSERT(181, sq~=wp);
            --self:ASSERT(182, sq~=bb);
            if(sq == to) then
              return true; -- indirect blockade
            end
            if(sq == bk) then
              break;
            end

         end
      end
     to = to + inc;
   end

   return false;
end

-- self:draw_kpkn()

function self:draw_kpkn( list, turn )  -- bool

   local wk = 0;       -- int
   local wp = 0;       -- int
   local bk = 0;       -- int
   local bn = 0;       -- int
   local inc = 0;      -- int
   local en2 = 0;      -- int
   local file = 0;     -- int
   local sq = 0;       -- int


   --self:ASSERT(183, list[1+0]~=nil);
   --self:ASSERT(184, self:COLOUR_IS_OK(turn));

   -- load

   wk = list[1+0];
   --self:ASSERT(185, self:SQUARE_IS_OK(wk));

   wp = list[1+1];
   --self:ASSERT(186, self:SQUARE_IS_OK(wp));
   --self:ASSERT(187, self:SQUARE_FILE(wp)<=self.FileD);

   bk = list[1+2];
   --self:ASSERT(188, self:SQUARE_IS_OK(bk));

   bn = list[1+3];
   --self:ASSERT(189, self:SQUARE_IS_OK(bn));

   --self:ASSERT(190, list[1+4]==self.SquareNone);

   -- blocked pawn?

   inc = self.PawnMoveInc[1+self.White];
   en2 = self:PAWN_PROMOTE(wp,self.White) + inc;

   file = self:SQUARE_FILE(wp);
   if(file == self.FileA  or  file == self.FileH) then
     en2 = en2 - inc;
   end

   sq = wp+inc;
   while(sq ~= en2) do

      --self:ASSERT(191, self:SQUARE_IS_OK(sq));

      if(sq == bn  or  self:PSEUDO_ATTACK(self.BN,sq-bn)) then
        return true; -- blockade
      end

      sq = sq + inc;
   end

   return false;
end

-- self:draw_knpk()

function self:draw_knpk( list, turn )  -- bool

   local wk = 0;       -- int
   local wn = 0;       -- int
   local wp = 0;       -- int
   local bk = 0;       -- int


   --self:ASSERT(192, list[1+0]~=nil);
   --self:ASSERT(193, self:COLOUR_IS_OK(turn));

   -- load

   wk = list[1+0];
   --self:ASSERT(194, self:SQUARE_IS_OK(wk));

   wn = list[1+1];
   --self:ASSERT(195, self:SQUARE_IS_OK(wn));

   wp = list[1+2];
   --self:ASSERT(196, self:SQUARE_IS_OK(wp));
   --self:ASSERT(197, self:SQUARE_FILE(wp)<=self.FileD);

   bk = list[1+3];
   --self:ASSERT(198, self:SQUARE_IS_OK(bk));

   --self:ASSERT(199, list[1+4]==self.SquareNone);

   -- test

   if(wp == self.A7  and  self:DISTANCE(bk,self.A8) <= 1) then
     return true;
   end

   return false;
end

-- self:draw_krpkr()

function self:draw_krpkr( list, turn )  -- bool

   local wk = 0;       -- int
   local wr = 0;       -- int
   local wp = 0;       -- int
   local bk = 0;       -- int
   local br = 0;       -- int

   local wp_file = 0;  -- int
   local wp_rank = 0;  -- int
   local bk_file = 0;  -- int
   local bk_rank = 0;  -- int
   local br_file = 0;  -- int
   local br_rank = 0;  -- int

   local prom = 0;     -- int

   --self:ASSERT(200, list[1+0]~=nil);
   --self:ASSERT(201, self:COLOUR_IS_OK(turn));

   -- load

   wk = list[1+0];
   --self:ASSERT(202, self:SQUARE_IS_OK(wk));

   wr = list[1+1];
   --self:ASSERT(203, self:SQUARE_IS_OK(wr));

   wp = list[1+2];
   --self:ASSERT(204, self:SQUARE_IS_OK(wp));
   --self:ASSERT(205, self:SQUARE_FILE(wp)<=self.FileD);

   bk = list[1+3];
   --self:ASSERT(206, self:SQUARE_IS_OK(bk));

   br = list[1+4];
   --self:ASSERT(207, self:SQUARE_IS_OK(br));

   --self:ASSERT(208, list[1+5]==self.SquareNone);

   -- test

   wp_file = self:SQUARE_FILE(wp);
   wp_rank = self:SQUARE_RANK(wp);

   bk_file = self:SQUARE_FILE(bk);
   bk_rank = self:SQUARE_RANK(bk);

   br_file = self:SQUARE_FILE(br);
   br_rank = self:SQUARE_RANK(br);

   prom = self:PAWN_PROMOTE(wp,self.White);

   if(bk == prom) then

      -- TODO: rook near self.Rank1 if wp_rank == self.Rank6?

      if(br_file > wp_file) then
        return true;
      end

   else
    if(bk_file == wp_file  and  bk_rank > wp_rank) then

      return true;

    else
     if(wr == prom  and  wp_rank == self.Rank7  and(bk == self.G7  or  bk == self.H7)  and  br_file == wp_file) then

      if(br_rank <= self.Rank3) then
         if(self:DISTANCE(wk,wp) > 1) then
           return true;
         end
      else -- br_rank >= self.Rank4
         if(self:DISTANCE(wk,wp) > 2) then
           return true;
         end
      end
     end
    end
   end

   return false;
end

-- self:draw_kbpkb()

function self:draw_kbpkb( list, turn )  -- bool

   local wk = 0;       -- int
   local wb = 0;       -- int
   local wp = 0;       -- int
   local bk = 0;       -- int
   local bb = 0;       -- int

   local inc = 0;      -- int
   local en2 = 0;      -- int
   local to = 0;       -- int
   local delta = 0;    -- int
   local inc_2 = 0;    -- int
   local sq = 0;       -- int


   --self:ASSERT(209, list[1+0]~=nil);
   --self:ASSERT(210, self:COLOUR_IS_OK(turn));

   -- load

   wk = list[1+0];
   --self:ASSERT(211, self:SQUARE_IS_OK(wk));

   wb = list[1+1];
   --self:ASSERT(212, self:SQUARE_IS_OK(wb));

   wp = list[1+2];
   --self:ASSERT(213, self:SQUARE_IS_OK(wp));
   --self:ASSERT(214, self:SQUARE_FILE(wp)<=self.FileD);

   bk = list[1+3];
   --self:ASSERT(215, self:SQUARE_IS_OK(bk));

   bb = list[1+4];
   --self:ASSERT(216, self:SQUARE_IS_OK(bb));

   --self:ASSERT(217, list[1+5]==self.SquareNone);

   -- opposit colour?

   if(self:SQUARE_COLOUR(wb) == self:SQUARE_COLOUR(bb)) then
     return false; -- TODO
   end

   -- blocked pawn?

   inc = self.PawnMoveInc[1+self.White];
   en2 = self:PAWN_PROMOTE(wp,self.White) + inc;

   to = wp+inc;
   while( to ~= en2 ) do

      --self:ASSERT(218, self:SQUARE_IS_OK(to));

      if(to == bb) then
        return true; -- direct blockade
      end

      delta = to - bb;
      --self:ASSERT(219, self:delta_is_ok(delta));

      if(self:PSEUDO_ATTACK(self.BB,delta)) then

         inc_2 = self:DELTA_INC_ALL(delta);
         --self:ASSERT(220, inc_2~=self.IncNone);

         sq = bb;
         while(true) do
            sq = sq + inc_2;
            --self:ASSERT(221, self:SQUARE_IS_OK(sq));
            --self:ASSERT(222, sq~=wk);
            --self:ASSERT(223, sq~=wb);
            --self:ASSERT(224, sq~=wp);
            --self:ASSERT(225, sq~=bb);
            if(sq == to) then
              return true; -- indirect blockade
            end
            if(sq == bk) then
              break;
            end
         end
      end
      to = to + inc;
   end

   return false;
end

-- self:shelter_square()

function self:shelter_square( board, square, colour )  -- int

   local penalty = 0;   -- int
   local file = 0;      -- int
   local rank = 0;      -- int

   --self:ASSERT(226, board.sp~=nil);
   --self:ASSERT(227, self:SQUARE_IS_OK(square));
   --self:ASSERT(228, self:COLOUR_IS_OK(colour));

   penalty = 0;

   file = self:SQUARE_FILE(square);
   rank = self:PAWN_RANK(square,colour);

   penalty = penalty +( self:shelter_file(board,file,rank,colour) * 2 );
   if(file ~= self.FileA) then
     penalty = penalty + self:shelter_file(board,file-1,rank,colour);
   end
   if(file ~= self.FileH) then
     penalty = penalty + self:shelter_file(board,file+1,rank,colour);
   end

   if(penalty == 0) then
     penalty = 11; -- weak back rank
   end

   if(self.UseStorm) then
      penalty = penalty + self:storm_file(board,file,colour);
      if(file ~= self.FileA) then
        penalty = penalty + self:storm_file(board,file-1,colour);
      end
      if(file ~= self.FileH) then
        penalty = penalty + self:storm_file(board,file+1,colour);
      end
   end

   return penalty;
end

-- self:shelter_file()

function self:shelter_file( board, file, rank, colour )  -- int

   local dist = 0;      -- int
   local penalty = 0;   -- int

   --self:ASSERT(229, board.sp~=nil);
   --self:ASSERT(230, file>=self.FileA and file<=self.FileH);
   --self:ASSERT(231, rank>=self.Rank1 and rank<=self.Rank8);
   --self:ASSERT(232, self:COLOUR_IS_OK(colour));

   dist = self.BitFirst[1+ bit.band( board.pawn_file[1+colour][1+file], self.BitGE[1+rank]) ];
   --self:ASSERT(233, dist>=self.Rank2 and dist<=self.Rank8);

   dist = self.Rank8 - dist;
   --self:ASSERT(234, dist>=0 and dist<=6);

   penalty = 36 -(dist * dist);
   --self:ASSERT(235, penalty>=0 and penalty<=36);

   return penalty;
end

-- self:storm_file()

function self:storm_file( board, file, colour )  -- int

   local dist = 0;      -- int
   local penalty = 0;   -- int

   --self:ASSERT(236, board.sp~=nil);
   --self:ASSERT(237, file>=self.FileA and file<=self.FileH);
   --self:ASSERT(238, self:COLOUR_IS_OK(colour));

   dist = self.BitLast[1+board.pawn_file[1+self:COLOUR_OPP(colour)][1+file] ];
   --self:ASSERT(239, dist>=self.Rank1 and dist<=self.Rank7);

   penalty = 0;

   if(dist == self.Rank4) then
      penalty = self.StormOpening * 1;
   else
    if(dist == self.Rank5) then
      penalty = self.StormOpening * 3;
    else
     if(dist == self.Rank6) then
      penalty = self.StormOpening * 6;
     end
    end
   end

   return penalty;
end

-- self:bishop_can_attack()

function self:bishop_can_attack( board, to, colour )  -- bool

   local ptr = 0;    -- int
   local from = 0;   -- int
   local piece = 0;  -- int

   --self:ASSERT(240, board.sp~=nil);
   --self:ASSERT(241, self:SQUARE_IS_OK(to));
   --self:ASSERT(242, self:COLOUR_IS_OK(colour));

   ptr = 1;                -- HACK: no king
   while(true) do
      from = board.piece[1+colour][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      if(self:PIECE_IS_BISHOP(piece)  and  self:SQUARE_COLOUR(from) == self:SQUARE_COLOUR(to)) then
         return true;
      end
      ptr = ptr + 1;
   end

   return false;
end

-- end of eval.cpp



-- fen.cpp

-- functions

function self:if_fen_err( logic, fenstr, pos )
 if(logic) then
  -- suppressed: self:my_fatal("self:board_from_fen(): bad FEN " .. fenstr .. " at pos=" .. self:formatd(pos) .. " \n");
 end
end

-- Guard: return true if c is nil or empty (FEN truncated / malformed)
function self:fen_char_invalid( c )
 return c == nil or c == ""
end

-- self:board_from_fen()

function self:board_from_fen( board, fenstr ) -- void

   local pos = 0;   -- int
   local file = 0;  -- int
   local rank = 0;  -- int
   local sq = 0;    -- int
   local c = " ";   -- char
   local nb = "";   -- string
   local i = 0;     -- int
   local len = 0;   -- int
   local piece = 0; -- int
   local pawn = 0;  -- int
   local fen = {};  -- char[]
   local gotoupdate = false;

   --self:ASSERT(243, board.sp~=nil);
   --self:ASSERT(244, fen~=nil);

   self:board_clear(board);

   for i = 0, string.len( fenstr ), 1 do
     fen[1+i] = string.sub( fenstr, 1+i, 1+i );
   end
   -- Return "" for out-of-bounds access so nil comparisons never crash on bad FENs
   setmetatable(fen, { __index = function() return "" end })

   pos = 0;
   c = fen[1+pos];

   -- piece placement

   for rank = self.Rank8, self.Rank1, -1 do

      file = self.FileA;
      while( file <= self.FileH ) do

         if(c >= "1"  and  c <= "8") then            -- empty square(s)

            len =(string.byte(c,1) - string.byte("0",1));

            for i = 0, len-1, 1 do
               self:if_fen_err( file > self.FileH, fenstr, pos );

               board.square[1+self:SQUARE_MAKE(file,rank)] = self.Empty;
               file = file + 1;
            end

         else    -- piece

            piece = self:piece_from_char(c);
            self:if_fen_err( piece == self.PieceNone256, fenstr, pos );

            board.square[1+self:SQUARE_MAKE(file,rank)] = piece;
            file = file + 1;
         end

         pos = pos + 1;
         c = fen[1+pos];
      end

      if(rank > self.Rank1) then
         self:if_fen_err( c ~= "/", fenstr, pos );
         pos = pos + 1;
         c = fen[1+pos];

      end
   end

   -- active colour

   self:if_fen_err( c ~= " ", fenstr, pos );

   pos = pos + 1;
   c = fen[1+pos];

   if(c=="w") then
      board.turn = self.White;
   else
    if(c=="b") then
      board.turn = self.Black;
    else
      self:if_fen_err( true, fenstr, pos );
    end
   end

   pos = pos + 1;
   c = fen[1+pos];

   -- castling

   self:if_fen_err( c ~= " ", fenstr, pos );

   pos = pos + 1;
   c = fen[1+pos];

   board.flags = self.FlagsNone;

   if(c == "-") then    -- no castling rights

      pos = pos + 1;
      c = fen[1+pos];

   else


      if(c == "K") then
         if(board.square[1+self.E1] == self.WK  and  board.square[1+self.H1] == self.WR) then
            board.flags = bit.bor( board.flags, self.FlagsWhiteKingCastle );
         end
         pos = pos + 1;
         c = fen[1+pos];
      end

      if(c == "Q") then
         if(board.square[1+self.E1] == self.WK  and  board.square[1+self.A1] == self.WR) then
           board.flags = bit.bor( board.flags, self.FlagsWhiteQueenCastle );
         end
         pos = pos + 1;
         c = fen[1+pos];
      end

      if(c == "k") then
         if(board.square[1+self.E8] == self.BK  and  board.square[1+self.H8] == self.BR) then
           board.flags = bit.bor( board.flags, self.FlagsBlackKingCastle );
         end
         pos = pos + 1;
         c = fen[1+pos];
      end

      if(c == "q") then
         if(board.square[1+self.E8] == self.BK  and  board.square[1+self.A8] == self.BR) then
           board.flags = bit.bor( board.flags, self.FlagsBlackQueenCastle );
         end
         pos = pos + 1;
         c = fen[1+pos];
      end
   end

   -- en-passant

   self:if_fen_err( c ~= " ", fenstr, pos );

   pos = pos + 1;
   c = fen[1+pos];

   if(c == "-") then   -- no en-passant

      sq = self.SquareNone;
      pos = pos + 1;
      c = fen[1+pos];

   else

      self:if_fen_err( c < "a"  or  c > "h", fenstr, pos );
      file = self:file_from_char(c);
      pos = pos + 1;
      c = fen[1+pos];

      self:if_fen_err( c ~= self:iif(self:COLOUR_IS_WHITE(board.turn) , "6" , "3"), fenstr, pos );

      rank = self:rank_from_char(c);
      pos = pos + 1;
      c = fen[1+pos];

      sq = self:SQUARE_MAKE(file,rank);
      pawn = self:SQUARE_EP_DUAL(sq);

      if(board.square[1+sq] ~= self.Empty
        or  board.square[1+pawn] ~= self.PawnMake[1+self:COLOUR_OPP(board.turn)]
        or(board.square[1+pawn-1] ~= self.PawnMake[1+board.turn]
         and  board.square[1+pawn+1] ~= self.PawnMake[1+board.turn])) then
         sq = self.SquareNone;
      end
   end

   board.ep_square = sq;

   -- halfmove clock

   board.ply_nb = 0;
   board.movenumb = 0;

   if(c ~= " ") then
      if(not self.Strict) then
        gotoupdate = true;
      else
        self:if_fen_err( true, fenstr, pos );
      end
   end

   if( not gotoupdate ) then
     pos = pos + 1;
     c = fen[1+pos];

     if(c<"0" or c>"9") then
        if(not self.Strict) then
          gotoupdate = true;
        else
          self:if_fen_err( true, fenstr, pos );
        end
     end
   end

   if( not gotoupdate ) then
      nb = self:str_after_ok( string.sub( fenstr, 1+pos ), " ");  -- ignore halfmove clock
      board.ply_nb = tonumber( nb ) or 0;
      board.movenumb = board.ply_nb;  -- just save it
   end

   -- board update

   -- update:
   self:board_init_list(board);
end

-- self:board_to_fen()

function self:board_to_fen( board, strfen )  -- bool

   local file = 0;   -- int
   local rank = 0;   -- int
   local sq = 0;     -- int
   local piece = 0;  -- int
   local c = " ";    -- string
   local len = 0;    -- int
   local fen = "";   -- string
   local str1 = self:string_t()

   --self:ASSERT(245, board.sp~=nil);

   -- piece placement

   for rank = self.Rank8, self.Rank1, -1 do

      file = self.FileA;
      while( file <= self.FileH ) do

         sq = self:SQUARE_MAKE(file,rank);
         piece = board.square[1+sq];
         --self:ASSERT(248, piece==self.Empty or self:piece_is_ok(piece));

         if(piece == self.Empty) then

            len = 0;
            while( file <= self.FileH  and  board.square[1+self:SQUARE_MAKE(file,rank)] == self.Empty ) do

               file = file + 1;
               len = len + 1;
            end

            --self:ASSERT(249, len>=1 and len<=8);
            c = string.format( "%c", string.byte("0",1) + len );

         else

            c = self:piece_to_char(piece);
            file = file + 1;
         end

         fen = fen .. c;

      end

      if( rank ~= self.Rank1 ) then
        fen = fen .. "/";
      end
   end

   -- active colour

   fen = fen .. " " .. self:iif(self:COLOUR_IS_WHITE(board.turn) , "w", "b" ) .. " ";

   -- castling

   if(board.flags == self.FlagsNone) then
      fen = fen .. "-";
   else
      if( bit.band( board.flags, self.FlagsWhiteKingCastle) ~= 0) then
        fen = fen .. "K";
      end
      if( bit.band( board.flags, self.FlagsWhiteQueenCastle) ~= 0) then
        fen = fen .. "Q";
      end
      if( bit.band( board.flags, self.FlagsBlackKingCastle) ~= 0) then
        fen = fen .. "k";
      end
      if( bit.band( board.flags, self.FlagsBlackQueenCastle) ~= 0) then
        fen = fen .. "q";
      end
   end

   fen = fen .. " ";

   -- en-passant

   if(board.ep_square == self.SquareNone) then
      fen = fen .. "-";
   else
      self:square_to_string(board.ep_square, str1 );
      fen = fen .. str1.v;
   end

   fen = fen .. " ";

   -- ignoring halfmove clock

   fen = fen .. "0 " .. self:formatd(board.movenumb );

   strfen.v = fen;

   return true;
end

-- to see on screen

function self:printboard() -- void

   local file = 0;   -- int
   local rank = 0;   -- int
   local sq = 0;     -- int
   local piece = 0;  -- int
   local str1 = self:string_t();
   local s = "";     --  string
   local board = self.SearchInput.board;

   -- piece placement

   for rank = self.Rank8, self.Rank1, -1 do

      file = self.FileA;
      while( file <= self.FileH ) do

         sq = self:SQUARE_MAKE(file,rank);
         piece = board.square[1+sq];
         --self:ASSERT(248, piece==self.Empty or self:piece_is_ok(piece));

         if(piece == self.Empty) then
           s = s .. ".";
         else
           s = s .. self:piece_to_char(piece);
         end
		 s = s .. " ";

         file = file + 1;
      end

      s = s .. "\n";
   end

   self:board_to_fen( board, str1 );

   s = s .. str1.v .. "\n";

   self:log(s);

end

-- end of fen.cpp


-- hash.cpp

-- 64-bit functions for 32-bit reality, we accept collisions for slower interpreter


-- functions

-- self:hash_init()

function self:hash_init()

   local i = 0;   -- int

   for i = 0, 15, 1 do
     self.Castle64[1+i] = self:hash_castle_key(i);
   end
end

-- self:hash_key()

function self:hash_key( board )    -- uint64
   local key = 0     -- uint64;
   local colour = 0; -- int
   local ptr = 0;    -- int
   local sq = 0;     -- int
   local piece = 0;  -- int

   --self:ASSERT(250, board.sp~=nil);

   -- init

   key = 0;

   -- pieces

   for colour = 0, 1, 1 do

      ptr = 0;
      while(true) do
         sq=board.piece[1+colour][1+ptr];
         if(sq== self.SquareNone) then
           break;
         end

         piece = board.square[1+sq];
         key = bit.bxor( key, self:hash_piece_key(piece,sq) );

         ptr = ptr + 1;
      end


      ptr = 0;
      while(true) do
         sq=board.pawn[1+colour][1+ptr];
         if(sq== self.SquareNone) then
           break;
         end

         piece = board.square[1+sq];
         key = bit.bxor( key, self:hash_piece_key(piece,sq) );

         ptr = ptr + 1;
      end

   end

   -- castle flags

   key = bit.bxor( key, self:hash_castle_key(board.flags) );

   -- en-passant square

   sq = board.ep_square;
   if(sq ~= self.SquareNone) then
      key = bit.bxor( key, self:hash_ep_key(sq) );
   end

   -- turn

   key = bit.bxor( key, self:hash_turn_key(board.turn) );

   return key;
end

-- self:hash_pawn_key()

function self:hash_pawn_key( board ) -- uint64

   local key = 0     -- uint64;
   local colour = 0; -- int
   local ptr = 0;    -- int
   local sq = 0;     -- int
   local piece = 0;  -- int

   --self:ASSERT(251, board.sp~=nil);

   -- init

   key = 0;

   -- pawns

   for colour = 0, 1, 1 do

      ptr = 0;
      while(true) do
         sq=board.pawn[1+colour][1+ptr];
         if(sq== self.SquareNone) then
           break;
         end

         piece = board.square[1+sq];
         key = bit.bxor( key, self:hash_piece_key(piece,sq) );

         ptr = ptr + 1;
      end

   end

   return key;
end

-- self:hash_material_key()

function self:hash_material_key( board ) -- uint64

   local key = 0     -- uint64;
   local piece = 0;  -- int
   local count = 0;  -- int

   --self:ASSERT(252, board.sp~=nil);

   -- init

   key = 0;

   -- counters

   for piece_12 = 0, 11, 1 do
      count = board.number[1+piece_12];
      key = bit.bxor( key, self:hash_counter_key(piece_12,count) );
   end

   return key;

end

-- self:hash_piece_key()

function self:hash_piece_key( piece, square )  -- uint64

   --self:ASSERT(253, self:piece_is_ok(piece));
   --self:ASSERT(254, self:SQUARE_IS_OK(square));

   return self.Random64[1+self.RandomPiece+bit.bxor(self.PieceTo12[1+piece],1)*64 + self.SquareTo64[1+square] ];
             -- HACK: xor 1 for PolyGlot book(not lua)
end

-- self:hash_castle_key()

function self:hash_castle_key( flags )  -- uint64

   local key = 0     -- uint64;
   local i = 0;      -- int

   --self:ASSERT(255, bit.band(flags,self.bnotF)==0);

   key = 0;

   for i = 0, 3, 1 do
      if( bit.band( flags, bit.lshift(1,i) ) ~= 0) then
        key = bit.bxor( key, self.Random64[1+self.RandomCastle+i] );
      end
   end

   return key;
end

-- self:hash_ep_key()

function self:hash_ep_key( square )  -- uint64

   --self:ASSERT(256, self:SQUARE_IS_OK(square));

   return self.Random64[1+self.RandomEnPassant+self:SQUARE_FILE(square)-self.FileA ];
end

-- self:hash_turn_key()

function self:hash_turn_key( colour )  -- uint64

   --self:ASSERT(257, self:COLOUR_IS_OK(colour));

   return self:iif(self:COLOUR_IS_WHITE(colour) , self.Random64[1+self.RandomTurn] , 0 );
end

-- self:hash_counter_key()

function self:hash_counter_key( piece_12, count )  -- uint64

   local key = 0     -- uint64;
   local i = 0;      -- int
   local index = 0;  -- int

   --self:ASSERT(258, piece_12>=0 and piece_12<12);
   --self:ASSERT(259, count>=0 and count<=10);

   -- init

   key = 0;

   -- counter

   index = piece_12 * 16;
   for i = 0, count-1, 1 do
     key = bit.bxor( key, self.Random64[1+index+i] );
   end

   return key;

end

-- end of hash.cpp





-- list.cpp

-- functions

-- self:list_is_ok()

function self:list_is_ok( list )  -- bool

   if(list.size == nil) then
     return false;
   end

   if(list.size < 0  or  list.size >= self.ListSize) then
     return false;
   end

   return true;
end

-- self:list_remove()

function self:list_remove( list, pos )  -- int

   local i = 0;   -- int

   --self:ASSERT(260, self:list_is_ok(list));
   --self:ASSERT(261, pos>=0 and pos<list.size);

   for i = pos, list.size-2, 1 do
      list.move[1+i] = list.move[1+i+1];
      list.value[1+i] = list.value[1+i+1];
   end

   list.size = list.size - 1;
end

-- self:list_copy()

function self:list_copy( dst, src )  -- void

   local i = 0;   -- int

   --self:ASSERT(262, dst.size~=nil);
   --self:ASSERT(263, self:list_is_ok(src));

   dst.size = src.size;

   for i = 0, src.size-1, 1 do
      dst.move[1+i] = src.move[1+i];
      dst.value[1+i] = src.value[1+i];
   end
end

-- self:list_sort()

function self:list_sort( list )  -- void

   local size = 0;   -- int
   local i = 0;      -- int
   local j = 0;      -- int
   local move = 0;   -- int
   local value = 0;  -- int

   --self:ASSERT(264, self:list_is_ok(list));

   -- init

   size = list.size;
   list.value[1+size] = -32768; -- HACK: sentinel

   -- insert sort(stable)

   for i = size-2, 0, -1 do

      move = list.move[1+i];
      value = list.value[1+i];

      j = i;
      while( value < list.value[1+j+1] ) do
         list.move[1+j] = list.move[1+j+1];
         list.value[1+j] = list.value[1+j+1];
         j = j + 1;
      end

      --self:ASSERT(265, j<size);

      list.move[1+j] = move;
      list.value[1+j] = value;
   end

   -- debug

   if(self.iDbg01) then
      for i = 0, size-2, 1 do
         --self:ASSERT(266, list.value[1+i]>=list.value[1+i+1]);
      end
   end
end

-- self:list_contain()

function self:list_contain( list, move )  -- bool

   local i = 0;   -- int

   --self:ASSERT(267, self:list_is_ok(list));
   --self:ASSERT(268, self:move_is_ok(move));

   for i = 0, list.size-1, 1 do
      if(list.move[1+i] == move) then
        return true;
      end
   end

   return false;
end

-- self:list_note()

function self:list_note( list )  -- void

   local i = 0;      -- int
   local move = 0;   -- int

   --self:ASSERT(269, self:list_is_ok(list));

   for i = 0, list.size-1, 1 do
      move = list.move[1+i];
      --self:ASSERT(270, self:move_is_ok(move));
      list.value[1+i] = -self:move_order(move);
   end
end

-- self:list_filter()

function self:list_filter( list, board, keep )  -- bool

   local pos = 0;   -- int
   local i = 0;     -- int
   local move = 0;  -- int
   local value = 0; -- int

   --self:ASSERT(271, list.size~=nil);
   --self:ASSERT(272, board.sp~=nil);
   ----self:ASSERT(273, test~=nil);
   --self:ASSERT(274, keep==true or keep==false);

   pos = 0;

   for i = 0, list.size-1, 1 do

      --self:ASSERT(275, pos>=0 and pos<=i);

      move = list.move[1+i];
      value = list.value[1+i];

      if(self:pseudo_is_legal(move,board) == keep) then
         list.move[1+pos] = move;
         list.value[1+pos] = value;
         pos = pos + 1;
      end
   end

   --self:ASSERT(276, pos>=0 and pos<=list.size);
   list.size = pos;

   -- debug

   --self:ASSERT(277, self:list_is_ok(list));
end

-- end of list.cpp




-- material.cpp

-- functions

-- self:material_init()

function self:material_init()

   -- UCI options

   self.MaterialWeight =(self:option_get_int("self.Material") * 256 + 50) / 100;

   -- material table

   self.Material.size = 0;
   self.Material.mask = 0;
end

-- self:material_alloc()

function self:material_alloc()

   --self:ASSERT(278, true);   -- sizeof(entry_t)==16

   if(self.UseTable) then

      self.Material.size = self.MaterialTableSize;
      self.Material.mask = self.Material.size - 1;   -- 2^x -1
      -- self.Material.table =(entry_t *) my_malloc(self.Material.size*sizeof(entry_t));

      self:material_clear();
   end

end

-- self:material_clear()

function self:material_clear()

   local i = 0;

   self.Material.table = {};
   self.Material.used = 0;
   self.Material.read_nb = 0;
   self.Material.read_hit = 0;
   self.Material.write_nb = 0;
   self.Material.write_collision = 0;

end

-- self:material_get_info()

function self:material_get_info( info, board )  -- void

   local key = 0;             -- uint64
   local entry = nil;         -- entry_t *
   local index = 0;

   --self:ASSERT(279, info.lock~=nil);
   --self:ASSERT(280, board.sp~=nil);

   -- probe

   if(self.UseTable) then

      self.Material.read_nb = self.Material.read_nb + 1;

      key = board.material_key;
      index = bit.band( self:KEY_INDEX(key), self.Material.mask );

      entry = self.Material.table[1+index];

      if(entry == nil or entry.lock == nil) then
        self.Material.table[1+index] = self:material_info_t();
        entry = self.Material.table[1+index];
      end

      if(entry.lock == self:KEY_LOCK(key)) then

         -- found

         self.Material.read_hit = self.Material.read_hit + 1;

         self:material_info_copy( info, entry );

         return;
      end
   end

   -- calculation

   self:material_comp_info(info,board);

   -- store

   if(self.UseTable) then

      self.Material.write_nb = self.Material.write_nb + 1;

      if(entry.lock == 0) then     -- HACK: assume free entry
         self.Material.used = self.Material.used + 1;
      else
         self.Material.write_collision = self.Material.write_collision + 1;
      end

      self:material_info_copy( entry, info );

      entry.lock = self:KEY_LOCK(key);
   end

end

-- self:material_comp_info()

function self:material_comp_info( info,  board)  -- void

   local wp = 0;   -- int
   local wn = 0;   -- int
   local wb = 0;   -- int
   local wr = 0;   -- int
   local wq = 0;   -- int
   local bp = 0;   -- int
   local bn = 0;   -- int
   local bb = 0;   -- int
   local br = 0;   -- int
   local bq = 0;   -- int

   local wt = 0;   -- int
   local bt = 0;   -- int
   local wm = 0;   -- int
   local bm = 0;   -- int

   local colour = 0;  -- int
   local recog = 0;   -- int
   local flags = 0;   -- int
   local cflags = { 0, 0 }; -- int[self.ColourNb]
   local mul = { 16, 16 };    -- int[self.ColourNb]
   local phase = 0;   -- int
   local opening = 0; -- int
   local endgame = 0; -- int
   local ifelse = false;

   --self:ASSERT(281, info.lock~=nil);
   --self:ASSERT(282, board.sp~=nil);

   -- init

   wp = board.number[1+self.WhitePawn12];
   wn = board.number[1+self.WhiteKnight12];
   wb = board.number[1+self.WhiteBishop12];
   wr = board.number[1+self.WhiteRook12];
   wq = board.number[1+self.WhiteQueen12];

   bp = board.number[1+self.BlackPawn12];
   bn = board.number[1+self.BlackKnight12];
   bb = board.number[1+self.BlackBishop12];
   br = board.number[1+self.BlackRook12];
   bq = board.number[1+self.BlackQueen12];

   wt = wq + wr + wb + wn + wp; -- no king
   bt = bq + br + bb + bn + bp; -- no king

   wm = wb + wn;
   bm = bb + bn;

   local w_maj = wq * 2 + wr;         -- int
   local w_min = wb + wn;             -- int
   local w_tot = w_maj * 2 + w_min;   -- int

   local b_maj = bq * 2 + br;         -- int
   local b_min = bb + bn;             -- int
   local b_tot = b_maj * 2 + b_min;   -- int

   -- recogniser

   recog = self.MAT_NONE;

   ifelse = true;

   if(ifelse and(wt == 0  and  bt == 0)) then

      recog = self.MAT_KK;

      ifelse = false;
   end

   if(ifelse and(wt == 1  and  bt == 0)) then

      if(wb == 1) then
        recog = self.MAT_KBK;
      end
      if(wn == 1) then
        recog = self.MAT_KNK;
      end
      if(wp == 1) then
        recog = self.MAT_KPK;
      end

      ifelse = false;
   end

   if(ifelse and(wt == 0  and  bt == 1)) then

      if(bb == 1) then
        recog = self.MAT_KKB;
      end
      if(bn == 1) then
        recog = self.MAT_KKN;
      end
      if(bp == 1) then
        recog = self.MAT_KKP;
      end

      ifelse = false;
   end

   if(ifelse and(wt == 1  and  bt == 1)) then

      if(wq == 1  and  bq == 1) then
        recog = self.MAT_KQKQ;
      end
      if(wq == 1  and  bp == 1) then
        recog = self.MAT_KQKP;
      end
      if(wp == 1  and  bq == 1) then
        recog = self.MAT_KPKQ;
      end
      if(wr == 1  and  br == 1) then
        recog = self.MAT_KRKR;
      end
      if(wr == 1  and  bp == 1) then
        recog = self.MAT_KRKP;
      end
      if(wp == 1  and  br == 1) then
        recog = self.MAT_KPKR;
      end
      if(wb == 1  and  bb == 1) then
        recog = self.MAT_KBKB;
      end
      if(wb == 1  and  bp == 1) then
        recog = self.MAT_KBKP;
      end
      if(wp == 1  and  bb == 1) then
        recog = self.MAT_KPKB;
      end
      if(wn == 1  and  bn == 1) then
        recog = self.MAT_KNKN;
      end
      if(wn == 1  and  bp == 1) then
        recog = self.MAT_KNKP;
      end
      if(wp == 1  and  bn == 1) then
        recog = self.MAT_KPKN;
      end

      ifelse = false;
   end

   if(ifelse and(wt == 2  and  bt == 0)) then

      if(wb == 1  and  wp == 1) then
        recog = self.MAT_KBPK;
      end
      if(wn == 1  and  wp == 1) then
        recog = self.MAT_KNPK;
      end

      ifelse = false;
   end

   if(ifelse and(wt == 0  and  bt == 2)) then

      if(bb == 1  and  bp == 1) then
        recog = self.MAT_KKBP;
      end
      if(bn == 1  and  bp == 1) then
        recog = self.MAT_KKNP;
      end

      ifelse = false;
   end

   if(ifelse and(wt == 2  and  bt == 1)) then

      if(wr == 1  and  wp == 1  and  br == 1) then
        recog = self.MAT_KRPKR;
      end
      if(wb == 1  and  wp == 1  and  bb == 1) then
        recog = self.MAT_KBPKB;
      end

      ifelse = false;
   end

   if(ifelse and(wt == 1  and  bt == 2)) then

      if(wr == 1  and  br == 1  and  bp == 1) then
        recog = self.MAT_KRKRP;
      end
      if(wb == 1  and  bb == 1  and  bp == 1) then
        recog = self.MAT_KBKBP;
      end

      ifelse = false;
   end

   -- draw node(exact-draw recogniser)

   flags = 0; -- TODO: MOVE ME

            -- if no major piece or pawn
   if(wq+wr+wp == 0  and  bq+br+bp == 0) then
         -- at most one minor => KK, KBK or KNK
      if(wm + bm <= 1 or  recog == self.MAT_KBKB) then
         flags = bit.bor( flags, self.DrawNodeFlag );
      end

   else
     if(recog == self.MAT_KPK   or  recog == self.MAT_KKP or  recog == self.MAT_KBPK  or  recog == self.MAT_KKBP) then
       flags = bit.bor( flags, self.DrawNodeFlag );
     end
   end

   -- bishop endgame
            -- if only bishops
   if(wq+wr+wn == 0  and  bq+br+bn == 0) then
      if(wb == 1  and  bb == 1) then
         if(wp-bp >= -2  and  wp-bp <= 2) then    -- pawn diff <= 2
            flags = bit.bor( flags, self.DrawBishopFlag );
         end
      end
   end

   -- white multiplier

   if(wp == 0) then  -- white has no pawns

      ifelse = true;
      if(ifelse and(w_tot == 1)) then

         --self:ASSERT(283, w_maj==0);
         --self:ASSERT(284, w_min==1);

         -- KBK* or KNK*, always insufficient

         mul[1+self.White] = 0;


         ifelse = false;
      end

      if(ifelse and(w_tot == 2  and  wn == 2)) then

         --self:ASSERT(285, w_maj==0);
         --self:ASSERT(286, w_min==2);

         -- KNNK*, usually insufficient

         if(b_tot ~= 0  or  bp == 0) then
            mul[1+self.White] = 0;
         else    -- KNNKP+, might not be draw
            mul[1+self.White] = 1; -- 1/16
         end

         ifelse = false;
      end

      if(ifelse and(w_tot == 2  and  wb == 2  and  b_tot == 1  and  bn == 1)) then

         --self:ASSERT(287, w_maj==0);
         --self:ASSERT(288, w_min==2);
         --self:ASSERT(289, b_maj==0);
         --self:ASSERT(290, b_min==1);

         -- KBBKN*, barely drawish(not at all?)

         mul[1+self.White] = 8; -- 1/2

         ifelse = false;
      end

      if(ifelse and(w_tot-b_tot <= 1  and  w_maj <= 2)) then

         -- no more than 1 minor up, drawish

         mul[1+self.White] = 2; -- 1/8
         ifelse = false;
      end

   else

    if(wp == 1) then -- white has one pawn

      if(b_min ~= 0) then

         -- assume black sacrifices a minor against the lone pawn

         b_min = b_min - 1;
         b_tot = b_tot + 1;

         ifelse = true;
         if(ifelse and(w_tot == 1)) then

            --self:ASSERT(291, w_maj==0);
            --self:ASSERT(292, w_min==1);

            -- KBK* or KNK*, always insufficient

            mul[1+self.White] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(w_tot == 2  and  wn == 2)) then

            --self:ASSERT(293, w_maj==0);
            --self:ASSERT(294, w_min==2);

            -- KNNK*, usually insufficient

            mul[1+self.White] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(w_tot-b_tot <= 1  and  w_maj <= 2)) then

            -- no more than 1 minor up, drawish

            mul[1+self.White] = 8; -- 1/2

            ifelse = false;
         end

      else
       if(br ~= 0) then

         -- assume black sacrifices a rook against the lone pawn

         b_maj = b_maj - 1;
         b_tot = b_tot - 2;

         ifelse = true;
         if(ifelse and(w_tot == 1)) then

            --self:ASSERT(295, w_maj==0);
            --self:ASSERT(296, w_min==1);

            -- KBK* or KNK*, always insufficient

            mul[1+self.White] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(w_tot == 2  and  wn == 2)) then

            --self:ASSERT(297, w_maj==0);
            --self:ASSERT(298, w_min==2);

            -- KNNK*, usually insufficient

            mul[1+self.White] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(w_tot-b_tot <= 1  and  w_maj <= 2)) then

            -- no more than 1 minor up, drawish

            mul[1+self.White] = 8; -- 1/2

            ifelse = false;
         end

       end
      end

    end
   end

   -- black multiplier

   if(bp == 0) then    -- black has no pawns


      ifelse = true;
      if(ifelse and(b_tot == 1)) then

         --self:ASSERT(299, b_maj==0);
         --self:ASSERT(300, b_min==1);

         -- KBK* or KNK*, always insufficient

         mul[1+self.Black] = 0;

         ifelse = false;
      end

      if(ifelse and(b_tot == 2  and  bn == 2)) then

         --self:ASSERT(301, b_maj==0);
         --self:ASSERT(302, b_min==2);

         -- KNNK*, usually insufficient

         if(w_tot ~= 0  or  wp == 0) then
            mul[1+self.Black] = 0;
         else   -- KNNKP+, might not be draw
            mul[1+self.Black] = 1; -- 1/16
         end

         ifelse = false;
      end

      if(ifelse and(b_tot == 2  and  bb == 2  and  w_tot == 1  and  wn == 1)) then

         --self:ASSERT(303, b_maj==0);
         --self:ASSERT(304, b_min==2);
         --self:ASSERT(305, w_maj==0);
         --self:ASSERT(306, w_min==1);

         -- KBBKN*, barely drawish(not at all?)

         mul[1+self.Black] = 8; -- 1/2

         ifelse = false;
      end

      if(ifelse and(b_tot-w_tot <= 1  and  b_maj <= 2)) then

         -- no more than 1 minor up, drawish

         mul[1+self.Black] = 2; -- 1/8

         ifelse = false;
      end

   else
    if(bp == 1) then  -- black has one pawn

      if(w_min ~= 0) then

         -- assume white sacrifices a minor against the lone pawn

         w_min = w_min - 1;
         w_tot = w_tot - 1;

         ifelse = true;
         if(ifelse and(b_tot == 1)) then

            --self:ASSERT(307, b_maj==0);
            --self:ASSERT(308, b_min==1);

            -- KBK* or KNK*, always insufficient

            mul[1+self.Black] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(b_tot == 2  and  bn == 2)) then

            --self:ASSERT(309, b_maj==0);
            --self:ASSERT(310, b_min==2);

            -- KNNK*, usually insufficient

            mul[1+self.Black] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(b_tot-w_tot <= 1  and  b_maj <= 2)) then

            -- no more than 1 minor up, drawish

            mul[1+self.Black] = 8; -- 1/2

            ifelse = false;
         end

      else
       if(wr ~= 0) then

         -- assume white sacrifices a rook against the lone pawn

         w_maj = w_maj - 1;
         w_tot = w_tot - 2;

         ifelse = true;
         if(ifelse and(b_tot == 1)) then

            --self:ASSERT(311, b_maj==0);
            --self:ASSERT(312, b_min==1);

            -- KBK* or KNK*, always insufficient

            mul[1+self.Black] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(b_tot == 2  and  bn == 2)) then

            --self:ASSERT(313, b_maj==0);
            --self:ASSERT(314, b_min==2);

            -- KNNK*, usually insufficient

            mul[1+self.Black] = 4; -- 1/4

            ifelse = false;
         end

         if(ifelse and(b_tot-w_tot <= 1  and  b_maj <= 2)) then

            -- no more than 1 minor up, drawish

            mul[1+self.Black] = 8; -- 1/2

            ifelse = false;
         end

       end
      end
    end
   end

   -- potential draw for white

   if(wt == wb+wp  and  wp >= 1) then
     cflags[1+self.White] = bit.bor( cflags[1+self.White], self.MatRookPawnFlag );
   end
   if(wt == wb+wp  and  wb <= 1  and  wp >= 1  and  bt > bp) then
     cflags[1+self.White] = bit.bor( cflags[1+self.White], self.MatBishopFlag );
   end

   if(wt == 2  and  wn == 1  and  wp == 1  and  bt > bp) then
     cflags[1+self.White] = bit.bor( cflags[1+self.White], self.MatKnightFlag );
   end

   -- potential draw for black

   if(bt == bb+bp  and  bp >= 1) then
     cflags[1+self.Black] = bit.bor( cflags[1+self.Black], self.MatRookPawnFlag );
   end
   if(bt == bb+bp  and  bb <= 1  and  bp >= 1  and  wt > wp) then
     cflags[1+self.Black] = bit.bor( cflags[1+self.Black], self.MatBishopFlag );
   end

   if(bt == 2  and  bn == 1  and  bp == 1  and  wt > wp) then
     cflags[1+self.Black] = bit.bor( cflags[1+self.Black], self.MatKnightFlag );
   end

   -- draw leaf(likely draw)

   if(recog == self.MAT_KQKQ  or  recog == self.MAT_KRKR) then
      mul[1+self.White] = 0;
      mul[1+self.Black] = 0;
   end

   -- king safety

   if(bq >= 1  and  bq+br+bb+bn >= 2) then
     cflags[1+self.White] = bit.bor( cflags[1+self.White], self.MatKingFlag );
   end
   if(wq >= 1  and  wq+wr+wb+wn >= 2) then
     cflags[1+self.Black] = bit.bor( cflags[1+self.Black], self.MatKingFlag );
   end

   -- phase(0: opening . 256: endgame)

   phase = self.TotalPhase;

   phase = phase -(wp * self.PawnPhase);
   phase = phase -(wn * self.KnightPhase);
   phase = phase -(wb * self.BishopPhase);
   phase = phase -(wr * self.RookPhase);
   phase = phase -(wq * self.QueenPhase);

   phase = phase -(bp * self.PawnPhase);
   phase = phase -(bn * self.KnightPhase);
   phase = phase -(bb * self.BishopPhase);
   phase = phase -(br * self.RookPhase);
   phase = phase -(bq * self.QueenPhase);

   if(phase < 0) then
     phase = 0;
   end

   --self:ASSERT(315, phase>=0 and phase<=self.TotalPhase);
   phase = math.min(((phase * 256) +(self.TotalPhase / 2)) / self.TotalPhase, 256 );

   --self:ASSERT(316, phase>=0 and phase<=256);

   -- material

   opening = 0;
   endgame = 0;

   opening = opening +(wp * self.PawnOpening);
   opening = opening +(wn * self.KnightOpening);
   opening = opening +(wb * self.BishopOpening);
   opening = opening +(wr * self.RookOpening);
   opening = opening +(wq * self.QueenOpening);

   opening = opening -(bp * self.PawnOpening);
   opening = opening -(bn * self.KnightOpening);
   opening = opening -(bb * self.BishopOpening);
   opening = opening -(br * self.RookOpening);
   opening = opening -(bq * self.QueenOpening);

   endgame = endgame +(wp * self.PawnEndgame);
   endgame = endgame +(wn * self.KnightEndgame);
   endgame = endgame +(wb * self.BishopEndgame);
   endgame = endgame +(wr * self.RookEndgame);
   endgame = endgame +(wq * self.QueenEndgame);

   endgame = endgame -(bp * self.PawnEndgame);
   endgame = endgame -(bn * self.KnightEndgame);
   endgame = endgame -(bb * self.BishopEndgame);
   endgame = endgame -(br * self.RookEndgame);
   endgame = endgame -(bq * self.QueenEndgame);

   -- bishop pair

   if(wb >= 2) then     -- HACK: assumes different colours
      opening = opening + self.BishopPairOpening;
      endgame = endgame + self.BishopPairEndgame;
   end

   if(bb >= 2) then     -- HACK: assumes different colours
      opening = opening - self.BishopPairOpening;
      endgame = endgame - self.BishopPairEndgame;
   end

   -- store info

   info.recog = recog;
   info.flags = flags;

   for colour = 0, 1, 1 do
     info.cflags[1+colour] = cflags[1+colour];
     info.mul[1+colour] = mul[1+colour];
   end

   info.phase = phase;
   info.opening =(opening * self.MaterialWeight) / 256;
   info.endgame =(endgame * self.MaterialWeight) / 256;
end

-- end of material.cpp



-- move.cpp

-- functions

-- self:move_is_ok()

function self:move_is_ok( move )  -- bool

   if(move < 0  or  move >= 65536 or move == self.MoveNone or move == self.Movenil) then
     return false;
   end
   return true;
end

-- self:move_promote()

function self:move_promote( move )  -- int

   local code = 0;   -- int
   local piece = 0;  -- int

   --self:ASSERT(317, self:move_is_ok(move));

   --self:ASSERT(318, self:MOVE_IS_PROMOTE(move));

   code = bit.band( bit.rshift(move,12), 3 );
   piece = self.PromotePiece[1+code];

   if(self:SQUARE_RANK(self:MOVE_TO(move)) == self.Rank8) then
      piece = bit.bor( piece, self.WhiteFlag );
   else
      --self:ASSERT(319, self:SQUARE_RANK(self:MOVE_TO(move))==self.Rank1);
      piece = bit.bor( piece, self.BlackFlag );
   end

   --self:ASSERT(320, self:piece_is_ok(piece));

   return piece;
end

-- self:move_order()

function self:move_order( move )  -- int

   --self:ASSERT(321, self:move_is_ok(move));

   return bit.bor( bit.lshift( bit.band(move,self.V07777),2 ), bit.band( bit.rshift(move,12),3 ) );
end

-- self:move_is_capture()

function self:move_is_capture( move, board )  -- bool

   --self:ASSERT(322, self:move_is_ok(move));
   --self:ASSERT(323, board.sp~=nil);

   return self:MOVE_IS_EN_PASSANT(move) or(board.square[1+self:MOVE_TO(move)] ~= self.Empty);
end

-- self:move_is_under_promote()

function self:move_is_under_promote( move )  -- bool

   --self:ASSERT(324, self:move_is_ok(move));

   return self:MOVE_IS_PROMOTE(move) and( bit.band( move, self.MoveAllFlags ) ~= self.MovePromoteQueen );
end

-- self:move_is_tactical()

function self:move_is_tactical( move, board )  -- bool

   --self:ASSERT(325, self:move_is_ok(move));
   --self:ASSERT(326, board.sp~=nil);

   return( bit.band(move,bit.lshift(1,15))~= 0 )  or(board.square[1+self:MOVE_TO(move)] ~= self.Empty); -- HACK
end

-- self:move_capture()


function self:move_capture( move, board )  -- int

   --self:ASSERT(327, self:move_is_ok(move));
   --self:ASSERT(328, board.sp~=nil);

   if(self:MOVE_IS_EN_PASSANT(move)) then
      return self:PAWN_OPP(board.square[1+self:MOVE_FROM(move)]);
   end

   return board.square[1+self:MOVE_TO(move)];
end

-- self:move_to_string()

function self:move_to_string( move, str1 )  -- bool

   local str2 = self:string_t();

   --self:ASSERT(329, move==self.Movenil or self:move_is_ok(move));
   --self:ASSERT(330, str1.v~=nil);

   -- nil move

   if(move == self.Movenil) then
      return true;
   end

   -- normal moves

   str1.v = "";
   self:square_to_string( self:MOVE_FROM(move), str2 );
   str1.v = str1.v .. str2.v;
   self:square_to_string( self:MOVE_TO(move), str2 );
   str1.v = str1.v .. str2.v;
   --self:ASSERT(332, string.len(str1.v)==4);

   -- promotes

   if(self:MOVE_IS_PROMOTE(move)) then
      str1.v = str1.v .. string.lower( self:piece_to_char(self:move_promote(move)) );
   end

   return true;
end

-- self:move_from_string()

function self:move_from_string( str1, board )  -- int

   local str2 = self:string_t();
   local c = " ";         -- char;

   local from = 0;        -- int
   local to = 0;          -- int
   local move = 0;        -- int
   local piece = 0;       -- int
   local delta = 0;       -- int

   --self:ASSERT(333, str1.v~=nil);
   --self:ASSERT(334, board.sp~=nil);

   -- from

   str2.v = string.sub( str1.v, 1, 2 );

   from = self:square_from_string(str2);
   if(from == self.SquareNone) then
     return self.MoveNone;
   end

   -- to

   str2.v = string.sub( str1.v, 3, 4 );

   to = self:square_from_string(str2);
   if(to == self.SquareNone) then
     return self.MoveNone;
   end

   move = self:MOVE_MAKE(from,to);

   -- promote

   if( string.len( str1.v )>4 ) then
     c = string.sub( str1.v, 5, 5 );
     if(c=="n") then
       move = bit.bor( move, self.MovePromoteKnight );
     end
     if(c=="b") then
       move = bit.bor( move, self.MovePromoteBishop );
     end
     if(c=="r") then
       move = bit.bor( move, self.MovePromoteRook );
     end
     if(c=="q") then
       move = bit.bor( move, self.MovePromoteQueen );
     end
   end

   -- flags

   piece = board.square[1+from];

   if(self:PIECE_IS_PAWN(piece)) then
      if(to == board.ep_square) then
        move = bit.bor( move, self.MoveEnPassant );
      end
   else
    if(self:PIECE_IS_KING(piece)) then
      delta = to - from;
      if(delta == 2  or  delta == -2) then
        move = bit.bor( move, self.MoveCastle );
      end
    end
   end

   return move;
end

-- end of move.cpp




-- move_check.cpp

-- functions

-- self:gen_quiet_checks()

function self:gen_quiet_checks( list,  board ) -- void

   --self:ASSERT(335, list.size~=nil);
   --self:ASSERT(336, board.sp~=nil);

   --self:ASSERT(337, not self:board_is_check(board));

   list.size=0;

   self:add_quiet_checks(list,board);
   self:add_castle_checks(list,board);

   -- debug

   --self:ASSERT(338, self:list_is_ok(list));
end

-- self:add_quiet_checks()

function self:add_quiet_checks( list,  board ) -- void

   local me = 0;    -- int
   local opp = 0;   -- int
   local king = 0;  -- int

   local ptr = 0;   -- int
   local ptr_2 = 0; -- int

   local from = 0;  -- int
   local to = 0;    -- int
   local sq = 0;    -- int

   local piece = 0;    -- int
   local inc_ptr = 0;  -- int
   local inc = 0;      -- int
   local pawn = 0;   -- int
   local rank = 0;   -- int
   local pin = {};   -- int[8+1]
   local gotonextpiece = false;

   --self:ASSERT(339, list.size~=nil);
   --self:ASSERT(340, board.sp~=nil);

   -- init

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   king = self:KING_POS(board,opp);

   self:find_pins(pin,board);

   -- indirect checks

   ptr = 0;
   while(true) do
      from = pin[1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      --self:ASSERT(341, self:is_pinned(board,from,opp));

      if(self:PIECE_IS_PAWN(piece)) then

         inc = self.PawnMoveInc[1+me];
         rank = self:PAWN_RANK(from,me);

         if(rank ~= self.Rank7) then    -- promotes are generated with captures
            to = from + inc;
            if(board.square[1+to] == self.Empty) then
               if(self:DELTA_INC_LINE(to-king) ~= self:DELTA_INC_LINE(from-king)) then
                  --self:ASSERT(342, not self.SquareIsPromote[1+to]);
                  self:LIST_ADD(list,self:MOVE_MAKE(from,to));
                  if(rank == self.Rank2) then
                     to = from +(2*inc);
                     if(board.square[1+to] == self.Empty) then
                        --self:ASSERT(343, self:DELTA_INC_LINE(to-king)~=self:DELTA_INC_LINE(from-king));
                        --self:ASSERT(344, not self.SquareIsPromote[1+to]);
                        self:LIST_ADD(list,self:MOVE_MAKE(from,to));
                     end
                  end
               end
            end
         end

      else
       if(self:PIECE_IS_SLIDER(piece)) then

         inc_ptr = 0;
         while(true) do
           inc = self.PieceInc[1+piece][1+inc_ptr];
           if( inc == self.IncNone ) then
             break;
           end

           to = from+inc;
           while(true) do

               if( board.square[1+to] ~= self.Empty ) then
                 break;
               end

               --self:ASSERT(345, self:DELTA_INC_LINE(to-king)~=self:DELTA_INC_LINE(from-king));
               self:LIST_ADD(list,self:MOVE_MAKE(from,to));

               to = to + inc;
           end
           inc_ptr = inc_ptr + 1;
         end

       else

         inc_ptr = 0;
         while(true) do
           inc = self.PieceInc[1+piece][1+inc_ptr];
           if( inc == self.IncNone ) then
             break;
           end

           to = from + inc;
           if(board.square[1+to] == self.Empty) then
               if(self:DELTA_INC_LINE(to-king) ~= self:DELTA_INC_LINE(from-king)) then
                  self:LIST_ADD(list,self:MOVE_MAKE(from,to));
               end
           end

           inc_ptr = inc_ptr + 1;
         end

       end
      end
      ptr = ptr + 1;
   end

   -- piece direct checks

   ptr = 1;       -- HACK: no king
   while(true) do
      from = board.piece[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      ptr_2 = 0;
      while(true) do
        sq = pin[1+ptr_2];
        if( sq == self.SquareNone ) then
          break;
        end

        if(sq == from) then
          gotonextpiece = true;
          break;
        end

        ptr_2 = ptr_2 + 1;
      end

      if(gotonextpiece) then

        gotonextpiece = false;

      else

       --self:ASSERT(346, not self:is_pinned(board,from,opp));

       piece = board.square[1+from];

       if(self:PIECE_IS_SLIDER(piece)) then

         inc_ptr = 0;
         while(true) do
           inc = self.PieceInc[1+piece][1+inc_ptr];
           if( inc == self.IncNone ) then
             break;
           end

           to = from+inc;
           while(true) do

               if( board.square[1+to] ~= self.Empty ) then
                 break;
               end

               if(self:PIECE_ATTACK(board,piece,to,king)) then
                  self:LIST_ADD(list,self:MOVE_MAKE(from,to));
               end

               to = to + inc;
           end
           inc_ptr = inc_ptr + 1;
         end


       else

         inc_ptr = 0;
         while(true) do
           inc = self.PieceInc[1+piece][1+inc_ptr];
           if( inc == self.IncNone ) then
             break;
           end

           to = from + inc;
           if(board.square[1+to] == self.Empty) then
               if(self:PSEUDO_ATTACK(piece,king-to)) then
                  self:LIST_ADD(list,self:MOVE_MAKE(from,to));
               end
           end

           inc_ptr = inc_ptr + 1;
         end

       end

      end

-- next_piece:

      ptr = ptr + 1;
   end

   -- pawn direct checks

   inc = self.PawnMoveInc[1+me];
   pawn = self.PawnMake[1+me];

   to = king -(inc-1);
   --self:ASSERT(347, self:PSEUDO_ATTACK(pawn,king-to));

   from = to - inc;
   if(board.square[1+from] == pawn) then
      if(board.square[1+to] == self.Empty) then
         --self:ASSERT(348, not self.SquareIsPromote[1+to]);
         self:LIST_ADD(list,self:MOVE_MAKE(from,to));
      end
   else
      from = to -(2*inc);
      if(board.square[1+from] == pawn) then
         if(self:PAWN_RANK(from,me) == self.Rank2
           and  board.square[1+to] == self.Empty
           and  board.square[1+from+inc] == self.Empty) then
            --self:ASSERT(349, not self.SquareIsPromote[1+to]);
            self:LIST_ADD(list,self:MOVE_MAKE(from,to));
         end
      end
   end

   to = king -(inc+1);
   --self:ASSERT(350, self:PSEUDO_ATTACK(pawn,king-to));

   from = to - inc;
   if(board.square[1+from] == pawn) then
      if(board.square[1+to] == self.Empty) then
         --self:ASSERT(351, not self.SquareIsPromote[1+to]);
         self:LIST_ADD(list,self:MOVE_MAKE(from,to));
      end
   else
      from = to -(2*inc);
      if(board.square[1+from] == pawn) then
         if(self:PAWN_RANK(from,me) == self.Rank2
           and  board.square[1+to] == self.Empty
           and  board.square[1+from+inc] == self.Empty) then
            --self:ASSERT(352, not self.SquareIsPromote[1+to]);
            self:LIST_ADD(list,self:MOVE_MAKE(from,to));
         end
      end
   end

end

-- self:add_castle_checks()

function self:add_castle_checks( list, board ) -- void

   --self:ASSERT(353, list.size~=nil);
   --self:ASSERT(354, board.sp~=nil);

   --self:ASSERT(355, not self:board_is_check(board));

   if(self:COLOUR_IS_WHITE(board.turn)) then

      if( bit.band( board.flags, self.FlagsWhiteKingCastle) ~= 0
        and  board.square[1+self.F1] == self.Empty
        and  board.square[1+self.G1] == self.Empty
        and(not self:is_attacked(board,self.F1,self.Black))) then
         self:add_check(list,self:MOVE_MAKE_FLAGS(self.E1,self.G1,self.MoveCastle),board);
      end

      if( bit.band( board.flags, self.FlagsWhiteQueenCastle) ~= 0
        and  board.square[1+self.D1] == self.Empty
        and  board.square[1+self.C1] == self.Empty
        and  board.square[1+self.B1] == self.Empty
        and(not self:is_attacked(board,self.D1,self.Black))) then
         self:add_check(list,self:MOVE_MAKE_FLAGS(self.E1,self.C1,self.MoveCastle),board);
      end

   else  -- black

      if( bit.band( board.flags, self.FlagsBlackKingCastle) ~= 0
        and  board.square[1+self.F8] == self.Empty
        and  board.square[1+self.G8] == self.Empty
        and(not self:is_attacked(board,self.F8,self.White))) then
         self:add_check(list,self:MOVE_MAKE_FLAGS(self.E8,self.G8,self.MoveCastle),board);
      end

      if( bit.band( board.flags, self.FlagsBlackQueenCastle) ~= 0
        and  board.square[1+self.D8] == self.Empty
        and  board.square[1+self.C8] == self.Empty
        and  board.square[1+self.B8] == self.Empty
        and(not self:is_attacked(board,self.D8,self.White))) then
         self:add_check(list,self:MOVE_MAKE_FLAGS(self.E8,self.C8,self.MoveCastle),board);
      end
   end
end

-- self:add_check()

function self:add_check( list, move, board )  -- int

   local undo = self:undo_t();    -- undo_t[1];

   --self:ASSERT(356, list.size~=nil);
   --self:ASSERT(357, self:move_is_ok(move));
   --self:ASSERT(358, board.sp~=nil);

   self:move_do(board,move,undo);
   if(self:IS_IN_CHECK(board,board.turn)) then
     self:LIST_ADD(list,move);
   end
   self:move_undo(board,move,undo);
end

-- self:move_is_check()

function self:move_is_check( move, board )  -- bool

   local undo = self:undo_t();    -- undo_t[1];

   local check = false;   -- bool
   local me = 0;          -- int
   local opp = 0;         -- int
   local king = 0;        -- int
   local from = 0;        -- int
   local to = 0;          -- int
   local piece = 0;       -- int

   --self:ASSERT(359, self:move_is_ok(move));
   --self:ASSERT(360, board.sp~=nil);

   -- slow test for complex moves

   if(self:MOVE_IS_SPECIAL(move)) then

      self:move_do(board,move,undo);
      check = self:IS_IN_CHECK(board,board.turn);
      self:move_undo(board,move,undo);

      return check;
   end

   -- init

   me = board.turn;
   opp = self:COLOUR_OPP(me);
   king = self:KING_POS(board,opp);

   from = self:MOVE_FROM(move);
   to = self:MOVE_TO(move);
   piece = board.square[1+from];
   --self:ASSERT(361, self:COLOUR_IS(piece,me));

   -- direct check

   if(self:PIECE_ATTACK(board,piece,to,king)) then
     return true;
   end

   -- indirect check

   if(self:is_pinned(board,from,opp)
     and  self:DELTA_INC_LINE(king-to) ~= self:DELTA_INC_LINE(king-from)) then
      return true;
   end

   return false;
end

-- self:find_pins()

function self:find_pins( list, board )  -- int

   local me = 0;    -- int
   local opp = 0;   -- int
   local king = 0;  -- int
   local ptr = 0;   -- int
   local from = 0;  -- int
   local piece = 0; -- int
   local delta = 0; -- int
   local inc = 0;   -- int
   local sq = 0;    -- int
   local pin = 0;   -- int
   local capture = 0;   -- int
   local q = 0;         -- int

   --self:ASSERT(363, board.sp~=nil);

   -- init

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   king = self:KING_POS(board,opp);

   ptr = 1;            -- HACK: no king
   while(true) do
      from = board.piece[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      delta = king - from;
      --self:ASSERT(364, self:delta_is_ok(delta));

      if(self:PSEUDO_ATTACK(piece,delta)) then

         --self:ASSERT(365, self:PIECE_IS_SLIDER(piece));

         inc = self:DELTA_INC_LINE(delta);
         --self:ASSERT(366, inc~=self.IncNone);

         --self:ASSERT(367, self:SLIDER_ATTACK(piece,inc));

         sq = from;

         while(true) do
           sq = sq + inc;
           capture = board.square[1+sq];
           if( capture ~= self.Empty ) then
             break;
           end
         end

         --self:ASSERT(368, sq~=king);

         if(self:COLOUR_IS(capture,me)) then
            pin = sq;

            while(true) do
              sq = sq + inc;

              if( board.square[1+sq] ~= self.Empty ) then
                break;
              end
            end

            if(sq == king) then

              list[1+q] = pin;
              q = q + 1;

            end
         end
      end

      ptr = ptr + 1;
   end

   list[1+q] = self.SquareNone;
end

-- end of move_check.cpp



-- move_do.cpp

-- functions

function self:initCmsk( sq, flagMask )
  self.CastleMask[1+sq] = bit.band( self.CastleMask[1+sq], bit.bnot( flagMask ) );
end

-- self:move_do_init()

function self:move_do_init()

   local sq = 0;   -- int

   for sq = 0, self.SquareNb-1, 1 do
     self.CastleMask[1+sq] = 0xF;
   end

   self:initCmsk( self.E1, self.FlagsWhiteKingCastle );
   self:initCmsk( self.H1, self.FlagsWhiteKingCastle );

   self:initCmsk( self.E1, self.FlagsWhiteQueenCastle );
   self:initCmsk( self.A1, self.FlagsWhiteQueenCastle );

   self:initCmsk( self.E8, self.FlagsBlackKingCastle );
   self:initCmsk( self.H8, self.FlagsBlackKingCastle );

   self:initCmsk( self.E8, self.FlagsBlackQueenCastle );
   self:initCmsk( self.A8, self.FlagsBlackQueenCastle );

end

-- self:move_do()

function self:move_do( board, move, undo )  -- int

   local me = 0;        -- int
   local opp = 0;       -- int
   local from = 0;      -- int
   local to = 0;        -- int
   local piece = 0;     -- int
   local pos = 0;       -- int
   local capture = 0;   -- int
   local old_flags = 0; -- int
   local new_flags = 0; -- int

   local delta = 0;  -- int
   local sq = 0;     -- int
   local pawn = 0;   -- int
   local rook = 0;   -- int

   --self:ASSERT(369, board.sp~=nil);
   --self:ASSERT(370, self:move_is_ok(move));
   --self:ASSERT(371, undo.flags~=nil);

   --self:ASSERT(372, self:board_is_legal(board));

   -- initialise undo

   undo.capture = false;

   undo.turn = board.turn;
   undo.flags = board.flags;
   undo.ep_square = board.ep_square;
   undo.ply_nb = board.ply_nb;

   undo.cap_sq = board.cap_sq;

   undo.opening = board.opening;
   undo.endgame = board.endgame;

   undo.key = board.key;
   undo.pawn_key = board.pawn_key;
   undo.material_key = board.material_key;

   -- init

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   from = self:MOVE_FROM(move);
   to = self:MOVE_TO(move);

   piece = board.square[1+from];
   --self:ASSERT(373, self:COLOUR_IS(piece,me));

   -- update key stack

   --self:ASSERT(374, board.sp<self.StackSize);
   board.stack[1+board.sp] = board.key;
   board.sp = board.sp + 1;

   -- update turn

   board.turn = opp;


   -- update castling rights

   old_flags = board.flags;
   new_flags = bit.band( bit.band( old_flags, self.CastleMask[1+from] ) , self.CastleMask[1+to] );

   board.flags = new_flags;


   -- update en-passant square

   sq = board.ep_square;
   if(sq ~= self.SquareNone) then

      board.ep_square = self.SquareNone;
   end

   if(self:PIECE_IS_PAWN(piece)) then

      delta = to - from;

      if(delta == 32  or  delta == -32) then
         pawn = self.PawnMake[1+opp];
         if(board.square[1+to-1] == pawn  or  board.square[1+to+1] == pawn) then
            board.ep_square =(from + to) / 2;
        end
      end
   end

   -- update move number(captures are handled later)

   board.ply_nb = board.ply_nb + 1;
   if(self:PIECE_IS_PAWN(piece)) then
     board.ply_nb = 0; -- conversion
   end

   -- update last square

   board.cap_sq = self.SquareNone;

   -- remove the captured piece

   sq = to;
   if(self:MOVE_IS_EN_PASSANT(move)) then
     sq = self:SQUARE_EP_DUAL(sq);
   end

   capture=board.square[1+sq];
   if(capture~= self.Empty) then

      --self:ASSERT(375, self:COLOUR_IS(capture,opp));
      --self:ASSERT(376, not self:PIECE_IS_KING(capture));

      undo.capture = true;
      undo.capture_square = sq;
      undo.capture_piece = capture;
      undo.capture_pos = board.pos[1+sq];

      self:square_clear(board,sq,capture,true);

      board.ply_nb = 0; -- conversion
      board.cap_sq = to;
   end

   -- move the piece

   if(self:MOVE_IS_PROMOTE(move)) then

      -- promote

      undo.pawn_pos = board.pos[1+from];

      self:square_clear(board,from,piece,true);

      piece = self:move_promote(move);

      -- insert the promote piece in MV order

      pos = board.piece_size[1+me];
      while( pos > 0  and  piece > board.square[1+board.piece[1+me][1+pos-1]] ) do
        pos = pos - 1;   -- HACK
      end

      self:square_set(board,to,piece,pos,true);

      board.cap_sq = to;

   else

      -- normal move

      self:square_move(board,from,to,piece,true);
   end

   -- move the rook in case of castling

   if(self:MOVE_IS_CASTLE(move)) then

      rook =  bit.bor( self.Rook64, self:COLOUR_FLAG(me) ); -- HACK

      if(to == self.G1) then
         self:square_move(board,self.H1,self.F1,rook,true);
      else
       if(to == self.C1) then
         self:square_move(board,self.A1,self.D1,rook,true);
       else
        if(to == self.G8) then
          self:square_move(board,self.H8,self.F8,rook,true);
        else
          if(to == self.C8) then
            self:square_move(board,self.A8,self.D8,rook,true);
          else
            --self:ASSERT(377, false);
          end
        end
       end
      end
   end

   -- debug

   --self:ASSERT(378, self:board_is_ok(board));

end

-- self:move_undo()

function self:move_undo( board, move, undo )  -- int

   local me = 0;    -- int
   local from = 0;  -- int
   local to = 0;    -- int
   local piece = 0; -- int
   local pos = 0;   -- int
   local rook = 0;  -- int

   --self:ASSERT(379, board.sp~=nil);
   --self:ASSERT(380, self:move_is_ok(move));
   --self:ASSERT(381, undo.flags~=nil);

   -- init

   me = undo.turn;

   from = self:MOVE_FROM(move);
   to = self:MOVE_TO(move);

   piece = board.square[1+to];
   --self:ASSERT(382, self:COLOUR_IS(piece,me));

   -- castle

   if(self:MOVE_IS_CASTLE(move)) then

      rook =  bit.bor( self.Rook64, self:COLOUR_FLAG(me) ); -- HACK

      if(to == self.G1) then
         self:square_move(board,self.F1,self.H1,rook,false);
      else
       if(to == self.C1) then
         self:square_move(board,self.D1,self.A1,rook,false);
       else
        if(to == self.G8) then
          self:square_move(board,self.F8,self.H8,rook,false);
        else
          if(to == self.C8) then
            self:square_move(board,self.D8,self.A8,rook,false);
          else
            --self:ASSERT(383, false);
          end
        end
       end
      end
   end



   -- move the piece backward

   if(self:MOVE_IS_PROMOTE(move)) then

      -- promote

      --self:ASSERT(384, piece==self:move_promote(move));
      self:square_clear(board,to,piece,false);

      piece = self.PawnMake[1+me];
      pos = undo.pawn_pos;

      self:square_set(board,from,piece,pos,false);

   else

      -- normal move

      self:square_move(board,to,from,piece,false);
   end

   -- put the captured piece back

   if(undo.capture) then
      self:square_set(board,undo.capture_square,undo.capture_piece,undo.capture_pos,false);
   end

   -- update board info

   board.turn = undo.turn;
   board.flags = undo.flags;
   board.ep_square = undo.ep_square;
   board.ply_nb = undo.ply_nb;

   board.cap_sq = undo.cap_sq;

   board.opening = undo.opening;
   board.endgame = undo.endgame;

   board.key = undo.key;
   board.pawn_key = undo.pawn_key;
   board.material_key = undo.material_key;

   -- update key stack

   --self:ASSERT(385, board.sp>0);
   board.sp = board.sp - 1;

   -- debug

   --self:ASSERT(386, self:board_is_ok(board));
   --self:ASSERT(387, self:board_is_legal(board));
end

-- self:move_do_nil()

function self:move_do_nil( board, undo )  -- void

   local sq = 0;   -- int

   --self:ASSERT(388, board.sp~=nil);
   --self:ASSERT(389, undo.flags~=nil);

   --self:ASSERT(390, self:board_is_legal(board));
   --self:ASSERT(391, not self:board_is_check(board));

   -- initialise undo

   undo.turn = board.turn;
   undo.ep_square = board.ep_square;
   undo.ply_nb = board.ply_nb;
   undo.cap_sq = board.cap_sq;
   undo.key = board.key;

   -- update key stack

   --self:ASSERT(392, board.sp<self.StackSize);
   board.stack[1+board.sp] = board.key;
   board.sp = board.sp + 1;

   -- update turn

   board.turn = self:COLOUR_OPP(board.turn);

   -- update en-passant square

   sq = board.ep_square;
   if(sq ~= self.SquareNone) then

      board.ep_square = self.SquareNone;
   end

   -- update move number

   board.ply_nb = 0; -- HACK: nil move is considered as a conversion

   -- update last square

   board.cap_sq = self.SquareNone;

   -- debug

   --self:ASSERT(393, self:board_is_ok(board));
end

-- self:move_undo_nil()

function self:move_undo_nil( board, undo )  -- void

   --self:ASSERT(394, board.sp~=nil);
   --self:ASSERT(395, undo.flags~=nil);

   --self:ASSERT(396, self:board_is_legal(board));
   --self:ASSERT(397, not self:board_is_check(board));

   -- update board info

   board.turn = undo.turn;
   board.ep_square = undo.ep_square;
   board.ply_nb = undo.ply_nb;
   board.cap_sq = undo.cap_sq;
   board.key = undo.key;

   -- update key stack

   --self:ASSERT(398, board.sp>0);
   board.sp = board.sp - 1;

   -- debug

   --self:ASSERT(399, self:board_is_ok(board));
end

-- self:square_clear()

function self:square_clear( board, square, piece, update )  -- bool

   local pos = 0;       -- int
   local piece_12 = 0;  -- int
   local colour = 0;    -- int
   local sq = 0;        -- int
   local i = 0;         -- int
   local size = 0;      -- int
   local sq_64 = 0;     -- int
   local t = 0;         -- int
   local hash_xor = 0;  -- uint64

   --self:ASSERT(400, board.sp~=nil);
   --self:ASSERT(401, self:SQUARE_IS_OK(square));
   --self:ASSERT(402, self:piece_is_ok(piece));
   --self:ASSERT(403, update==true or update==false);

   -- init

   pos = board.pos[1+square];
   --self:ASSERT(404, pos>=0);

   piece_12 = self.PieceTo12[1+piece];
   colour = self:PIECE_COLOUR(piece);

   -- square

   --self:ASSERT(405, board.square[1+square]==piece);
   board.square[1+square] = self.Empty;

   -- piece list

   if(not self:PIECE_IS_PAWN(piece)) then

      -- init

      size = board.piece_size[1+colour];
      --self:ASSERT(406, size>=1);

      -- stable swap

      --self:ASSERT(407, pos>=0 and pos<size);

      --self:ASSERT(408, board.pos[1+square]==pos);
      board.pos[1+square] = -1;

      for i = pos, size-2, 1 do

         sq = board.piece[1+colour][1+i+1];

         board.piece[1+colour][1+i] = sq;

         --self:ASSERT(409, board.pos[1+sq]==i+1);
         board.pos[1+sq] = i;
      end

      -- size

      size = size - 1;

      board.piece[1+colour][1+size] = self.SquareNone;
      board.piece_size[1+colour] = size;

   else

      -- init

      size = board.pawn_size[1+colour];
      --self:ASSERT(410, size>=1);

      -- stable swap

      --self:ASSERT(411, pos>=0 and pos<size);

      --self:ASSERT(412, board.pos[1+square]==pos);
      board.pos[1+square] = -1;

      for i = pos, size-2, 1 do

         sq = board.pawn[1+colour][1+i+1];

         board.pawn[1+colour][1+i] = sq;

         --self:ASSERT(413, board.pos[1+sq]==i+1);
         board.pos[1+sq] = i;
      end

      -- size

      size = size - 1;

      board.pawn[1+colour][1+size] = self.SquareNone;
      board.pawn_size[1+colour] = size;

      -- pawn "bitboard"

      t = self:SQUARE_FILE(square);
      board.pawn_file[1+colour][1+t] = bit.bxor( board.pawn_file[1+colour][1+t] ,
            self.BitEQ[1+self:PAWN_RANK(square,colour)] );
   end

   -- material

   --self:ASSERT(414, board.piece_nb>0);
   board.piece_nb = board.piece_nb - 1;

   --self:ASSERT(415, board.number[1+piece_12]>0);
   board.number[1+piece_12] = board.number[1+piece_12] - 1;

   -- update

   if(update) then

      -- init

      sq_64 = self.SquareTo64[1+square];

      -- PST

      board.opening = board.opening - self:Pget( piece_12, sq_64, self.Opening );
      board.endgame = board.endgame - self:Pget( piece_12, sq_64, self.Endgame );

      -- hash key

      hash_xor = self.Random64[1+self.RandomPiece+(bit.bxor(piece_12,1)*64)+sq_64];
         -- HACK: xor 1 for PolyGlot book(not lua)

      board.key = bit.bxor( board.key, hash_xor);
      if(self:PIECE_IS_PAWN(piece)) then
        board.pawn_key = bit.bxor( board.pawn_key, hash_xor);
      end

      -- material key

      board.material_key = bit.bxor( board.material_key, self.Random64[1+(piece_12*16)+board.number[1+piece_12]] );


   end
end

-- self:square_set()

function self:square_set( board, square, piece, pos, update )  -- bool

   local piece_12 = 0;  -- int
   local colour = 0;    -- int
   local sq = 0;        -- int
   local i = 0;         -- int
   local size = 0;      -- int
   local sq_64 = 0;     -- int
   local t = 0;         -- int
   local hash_xor = 0;  -- uint64


   --self:ASSERT(416, board.sp~=nil);
   --self:ASSERT(417, self:SQUARE_IS_OK(square));
   --self:ASSERT(418, self:piece_is_ok(piece));
   --self:ASSERT(419, pos>=0);
   --self:ASSERT(420, update==true or update==false);

   -- init

   piece_12 = self.PieceTo12[1+piece];
   colour = self:PIECE_COLOUR(piece);

   -- square

   --self:ASSERT(421, board.square[1+square]==self.Empty);
   board.square[1+square] = piece;

   -- piece list

   if(not self:PIECE_IS_PAWN(piece)) then

      -- init

      size = board.piece_size[1+colour];
      --self:ASSERT(422, size>=0);

      -- size

      size = size + 1;

      board.piece[1+colour][1+size] = self.SquareNone;
      board.piece_size[1+colour] = size;

      -- stable swap

      --self:ASSERT(423, pos>=0 and pos<size);

      for i = size-1, pos+1, -1 do

         sq = board.piece[1+colour][1+i-1];

         board.piece[1+colour][1+i] = sq;

         --self:ASSERT(424, board.pos[1+sq]==i-1);
         board.pos[1+sq] = i;
      end

      board.piece[1+colour][1+pos] = square;

      --self:ASSERT(425, board.pos[1+square]==-1);
      board.pos[1+square] = pos;

   else

      -- init

      size = board.pawn_size[1+colour];
      --self:ASSERT(426, size>=0);

      -- size

      size = size + 1;

      board.pawn[1+colour][1+size] = self.SquareNone;
      board.pawn_size[1+colour] = size;

      -- stable swap

      --self:ASSERT(427, pos>=0 and pos<size);

      for i = size-1, pos+1, -1 do

         sq = board.pawn[1+colour][1+i-1];

         board.pawn[1+colour][1+i] = sq;

         --self:ASSERT(428, board.pos[1+sq]==i-1);
         board.pos[1+sq] = i;
      end

      board.pawn[1+colour][1+pos] = square;

      --self:ASSERT(429, board.pos[1+square]==-1);
      board.pos[1+square] = pos;

      -- pawn "bitboard"

      t = self:SQUARE_FILE(square);
      board.pawn_file[1+colour][1+t] = bit.bxor( board.pawn_file[1+colour][1+t] ,
            self.BitEQ[1+self:PAWN_RANK(square,colour)] );


   end

   -- material

   --self:ASSERT(430, board.piece_nb<32);
   board.piece_nb = board.piece_nb + 1;

   --self:ASSERT(431, board.number[1+piece_12]<9);
   board.number[1+piece_12] = board.number[1+piece_12] + 1;

   -- update

   if(update) then

      -- init

      sq_64 = self.SquareTo64[1+square];

      -- PST

      board.opening = board.opening + self:Pget( piece_12, sq_64, self.Opening );
      board.endgame = board.endgame +  self:Pget( piece_12, sq_64, self.Endgame );
      -- hash key

      hash_xor = self.Random64[1+self.RandomPiece+(bit.bxor(piece_12,1)*64)+sq_64];
         -- HACK: xor 1 for PolyGlot book(not lua)

      board.key = bit.bxor( board.key, hash_xor);
      if(self:PIECE_IS_PAWN(piece)) then
        board.pawn_key = bit.bxor( board.pawn_key, hash_xor);
      end

      -- material key

      board.material_key = bit.bxor( board.material_key, self.Random64[1+(piece_12*16)+board.number[1+piece_12]] );

   end
end

-- self:square_move()

function self:square_move( board, from, to, piece, update )  -- bool

   local piece_12 = 0;    -- int
   local colour = 0;      -- int
   local pos = 0;         -- int
   local from_64 = 0;     -- int
   local to_64 = 0;       -- int
   local piece_index = 0; -- int
   local t = 0;           -- int
   local hash_xor = 0;    -- uint64


   --self:ASSERT(432, board.sp~=nil);
   --self:ASSERT(433, self:SQUARE_IS_OK(from));
   --self:ASSERT(434, self:SQUARE_IS_OK(to));
   --self:ASSERT(435, self:piece_is_ok(piece));
   --self:ASSERT(436, update==true or update==false);

   -- init

   colour = self:PIECE_COLOUR(piece);

   pos = board.pos[1+from];
   --self:ASSERT(437, pos>=0);

   -- from

   --self:ASSERT(438, board.square[1+from]==piece);
   board.square[1+from] = self.Empty;

   --self:ASSERT(439, board.pos[1+from]==pos);
   board.pos[1+from] = -1; -- not needed

   -- to

   --self:ASSERT(440, board.square[1+to]==self.Empty);
   board.square[1+to] = piece;

   --self:ASSERT(441, board.pos[1+to]==-1);
   board.pos[1+to] = pos;

   -- piece list

   if(not self:PIECE_IS_PAWN(piece)) then

      --self:ASSERT(442, board.piece[1+colour][1+pos]==from);
      board.piece[1+colour][1+pos] = to;

   else

      --self:ASSERT(443, board.pawn[1+colour][1+pos]==from);
      board.pawn[1+colour][1+pos] = to;

      -- pawn "bitboard"

      t = self:SQUARE_FILE(from);
      board.pawn_file[1+colour][1+t] = bit.bxor( board.pawn_file[1+colour][1+t] ,
            self.BitEQ[1+self:PAWN_RANK(from,colour)] );
      t = self:SQUARE_FILE(to);
      board.pawn_file[1+colour][1+t] = bit.bxor( board.pawn_file[1+colour][1+t] ,
            self.BitEQ[1+self:PAWN_RANK(to,colour)] );

   end

   -- update

   if(update) then

      -- init

      from_64 = self.SquareTo64[1+from];
      to_64 = self.SquareTo64[1+to];
      piece_12 = self.PieceTo12[1+piece];

      -- PST

 	  board.opening = board.opening + self:Pget(piece_12,to_64,self.Opening) - self:Pget(piece_12,from_64,self.Opening);
      board.endgame = board.endgame + self:Pget(piece_12,to_64,self.Endgame) - self:Pget(piece_12,from_64,self.Endgame);

      -- hash key

      piece_index = self.RandomPiece +(bit.bxor(piece_12,1) * 64);
          -- HACK: xor 1 for PolyGlot book(not lua)

      hash_xor =  bit.bxor( self.Random64[1+piece_index+to_64], self.Random64[1+piece_index+from_64] );

      board.key = bit.bxor( board.key, hash_xor );
      if(self:PIECE_IS_PAWN(piece)) then
        board.pawn_key = bit.bxor( board.pawn_key, hash_xor);
      end

   end

end

-- end of move_do.cpp



-- move_evasion.cpp

-- functions

-- self:gen_legal_evasions()

function self:gen_legal_evasions( list, board, attack ) -- void

   --self:ASSERT(444, list.size~=nil);
   --self:ASSERT(445, board.sp~=nil);
   --self:ASSERT(446, attack.dn~=nil);

   self:gen_evasions(list,board,attack,true,false);

   -- debug

   --self:ASSERT(447, self:list_is_ok(list));
end

-- self:gen_pseudo_evasions()

function self:gen_pseudo_evasions( list, board, attack ) -- void

   --self:ASSERT(448, list.size~=nil);
   --self:ASSERT(449, board.sp~=nil);
   --self:ASSERT(450, attack.dn~=nil);

   self:gen_evasions(list,board,attack,false,false);

   -- debug

   --self:ASSERT(451, self:list_is_ok(list));
end

-- self:legal_evasion_exist()

function self:legal_evasion_exist( board, attack )  -- bool

   local list = self:list_t();  -- list[1] dummy

   --self:ASSERT(452, board.sp~=nil);
   --self:ASSERT(453, attack.dn~=nil);

   return self:gen_evasions(list,board,attack,true,true);
end

-- self:gen_evasions()

function self:gen_evasions( list, board, attack, legal, stop )  -- bool
   local me = 0;         -- int
   local opp = 0;        -- int
   local opp_flag = 0;   -- int
   local king = 0;       -- int
   local inc_ptr = 0;    -- int
   local inc = 0;        -- int
   local to = 0;         -- int
   local piece = 0;      -- int

   --self:ASSERT(454, list.size~=nil);
   --self:ASSERT(455, board.sp~=nil);
   --self:ASSERT(456, attack.dn~=nil);
   --self:ASSERT(457, legal==true or legal==false);
   --self:ASSERT(458, stop==true or stop==false);

   --self:ASSERT(459, self:board_is_check(board));
   --self:ASSERT(460, self:ATTACK_IN_CHECK(attack));

   -- init

   list.size=0;

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   opp_flag = self:COLOUR_FLAG(opp);

   king = self:KING_POS(board,me);

   inc_ptr = 0;
   while(true) do
      inc = self.KingInc[1+inc_ptr];
      if( inc == self.IncNone ) then
        break;
      end
        -- avoid escaping along a check line
      if(inc ~= -attack.di[1+0]  and  inc ~= -attack.di[1+1]) then
         to = king + inc;
         piece = board.square[1+to];
         if(piece == self.Empty  or  self:FLAG_IS(piece,opp_flag)) then
            if(not legal  or  not self:is_attacked(board,to,opp)) then
               if(stop) then
                 return true;
               end
               self:LIST_ADD(list,self:MOVE_MAKE(king,to));
            end
         end
      end

      inc_ptr = inc_ptr + 1;
   end


   if(attack.dn >= 2) then
     return false; -- double check, we are done
   end

   -- single check

   --self:ASSERT(461, attack.dn==1);

   -- capture the checking piece

   if(self:add_pawn_captures(list,board,attack.ds[1+0],legal,stop)  and  stop) then
     return true;
   end
   if(self:add_piece_moves(list,board,attack.ds[1+0],legal,stop)  and  stop) then
     return true;
   end

   -- interpose a piece

   inc = attack.di[1+0];

   if(inc ~= self.IncNone) then -- line
      to = king+inc;
      while( to ~= attack.ds[1+0] ) do

        --self:ASSERT(462, self:SQUARE_IS_OK(to));
        --self:ASSERT(463, board.square[1+to]==self.Empty);
        if(self:add_pawn_moves(list,board,to,legal,stop)  and  stop) then
          return true;
        end
        if(self:add_piece_moves(list,board,to,legal,stop)  and  stop) then
          return true;
        end
        to = to + inc;
      end
   end

   return false;

end

-- self:add_pawn_moves()

function self:add_pawn_moves( list, board, to, legal, stop )  -- bool
   local me = 0;      -- int
   local inc = 0;     -- int
   local pawn = 0;    -- int
   local from = 0;    -- int
   local piece = 0;   -- int

   --self:ASSERT(464, list.size~=nil);
   --self:ASSERT(465, board.sp~=nil);
   --self:ASSERT(466, self:SQUARE_IS_OK(to));
   --self:ASSERT(467, legal==true or legal==false);
   --self:ASSERT(468, stop==true or stop==false);

   --self:ASSERT(469, board.square[1+to]==self.Empty);

   me = board.turn;

   inc = self.PawnMoveInc[1+me];
   pawn = self.PawnMake[1+me];

   from = to - inc;
   piece = board.square[1+from];

   if(piece == pawn) then  -- single push

      if((not legal)  or(not self:is_pinned(board,from,me))) then
         if(stop) then
           return true;
         end
         self:add_pawn_move(list,from,to);
      end

   else
    if(piece == self.Empty  and  self:PAWN_RANK(to,me) == self.Rank4)  then   -- double push

      from = to -(2*inc);
      if(board.square[1+from] == pawn) then
         if((not legal)  or(not self:is_pinned(board,from,me))) then
            if(stop) then
              return true;
            end
            --self:ASSERT(470, not self.SquareIsPromote[1+to]);
            self:LIST_ADD(list,self:MOVE_MAKE(from,to));
         end
      end
    end
   end

   return false;
end

-- self:add_pawn_captures()

function self:add_pawn_captures( list, board, to, legal, stop ) -- bool
   local me = 0;     -- int
   local inc = 0;    -- int
   local pawn = 0;   -- int
   local from = 0;   -- int

   --self:ASSERT(471, list.size~=nil);
   --self:ASSERT(472, board.sp~=nil);
   --self:ASSERT(473, self:SQUARE_IS_OK(to));
   --self:ASSERT(474, legal==true or legal==false);
   --self:ASSERT(475, stop==true or stop==false);

   --self:ASSERT(476, self:COLOUR_IS(board.square[1+to],self:COLOUR_OPP(board.turn)));

   me = board.turn;

   inc = self.PawnMoveInc[1+me];
   pawn = self.PawnMake[1+me];

   from = to -(inc-1);
   if(board.square[1+from] == pawn) then
      if((not legal)  or(not self:is_pinned(board,from,me))) then
         if(stop) then
           return true;
         end
         self:add_pawn_move(list,from,to);
      end
   end

   from = to -(inc+1);
   if(board.square[1+from] == pawn) then
      if((not legal)  or(not self:is_pinned(board,from,me))) then
         if(stop) then
           return true;
         end
         self:add_pawn_move(list,from,to);
      end
   end

   if(board.ep_square ~= self.SquareNone and  to == self:SQUARE_EP_DUAL(board.ep_square)) then

      --self:ASSERT(477, self:PAWN_RANK(to,me)==self.Rank5);
      --self:ASSERT(478, self:PIECE_IS_PAWN(board.square[1+to]));

      to = board.ep_square;
      --self:ASSERT(479, self:PAWN_RANK(to,me)==self.Rank6);
      --self:ASSERT(480, board.square[1+to]==self.Empty);

      from = to -(inc-1);
      if(board.square[1+from] == pawn) then
         if((not legal)  or(not self:is_pinned(board,from,me))) then
            if(stop) then
              return true;
            end
            --self:ASSERT(481, not self.SquareIsPromote[1+to]);
            self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(from,to,self.MoveEnPassant));
         end
      end

      from = to -(inc+1);
      if(board.square[1+from] == pawn) then
         if((not legal)  or(not self:is_pinned(board,from,me))) then
            if(stop) then
              return true;
            end
            --self:ASSERT(482, not self.SquareIsPromote[1+to]);
            self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(from,to,self.MoveEnPassant));
         end
      end
   end

   return false;
end

-- self:add_piece_moves()

function self:add_piece_moves( list, board, to, legal, stop)  -- bool
   local me = 0;      -- int
   local ptr = 0;     -- int
   local from = 0;    -- int
   local piece = 0;   -- int

   --self:ASSERT(483, list.size~=nil);
   --self:ASSERT(484, board.sp~=nil);
   --self:ASSERT(485, self:SQUARE_IS_OK(to));
   --self:ASSERT(486, legal==true or legal==false);
   --self:ASSERT(487, stop==true or stop==false);

   me = board.turn;

   ptr = 1;            -- HACK: no king
   while(true) do
      from = board.piece[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      if(self:PIECE_ATTACK(board,piece,from,to)) then
         if((not legal)  or(not self:is_pinned(board,from,me))) then
            if(stop) then
              return true;
            end
            self:LIST_ADD(list,self:MOVE_MAKE(from,to));
         end
      end

      ptr = ptr + 1;
   end

   return false;

end

-- end of move_evasion.cpp



-- move_gen.cpp

-- functions

-- self:gen_legal_moves()

function self:gen_legal_moves( list, board )  -- void

   local attack = self:attack_t();  -- attack_t[1]

   --self:ASSERT(488, list.size~=nil);
   --self:ASSERT(489, board.sp~=nil);

   self:attack_set(attack,board);

   if(self:ATTACK_IN_CHECK(attack)) then
      self:gen_legal_evasions(list,board,attack);
   else
      self:gen_moves(list,board);
      self:list_filter(list,board, true);
   end

   -- debug

   --self:ASSERT(490, self:list_is_ok(list));
end

-- self:gen_moves()

function self:gen_moves( list, board ) -- void

   --self:ASSERT(491, list.size~=nil);
   --self:ASSERT(492, board.sp~=nil);

   --self:ASSERT(493, not self:board_is_check(board));

   list.size=0;

   self:add_moves(list,board);

   self:add_en_passant_captures(list,board);
   self:add_castle_moves(list,board);

   -- debug

   --self:ASSERT(494, self:list_is_ok(list));
end

-- self:gen_captures()

function self:gen_captures( list, board ) -- void

   --self:ASSERT(495, list.size~=nil);
   --self:ASSERT(496, board.sp~=nil);

   list.size=0;

   self:add_captures(list,board);
   self:add_en_passant_captures(list,board);

   -- debug

   --self:ASSERT(497, self:list_is_ok(list));
end

-- self:gen_quiet_moves()

function self:gen_quiet_moves( list, board ) -- void

   --self:ASSERT(498, list.size~=nil);
   --self:ASSERT(499, board.sp~=nil);

   --self:ASSERT(500, not self:board_is_check(board));

   list.size=0;

   self:add_quiet_moves(list,board);
   self:add_castle_moves(list,board);

   -- debug

   --self:ASSERT(501, self:list_is_ok(list));
end

-- self:add_moves()

function self:add_moves( list, board ) -- void

   local me = 0;         -- int
   local opp = 0;        -- int
   local opp_flag = 0;   -- int
   local ptr = 0;        -- int
   local from = 0;       -- int
   local to = 0;         -- int
   local piece = 0;      -- int
   local capture = 0;    -- int
   local inc_ptr = 0;    -- int
   local inc = 0;        -- int

   --self:ASSERT(502, list.size~=nil);
   --self:ASSERT(503, board.sp~=nil);

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   opp_flag = self:COLOUR_FLAG(opp);

   -- piece moves

   ptr = 0;
   while(true) do
      from = board.piece[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      if(self:PIECE_IS_SLIDER(piece)) then

         inc_ptr = 0;
         while(true) do
           inc = self.PieceInc[1+piece][1+inc_ptr];
           if( inc == self.IncNone ) then
             break;
           end

           to = from+inc;
           while(true) do
               capture=board.square[1+to];
               if( capture ~= self.Empty ) then
                 break;
               end

               self:LIST_ADD(list,self:MOVE_MAKE(from,to));

               to = to + inc;
           end

           if(self:FLAG_IS(capture,opp_flag)) then
              self:LIST_ADD(list,self:MOVE_MAKE(from,to));
           end

           inc_ptr = inc_ptr + 1;
         end

      else

         inc_ptr = 0;
         while(true) do
           inc = self.PieceInc[1+piece][1+inc_ptr];
           if( inc == self.IncNone ) then
             break;
           end

           to = from + inc;
           capture = board.square[1+to];
           if(capture == self.Empty  or  self:FLAG_IS(capture,opp_flag)) then
              self:LIST_ADD(list,self:MOVE_MAKE(from,to));
           end

           inc_ptr = inc_ptr + 1;
         end

      end

      ptr = ptr + 1;
   end


   -- pawn moves

   inc = self.PawnMoveInc[1+me];

   ptr = 0;
   while(true) do
      from = board.pawn[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      to = from +(inc-1);
      if(self:FLAG_IS(board.square[1+to],opp_flag)) then
         self:add_pawn_move(list,from,to);
      end

      to = from +(inc+1);
      if(self:FLAG_IS(board.square[1+to],opp_flag)) then
         self:add_pawn_move(list,from,to);
      end

      to = from + inc;
      if(board.square[1+to] == self.Empty) then
         self:add_pawn_move(list,from,to);
         if(self:PAWN_RANK(from,me) == self.Rank2) then
            to = from +(2*inc);
            if(board.square[1+to] == self.Empty) then
               --self:ASSERT(504, not self.SquareIsPromote[1+to]);
               self:LIST_ADD(list,self:MOVE_MAKE(from,to));
            end
         end
      end

      ptr = ptr + 1;
   end

end

--
function self:add_capt1( from, dt, list, board, opp_flag )
  local to = from + dt;
  if(self:FLAG_IS(board.square[1+to],opp_flag)) then
    self:LIST_ADD(list,self:MOVE_MAKE(from,to));
  end
end


--
function self:add_capt2( from, dt, list, board, opp_flag )
 local to = from + dt;
 local capture = 0;
 while(true) do
   capture=board.square[1+to];
   if(capture~=self.Empty) then
     break;
   end
   to = to + dt;
 end
 if(self:FLAG_IS(capture,opp_flag)) then
   self:LIST_ADD(list,self:MOVE_MAKE(from,to));
 end
end

--
function self:add_capt3( from, dt, list, board, opp_flag )
 local to = from + dt;
 if(self:FLAG_IS(board.square[1+to],opp_flag)) then
   self:add_pawn_move(list,from,to);
 end
end

--
function self:add_capt4( from, dt, list, board )
 local to = from + dt;
 if(board.square[1+to] == self.Empty) then
   self:add_promote(list,self:MOVE_MAKE(from,to));
 end
end

-- self:add_captures()

function self:add_captures( list, board ) -- void

   local me = 0;         -- int
   local opp = 0;        -- int
   local opp_flag = 0;   -- int
   local ptr = 0;        -- int
   local from = 0;       -- int
   local piece = 0;      -- int
   local p = 0;

   --self:ASSERT(505, list.size~=nil);
   --self:ASSERT(506, board.sp~=nil);

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   opp_flag = self:COLOUR_FLAG(opp);

   -- piece captures

   ptr = 0;
   while(true) do
      from = board.piece[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      p = self:PIECE_TYPE(piece);

      if(p == self.Knight64) then

         self:add_capt1( from, -33, list, board, opp_flag );
         self:add_capt1( from, -31, list, board, opp_flag );
         self:add_capt1( from, -18, list, board, opp_flag );
         self:add_capt1( from, -14, list, board, opp_flag );
         self:add_capt1( from, 14, list, board, opp_flag );
         self:add_capt1( from, 18, list, board, opp_flag );
         self:add_capt1( from, 31, list, board, opp_flag );
         self:add_capt1( from, 33, list, board, opp_flag );
      else

       if(p == self.Bishop64) then

         self:add_capt2( from, -17, list, board, opp_flag );
         self:add_capt2( from, -15, list, board, opp_flag );
         self:add_capt2( from, 15, list, board, opp_flag );
         self:add_capt2( from, 17, list, board, opp_flag );

       else

        if(p == self.Rook64) then

          self:add_capt2( from, -16, list, board, opp_flag );
          self:add_capt2( from, -1, list, board, opp_flag );
          self:add_capt2( from, 1, list, board, opp_flag );
          self:add_capt2( from, 16, list, board, opp_flag );

        else

         if(p == self.Queen64) then

            self:add_capt2( from, -17, list, board, opp_flag );
            self:add_capt2( from, -16, list, board, opp_flag );
            self:add_capt2( from, -15, list, board, opp_flag );
            self:add_capt2( from, -1, list, board, opp_flag );
            self:add_capt2( from, 1, list, board, opp_flag );
            self:add_capt2( from, 15, list, board, opp_flag );
            self:add_capt2( from, 16, list, board, opp_flag );
            self:add_capt2( from, 17, list, board, opp_flag );

         else

           if(p == self.King64) then

              self:add_capt1( from, -17, list, board, opp_flag );
              self:add_capt1( from, -16, list, board, opp_flag );
              self:add_capt1( from, -15, list, board, opp_flag );
              self:add_capt1( from, -1, list, board, opp_flag );
              self:add_capt1( from, 1, list, board, opp_flag );
              self:add_capt1( from, 15, list, board, opp_flag );
              self:add_capt1( from, 16, list, board, opp_flag );
              self:add_capt1( from, 17, list, board, opp_flag );

           else

              --self:ASSERT(507, false);

           end
         end
        end
       end
      end

      ptr = ptr + 1;
   end

   -- pawn captures

   if(self:COLOUR_IS_WHITE(me)) then

      ptr = 0;
      while(true) do
         from = board.pawn[1+me][1+ptr];
         if( from == self.SquareNone ) then
           break;
         end

         self:add_capt3( from, 15, list, board, opp_flag );
         self:add_capt3( from, 17, list, board, opp_flag );

         -- promote

         if(self:SQUARE_RANK(from) == self.Rank7) then
            self:add_capt4( from, 16, list, board );
         end

         ptr = ptr + 1;
      end

   else  -- black

      ptr = 0;
      while(true) do
         from = board.pawn[1+me][1+ptr];
         if( from == self.SquareNone ) then
           break;
         end

         self:add_capt3( from, -17, list, board, opp_flag );
         self:add_capt3( from, -15, list, board, opp_flag );

         -- promote

         if(self:SQUARE_RANK(from) == self.Rank2) then
            self:add_capt4( from, -16, list, board );
         end

         ptr = ptr + 1;
      end

   end

end


--
function self:add_quietm1( from, dt, list, board )
 local to = from + dt;
 if(board.square[1+to] == self.Empty) then
   self:LIST_ADD(list,self:MOVE_MAKE(from,to));
 end
end

--
function self:add_quietm2( from, dt, list, board )
 local to = from + dt;
 while(true) do
   if(board.square[1+to]~=self.Empty) then
     break;
   end
   self:LIST_ADD(list,self:MOVE_MAKE(from,to));
   to = to + dt;
 end
end

-- self:add_quiet_moves()

function self:add_quiet_moves( list, board ) -- void

   local me = 0;         -- int
   local ptr = 0;        -- int
   local from = 0;       -- int
   local to = 0;         -- int
   local piece = 0;      -- int
   local p = 0;


   --self:ASSERT(508, list.size~=nil);
   --self:ASSERT(509, board.sp~=nil);

   me = board.turn;

   -- piece moves

   ptr = 0;
   while(true) do
      from = board.piece[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      piece = board.square[1+from];

      p = self:PIECE_TYPE(piece);

      if(p == self.Knight64) then

         self:add_quietm1( from, -33, list, board );
         self:add_quietm1( from, -31, list, board );
         self:add_quietm1( from, -18, list, board );
         self:add_quietm1( from, -14, list, board );
         self:add_quietm1( from, 14, list, board );
         self:add_quietm1( from, 18, list, board );
         self:add_quietm1( from, 31, list, board );
         self:add_quietm1( from, 33, list, board );
      else

       if(p == self.Bishop64) then

         self:add_quietm2( from, -17, list, board );
         self:add_quietm2( from, -15, list, board );
         self:add_quietm2( from, 15, list, board );
         self:add_quietm2( from, 17, list, board );

       else

        if(p == self.Rook64) then

          self:add_quietm2( from, -16, list, board );
          self:add_quietm2( from, -1, list, board );
          self:add_quietm2( from, 1, list, board );
          self:add_quietm2( from, 16, list, board );

        else

         if(p == self.Queen64) then

            self:add_quietm2( from, -17, list, board );
            self:add_quietm2( from, -16, list, board );
            self:add_quietm2( from, -15, list, board );
            self:add_quietm2( from, -1, list, board );
            self:add_quietm2( from, 1, list, board );
            self:add_quietm2( from, 15, list, board );
            self:add_quietm2( from, 16, list, board );
            self:add_quietm2( from, 17, list, board );

         else

           if(p == self.King64) then

              self:add_quietm1( from, -17, list, board );
              self:add_quietm1( from, -16, list, board );
              self:add_quietm1( from, -15, list, board );
              self:add_quietm1( from, -1, list, board );
              self:add_quietm1( from, 1, list, board );
              self:add_quietm1( from, 15, list, board );
              self:add_quietm1( from, 16, list, board );
              self:add_quietm1( from, 17, list, board );

           else

              --self:ASSERT(510, false);

           end
         end
        end
       end
      end

      ptr = ptr + 1;
   end

   -- pawn moves

   if(self:COLOUR_IS_WHITE(me)) then

      ptr = 0;
      while(true) do
         from = board.pawn[1+me][1+ptr];
         if( from == self.SquareNone ) then
           break;
         end

         -- non promotes

         if(self:SQUARE_RANK(from) ~= self.Rank7) then
            to = from + 16;
            if(board.square[1+to] == self.Empty) then
               --self:ASSERT(511, not self.SquareIsPromote[1+to]);
               self:LIST_ADD(list,self:MOVE_MAKE(from,to));
               if(self:SQUARE_RANK(from) == self.Rank2) then
                  to = from + 32;
                  if(board.square[1+to] == self.Empty) then
                     --self:ASSERT(512, not self.SquareIsPromote[1+to]);
                     self:LIST_ADD(list,self:MOVE_MAKE(from,to));
                  end
               end
            end
         end

         ptr = ptr + 1;
      end

   else  -- black

      ptr = 0;
      while(true) do
         from = board.pawn[1+me][1+ptr];
         if( from == self.SquareNone ) then
           break;
         end

         -- non promotes

         if(self:SQUARE_RANK(from) ~= self.Rank2) then
            to = from - 16;
            if(board.square[1+to] == self.Empty) then
               --self:ASSERT(513, not self.SquareIsPromote[1+to]);
               self:LIST_ADD(list,self:MOVE_MAKE(from,to));
               if(self:SQUARE_RANK(from) == self.Rank7) then
                  to = from - 32;
                  if(board.square[1+to] == self.Empty) then
                     --self:ASSERT(514, not self.SquareIsPromote[1+to]);
                     self:LIST_ADD(list,self:MOVE_MAKE(from,to));
                  end
               end
            end
         end

         ptr = ptr + 1;
      end

   end

end

-- self:add_promotes()

function self:add_promotes( list, board ) -- void

   local me = 0;    -- int
   local inc = 0;   -- int
   local ptr = 0;   -- int
   local from = 0;  -- int
   local to = 0;    -- int

   --self:ASSERT(515, list.size~=nil);
   --self:ASSERT(516, board.sp~=nil);

   me = board.turn;

   inc = self.PawnMoveInc[1+me];

   ptr = 0;
   while(true) do
      from = board.pawn[1+me][1+ptr];
      if( from == self.SquareNone ) then
        break;
      end

      if(self:PAWN_RANK(from,me) == self.Rank7) then
         self:add_capt4( from, inc, list, board );
         to = from + inc;
      end

      ptr = ptr + 1;
   end
end

-- self:add_en_passant_captures()

function self:add_en_passant_captures( list, board ) -- void

   local from = 0;  -- int
   local to = 0;    -- int
   local me = 0;    -- int
   local inc = 0;   -- int
   local pawn = 0;  -- int

   --self:ASSERT(517, list.size~=nil);
   --self:ASSERT(518, board.sp~=nil);

   to = board.ep_square;

   if(to ~= self.SquareNone) then

      me = board.turn;

      inc = self.PawnMoveInc[1+me];
      pawn = self.PawnMake[1+me];

      from = to -(inc-1);
      if(board.square[1+from] == pawn) then
         --self:ASSERT(519, not self.SquareIsPromote[1+to]);
         self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(from,to,self.MoveEnPassant));
      end

      from = to -(inc+1);
      if(board.square[1+from] == pawn) then
         --self:ASSERT(520, not self.SquareIsPromote[1+to]);
         self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(from,to,self.MoveEnPassant));
      end

   end

end

-- self:add_castle_moves()

function self:add_castle_moves( list, board ) -- void

   --self:ASSERT(521, list.size~=nil);
   --self:ASSERT(522, board.sp~=nil);

   --self:ASSERT(523, not self:board_is_check(board));

   if(self:COLOUR_IS_WHITE(board.turn)) then

      if( bit.band( board.flags, self.FlagsWhiteKingCastle ) ~= 0
        and  board.square[1+self.F1] == self.Empty
        and  board.square[1+self.G1] == self.Empty
        and(not self:is_attacked(board,self.F1,self.Black))) then
         self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(self.E1,self.G1,self.MoveCastle));
      end

      if( bit.band( board.flags, self.FlagsWhiteQueenCastle ) ~= 0
        and  board.square[1+self.D1] == self.Empty
        and  board.square[1+self.C1] == self.Empty
        and  board.square[1+self.B1] == self.Empty
        and(not self:is_attacked(board,self.D1,self.Black))) then
         self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(self.E1,self.C1,self.MoveCastle));
      end

   else  -- black

      if( bit.band( board.flags, self.FlagsBlackKingCastle ) ~= 0
        and  board.square[1+self.F8] == self.Empty
        and  board.square[1+self.G8] == self.Empty
        and(not self:is_attacked(board,self.F8,self.White))) then
         self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(self.E8,self.G8,self.MoveCastle));
      end

      if( bit.band( board.flags, self.FlagsBlackQueenCastle ) ~= 0
        and  board.square[1+self.D8] == self.Empty
        and  board.square[1+self.C8] == self.Empty
        and  board.square[1+self.B8] == self.Empty
        and(not self:is_attacked(board,self.D8,self.White))) then
         self:LIST_ADD(list,self:MOVE_MAKE_FLAGS(self.E8,self.C8,self.MoveCastle));
      end
   end
end

-- self:add_pawn_move()

function self:add_pawn_move( list, from, to )  -- void

   local move = 0;   -- int

   --self:ASSERT(524, list.size~=nil);
   --self:ASSERT(525, self:SQUARE_IS_OK(from));
   --self:ASSERT(526, self:SQUARE_IS_OK(to));

   move = self:MOVE_MAKE(from,to);

   if(self.SquareIsPromote[1+to]) then
      self:LIST_ADD(list,bit.bor(move,self.MovePromoteQueen));
      self:LIST_ADD(list,bit.bor(move,self.MovePromoteKnight));
      self:LIST_ADD(list,bit.bor(move,self.MovePromoteRook));
      self:LIST_ADD(list,bit.bor(move,self.MovePromoteBishop));
   else
      self:LIST_ADD(list,move);
   end
end

-- self:add_promote()

function self:add_promote( list, move )  -- void

   --self:ASSERT(527, list.size~=nil);
   --self:ASSERT(528, self:move_is_ok(move));

   --self:ASSERT(529, bit.band(move,self.bnotV07777)==0); -- HACK
   --self:ASSERT(530, self.SquareIsPromote[1+self:MOVE_TO(move)]);

   self:LIST_ADD(list,bit.bor(move,self.MovePromoteQueen));
   self:LIST_ADD(list,bit.bor(move,self.MovePromoteKnight));
   self:LIST_ADD(list,bit.bor(move,self.MovePromoteRook));
   self:LIST_ADD(list,bit.bor(move,self.MovePromoteBishop));
end

-- end of move_gen.cpp




-- move_legal.cpp

-- functions

-- self:move_is_pseudo()

function self:move_is_pseudo( move, board ) -- bool

   local me = 0;      -- int
   local opp = 0;     -- int
   local from = 0;    -- int
   local to = 0;      -- int
   local piece = 0;   -- int
   local capture = 0; -- int
   local inc = 0;     -- int
   local delta = 0;   -- int

   --self:ASSERT(531, self:move_is_ok(move));
   --self:ASSERT(532, board.sp~=nil);

   --self:ASSERT(533, not self:board_is_check(board));

   -- special cases

   if(self:MOVE_IS_SPECIAL(move)) then
      return self:move_is_pseudo_debug(move,board);
   end

   --self:ASSERT(534, bit.band(move,self.bnotV07777)==0);

   -- init

   me = board.turn;
   opp = self:COLOUR_OPP(board.turn);

   -- from

   from = self:MOVE_FROM(move);
   --self:ASSERT(535, self:SQUARE_IS_OK(from));

   piece = board.square[1+from];
   if(not self:COLOUR_IS(piece,me)) then
     return false;
   end

   --self:ASSERT(536, self:piece_is_ok(piece));

   -- to

   to = self:MOVE_TO(move);
   --self:ASSERT(537, self:SQUARE_IS_OK(to));

   capture = board.square[1+to];
   if(self:COLOUR_IS(capture,me)) then
     return false;
   end

   -- move

   if(self:PIECE_IS_PAWN(piece)) then

      if(self.SquareIsPromote[1+to]) then
        return false;
      end

      inc = self.PawnMoveInc[1+me];
      delta = to - from;
      --self:ASSERT(538, self:delta_is_ok(delta));

      if(capture == self.Empty) then

         -- pawn push

         if(delta == inc) then
           return true;
         end

         if(delta ==(2*inc)
           and  self:PAWN_RANK(from,me) == self.Rank2
           and  board.square[1+from+inc] == self.Empty) then
            return true;
         end

      else

         -- pawn capture

         if(delta ==(inc-1)  or  delta ==(inc+1)) then
           return true;
         end
      end

   else

      if(self:PIECE_ATTACK(board,piece,from,to)) then
        return true;
      end
   end

   return false;
end

-- self:quiet_is_pseudo()

function self:quiet_is_pseudo( move, board )  -- bool

   local me = 0;      -- int
   local opp = 0;     -- int
   local from = 0;    -- int
   local to = 0;      -- int
   local piece = 0;   -- int
   local inc = 0;     -- int
   local delta = 0;   -- int

   --self:ASSERT(539, self:move_is_ok(move));
   --self:ASSERT(540, board.sp~=nil);

   --self:ASSERT(541, not self:board_is_check(board));

   -- special cases

   if(self:MOVE_IS_CASTLE(move)) then
      return self:move_is_pseudo_debug(move,board);
   else
    if(self:MOVE_IS_SPECIAL(move)) then
      return false;
    end
   end

   --self:ASSERT(542, bit.band(move,self.bnotV07777)==0);

   -- init

   me = board.turn;
   opp = self:COLOUR_OPP(board.turn);

   -- from

   from = self:MOVE_FROM(move);
   --self:ASSERT(543, self:SQUARE_IS_OK(from));

   piece = board.square[1+from];
   if(not self:COLOUR_IS(piece,me)) then
     return false;
   end

   --self:ASSERT(544, self:piece_is_ok(piece));

   -- to

   to = self:MOVE_TO(move);
   --self:ASSERT(545, self:SQUARE_IS_OK(to));

   if(board.square[1+to] ~= self.Empty) then
     return false; -- capture
   end

   -- move

   if(self:PIECE_IS_PAWN(piece)) then

      if(self.SquareIsPromote[1+to]) then
        return false;
      end

      inc = self.PawnMoveInc[1+me];
      delta = to - from;
      --self:ASSERT(546, self:delta_is_ok(delta));

      -- pawn push

      if(delta == inc) then
        return true;
      end

      if(delta ==(2*inc)
        and  self:PAWN_RANK(from,me) == self.Rank2
        and  board.square[1+from+inc] == self.Empty) then
         return true;
      end

   else

      if(self:PIECE_ATTACK(board,piece,from,to)) then
        return true;
      end
   end

   return false;
end

-- self:pseudo_is_legal()

function self:pseudo_is_legal( move, board ) -- bool

   local opp = 0;        -- int
   local me = 0;         -- int
   local from = 0;       -- int
   local to = 0;         -- int
   local piece = 0;      -- int
   local legal = false;  -- bool
   local king = 0;       -- int
   local undo = self:undo_t();  --undo_t[1]

   --self:ASSERT(547, self:move_is_ok(move));
   --self:ASSERT(548, board.sp~=nil);

   -- init

   me = board.turn;
   opp = self:COLOUR_OPP(me);

   from = self:MOVE_FROM(move);
   to = self:MOVE_TO(move);

   piece = board.square[1+from];
   --self:ASSERT(549, self:COLOUR_IS(piece,me));

   -- slow test for en-passant captures

   if(self:MOVE_IS_EN_PASSANT(move)) then

      self:move_do(board,move,undo);
      legal = not self:IS_IN_CHECK(board,me);
      self:move_undo(board,move,undo);

      return legal;
   end

   -- king moves(including castle)

   if(self:PIECE_IS_KING(piece)) then

      legal = not self:is_attacked(board,to,opp);

      if(self.iDbg01) then
         --self:ASSERT(550, board.square[1+from]==piece);
         board.square[1+from] = self.Empty;
         --self:ASSERT(551, legal==not self:is_attacked(board,to,opp));
         board.square[1+from] = piece;
      end

      return legal;
   end

   -- pins

   if(self:is_pinned(board,from,me)) then
      king = self:KING_POS(board,me);
      return(self:DELTA_INC_LINE(king-to) == self:DELTA_INC_LINE(king-from)); -- does not discover the line
   end

   return true;
end

-- self:move_is_pseudo_debug()

function self:move_is_pseudo_debug( move, board) -- bool

   local list = self:list_t();  --list_t[1]

   --self:ASSERT(552, self:move_is_ok(move));
   --self:ASSERT(553, board.sp~=nil);

   --self:ASSERT(554, not self:board_is_check(board));

   self:gen_moves(list,board);

   return self:list_contain(list,move);
end

-- end of move_legal.cpp



-- option.cpp

-- functions

-- self:option_init()

function self:option_init() -- void

   local opt = nil;
   local i = 0;

   -- options are as they are for the execuatable version
   self.Option[1] = self:opt_t_def( "Hash",  false, "16", "spin", "min 4 max 1024", nil );
   self.Option[2] = self:opt_t_def( "Ponder",  false, "false", "check", "", nil );
   self.Option[3] = self:opt_t_def( "OwnBook",  false, "false", "check", "", nil );
   self.Option[4] = self:opt_t_def( "BookFile",   false, "book_small.bin", "string", "", nil );
   self.Option[5] = self:opt_t_def( "nilMove Pruning",  true, "Fail High", "combo", "var Always var Fail High var Never", nil );
   self.Option[6] = self:opt_t_def( "nilMove Reduction",  true, "3", "spin", "min 1 max 3", nil );
   self.Option[7] = self:opt_t_def( "Verification Search",  true, "self.Endgame", "combo", "var Always var self.Endgame var Never", nil );
   self.Option[8] = self:opt_t_def( "Verification Reduction",  true, "5", "spin", "min 1 max 6", nil );
   self.Option[9] = self:opt_t_def( "self.History Pruning", true, "true", "check", "", nil );
   self.Option[10] = self:opt_t_def( "self.History Threshold",  true, "60", "spin", "min 0 max 100", nil );
   self.Option[11] = self:opt_t_def( "Futility Pruning",  true, "false", "check", "", nil );
   self.Option[12] = self:opt_t_def( "Futility Margin",  true, "100", "spin",  "min 0 max 500", nil );
   self.Option[13] = self:opt_t_def( "Delta Pruning",  true, "false", "check", "", nil );
   self.Option[14] = self:opt_t_def( "Delta Margin",  true, "50", "spin",  "min 0 max 500", nil );
   self.Option[15] = self:opt_t_def( "Quiescence Check Plies", true, "1", "spin", "min 0 max 2", nil );
   self.Option[16] = self:opt_t_def( "self.Material",  true, "100", "spin", "min 0 max 400", nil );
   self.Option[17] = self:opt_t_def( "Piece Activity",  true, "100", "spin", "min 0 max 400", nil );
   self.Option[18] = self:opt_t_def( "King Safety",  true, "100", "spin", "min 0 max 400", nil );
   self.Option[19] = self:opt_t_def( "self.Pawn Structure",  true, "100", "spin", "min 0 max 400", nil );
   self.Option[20] = self:opt_t_def( "Passed Pawns",  true, "100", "spin", "min 0 max 400", nil );
   self.Option[21] = self:opt_t_def( nil, false, nil, nil, nil, nil );


   while(true) do
     opt = self.Option[1+i];
     if( opt.var == nil ) then
       break;
     end
     self:option_set( opt.var, opt.init );
     i = i + 1;
   end
end

-- self:option_list()

function self:option_list() -- void

   local opt = nil;
   local i = 0;

   while(true) do
     opt = self.Option[1+i];
     if( opt.var == nil ) then
       break;
     end

     if(opt.declare) then
         self:send("option name ".. opt.var .." type ".. opt.type .." default ".. opt.val .. opt.extra);
     end

     i = i + 1;
   end
end

-- self:option_set()

function self:option_set( var, val )  -- bool

   local i = 0;

   --self:ASSERT(555, var~=nil);
   --self:ASSERT(556, val~=nil);

   i = self:option_find(var);
   if(i == nil) then return
     false;
   end

   self.Option[1+i].val = val;

   return true;
end

-- self:option_get()

function self:option_get( var ) -- string

   local i = 0;

   --self:ASSERT(557, var~=nil);

   i = self:option_find(var);
   if(i == nil) then return
     self:my_fatal("self:option_get(): unknown option : ".. var .. "\n");
   end

   return self.Option[1+i].val;
end

-- self:option_get_bool()

function self:option_get_bool( var ) -- const bool

   local val = self:option_get(var);   -- string

   if(self:string_equal(val,"true")  or  self:string_equal(val,"yes")  or  self:string_equal(val,"1")) then
      return true;
   else
    if(self:string_equal(val,"false")  or  self:string_equal(val,"no")  or  self:string_equal(val,"0")) then
      return false;
    end
   end

   --self:ASSERT(558, false);

   return false;
end

-- self:option_get_int()

function self:option_get_int( var )  -- int
   return tonumber( self:option_get(var) );
end

-- self:option_get_string()

function self:option_get_string( var ) -- string
   return self:option_get(var);
end

-- self:option_find()

function self:option_find( var ) -- int

   local opt = nil;
   local i = 0;

   --self:ASSERT(559, var~=nil);

   while(true) do
     opt = self.Option[1+i];
     if( opt.var == nil ) then
       break;
     end

     if(self:string_equal(opt.var,var)) then
       return i;
     end


     i = i + 1;
   end

   return nil;
end

-- end of option.cpp



-- pawn.cpp

-- functions

-- self:pawn_init_bit()

function self:pawn_init_bit()  -- void

   local rank = 0;   -- int
   local first = 0;  -- int
   local last = 0;   -- int
   local count = 0;  -- int
   local b = 0;      -- int
   local rev = 0;    -- int


   -- rank-indexed Bit*[]

   for rank = 0, self.RankNb-1, 1 do

      self.BitEQ[1+rank] = 0;
      self.BitLT[1+rank] = 0;
      self.BitLE[1+rank] = 0;
      self.BitGT[1+rank] = 0;
      self.BitGE[1+rank] = 0;

      self.BitRank1[1+rank] = 0;
      self.BitRank2[1+rank] = 0;
      self.BitRank3[1+rank] = 0;
   end

   for rank = self.Rank1, self.Rank8, 1 do
      self.BitEQ[1+rank] = bit.lshift( 1, rank - self.Rank1);
      self.BitLT[1+rank] = self.BitEQ[1+rank] - 1;
      self.BitLE[1+rank] = bit.bor( self.BitLT[1+rank], self.BitEQ[1+rank] );
      self.BitGT[1+rank] = bit.bxor( self.BitLE[1+rank], 0xFF );
      self.BitGE[1+rank] = bit.bor( self.BitGT[1+rank], self.BitEQ[1+rank]);
   end

   for rank = self.Rank1, self.Rank8, 1 do
      self.BitRank1[1+rank] = self.BitEQ[1+rank+1];
      self.BitRank2[1+rank] = bit.bor( self.BitEQ[1+rank+1], self.BitEQ[1+rank+2]) ;
      self.BitRank3[1+rank] = bit.bor( bit.bor( self.BitEQ[1+rank+1], self.BitEQ[1+rank+2] ) , self.BitEQ[1+rank+3] );
   end

   -- bit-indexed Bit*[1+]

   for b = 0, 0x100-1, 1 do

      first = self.Rank8;  -- HACK for pawn shelter
      last = self.Rank1;   -- HACK
      count = 0;
      rev = 0;

      for rank = self.Rank1, self.Rank8, 1 do
         if( bit.band( b, self.BitEQ[1+rank] ) ~= 0) then
            if(rank < first) then
              first = rank;
            end
            if(rank > last) then
              last = rank;
            end
            count = count + 1;
            rev = bit.bor( rev, self.BitEQ[1+self:RANK_OPP(rank)] );
         end
      end

      self.BitFirst[1+b] = first;
      self.BitLast[1+b] = last;
      self.BitCount[1+b] = count;
      self.BitRev[1+b] = rev;
   end

end

-- self:pawn_init()

function self:pawn_init() -- void

   local rank = 0;   -- int

   -- UCI options

   self.PawnStructureWeight =(self:option_get_int("self.Pawn Structure") * 256 + 50) / 100;

   -- bonus

   for rank = 0, self.RankNb-1, 1 do

     self.Bonus[1+rank] = 0;
   end

   self.Bonus[1+self.Rank4] = 26;
   self.Bonus[1+self.Rank5] = 77;
   self.Bonus[1+self.Rank6] = 154;
   self.Bonus[1+self.Rank7] = 256;

   -- pawn hash-table

   self.Pawn.size = 0;
   self.Pawn.mask = 0;

end

-- self:pawn_alloc()

function self:pawn_alloc()  -- void

   --self:ASSERT(560, true );   -- sizeof(entry_t)==16

   if(self.UseTable) then

      self.Pawn.size = self.PawnTableSize;
      self.Pawn.mask = self.Pawn.size - 1;     -- 2^x -1
      -- self.Pawn.table =(entry_t *) my_malloc(self.Pawn.size*sizeof(entry_t));
      self:pawn_clear();
   end

end

-- self:pawn_clear()

function self:pawn_clear()  -- void

   local i = 0;

   self.Pawn.table = {};
   self.Pawn.used = 0;
   self.Pawn.read_nb = 0;
   self.Pawn.read_hit = 0;
   self.Pawn.write_nb = 0;
   self.Pawn.write_collision = 0;

end

-- self:pawn_get_info()

function self:pawn_get_info( info, board ) -- const

   local key = 0;           -- uint64
   local entry = nil        -- entry_t *;
   local index = 0;

   --self:ASSERT(561, info.lock~=nil);
   --self:ASSERT(562, board.sp~=nil);

   -- probe

  if(self.UseTable) then

      self.Pawn.read_nb = self.Pawn.read_nb + 1;

      key = board.pawn_key;
      index = bit.band( self:KEY_INDEX(key), self.Pawn.mask );

      entry = self.Pawn.table[1+index];
      if(entry == nil or entry.lock == nil) then
        self.Pawn.table[1+index] = self:pawn_info_t();
        entry = self.Pawn.table[1+index];
      end

      if(entry.lock == self:KEY_LOCK(key)) then

         -- found

         self.Pawn.read_hit = self.Pawn.read_hit + 1;

         self:pawn_info_copy( info, entry );

         return;
      end
   end

   -- calculation

   self:pawn_comp_info(info,board);

   -- store

   if(self.UseTable) then

      self.Pawn.write_nb = self.Pawn.write_nb + 1;

      if(entry.lock == 0) then    -- HACK: assume free entry
         self.Pawn.used = self.Pawn.used + 1;
      else
         self.Pawn.write_collision = self.Pawn.write_collision + 1;
      end

      self:pawn_info_copy( entry, info );

      entry.lock = self:KEY_LOCK(key);
   end

end

-- self:pawn_comp_info()

function self:pawn_comp_info( info, board ) -- void

   local colour = 0;   -- int
   local file = 0;     -- int
   local rank = 0;     -- int
   local me = 0;       -- int
   local opp = 0;      -- int
   local ptr = 0;      -- int
   local sq = 0;       -- int
   local backward = false;    -- bool
   local candidate = false;   -- bool
   local doubled = false;     -- bool
   local isolated = false;    -- bool
   local open = false;        -- bool
   local passed = false;      -- bool
   local t1 = 0;        -- int
   local t2 = 0;        -- int
   local n = 0;         -- int
   local bits = 0;      -- int
   local opening = { 0, 0 };  -- int[self.ColourNb]
   local endgame = { 0, 0 };  -- int[self.ColourNb]
   local flags = { 0, 0 };    -- int[self.ColourNb]
   local file_bits = { 0, 0 };   -- int[self.ColourNb]
   local passed_bits = { 0, 0 }; -- int[self.ColourNb]
   local single_file = { 0, 0 }; -- int[self.ColourNb]
   local q = 0;
   local om = 0;
   local em = 0;

   --self:ASSERT(563, info.lock~=nil);
   --self:ASSERT(564, board.sp~=nil);

   -- pawn_file[]

-- #if DEBUG
   for colour = 0, 1, 1 do

      local pawn_file = {};   -- int[self.FileNb]

      me = colour;

      for file = 0, self.FileNb-1, 1 do
         pawn_file[1+file] = 0;
      end

      ptr = 0;
      while(true) do
         sq=board.pawn[1+me][1+ptr];
         if(sq==self.SquareNone) then
           break;
         end

         file = self:SQUARE_FILE(sq);
         rank = self:PAWN_RANK(sq,me);

         --self:ASSERT(565, file>=self.FileA and file<=self.FileH);
         --self:ASSERT(566, rank>=self.Rank2 and rank<=self.Rank7);

         pawn_file[1+file] =  bit.bor( pawn_file[1+file] , self.BitEQ[1+rank] );

         ptr = ptr + 1;
      end

      for file = 0, self.FileNb-1, 1 do
         if(board.pawn_file[1+colour][1+file] ~= pawn_file[1+file]) then
           self:my_fatal("board.pawn_file[][]\n");
         end
      end
   end
-- #endif


   -- features and scoring

   for colour = 0, 1, 1 do

      me = colour;
      opp = self:COLOUR_OPP(me);

      ptr = 0;
      while(true) do
         sq=board.pawn[1+me][1+ptr];
         if(sq==self.SquareNone) then
           break;
         end


         -- init

         file = self:SQUARE_FILE(sq);
         rank = self:PAWN_RANK(sq,me);

         --self:ASSERT(567, file>=self.FileA and file<=self.FileH);
         --self:ASSERT(568, rank>=self.Rank2 and rank<=self.Rank7);

         -- flags

         file_bits[1+me] = bit.bor( file_bits[1+me], self.BitEQ[1+file] );
         if(rank == self.Rank2) then
            flags[1+me] = bit.bor( flags[1+me], self.BackRankFlag );
         end

         -- features

         backward = false;
         candidate = false;
         doubled = false;
         isolated = false;
         open = false;
         passed = false;

         t1 = bit.bor( board.pawn_file[1+me][1+file-1], board.pawn_file[1+me][1+file+1] );
         t2 = bit.bor( board.pawn_file[1+me][1+file], self.BitRev[1+board.pawn_file[1+opp][1+file]] );

         -- doubled

         if( bit.band( board.pawn_file[1+me][1+file] , self.BitLT[1+rank] ) ~= 0) then
            doubled = true;
         end

         -- isolated and backward

         if(t1 == 0) then

            isolated = true;

         else
          if( bit.band( t1, self.BitLE[1+rank] ) == 0) then

            backward = true;

            -- really backward?

            if( bit.band( t1, self.BitRank1[1+rank] ) ~= 0) then

               --self:ASSERT(569, rank+2<=self.Rank8);
               q = bit.band(t2,self.BitRank1[1+rank]);
               q = bit.bor(q,self.BitRev[1+board.pawn_file[1+opp][1+file-1]]);
               q = bit.bor(q,self.BitRev[1+board.pawn_file[1+opp][1+file+1]]);

               if( bit.band( q, self.BitRank2[1+rank] ) == 0) then

                  backward = false;
               end

            else
             if(rank == self.Rank2  and( bit.band( t1 , self.BitEQ[1+rank+2] ) ~= 0)) then

               --self:ASSERT(570, rank+3<=self.Rank8);
               q = bit.band(t2,self.BitRank2[1+rank]);
               q = bit.bor(q,self.BitRev[1+board.pawn_file[1+opp][1+file-1]]);
               q = bit.bor(q,self.BitRev[1+board.pawn_file[1+opp][1+file+1]]);

               if( bit.band( q, self.BitRank3[1+rank] ) == 0) then

                  backward = false;
               end
             end
            end
          end
         end

         -- open, candidate and passed

         if( bit.band( t2, self.BitGT[1+rank] ) == 0) then

            open = true;

            q = bit.bor( self.BitRev[1+board.pawn_file[1+opp][1+file-1]] ,
                     self.BitRev[1+board.pawn_file[1+opp][1+file+1]]);

            if( bit.band( q, self.BitGT[1+rank] ) == 0) then

               passed = true;
               passed_bits[1+me] = bit.bor( passed_bits[1+me], self.BitEQ[1+file] );

            else

               -- candidate?

               n = 0;

               n = n + self.BitCount[1+bit.band( board.pawn_file[1+me][1+file-1], self.BitLE[1+rank] ) ];
               n = n + self.BitCount[1+bit.band( board.pawn_file[1+me][1+file+1], self.BitLE[1+rank] ) ];

               n = n - self.BitCount[1+bit.band( self.BitRev[1+board.pawn_file[1+opp][1+file-1]], self.BitGT[1+rank] ) ];
               n = n - self.BitCount[1+bit.band( self.BitRev[1+board.pawn_file[1+opp][1+file+1]], self.BitGT[1+rank] ) ];

               if(n >= 0) then

                  -- safe?

                  n = 0;

                  n = n + self.BitCount[1+bit.band( board.pawn_file[1+me][1+file-1], self.BitEQ[1+rank-1] ) ];
                  n = n + self.BitCount[1+bit.band( board.pawn_file[1+me][1+file+1], self.BitEQ[1+rank-1] ) ];

                  n = n - self.BitCount[1+bit.band( self.BitRev[1+board.pawn_file[1+opp][1+file-1]], self.BitEQ[1+rank+1] ) ];
                  n = n - self.BitCount[1+bit.band( self.BitRev[1+board.pawn_file[1+opp][1+file+1]], self.BitEQ[1+rank+1] ) ];

                  if(n >= 0) then
                    candidate = true;
                  end
               end
            end
         end

         -- score

         om = opening[1+me];
         em = endgame[1+me];

         if(doubled) then
            om = om - self.DoubledOpening;
            em = em - self.DoubledEndgame;
         end

         if(isolated) then
            if(open) then
               om = om - self.IsolatedOpeningOpen;
               em = em - self.IsolatedEndgame;
            else
               om = om - self.IsolatedOpening;
               em = em - self.IsolatedEndgame;
            end
         end

         if(backward) then
            if(open) then
               om = om - self.BackwardOpeningOpen;
               em = em - self.BackwardEndgame;
            else
               om = om - self.BackwardOpening;
               em = em - self.BackwardEndgame;
            end
         end

         if(candidate) then
            om = om + self:quad(self.CandidateOpeningMin,self.CandidateOpeningMax,rank);
            em = em + self:quad(self.CandidateEndgameMin,self.CandidateEndgameMax,rank);
         end


         opening[1+me] = om;
         endgame[1+me] = em;

         ptr = ptr + 1;

      end
   end

   -- store info

   info.opening =((opening[1+self.White] - opening[1+self.Black]) * self.PawnStructureWeight) / 256;
   info.endgame =((endgame[1+self.White] - endgame[1+self.Black]) * self.PawnStructureWeight) / 256;

   for colour = 0, 1, 1 do

      me = colour;
      opp = self:COLOUR_OPP(me);

      -- draw flags

      bits = file_bits[1+me];

      if(bits ~= 0  and( bit.band( bits, bits-1 ) == 0) ) then  -- one set bit

         file = self.BitFirst[1+bits];
         rank = self.BitFirst[1+board.pawn_file[1+me][1+file] ];
         --self:ASSERT(571, rank>=self.Rank2);

         q = bit.bor( self.BitRev[1+board.pawn_file[1+opp][1+file-1]],
                     self.BitRev[1+board.pawn_file[1+opp][1+file+1]] );

         if( bit.band( q, self.BitGT[1+rank] ) == 0) then

            rank = self.BitLast[1+board.pawn_file[1+me][1+file] ];
            single_file[1+me] = self:SQUARE_MAKE(file,rank);
         end
      end

      info.flags[1+colour] = flags[1+colour];
      info.passed_bits[1+colour] = passed_bits[1+colour];
      info.single_file[1+colour] = single_file[1+colour];
   end

end

-- self:quad()

function self:quad( y_min, y_max, x )  -- int

   local y = 0;   -- int

   --self:ASSERT(572, y_min>=0 and y_min<=y_max and y_max<=32767);
   --self:ASSERT(573, x>=self.Rank2 and x<=self.Rank7);

   y =  math.floor( y_min +((y_max - y_min) * self.Bonus[1+x] + 128) / 256 );

   --self:ASSERT(574, y>=y_min and y<=y_max);

   return y;
end

-- end of pawn.cpp



-- piece.cpp

-- functions

-- self:piece_init()

function self:piece_init() -- void

   local piece = 0;      -- int
   local piece_12 = 0;   -- int

   -- self.PieceTo12[], self.PieceOrder[], self.PieceInc[]

   for piece = 0, self.PieceNb-1, 1 do
     self.PieceTo12[1+piece] = -1;
     self.PieceOrder[1+piece] = -1;
     self.PieceInc[1+piece] = nil;
   end

   for piece_12 = 0, 11, 1 do
      self.PieceTo12[1+self.PieceFrom12[1+piece_12]] = piece_12;
      self.PieceOrder[1+self.PieceFrom12[1+piece_12]] = bit.rshift( piece_12, 1 );
   end

   self.PieceInc[1+self.WhiteKnight256] = self.KnightInc;
   self.PieceInc[1+self.WhiteBishop256] = self.BishopInc;
   self.PieceInc[1+self.WhiteRook256]   = self.RookInc;
   self.PieceInc[1+self.WhiteQueen256]  = self.QueenInc;
   self.PieceInc[1+self.WhiteKing256]   = self.KingInc;

   self.PieceInc[1+self.BlackKnight256] = self.KnightInc;
   self.PieceInc[1+self.BlackBishop256] = self.BishopInc;
   self.PieceInc[1+self.BlackRook256]   = self.RookInc;
   self.PieceInc[1+self.BlackQueen256]  = self.QueenInc;
   self.PieceInc[1+self.BlackKing256]   = self.KingInc;

end

-- self:piece_is_ok()

function self:piece_is_ok( piece )  -- bool

   if(piece < 0  or  piece >= self.PieceNb) then
     return false;
   end
   if(self.PieceTo12[1+piece] < 0) then
     return false;
   end
   return true;
end

-- self:piece_to_char()

function self:piece_to_char( piece )  -- int

   local i = self.PieceTo12[1+piece];

   --self:ASSERT(576, self:piece_is_ok(piece));

   return string.sub(self.PieceString, 1+i, 1+i );
end

-- self:piece_from_char()

function self:piece_from_char( c )  -- int

   local ptr = string.find(self.PieceString,c);   -- int

   if(ptr == nil) then
     return self.PieceNone256;
   end
   ptr = ptr - 1;

   --self:ASSERT(575, ptr>=0 and ptr<12);

   return self.PieceFrom12[1+ptr];
end

-- end of piece.cpp




-- protocol.cpp

-- functions

function self:setstartpos()

   -- init(to help debugging)

   self.Init = false;

   self:search_clear();

   self:board_from_fen(self.SearchInput.board,self.StartFen);

end

-- self:inits()

function self:inits()

   if(not self.Init) then

      -- late initialisation

      self.Init = true;

      if(self:option_get_bool("OwnBook")) then
        --   book_open(self:option_get_string("BookFile"));
        self:send("Sorry, no book.");
      end

      self:trans_alloc(self.Trans);

      self:pawn_init();
      self:pawn_alloc();

      self:material_init();
      self:material_alloc();

      self:pst_init();
      self:eval_init();
   end
end

-- loop_step()

function self:do_input( cmd, loopFn, stepFn, onDone )

   loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
   stepFn = stepFn or function(items)
       if type(items) == "function" then
          items()
       else
          for _, step in ipairs(items) do
              step()
          end
       end
   end
   onDone = onDone or function() end

   local ifelse = true;

   -- parse

   if(ifelse and self:string_start_with(cmd,"go")) then

      self:inits();
      self:parse_go( cmd, loopFn, stepFn, onDone );

      ifelse = false;
      return;  -- parse_go calls onDone asynchronously when search completes
   end

   if(ifelse and self:string_equal(cmd,"isready")) then

      self:inits();
      self:send("readyok");

      ifelse = false;
   end


   if(ifelse and self:string_start_with(cmd,"position ")) then

      self:inits();
      self:parse_position(cmd);

      ifelse = false;
   end


   if(ifelse and self:string_start_with(cmd,"setoption ")) then

      self:parse_setoption( cmd );

      ifelse = false;
   end


   if(ifelse and self:string_equal(cmd,"help")) then


      self:send("supports commands: setposition fen, setposition moves, go depth, go movetime ");

      -- can manage also options, but for lua is better to use the default settings

      -- self:option_list();

      ifelse = false;
   end

   -- For non-go commands, call onDone synchronously
   onDone();

end

-- self:parse_go()

function self:parse_go( cmd, loopFn, stepFn, onDone ) -- void

   loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
   stepFn = stepFn or function(items)
       if type(items) == "function" then
           items()
       else
           for _, step in ipairs(items) do
               step()
           end
       end
   end
   onDone = onDone or function() end

   local cmd1 = "";          -- string
   local cmd2 = "";          -- string
   local infinite = false;   -- bool
   local depth = -1;         -- int
   local movetime = -1.0;    -- int
   local ifelse = false;
   local save_board = self:string_t();

   -- parse

   cmd1 = self:str_after_ok(cmd," ");    -- skip "go"
   cmd2 = self:str_after_ok(cmd1," ");   -- value
   cmd1 = self:str_before_ok(cmd1.." "," ");

   ifelse = true;
   if(ifelse and self:string_equal(cmd1,"depth")) then

      depth = tonumber(cmd2);
      --self:ASSERT(590, depth>=0);

      ifelse = false;
   end

   if(ifelse and self:string_equal(cmd1,"infinite")) then

      infinite = true;

      ifelse = false;
   end

   if(ifelse and self:string_equal(cmd1,"movetime")) then

      movetime = tonumber(cmd2);
      --self:ASSERT(593, movetime>=0.0);

      ifelse = false;
   end

   if(ifelse) then

      movetime = 10;   -- Otherwise constantly 10 secs

      ifelse = false;
   end


   -- init

   self:ClearAll();

   -- depth limit

   if(depth >= 0) then
      self.SearchInput.depth_is_limited = true;
      self.SearchInput.depth_limit = depth;
   end

   -- time limit

   if(movetime >= 0.0) then

      -- fixed time

      self.SearchInput.time_is_limited = true;
      self.SearchInput.time_limit_1 = movetime;
      self.SearchInput.time_limit_2 = movetime;

   end

   if(infinite) then
      self.SearchInput.infinite = true;
   end

   -- search

   if( not self.ShowInfo) then
     self:send("Thinking(self.ShowInfo=false)...");
   end

   self:board_to_fen(self.SearchInput.board, save_board);   -- save board for sure

   self:search(loopFn, stepFn, function()
      self:search_update_current();

      self:board_from_fen(self.SearchInput.board, save_board.v); -- and restore after search

      self:send_best_move();

      onDone();
   end)

end

-- self:parse_position()

function self:parse_position( cmd ) -- void

   local cmd1 = "";          -- string
   local cmd2 = "";          -- string
   local mc = 0;
   local mnext = "";         -- string

   local move_string = self:string_t();   -- string

   local move = 0;          -- int
   local undo = self:undo_t();  -- undo_t[1]

   cmd1 = self:str_after_ok(cmd," ");    -- skip "position"
   cmd2 = self:str_after_ok(cmd1," ");   -- value

   -- start position

   if( self:string_start_with(cmd1,"fen") ) then  -- "fen" present

      self:board_from_fen(self.SearchInput.board,cmd2);

   else

    if( self:string_start_with(cmd1,"moves") ) then  -- "moves" present

      self:board_from_fen(self.SearchInput.board,self.StartFen);

      mc = 0;

      mnext = cmd2;
      while(true) do

         if( string.len(mnext)==0 ) then
           break;
         end

         move_string.v = self:iif( string.find( mnext," ")==nil, mnext, self:str_before_ok(mnext," ") );

         move = self:move_from_string(move_string,self.SearchInput.board);

         self:move_do(self.SearchInput.board,move,undo);

         mnext = self:str_after_ok(mnext," ");

         mc = mc + 1

      end

      self.SearchInput.board.movenumb = 1+math.floor(mc/2);

    else

      -- HACK: assumes startpos

      self:board_from_fen(self.SearchInput.board,self.StartFen);

    end
   end

end

-- self:parse_setoption()

function self:parse_setoption( cmd ) -- void

   local cmd1 = "";    -- string
   local cmd2 = "";    -- string

   local name = "";    -- string
   local value = "";   -- string

   cmd1 = self:str_after_ok(cmd," ");    -- skip "setoption"

   name = self:str_after_ok(cmd1,"name ");
   name = self:str_before_ok(name.." "," ");

   value = self:str_after_ok(cmd1,"value ");
   value  = self:str_before_ok(value.." "," ");


   if( string.len(name)>0 and string.len(value)>0 )  then

     -- update

     self:option_set(name,value);
   end

   -- update transposition-table size if needed

   if(self.Init  and  self:string_equal(name,"Hash")) then  -- self.Init => already allocated

      if(self:option_get_int("Hash") >= 4) then
         self:trans_alloc(self.Trans);
      end
   end

end


-- self:send_best_move()

function self:send_ndtm( ch )

   local s = "info";
   local s2 = "";

   if(ch>5) then
     s = s .. " depth " .. self:formatd( self.SearchCurrent.depth );
     s = s .. " seldepth " .. self:formatd( self.SearchCurrent.max_depth ) .. " ";
   end

   if(ch>=20 and ch<=22) then
     s2 = s2 .. " score mate " .. self:formatd( self.SearchCurrent.mate ) .. " ";
   end
   if(ch==11 or ch==21) then
     s2 = s2 .. "lowerbound ";
   end
   if(ch==12 or ch==22) then
     s2 = s2 .. "upperbound ";
   end

   s = s .. " " .. s2 .. "time ".. string.format("%.0f", self.SearchCurrent.time ) .. "s";
   s = s .. " nodes " .. self:formatd( self.SearchCurrent.node_nb );
   s = s .. " nps " ..   string.format("%.0f", self.SearchCurrent.speed );

   self:send( s );

end

function self:send_best_move()

   local move_string = self:string_t();     -- string
   local ponder_string = self:string_t();   -- string

   local move = 0;      -- int
   local pv = nil;

   -- info

   self:send_ndtm(1);


   self:trans_stats(self.Trans);
   -- pawn_stats();
   -- material_stats();

   -- best move

   move = self.SearchBest.move;
   pv = self.SearchBest.pv;

   self:move_to_string(move,move_string);

   if((false) and pv[1+0] == move  and  self:move_is_ok(pv[1+1])) then

        -- no pondering for lua, too slow

      self:move_to_string(pv[1+1],ponder_string);
      self:send("bestmove " .. move_string.v .. " ponder " .. ponder_string.v);
   else
      self:send("bestmove " .. move_string.v);
   end

   self.bestmv = move_string.v;

   self:format_best_mv2( move );

end

-- move for pgn

function self:format_best_mv2( move )

   local piece = 0;
   local piecech = "";
   local mvattr = "";
   local promos = "";
   local ckmt = "";
   local board = self.SearchInput.board;

   if( self:MOVE_IS_CASTLE(move) ) then
      self.bestmv2 = self:iif( string.sub(self.bestmv,3,3) == "g", "0-0","0-0-0" );
   else
      piece = board.square[1+self:MOVE_FROM(move)];
      if( not self:piece_is_ok( piece ) or piece == self.PieceNone64 ) then
        piece = board.square[1+self:MOVE_TO(move)];
      end

      piecech = string.upper( self:piece_to_char(piece) );
      if( piecech == "P") then
        piecech = "";
      end

      mvattr = self:iif( self:move_is_capture(move,board), "x", "-" );

      if( string.len(self.bestmv)>4 ) then
        promos = string.sub(self.bestmv,5,5);
      end

      if( self:move_is_check(move,board) ) then
        ckmt = "+";
      end

      self.bestmv2 = piecech .. string.sub(self.bestmv,1,2) .. mvattr .. string.sub(self.bestmv,3,4) .. promos .. ckmt;

   end
end


-- self:send()

function self:send( str1 ) -- void

   --self:ASSERT(605, str1~=nil);

   if( not self.ShowInfo and self:string_start_with(str1,"info ")) then
     return;
   end

   self:log( str1 );
end

-- self:string_equal()

function self:string_equal( s1, s2 ) -- bool

   --self:ASSERT(606, s1~=nil);
   --self:ASSERT(607, s2~=nil);

   return(s1==s2);
end

-- self:string_start_with()

function self:string_start_with( s1,  s2 ) -- bool

   local l1=string.len(s1);
   local l2=string.len(s2);

   --self:ASSERT(608, s1~=nil);
   --self:ASSERT(609, s2~=nil);

   return(l1>=l2) and(string.sub(s1,1,l2)==s2);
end


-- self:str_before_ok()

function self:str_before_ok( str1, c )
  local i = string.find( str1, c );
  if(i~=nil) then
    return string.sub( str1, 1, i-1 );
  end
  return "";
end

-- self:str_after_ok()

function self:str_after_ok( str1, c )
  local i = string.find( str1, c );
  if(i~=nil) then
    return string.sub( str1, i+ string.len(c) );
  end
  return "";
end

-- end of protocol.cpp



-- pst.cpp



-- macros

function self:Pget( piece_12,square_64,stage )
  return self.Pst[1+piece_12][1+square_64][1+stage];
end

function self:Pset( piece_12,square_64,stage, value )
  self.Pst[1+piece_12][1+square_64][1+stage] = value;
end

function self:Padd( piece_12,square_64,stage, value )
  self.Pst[1+piece_12][1+square_64][1+stage] = self.Pst[1+piece_12][1+square_64][1+stage] + value;
end

function self:Pmul( piece_12,square_64,stage, value )
  self.Pst[1+piece_12][1+square_64][1+stage] = self.Pst[1+piece_12][1+square_64][1+stage] * value;
end

-- functions

-- self:pst_init()

function self:pst_init()

   local i = 0;      -- int
   local piece = 0;  -- int
   local sq = 0;     -- int
   local stage = 0;  -- int

   -- UCI options

   self.PieceActivityWeight =(self:option_get_int("Piece Activity") * 256 + 50) / 100;
   self.KingSafetyWeight    =(self:option_get_int("King Safety")    * 256 + 50) / 100;
   self.PawnStructureWeight =(self:option_get_int("self.Pawn Structure") * 256 + 50) / 100;

   -- init

   for piece = 0, 11, 1 do
      self.Pst[1+piece] = {};
      for sq = 0, 63, 1 do
         self.Pst[1+piece][1+sq] = {};
         for stage = 0, self.StageNb-1, 1 do
            self:Pset(piece,sq,stage, 0);
         end
      end
   end

   -- pawns

   piece = self.WhitePawn12;

   -- file

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening, self.PawnFile[1+self:square_file(sq)] * self.PawnFileOpening );
   end

   -- centre control

   self:Padd(piece,self.pD3,self.Opening, 10);
   self:Padd(piece,self.pE3,self.Opening, 10);

   self:Padd(piece,self.pD4,self.Opening, 20);
   self:Padd(piece,self.pE4,self.Opening, 20);

   self:Padd(piece,self.pD5,self.Opening, 10);
   self:Padd(piece,self.pE5,self.Opening, 10);

   -- weight

   for sq = 0, 63, 1 do
      self:Pmul(piece,sq,self.Opening,  self.PawnStructureWeight / 256);
      self:Pmul(piece,sq,self.Endgame,  self.PawnStructureWeight / 256);
   end

   -- knights

   piece = self.WhiteKnight12;

   -- centre

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening, self.KnightLine[1+self:square_file(sq)] * self.KnightCentreOpening);
      self:Padd(piece,sq,self.Opening, self.KnightLine[1+self:square_rank(sq)] * self.KnightCentreOpening);
      self:Padd(piece,sq,self.Endgame, self.KnightLine[1+self:square_file(sq)] * self.KnightCentreEndgame);
      self:Padd(piece,sq,self.Endgame, self.KnightLine[1+self:square_rank(sq)] * self.KnightCentreEndgame);
   end

   -- rank

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening, self.KnightRank[1+self:square_rank(sq)] * self.KnightRankOpening);
   end

   -- back rank

   for sq = self.pA1, self.pH1, 1 do    -- HACK: only first rank
      self:Padd(piece,sq,self.Opening, -self.KnightBackRankOpening);
   end

   -- "trapped"

   self:Padd(piece,self.pA8,self.Opening, -self.KnightTrapped);
   self:Padd(piece,self.pH8,self.Opening, -self.KnightTrapped);

   -- weight

   for sq = 0, 63, 1 do
      self:Pmul(piece,sq,self.Opening,  self.PieceActivityWeight / 256);
      self:Pmul(piece,sq,self.Endgame,  self.PieceActivityWeight / 256);
   end

   -- bishops

   piece = self.WhiteBishop12;

   -- centre

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening,  self.BishopLine[1+self:square_file(sq)] * self.BishopCentreOpening);
      self:Padd(piece,sq,self.Opening,  self.BishopLine[1+self:square_rank(sq)] * self.BishopCentreOpening);
      self:Padd(piece,sq,self.Endgame,  self.BishopLine[1+self:square_file(sq)] * self.BishopCentreEndgame);
      self:Padd(piece,sq,self.Endgame,  self.BishopLine[1+self:square_rank(sq)] * self.BishopCentreEndgame);
   end

   -- back rank

   for sq = self.pA1, self.pH1, 1 do    -- HACK: only first rank
      self:Padd(piece,sq,self.Opening, -self.BishopBackRankOpening);
   end

   -- main diagonals

   for i = 0, 7, 1 do
      sq = self:square_make(i,i);
      self:Padd(piece,sq,self.Opening, self.BishopDiagonalOpening);
      self:Padd(piece,self:square_opp(sq),self.Opening, self.BishopDiagonalOpening);
   end

   -- weight

   for sq = 0, 63, 1 do
      self:Pmul(piece,sq,self.Opening,  self.PieceActivityWeight / 256);
      self:Pmul(piece,sq,self.Endgame,  self.PieceActivityWeight / 256);
   end

   -- rooks

   piece = self.WhiteRook12;

   -- file

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening, self.RookFile[1+self:square_file(sq)] * self.RookFileOpening);
   end

   -- weight

   for sq = 0, 63, 1 do
      self:Pmul(piece,sq,self.Opening,  self.PieceActivityWeight / 256);
      self:Pmul(piece,sq,self.Endgame,  self.PieceActivityWeight / 256);
   end

   -- queens

   piece = self.WhiteQueen12;

   -- centre

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening, self.QueenLine[1+self:square_file(sq)] * self.QueenCentreOpening);
      self:Padd(piece,sq,self.Opening, self.QueenLine[1+self:square_rank(sq)] * self.QueenCentreOpening);
      self:Padd(piece,sq,self.Endgame, self.QueenLine[1+self:square_file(sq)] * self.QueenCentreEndgame);
      self:Padd(piece,sq,self.Endgame, self.QueenLine[1+self:square_rank(sq)] * self.QueenCentreEndgame);
   end

   -- back rank

   for sq = self.pA1, self.pH1, 1 do    -- HACK: only first rank
      self:Padd(piece,sq,self.Opening, -self.QueenBackRankOpening);
   end

   -- weight

   for sq = 0, 63, 1 do
      self:Pmul(piece,sq,self.Opening, self.PieceActivityWeight / 256);
      self:Pmul(piece,sq,self.Endgame, self.PieceActivityWeight / 256);
   end

   -- kings

   piece = self.WhiteKing12;

   -- centre

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Endgame, self.KingLine[1+self:square_file(sq)] * self.KingCentreEndgame);
      self:Padd(piece,sq,self.Endgame, self.KingLine[1+self:square_rank(sq)] * self.KingCentreEndgame);
   end

   -- file

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening, self.KingFile[1+self:square_file(sq)] * self.KingFileOpening);
   end

   -- rank

   for sq = 0, 63, 1 do
      self:Padd(piece,sq,self.Opening, self.KingRank[1+self:square_rank(sq)] * self.KingRankOpening);
   end

   -- weight

   for sq = 0, 63, 1 do
      self:Pmul(piece,sq,self.Opening, self.KingSafetyWeight / 256);
      self:Pmul(piece,sq,self.Endgame, self.PieceActivityWeight / 256);
   end

   -- symmetry copy for black

   for piece = 0, 11, 2 do -- HACK
      for sq = 0, 63, 1 do
         for stage = 0, self.StageNb-1, 1 do
            self:Pset(piece+1,sq,stage, -self:Pget(piece,self:square_opp(sq),stage) ); -- HACK
         end
      end
   end

end

-- self:square_make()

function self:square_make( file, rank )  -- int

   --self:ASSERT(610, file>=0 and file<8);
   --self:ASSERT(611, rank>=0 and rank<8);

   return bit.bor( bit.lshift(rank,3) , file);
end

-- self:square_file()

function self:square_file( square )  -- int

   --self:ASSERT(612, square>=0 and square<64);

   return bit.band( square, 7 );
end

-- self:square_rank()

function self:square_rank( square )  -- int

   --self:ASSERT(613, square>=0 and square<64);

   return bit.rshift(square,3);
end

-- self:square_opp()

function self:square_opp( square )  -- int

   --self:ASSERT(614, square>=0 and square<64);

   return bit.bxor(square,56);
end

-- end of pst.cpp




-- pv.cpp


-- functions

-- self:pv_is_ok()

function self:pv_is_ok( pv )  -- bool

   local pos = 0;    -- int
   local move = 0;   -- int

   if(pv[1+0] == nil) then
     return false;
   end

   while(true) do

      if(pos >= 256) then
        return false;
      end
      move = pv[1+pos];

      if(move == self.MoveNone) then
        return true;
      end
      if(not self:move_is_ok(move)) then
        return false;
      end

      pos = pos + 1;
   end

   return true;
end

-- self:pv_copy()

function self:pv_copy( dst, src ) -- void

   local i = 0;  -- int
   local m = 0;  -- int

   --self:ASSERT(615, self:pv_is_ok(src));

   while(true) do
      m = src[1+i];
      dst[1+i] = m;
      if( m == self.MoveNone) then
        break;
      end
      i = i + 1;
   end

end

-- self:pv_cat()

function self:pv_cat( dst, src, move )  -- int

   local i = 0;  -- int
   local m = 0;  -- int

   --self:ASSERT(617, self:pv_is_ok(src));

   dst[1+0] = move;

   while(true) do
      m = src[1+i];
      dst[1+i+1] = m;
      if( m == self.MoveNone) then
        break;
      end
      i = i + 1;
   end

end

-- self:pv_to_string()


function self:pv_to_string( pv, str1 )  -- bool

   local i = 0;              -- int
   local move = 0;           -- int
   local str2 = self:string_t();  -- string_t[1]

   --self:ASSERT(619, self:pv_is_ok(pv));
   --self:ASSERT(620, str1.v~=nil);

   -- init

   str1.v = "";

   -- loop

   while(true) do

      move = pv[1+i];
      if(move==self.MoveNone) then
        break;
      end

      if(i>0) then
        str1.v = str1.v .. " ";
      end

      self:move_to_string(move, str2);
      str1.v = str1.v .. str2.v;

      i = i + 1;
   end

   return true;

end

-- end of pv.cpp




-- random.cpp

-- we simply ignore 32bits of number
-- so, we can't read polyglot book
-- anyway, we can hash now

-- functions

function self:Rn64(s64b)
 self.Random64 [1+self.R64_i] = tonumber( string.sub( s64b, 1, 10 ) );
 self.R64_i = self.R64_i + 1;
end

-- self:random_init()

function self:random_init()

   self:Rn64("0x9D39247E33776D41"); self:Rn64("0x2AF7398005AAA5C7"); self:Rn64("0x44DB015024623547"); self:Rn64("0x9C15F73E62A76AE2");
   self:Rn64("0x75834465489C0C89"); self:Rn64("0x3290AC3A203001BF"); self:Rn64("0x0FBBAD1F61042279"); self:Rn64("0xE83A908FF2FB60CA");
   self:Rn64("0x0D7E765D58755C10"); self:Rn64("0x1A083822CEAFE02D"); self:Rn64("0x9605D5F0E25EC3B0"); self:Rn64("0xD021FF5CD13A2ED5");
   self:Rn64("0x40BDF15D4A672E32"); self:Rn64("0x011355146FD56395"); self:Rn64("0x5DB4832046F3D9E5"); self:Rn64("0x239F8B2D7FF719CC");
   self:Rn64("0x05D1A1AE85B49AA1"); self:Rn64("0x679F848F6E8FC971"); self:Rn64("0x7449BBFF801FED0B"); self:Rn64("0x7D11CDB1C3B7ADF0");
   self:Rn64("0x82C7709E781EB7CC"); self:Rn64("0xF3218F1C9510786C"); self:Rn64("0x331478F3AF51BBE6"); self:Rn64("0x4BB38DE5E7219443");
   self:Rn64("0xAA649C6EBCFD50FC"); self:Rn64("0x8DBD98A352AFD40B"); self:Rn64("0x87D2074B81D79217"); self:Rn64("0x19F3C751D3E92AE1");
   self:Rn64("0xB4AB30F062B19ABF"); self:Rn64("0x7B0500AC42047AC4"); self:Rn64("0xC9452CA81A09D85D"); self:Rn64("0x24AA6C514DA27500");
   self:Rn64("0x4C9F34427501B447"); self:Rn64("0x14A68FD73C910841"); self:Rn64("0xA71B9B83461CBD93"); self:Rn64("0x03488B95B0F1850F");
   self:Rn64("0x637B2B34FF93C040"); self:Rn64("0x09D1BC9A3DD90A94"); self:Rn64("0x3575668334A1DD3B"); self:Rn64("0x735E2B97A4C45A23");
   self:Rn64("0x18727070F1BD400B"); self:Rn64("0x1FCBACD259BF02E7"); self:Rn64("0xD310A7C2CE9B6555"); self:Rn64("0xBF983FE0FE5D8244");
   self:Rn64("0x9F74D14F7454A824"); self:Rn64("0x51EBDC4AB9BA3035"); self:Rn64("0x5C82C505DB9AB0FA"); self:Rn64("0xFCF7FE8A3430B241");
   self:Rn64("0x3253A729B9BA3DDE"); self:Rn64("0x8C74C368081B3075"); self:Rn64("0xB9BC6C87167C33E7"); self:Rn64("0x7EF48F2B83024E20");
   self:Rn64("0x11D505D4C351BD7F"); self:Rn64("0x6568FCA92C76A243"); self:Rn64("0x4DE0B0F40F32A7B8"); self:Rn64("0x96D693460CC37E5D");
   self:Rn64("0x42E240CB63689F2F"); self:Rn64("0x6D2BDCDAE2919661"); self:Rn64("0x42880B0236E4D951"); self:Rn64("0x5F0F4A5898171BB6");
   self:Rn64("0x39F890F579F92F88"); self:Rn64("0x93C5B5F47356388B"); self:Rn64("0x63DC359D8D231B78"); self:Rn64("0xEC16CA8AEA98AD76");
   self:Rn64("0x5355F900C2A82DC7"); self:Rn64("0x07FB9F855A997142"); self:Rn64("0x5093417AA8A7ED5E"); self:Rn64("0x7BCBC38DA25A7F3C");
   self:Rn64("0x19FC8A768CF4B6D4"); self:Rn64("0x637A7780DECFC0D9"); self:Rn64("0x8249A47AEE0E41F7"); self:Rn64("0x79AD695501E7D1E8");
   self:Rn64("0x14ACBAF4777D5776"); self:Rn64("0xF145B6BECCDEA195"); self:Rn64("0xDABF2AC8201752FC"); self:Rn64("0x24C3C94DF9C8D3F6");
   self:Rn64("0xBB6E2924F03912EA"); self:Rn64("0x0CE26C0B95C980D9"); self:Rn64("0xA49CD132BFBF7CC4"); self:Rn64("0xE99D662AF4243939");
   self:Rn64("0x27E6AD7891165C3F"); self:Rn64("0x8535F040B9744FF1"); self:Rn64("0x54B3F4FA5F40D873"); self:Rn64("0x72B12C32127FED2B");
   self:Rn64("0xEE954D3C7B411F47"); self:Rn64("0x9A85AC909A24EAA1"); self:Rn64("0x70AC4CD9F04F21F5"); self:Rn64("0xF9B89D3E99A075C2");
   self:Rn64("0x87B3E2B2B5C907B1"); self:Rn64("0xA366E5B8C54F48B8"); self:Rn64("0xAE4A9346CC3F7CF2"); self:Rn64("0x1920C04D47267BBD");
   self:Rn64("0x87BF02C6B49E2AE9"); self:Rn64("0x092237AC237F3859"); self:Rn64("0xFF07F64EF8ED14D0"); self:Rn64("0x8DE8DCA9F03CC54E");
   self:Rn64("0x9C1633264DB49C89"); self:Rn64("0xB3F22C3D0B0B38ED"); self:Rn64("0x390E5FB44D01144B"); self:Rn64("0x5BFEA5B4712768E9");
   self:Rn64("0x1E1032911FA78984"); self:Rn64("0x9A74ACB964E78CB3"); self:Rn64("0x4F80F7A035DAFB04"); self:Rn64("0x6304D09A0B3738C4");
   self:Rn64("0x2171E64683023A08"); self:Rn64("0x5B9B63EB9CEFF80C"); self:Rn64("0x506AACF489889342"); self:Rn64("0x1881AFC9A3A701D6");
   self:Rn64("0x6503080440750644"); self:Rn64("0xDFD395339CDBF4A7"); self:Rn64("0xEF927DBCF00C20F2"); self:Rn64("0x7B32F7D1E03680EC");
   self:Rn64("0xB9FD7620E7316243"); self:Rn64("0x05A7E8A57DB91B77"); self:Rn64("0xB5889C6E15630A75"); self:Rn64("0x4A750A09CE9573F7");
   self:Rn64("0xCF464CEC899A2F8A"); self:Rn64("0xF538639CE705B824"); self:Rn64("0x3C79A0FF5580EF7F"); self:Rn64("0xEDE6C87F8477609D");
   self:Rn64("0x799E81F05BC93F31"); self:Rn64("0x86536B8CF3428A8C"); self:Rn64("0x97D7374C60087B73"); self:Rn64("0xA246637CFF328532");
   self:Rn64("0x043FCAE60CC0EBA0"); self:Rn64("0x920E449535DD359E"); self:Rn64("0x70EB093B15B290CC"); self:Rn64("0x73A1921916591CBD");
   self:Rn64("0x56436C9FE1A1AA8D"); self:Rn64("0xEFAC4B70633B8F81"); self:Rn64("0xBB215798D45DF7AF"); self:Rn64("0x45F20042F24F1768");
   self:Rn64("0x930F80F4E8EB7462"); self:Rn64("0xFF6712FFCFD75EA1"); self:Rn64("0xAE623FD67468AA70"); self:Rn64("0xDD2C5BC84BC8D8FC");
   self:Rn64("0x7EED120D54CF2DD9"); self:Rn64("0x22FE545401165F1C"); self:Rn64("0xC91800E98FB99929"); self:Rn64("0x808BD68E6AC10365");
   self:Rn64("0xDEC468145B7605F6"); self:Rn64("0x1BEDE3A3AEF53302"); self:Rn64("0x43539603D6C55602"); self:Rn64("0xAA969B5C691CCB7A");
   self:Rn64("0xA87832D392EFEE56"); self:Rn64("0x65942C7B3C7E11AE"); self:Rn64("0xDED2D633CAD004F6"); self:Rn64("0x21F08570F420E565");
   self:Rn64("0xB415938D7DA94E3C"); self:Rn64("0x91B859E59ECB6350"); self:Rn64("0x10CFF333E0ED804A"); self:Rn64("0x28AED140BE0BB7DD");
   self:Rn64("0xC5CC1D89724FA456"); self:Rn64("0x5648F680F11A2741"); self:Rn64("0x2D255069F0B7DAB3"); self:Rn64("0x9BC5A38EF729ABD4");
   self:Rn64("0xEF2F054308F6A2BC"); self:Rn64("0xAF2042F5CC5C2858"); self:Rn64("0x480412BAB7F5BE2A"); self:Rn64("0xAEF3AF4A563DFE43");
   self:Rn64("0x19AFE59AE451497F"); self:Rn64("0x52593803DFF1E840"); self:Rn64("0xF4F076E65F2CE6F0"); self:Rn64("0x11379625747D5AF3");
   self:Rn64("0xBCE5D2248682C115"); self:Rn64("0x9DA4243DE836994F"); self:Rn64("0x066F70B33FE09017"); self:Rn64("0x4DC4DE189B671A1C");
   self:Rn64("0x51039AB7712457C3"); self:Rn64("0xC07A3F80C31FB4B4"); self:Rn64("0xB46EE9C5E64A6E7C"); self:Rn64("0xB3819A42ABE61C87");
   self:Rn64("0x21A007933A522A20"); self:Rn64("0x2DF16F761598AA4F"); self:Rn64("0x763C4A1371B368FD"); self:Rn64("0xF793C46702E086A0");
   self:Rn64("0xD7288E012AEB8D31"); self:Rn64("0xDE336A2A4BC1C44B"); self:Rn64("0x0BF692B38D079F23"); self:Rn64("0x2C604A7A177326B3");
   self:Rn64("0x4850E73E03EB6064"); self:Rn64("0xCFC447F1E53C8E1B"); self:Rn64("0xB05CA3F564268D99"); self:Rn64("0x9AE182C8BC9474E8");
   self:Rn64("0xA4FC4BD4FC5558CA"); self:Rn64("0xE755178D58FC4E76"); self:Rn64("0x69B97DB1A4C03DFE"); self:Rn64("0xF9B5B7C4ACC67C96");
   self:Rn64("0xFC6A82D64B8655FB"); self:Rn64("0x9C684CB6C4D24417"); self:Rn64("0x8EC97D2917456ED0"); self:Rn64("0x6703DF9D2924E97E");
   self:Rn64("0xC547F57E42A7444E"); self:Rn64("0x78E37644E7CAD29E"); self:Rn64("0xFE9A44E9362F05FA"); self:Rn64("0x08BD35CC38336615");
   self:Rn64("0x9315E5EB3A129ACE"); self:Rn64("0x94061B871E04DF75"); self:Rn64("0xDF1D9F9D784BA010"); self:Rn64("0x3BBA57B68871B59D");
   self:Rn64("0xD2B7ADEEDED1F73F"); self:Rn64("0xF7A255D83BC373F8"); self:Rn64("0xD7F4F2448C0CEB81"); self:Rn64("0xD95BE88CD210FFA7");
   self:Rn64("0x336F52F8FF4728E7"); self:Rn64("0xA74049DAC312AC71"); self:Rn64("0xA2F61BB6E437FDB5"); self:Rn64("0x4F2A5CB07F6A35B3");
   self:Rn64("0x87D380BDA5BF7859"); self:Rn64("0x16B9F7E06C453A21"); self:Rn64("0x7BA2484C8A0FD54E"); self:Rn64("0xF3A678CAD9A2E38C");
   self:Rn64("0x39B0BF7DDE437BA2"); self:Rn64("0xFCAF55C1BF8A4424"); self:Rn64("0x18FCF680573FA594"); self:Rn64("0x4C0563B89F495AC3");
   self:Rn64("0x40E087931A00930D"); self:Rn64("0x8CFFA9412EB642C1"); self:Rn64("0x68CA39053261169F"); self:Rn64("0x7A1EE967D27579E2");
   self:Rn64("0x9D1D60E5076F5B6F"); self:Rn64("0x3810E399B6F65BA2"); self:Rn64("0x32095B6D4AB5F9B1"); self:Rn64("0x35CAB62109DD038A");
   self:Rn64("0xA90B24499FCFAFB1"); self:Rn64("0x77A225A07CC2C6BD"); self:Rn64("0x513E5E634C70E331"); self:Rn64("0x4361C0CA3F692F12");
   self:Rn64("0xD941ACA44B20A45B"); self:Rn64("0x528F7C8602C5807B"); self:Rn64("0x52AB92BEB9613989"); self:Rn64("0x9D1DFA2EFC557F73");
   self:Rn64("0x722FF175F572C348"); self:Rn64("0x1D1260A51107FE97"); self:Rn64("0x7A249A57EC0C9BA2"); self:Rn64("0x04208FE9E8F7F2D6");
   self:Rn64("0x5A110C6058B920A0"); self:Rn64("0x0CD9A497658A5698"); self:Rn64("0x56FD23C8F9715A4C"); self:Rn64("0x284C847B9D887AAE");
   self:Rn64("0x04FEABFBBDB619CB"); self:Rn64("0x742E1E651C60BA83"); self:Rn64("0x9A9632E65904AD3C"); self:Rn64("0x881B82A13B51B9E2");
   self:Rn64("0x506E6744CD974924"); self:Rn64("0xB0183DB56FFC6A79"); self:Rn64("0x0ED9B915C66ED37E"); self:Rn64("0x5E11E86D5873D484");
   self:Rn64("0xF678647E3519AC6E"); self:Rn64("0x1B85D488D0F20CC5"); self:Rn64("0xDAB9FE6525D89021"); self:Rn64("0x0D151D86ADB73615");
   self:Rn64("0xA865A54EDCC0F019"); self:Rn64("0x93C42566AEF98FFB"); self:Rn64("0x99E7AFEABE000731"); self:Rn64("0x48CBFF086DDF285A");
   self:Rn64("0x7F9B6AF1EBF78BAF"); self:Rn64("0x58627E1A149BBA21"); self:Rn64("0x2CD16E2ABD791E33"); self:Rn64("0xD363EFF5F0977996");
   self:Rn64("0x0CE2A38C344A6EED"); self:Rn64("0x1A804AADB9CFA741"); self:Rn64("0x907F30421D78C5DE"); self:Rn64("0x501F65EDB3034D07");
   self:Rn64("0x37624AE5A48FA6E9"); self:Rn64("0x957BAF61700CFF4E"); self:Rn64("0x3A6C27934E31188A"); self:Rn64("0xD49503536ABCA345");
   self:Rn64("0x088E049589C432E0"); self:Rn64("0xF943AEE7FEBF21B8"); self:Rn64("0x6C3B8E3E336139D3"); self:Rn64("0x364F6FFA464EE52E");
   self:Rn64("0xD60F6DCEDC314222"); self:Rn64("0x56963B0DCA418FC0"); self:Rn64("0x16F50EDF91E513AF"); self:Rn64("0xEF1955914B609F93");
   self:Rn64("0x565601C0364E3228"); self:Rn64("0xECB53939887E8175"); self:Rn64("0xBAC7A9A18531294B"); self:Rn64("0xB344C470397BBA52");
   self:Rn64("0x65D34954DAF3CEBD"); self:Rn64("0xB4B81B3FA97511E2"); self:Rn64("0xB422061193D6F6A7"); self:Rn64("0x071582401C38434D");
   self:Rn64("0x7A13F18BBEDC4FF5"); self:Rn64("0xBC4097B116C524D2"); self:Rn64("0x59B97885E2F2EA28"); self:Rn64("0x99170A5DC3115544");
   self:Rn64("0x6F423357E7C6A9F9"); self:Rn64("0x325928EE6E6F8794"); self:Rn64("0xD0E4366228B03343"); self:Rn64("0x565C31F7DE89EA27");
   self:Rn64("0x30F5611484119414"); self:Rn64("0xD873DB391292ED4F"); self:Rn64("0x7BD94E1D8E17DEBC"); self:Rn64("0xC7D9F16864A76E94");
   self:Rn64("0x947AE053EE56E63C"); self:Rn64("0xC8C93882F9475F5F"); self:Rn64("0x3A9BF55BA91F81CA"); self:Rn64("0xD9A11FBB3D9808E4");
   self:Rn64("0x0FD22063EDC29FCA"); self:Rn64("0xB3F256D8ACA0B0B9"); self:Rn64("0xB03031A8B4516E84"); self:Rn64("0x35DD37D5871448AF");
   self:Rn64("0xE9F6082B05542E4E"); self:Rn64("0xEBFAFA33D7254B59"); self:Rn64("0x9255ABB50D532280"); self:Rn64("0xB9AB4CE57F2D34F3");
   self:Rn64("0x693501D628297551"); self:Rn64("0xC62C58F97DD949BF"); self:Rn64("0xCD454F8F19C5126A"); self:Rn64("0xBBE83F4ECC2BDECB");
   self:Rn64("0xDC842B7E2819E230"); self:Rn64("0xBA89142E007503B8"); self:Rn64("0xA3BC941D0A5061CB"); self:Rn64("0xE9F6760E32CD8021");
   self:Rn64("0x09C7E552BC76492F"); self:Rn64("0x852F54934DA55CC9"); self:Rn64("0x8107FCCF064FCF56"); self:Rn64("0x098954D51FFF6580");
   self:Rn64("0x23B70EDB1955C4BF"); self:Rn64("0xC330DE426430F69D"); self:Rn64("0x4715ED43E8A45C0A"); self:Rn64("0xA8D7E4DAB780A08D");
   self:Rn64("0x0572B974F03CE0BB"); self:Rn64("0xB57D2E985E1419C7"); self:Rn64("0xE8D9ECBE2CF3D73F"); self:Rn64("0x2FE4B17170E59750");
   self:Rn64("0x11317BA87905E790"); self:Rn64("0x7FBF21EC8A1F45EC"); self:Rn64("0x1725CABFCB045B00"); self:Rn64("0x964E915CD5E2B207");
   self:Rn64("0x3E2B8BCBF016D66D"); self:Rn64("0xBE7444E39328A0AC"); self:Rn64("0xF85B2B4FBCDE44B7"); self:Rn64("0x49353FEA39BA63B1");
   self:Rn64("0x1DD01AAFCD53486A"); self:Rn64("0x1FCA8A92FD719F85"); self:Rn64("0xFC7C95D827357AFA"); self:Rn64("0x18A6A990C8B35EBD");
   self:Rn64("0xCCCB7005C6B9C28D"); self:Rn64("0x3BDBB92C43B17F26"); self:Rn64("0xAA70B5B4F89695A2"); self:Rn64("0xE94C39A54A98307F");
   self:Rn64("0xB7A0B174CFF6F36E"); self:Rn64("0xD4DBA84729AF48AD"); self:Rn64("0x2E18BC1AD9704A68"); self:Rn64("0x2DE0966DAF2F8B1C");
   self:Rn64("0xB9C11D5B1E43A07E"); self:Rn64("0x64972D68DEE33360"); self:Rn64("0x94628D38D0C20584"); self:Rn64("0xDBC0D2B6AB90A559");
   self:Rn64("0xD2733C4335C6A72F"); self:Rn64("0x7E75D99D94A70F4D"); self:Rn64("0x6CED1983376FA72B"); self:Rn64("0x97FCAACBF030BC24");
   self:Rn64("0x7B77497B32503B12"); self:Rn64("0x8547EDDFB81CCB94"); self:Rn64("0x79999CDFF70902CB"); self:Rn64("0xCFFE1939438E9B24");
   self:Rn64("0x829626E3892D95D7"); self:Rn64("0x92FAE24291F2B3F1"); self:Rn64("0x63E22C147B9C3403"); self:Rn64("0xC678B6D860284A1C");
   self:Rn64("0x5873888850659AE7"); self:Rn64("0x0981DCD296A8736D"); self:Rn64("0x9F65789A6509A440"); self:Rn64("0x9FF38FED72E9052F");
   self:Rn64("0xE479EE5B9930578C"); self:Rn64("0xE7F28ECD2D49EECD"); self:Rn64("0x56C074A581EA17FE"); self:Rn64("0x5544F7D774B14AEF");
   self:Rn64("0x7B3F0195FC6F290F"); self:Rn64("0x12153635B2C0CF57"); self:Rn64("0x7F5126DBBA5E0CA7"); self:Rn64("0x7A76956C3EAFB413");
   self:Rn64("0x3D5774A11D31AB39"); self:Rn64("0x8A1B083821F40CB4"); self:Rn64("0x7B4A38E32537DF62"); self:Rn64("0x950113646D1D6E03");
   self:Rn64("0x4DA8979A0041E8A9"); self:Rn64("0x3BC36E078F7515D7"); self:Rn64("0x5D0A12F27AD310D1"); self:Rn64("0x7F9D1A2E1EBE1327");
   self:Rn64("0xDA3A361B1C5157B1"); self:Rn64("0xDCDD7D20903D0C25"); self:Rn64("0x36833336D068F707"); self:Rn64("0xCE68341F79893389");
   self:Rn64("0xAB9090168DD05F34"); self:Rn64("0x43954B3252DC25E5"); self:Rn64("0xB438C2B67F98E5E9"); self:Rn64("0x10DCD78E3851A492");
   self:Rn64("0xDBC27AB5447822BF"); self:Rn64("0x9B3CDB65F82CA382"); self:Rn64("0xB67B7896167B4C84"); self:Rn64("0xBFCED1B0048EAC50");
   self:Rn64("0xA9119B60369FFEBD"); self:Rn64("0x1FFF7AC80904BF45"); self:Rn64("0xAC12FB171817EEE7"); self:Rn64("0xAF08DA9177DDA93D");
   self:Rn64("0x1B0CAB936E65C744"); self:Rn64("0xB559EB1D04E5E932"); self:Rn64("0xC37B45B3F8D6F2BA"); self:Rn64("0xC3A9DC228CAAC9E9");
   self:Rn64("0xF3B8B6675A6507FF"); self:Rn64("0x9FC477DE4ED681DA"); self:Rn64("0x67378D8ECCEF96CB"); self:Rn64("0x6DD856D94D259236");
   self:Rn64("0xA319CE15B0B4DB31"); self:Rn64("0x073973751F12DD5E"); self:Rn64("0x8A8E849EB32781A5"); self:Rn64("0xE1925C71285279F5");
   self:Rn64("0x74C04BF1790C0EFE"); self:Rn64("0x4DDA48153C94938A"); self:Rn64("0x9D266D6A1CC0542C"); self:Rn64("0x7440FB816508C4FE");
   self:Rn64("0x13328503DF48229F"); self:Rn64("0xD6BF7BAEE43CAC40"); self:Rn64("0x4838D65F6EF6748F"); self:Rn64("0x1E152328F3318DEA");
   self:Rn64("0x8F8419A348F296BF"); self:Rn64("0x72C8834A5957B511"); self:Rn64("0xD7A023A73260B45C"); self:Rn64("0x94EBC8ABCFB56DAE");
   self:Rn64("0x9FC10D0F989993E0"); self:Rn64("0xDE68A2355B93CAE6"); self:Rn64("0xA44CFE79AE538BBE"); self:Rn64("0x9D1D84FCCE371425");
   self:Rn64("0x51D2B1AB2DDFB636"); self:Rn64("0x2FD7E4B9E72CD38C"); self:Rn64("0x65CA5B96B7552210"); self:Rn64("0xDD69A0D8AB3B546D");
   self:Rn64("0x604D51B25FBF70E2"); self:Rn64("0x73AA8A564FB7AC9E"); self:Rn64("0x1A8C1E992B941148"); self:Rn64("0xAAC40A2703D9BEA0");
   self:Rn64("0x764DBEAE7FA4F3A6"); self:Rn64("0x1E99B96E70A9BE8B"); self:Rn64("0x2C5E9DEB57EF4743"); self:Rn64("0x3A938FEE32D29981");
   self:Rn64("0x26E6DB8FFDF5ADFE"); self:Rn64("0x469356C504EC9F9D"); self:Rn64("0xC8763C5B08D1908C"); self:Rn64("0x3F6C6AF859D80055");
   self:Rn64("0x7F7CC39420A3A545"); self:Rn64("0x9BFB227EBDF4C5CE"); self:Rn64("0x89039D79D6FC5C5C"); self:Rn64("0x8FE88B57305E2AB6");
   self:Rn64("0xA09E8C8C35AB96DE"); self:Rn64("0xFA7E393983325753"); self:Rn64("0xD6B6D0ECC617C699"); self:Rn64("0xDFEA21EA9E7557E3");
   self:Rn64("0xB67C1FA481680AF8"); self:Rn64("0xCA1E3785A9E724E5"); self:Rn64("0x1CFC8BED0D681639"); self:Rn64("0xD18D8549D140CAEA");
   self:Rn64("0x4ED0FE7E9DC91335"); self:Rn64("0xE4DBF0634473F5D2"); self:Rn64("0x1761F93A44D5AEFE"); self:Rn64("0x53898E4C3910DA55");
   self:Rn64("0x734DE8181F6EC39A"); self:Rn64("0x2680B122BAA28D97"); self:Rn64("0x298AF231C85BAFAB"); self:Rn64("0x7983EED3740847D5");
   self:Rn64("0x66C1A2A1A60CD889"); self:Rn64("0x9E17E49642A3E4C1"); self:Rn64("0xEDB454E7BADC0805"); self:Rn64("0x50B704CAB602C329");
   self:Rn64("0x4CC317FB9CDDD023"); self:Rn64("0x66B4835D9EAFEA22"); self:Rn64("0x219B97E26FFC81BD"); self:Rn64("0x261E4E4C0A333A9D");
   self:Rn64("0x1FE2CCA76517DB90"); self:Rn64("0xD7504DFA8816EDBB"); self:Rn64("0xB9571FA04DC089C8"); self:Rn64("0x1DDC0325259B27DE");
   self:Rn64("0xCF3F4688801EB9AA"); self:Rn64("0xF4F5D05C10CAB243"); self:Rn64("0x38B6525C21A42B0E"); self:Rn64("0x36F60E2BA4FA6800");
   self:Rn64("0xEB3593803173E0CE"); self:Rn64("0x9C4CD6257C5A3603"); self:Rn64("0xAF0C317D32ADAA8A"); self:Rn64("0x258E5A80C7204C4B");
   self:Rn64("0x8B889D624D44885D"); self:Rn64("0xF4D14597E660F855"); self:Rn64("0xD4347F66EC8941C3"); self:Rn64("0xE699ED85B0DFB40D");
   self:Rn64("0x2472F6207C2D0484"); self:Rn64("0xC2A1E7B5B459AEB5"); self:Rn64("0xAB4F6451CC1D45EC"); self:Rn64("0x63767572AE3D6174");
   self:Rn64("0xA59E0BD101731A28"); self:Rn64("0x116D0016CB948F09"); self:Rn64("0x2CF9C8CA052F6E9F"); self:Rn64("0x0B090A7560A968E3");
   self:Rn64("0xABEEDDB2DDE06FF1"); self:Rn64("0x58EFC10B06A2068D"); self:Rn64("0xC6E57A78FBD986E0"); self:Rn64("0x2EAB8CA63CE802D7");
   self:Rn64("0x14A195640116F336"); self:Rn64("0x7C0828DD624EC390"); self:Rn64("0xD74BBE77E6116AC7"); self:Rn64("0x804456AF10F5FB53");
   self:Rn64("0xEBE9EA2ADF4321C7"); self:Rn64("0x03219A39EE587A30"); self:Rn64("0x49787FEF17AF9924"); self:Rn64("0xA1E9300CD8520548");
   self:Rn64("0x5B45E522E4B1B4EF"); self:Rn64("0xB49C3B3995091A36"); self:Rn64("0xD4490AD526F14431"); self:Rn64("0x12A8F216AF9418C2");
   self:Rn64("0x001F837CC7350524"); self:Rn64("0x1877B51E57A764D5"); self:Rn64("0xA2853B80F17F58EE"); self:Rn64("0x993E1DE72D36D310");
   self:Rn64("0xB3598080CE64A656"); self:Rn64("0x252F59CF0D9F04BB"); self:Rn64("0xD23C8E176D113600"); self:Rn64("0x1BDA0492E7E4586E");
   self:Rn64("0x21E0BD5026C619BF"); self:Rn64("0x3B097ADAF088F94E"); self:Rn64("0x8D14DEDB30BE846E"); self:Rn64("0xF95CFFA23AF5F6F4");
   self:Rn64("0x3871700761B3F743"); self:Rn64("0xCA672B91E9E4FA16"); self:Rn64("0x64C8E531BFF53B55"); self:Rn64("0x241260ED4AD1E87D");
   self:Rn64("0x106C09B972D2E822"); self:Rn64("0x7FBA195410E5CA30"); self:Rn64("0x7884D9BC6CB569D8"); self:Rn64("0x0647DFEDCD894A29");
   self:Rn64("0x63573FF03E224774"); self:Rn64("0x4FC8E9560F91B123"); self:Rn64("0x1DB956E450275779"); self:Rn64("0xB8D91274B9E9D4FB");
   self:Rn64("0xA2EBEE47E2FBFCE1"); self:Rn64("0xD9F1F30CCD97FB09"); self:Rn64("0xEFED53D75FD64E6B"); self:Rn64("0x2E6D02C36017F67F");
   self:Rn64("0xA9AA4D20DB084E9B"); self:Rn64("0xB64BE8D8B25396C1"); self:Rn64("0x70CB6AF7C2D5BCF0"); self:Rn64("0x98F076A4F7A2322E");
   self:Rn64("0xBF84470805E69B5F"); self:Rn64("0x94C3251F06F90CF3"); self:Rn64("0x3E003E616A6591E9"); self:Rn64("0xB925A6CD0421AFF3");
   self:Rn64("0x61BDD1307C66E300"); self:Rn64("0xBF8D5108E27E0D48"); self:Rn64("0x240AB57A8B888B20"); self:Rn64("0xFC87614BAF287E07");
   self:Rn64("0xEF02CDD06FFDB432"); self:Rn64("0xA1082C0466DF6C0A"); self:Rn64("0x8215E577001332C8"); self:Rn64("0xD39BB9C3A48DB6CF");
   self:Rn64("0x2738259634305C14"); self:Rn64("0x61CF4F94C97DF93D"); self:Rn64("0x1B6BACA2AE4E125B"); self:Rn64("0x758F450C88572E0B");
   self:Rn64("0x959F587D507A8359"); self:Rn64("0xB063E962E045F54D"); self:Rn64("0x60E8ED72C0DFF5D1"); self:Rn64("0x7B64978555326F9F");
   self:Rn64("0xFD080D236DA814BA"); self:Rn64("0x8C90FD9B083F4558"); self:Rn64("0x106F72FE81E2C590"); self:Rn64("0x7976033A39F7D952");
   self:Rn64("0xA4EC0132764CA04B"); self:Rn64("0x733EA705FAE4FA77"); self:Rn64("0xB4D8F77BC3E56167"); self:Rn64("0x9E21F4F903B33FD9");
   self:Rn64("0x9D765E419FB69F6D"); self:Rn64("0xD30C088BA61EA5EF"); self:Rn64("0x5D94337FBFAF7F5B"); self:Rn64("0x1A4E4822EB4D7A59");
   self:Rn64("0x6FFE73E81B637FB3"); self:Rn64("0xDDF957BC36D8B9CA"); self:Rn64("0x64D0E29EEA8838B3"); self:Rn64("0x08DD9BDFD96B9F63");
   self:Rn64("0x087E79E5A57D1D13"); self:Rn64("0xE328E230E3E2B3FB"); self:Rn64("0x1C2559E30F0946BE"); self:Rn64("0x720BF5F26F4D2EAA");
   self:Rn64("0xB0774D261CC609DB"); self:Rn64("0x443F64EC5A371195"); self:Rn64("0x4112CF68649A260E"); self:Rn64("0xD813F2FAB7F5C5CA");
   self:Rn64("0x660D3257380841EE"); self:Rn64("0x59AC2C7873F910A3"); self:Rn64("0xE846963877671A17"); self:Rn64("0x93B633ABFA3469F8");
   self:Rn64("0xC0C0F5A60EF4CDCF"); self:Rn64("0xCAF21ECD4377B28C"); self:Rn64("0x57277707199B8175"); self:Rn64("0x506C11B9D90E8B1D");
   self:Rn64("0xD83CC2687A19255F"); self:Rn64("0x4A29C6465A314CD1"); self:Rn64("0xED2DF21216235097"); self:Rn64("0xB5635C95FF7296E2");
   self:Rn64("0x22AF003AB672E811"); self:Rn64("0x52E762596BF68235"); self:Rn64("0x9AEBA33AC6ECC6B0"); self:Rn64("0x944F6DE09134DFB6");
   self:Rn64("0x6C47BEC883A7DE39"); self:Rn64("0x6AD047C430A12104"); self:Rn64("0xA5B1CFDBA0AB4067"); self:Rn64("0x7C45D833AFF07862");
   self:Rn64("0x5092EF950A16DA0B"); self:Rn64("0x9338E69C052B8E7B"); self:Rn64("0x455A4B4CFE30E3F5"); self:Rn64("0x6B02E63195AD0CF8");
   self:Rn64("0x6B17B224BAD6BF27"); self:Rn64("0xD1E0CCD25BB9C169"); self:Rn64("0xDE0C89A556B9AE70"); self:Rn64("0x50065E535A213CF6");
   self:Rn64("0x9C1169FA2777B874"); self:Rn64("0x78EDEFD694AF1EED"); self:Rn64("0x6DC93D9526A50E68"); self:Rn64("0xEE97F453F06791ED");
   self:Rn64("0x32AB0EDB696703D3"); self:Rn64("0x3A6853C7E70757A7"); self:Rn64("0x31865CED6120F37D"); self:Rn64("0x67FEF95D92607890");
   self:Rn64("0x1F2B1D1F15F6DC9C"); self:Rn64("0xB69E38A8965C6B65"); self:Rn64("0xAA9119FF184CCCF4"); self:Rn64("0xF43C732873F24C13");
   self:Rn64("0xFB4A3D794A9A80D2"); self:Rn64("0x3550C2321FD6109C"); self:Rn64("0x371F77E76BB8417E"); self:Rn64("0x6BFA9AAE5EC05779");
   self:Rn64("0xCD04F3FF001A4778"); self:Rn64("0xE3273522064480CA"); self:Rn64("0x9F91508BFFCFC14A"); self:Rn64("0x049A7F41061A9E60");
   self:Rn64("0xFCB6BE43A9F2FE9B"); self:Rn64("0x08DE8A1C7797DA9B"); self:Rn64("0x8F9887E6078735A1"); self:Rn64("0xB5B4071DBFC73A66");
   self:Rn64("0x230E343DFBA08D33"); self:Rn64("0x43ED7F5A0FAE657D"); self:Rn64("0x3A88A0FBBCB05C63"); self:Rn64("0x21874B8B4D2DBC4F");
   self:Rn64("0x1BDEA12E35F6A8C9"); self:Rn64("0x53C065C6C8E63528"); self:Rn64("0xE34A1D250E7A8D6B"); self:Rn64("0xD6B04D3B7651DD7E");
   self:Rn64("0x5E90277E7CB39E2D"); self:Rn64("0x2C046F22062DC67D"); self:Rn64("0xB10BB459132D0A26"); self:Rn64("0x3FA9DDFB67E2F199");
   self:Rn64("0x0E09B88E1914F7AF"); self:Rn64("0x10E8B35AF3EEAB37"); self:Rn64("0x9EEDECA8E272B933"); self:Rn64("0xD4C718BC4AE8AE5F");
   self:Rn64("0x81536D601170FC20"); self:Rn64("0x91B534F885818A06"); self:Rn64("0xEC8177F83F900978"); self:Rn64("0x190E714FADA5156E");
   self:Rn64("0xB592BF39B0364963"); self:Rn64("0x89C350C893AE7DC1"); self:Rn64("0xAC042E70F8B383F2"); self:Rn64("0xB49B52E587A1EE60");
   self:Rn64("0xFB152FE3FF26DA89"); self:Rn64("0x3E666E6F69AE2C15"); self:Rn64("0x3B544EBE544C19F9"); self:Rn64("0xE805A1E290CF2456");
   self:Rn64("0x24B33C9D7ED25117"); self:Rn64("0xE74733427B72F0C1"); self:Rn64("0x0A804D18B7097475"); self:Rn64("0x57E3306D881EDB4F");
   self:Rn64("0x4AE7D6A36EB5DBCB"); self:Rn64("0x2D8D5432157064C8"); self:Rn64("0xD1E649DE1E7F268B"); self:Rn64("0x8A328A1CEDFE552C");
   self:Rn64("0x07A3AEC79624C7DA"); self:Rn64("0x84547DDC3E203C94"); self:Rn64("0x990A98FD5071D263"); self:Rn64("0x1A4FF12616EEFC89");
   self:Rn64("0xF6F7FD1431714200"); self:Rn64("0x30C05B1BA332F41C"); self:Rn64("0x8D2636B81555A786"); self:Rn64("0x46C9FEB55D120902");
   self:Rn64("0xCCEC0A73B49C9921"); self:Rn64("0x4E9D2827355FC492"); self:Rn64("0x19EBB029435DCB0F"); self:Rn64("0x4659D2B743848A2C");
   self:Rn64("0x963EF2C96B33BE31"); self:Rn64("0x74F85198B05A2E7D"); self:Rn64("0x5A0F544DD2B1FB18"); self:Rn64("0x03727073C2E134B1");
   self:Rn64("0xC7F6AA2DE59AEA61"); self:Rn64("0x352787BAA0D7C22F"); self:Rn64("0x9853EAB63B5E0B35"); self:Rn64("0xABBDCDD7ED5C0860");
   self:Rn64("0xCF05DAF5AC8D77B0"); self:Rn64("0x49CAD48CEBF4A71E"); self:Rn64("0x7A4C10EC2158C4A6"); self:Rn64("0xD9E92AA246BF719E");
   self:Rn64("0x13AE978D09FE5557"); self:Rn64("0x730499AF921549FF"); self:Rn64("0x4E4B705B92903BA4"); self:Rn64("0xFF577222C14F0A3A");
   self:Rn64("0x55B6344CF97AAFAE"); self:Rn64("0xB862225B055B6960"); self:Rn64("0xCAC09AFBDDD2CDB4"); self:Rn64("0xDAF8E9829FE96B5F");
   self:Rn64("0xB5FDFC5D3132C498"); self:Rn64("0x310CB380DB6F7503"); self:Rn64("0xE87FBB46217A360E"); self:Rn64("0x2102AE466EBB1148");
   self:Rn64("0xF8549E1A3AA5E00D"); self:Rn64("0x07A69AFDCC42261A"); self:Rn64("0xC4C118BFE78FEAAE"); self:Rn64("0xF9F4892ED96BD438");
   self:Rn64("0x1AF3DBE25D8F45DA"); self:Rn64("0xF5B4B0B0D2DEEEB4"); self:Rn64("0x962ACEEFA82E1C84"); self:Rn64("0x046E3ECAAF453CE9");
   self:Rn64("0xF05D129681949A4C"); self:Rn64("0x964781CE734B3C84"); self:Rn64("0x9C2ED44081CE5FBD"); self:Rn64("0x522E23F3925E319E");
   self:Rn64("0x177E00F9FC32F791"); self:Rn64("0x2BC60A63A6F3B3F2"); self:Rn64("0x222BBFAE61725606"); self:Rn64("0x486289DDCC3D6780");
   self:Rn64("0x7DC7785B8EFDFC80"); self:Rn64("0x8AF38731C02BA980"); self:Rn64("0x1FAB64EA29A2DDF7"); self:Rn64("0xE4D9429322CD065A");
   self:Rn64("0x9DA058C67844F20C"); self:Rn64("0x24C0E332B70019B0"); self:Rn64("0x233003B5A6CFE6AD"); self:Rn64("0xD586BD01C5C217F6");
   self:Rn64("0x5E5637885F29BC2B"); self:Rn64("0x7EBA726D8C94094B"); self:Rn64("0x0A56A5F0BFE39272"); self:Rn64("0xD79476A84EE20D06");
   self:Rn64("0x9E4C1269BAA4BF37"); self:Rn64("0x17EFEE45B0DEE640"); self:Rn64("0x1D95B0A5FCF90BC6"); self:Rn64("0x93CBE0B699C2585D");
   self:Rn64("0x65FA4F227A2B6D79"); self:Rn64("0xD5F9E858292504D5"); self:Rn64("0xC2B5A03F71471A6F"); self:Rn64("0x59300222B4561E00");
   self:Rn64("0xCE2F8642CA0712DC"); self:Rn64("0x7CA9723FBB2E8988"); self:Rn64("0x2785338347F2BA08"); self:Rn64("0xC61BB3A141E50E8C");
   self:Rn64("0x150F361DAB9DEC26"); self:Rn64("0x9F6A419D382595F4"); self:Rn64("0x64A53DC924FE7AC9"); self:Rn64("0x142DE49FFF7A7C3D");
   self:Rn64("0x0C335248857FA9E7"); self:Rn64("0x0A9C32D5EAE45305"); self:Rn64("0xE6C42178C4BBB92E"); self:Rn64("0x71F1CE2490D20B07");
   self:Rn64("0xF1BCC3D275AFE51A"); self:Rn64("0xE728E8C83C334074"); self:Rn64("0x96FBF83A12884624"); self:Rn64("0x81A1549FD6573DA5");
   self:Rn64("0x5FA7867CAF35E149"); self:Rn64("0x56986E2EF3ED091B"); self:Rn64("0x917F1DD5F8886C61"); self:Rn64("0xD20D8C88C8FFE65F");
   self:Rn64("0x31D71DCE64B2C310"); self:Rn64("0xF165B587DF898190"); self:Rn64("0xA57E6339DD2CF3A0"); self:Rn64("0x1EF6E6DBB1961EC9");
   self:Rn64("0x70CC73D90BC26E24"); self:Rn64("0xE21A6B35DF0C3AD7"); self:Rn64("0x003A93D8B2806962"); self:Rn64("0x1C99DED33CB890A1");
   self:Rn64("0xCF3145DE0ADD4289"); self:Rn64("0xD0E4427A5514FB72"); self:Rn64("0x77C621CC9FB3A483"); self:Rn64("0x67A34DAC4356550B");
   self:Rn64("0xF8D626AAAF278509");


-- We know it.
--   if((self.Random64[1+self.RandomNb-1] bit.rshift(,) 32) ~= 0xF8D626AA) { // upper half of the last element of the array
--      self:my_fatal("broken 64-bit types\n");
--   }

end

-- end of random.cpp




-- recog.cpp

-- functions

-- self:recog_draw()

function self:recog_draw( board )  -- bool

   local mat_info = self:material_info_t();  -- material_info_t[1]
   local ifelse = false;

   local me = 0;    -- int
   local opp = 0;   -- int
   local wp = 0;   -- int
   local wk = 0;   -- int
   local bk = 0;   -- int
   local wb = 0;   -- int
   local bb = 0;   -- int

   --self:ASSERT(622, board.sp~=nil);

   -- material

   if(board.piece_nb > 4) then
     return false;
   end

   self:material_get_info(mat_info,board);

   if( bit.band( mat_info.flags, self.DrawNodeFlag ) == 0) then
     return false;
   end

   -- recognisers


   ifelse = true;
   if(mat_info.recog == self.MAT_KK) then

      -- KK

      return true;
   end

   if(mat_info.recog == self.MAT_KBK) then

      -- KBK(white)

      return true;
   end

   if(mat_info.recog == self.MAT_KKB) then

      -- KBK(black)

      return true;
   end

   if(mat_info.recog == self.MAT_KNK) then

      -- KNK(white)

      return true;
   end

   if(mat_info.recog == self.MAT_KKN) then

      -- KNK(black)

      return true;
   end

   if(mat_info.recog == self.MAT_KPK) then

      -- KPK(white)

      me = self.White;
      opp = self:COLOUR_OPP(me);

      wp = board.pawn[1+me][1+0];
      wk = self:KING_POS(board,me);
      bk = self:KING_POS(board,opp);

      if(self:SQUARE_FILE(wp) >= self.FileE) then
         wp = self:SQUARE_FILE_MIRROR(wp);
         wk = self:SQUARE_FILE_MIRROR(wk);
         bk = self:SQUARE_FILE_MIRROR(bk);
      end

      if(self:kpk_draw(wp,wk,bk,board.turn)) then
         return true;
      end
      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KKP) then

      -- KPK(black)

      me = self.Black;
      opp = self:COLOUR_OPP(me);

      wp = self:SQUARE_RANK_MIRROR(board.pawn[1+me][1+0]);
      wk = self:SQUARE_RANK_MIRROR(self:KING_POS(board,me));
      bk = self:SQUARE_RANK_MIRROR(self:KING_POS(board,opp));

      if(self:SQUARE_FILE(wp) >= self.FileE) then
         wp = self:SQUARE_FILE_MIRROR(wp);
         wk = self:SQUARE_FILE_MIRROR(wk);
         bk = self:SQUARE_FILE_MIRROR(bk);
      end

      if(self:kpk_draw(wp,wk,bk,self:COLOUR_OPP(board.turn))) then
         return true;
      end
      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KBKB) then

      -- KBKB

      wb = board.piece[1+self.White][1+1];
      bb = board.piece[1+self.Black][1+1];

      if(self:SQUARE_COLOUR(wb) == self:SQUARE_COLOUR(bb)) then   -- bishops on same colour
         return true;
      end
      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KBPK) then

      -- KBPK(white)

      me = self.White;
      opp = self:COLOUR_OPP(me);

      wp = board.pawn[1+me][1+0];
      wb = board.piece[1+me][1+1];
      bk = self:KING_POS(board,opp);

      if(self:SQUARE_FILE(wp) >= self.FileE) then
         wp = self:SQUARE_FILE_MIRROR(wp);
         wb = self:SQUARE_FILE_MIRROR(wb);
         bk = self:SQUARE_FILE_MIRROR(bk);
      end

      if(self:kbpk_draw(wp,wb,bk)) then
        return true;
      end
      ifelse = false;
   end

   if(ifelse and mat_info.recog == self.MAT_KKBP) then

      -- KBPK(black)

      me = self.Black;
      opp = self:COLOUR_OPP(me);

      wp = self:SQUARE_RANK_MIRROR(board.pawn[1+me][1+0]);
      wb = self:SQUARE_RANK_MIRROR(board.piece[1+me][1+1]);
      bk = self:SQUARE_RANK_MIRROR(self:KING_POS(board,opp));

      if(self:SQUARE_FILE(wp) >= self.FileE) then
         wp = self:SQUARE_FILE_MIRROR(wp);
         wb = self:SQUARE_FILE_MIRROR(wb);
         bk = self:SQUARE_FILE_MIRROR(bk);
      end

      if(self:kbpk_draw(wp,wb,bk)) then
        return true;
      end
      ifelse = false;
   end

   if(ifelse) then
      --self:ASSERT(623, false);
   end

   return false;
end

-- self:kpk_draw()

function self:kpk_draw( wp, wk, bk, turn )  -- bool

   local wp_file = 0;   -- int
   local wp_rank = 0;   -- int
   local wk_file = 0;   -- int
   local bk_file = 0;   -- int
   local bk_rank = 0;   -- int
   local ifelse = false;

   --self:ASSERT(624, self:SQUARE_IS_OK(wp));
   --self:ASSERT(625, self:SQUARE_IS_OK(wk));
   --self:ASSERT(626, self:SQUARE_IS_OK(bk));
   --self:ASSERT(627, self:COLOUR_IS_OK(turn));

   --self:ASSERT(628, self:SQUARE_FILE(wp)<=self.FileD);

   wp_file = self:SQUARE_FILE(wp);
   wp_rank = self:SQUARE_RANK(wp);

   wk_file = self:SQUARE_FILE(wk);

   bk_file = self:SQUARE_FILE(bk);
   bk_rank = self:SQUARE_RANK(bk);

   ifelse = true;
   if(ifelse and(bk == wp+16)) then

      if(wp_rank <= self.Rank6) then

         return true;

      else

         --self:ASSERT(629, wp_rank==self.Rank7);

         if(self:COLOUR_IS_WHITE(turn)) then
            if(wk == wp-15  or  wk == wp-17) then
              return true;
            end
         else
            if(wk ~= wp-15  and  wk ~= wp-17) then
              return true;
            end
         end
      end
      ifelse = false;
   end

   if(ifelse and(bk == wp+32)) then

      if(wp_rank <= self.Rank5) then

         return true;

      else

         --self:ASSERT(630, wp_rank==self.Rank6);

         if(self:COLOUR_IS_WHITE(turn)) then
            if(wk ~= wp-1  and  wk ~= wp+1) then
              return true;
            end
         else
            return true;
         end
      end

      ifelse = false;
   end

   if(ifelse and(wk == wp-1  or  wk == wp+1)) then

      if(bk == wk+32  and  self:COLOUR_IS_WHITE(turn)) then    -- opposition
         return true;
      end

      ifelse = false;
   end

   if(ifelse and(wk == wp+15  or  wk == wp+16  or  wk == wp+17)) then

      if(wp_rank <= self.Rank4) then
         if(bk == wk+32  and  self:COLOUR_IS_WHITE(turn)) then   -- opposition
            return true;
         end
      end
      ifelse = false;
   end

   -- rook pawn

   if(wp_file == self.FileA) then

      if(self:DISTANCE(bk,self.A8) <= 1) then
        return true;
      end

      if(wk_file == self.FileA) then
         if(wp_rank == self.Rank2) then
            wp_rank = wp_rank + 1; -- HACK
         end
         if(bk_file == self.FileC  and  bk_rank > wp_rank) then
            return true;
         end
      end
   end

   return false;

end

-- self:kbpk_draw()

function self:kbpk_draw( wp, wb, bk )  -- bool

   --self:ASSERT(631, self:SQUARE_IS_OK(wp));
   --self:ASSERT(632, self:SQUARE_IS_OK(wb));
   --self:ASSERT(633, self:SQUARE_IS_OK(bk));

   if(self:SQUARE_FILE(wp) == self.FileA
     and  self:DISTANCE(bk,self.A8) <= 1
     and  self:SQUARE_COLOUR(wb) ~= self:SQUARE_COLOUR(self.A8)) then
      return true;
   end

   return false;
end

-- end of recog.cpp



-- search.cpp

-- functions

-- self:depth_is_ok()

function self:depth_is_ok( depth ) -- bool

   return(depth > -128)  and(depth < self.DepthMax);
end

-- self:height_is_ok()

function self:height_is_ok( height ) -- bool

   return(height >= 0)  and(height < self.HeightMax);
end

-- self:search_clear()

function self:search_clear() -- void

   -- self.SearchInput

   self.SearchInput.infinite = false;
   self.SearchInput.depth_is_limited = false;
   self.SearchInput.depth_limit = 0;
   self.SearchInput.time_is_limited = false;
   self.SearchInput.time_limit_1 = 0.0;
   self.SearchInput.time_limit_2 = 0.0;

   -- self.SearchInfo

   self.SearchInfo.can_stop = false;
   self.SearchInfo.stop = false;
   self.SearchInfo.check_nb = 10000;  -- was 100000
   self.SearchInfo.check_inc = 10000; -- was 100000
   self.SearchInfo.last_time = 0.0;

   -- self.SearchBest

   self.SearchBest.move = self.MoveNone;
   self.SearchBest.value = 0;
   self.SearchBest.flags = self.SearchUnknown;
   self.SearchBest.pv[1+0] = self.MoveNone;

   -- self.SearchRoot

   self.SearchRoot.depth = 0;
   self.SearchRoot.move = self.MoveNone;
   self.SearchRoot.move_pos = 0;
   self.SearchRoot.move_nb = 0;
   self.SearchRoot.last_value = 0;
   self.SearchRoot.bad_1 = false;
   self.SearchRoot.bad_2 = false;
   self.SearchRoot.change = false;
   self.SearchRoot.easy = false;
   self.SearchRoot.flag = false;

   -- self.SearchCurrent

   self.SearchCurrent.mate = 0;
   self.SearchCurrent.depth = 0;
   self.SearchCurrent.max_depth = 0;
   self.SearchCurrent.node_nb = 0;
   self.SearchCurrent.time = 0.0;
   self.SearchCurrent.speed = 0.0;
end

-- self:search()

function self:search(loopFn, delayFn, onComplete) -- void

   -- loopFn(stepFn, doneFn): drives the iterative-deepening loop.
   --   stepFn()  – runs one depth iteration; returns false when search is done.
   --   doneFn()  – called once after stepFn returns false.
   -- Default (sync): run all steps in a tight while-loop.
   loopFn = loopFn or function(step, done) while step() ~= false do end; done() end
   stepFn = stepFn or function(items)
       if type(items) == "function" then
           items()
       else
           for _, step in ipairs(items) do
               step()
           end
       end
   end
   onComplete = onComplete or function() end

   local move = self.MoveNone;    -- int

   --self:ASSERT(634, self:board_is_ok(self.SearchInput.board));

   -- opening book

   if(self:option_get_bool("OwnBook")  and(not self.SearchInput.infinite)) then

      -- no book here
      -- move = book_move(self.SearchInput.board);

      if(move ~= self.MoveNone) then

         -- play book move

         self.SearchBest.move = move;
         self.SearchBest.value = 1;
         self.SearchBest.flags = self.SearchExact;
         self.SearchBest.depth = 1;
         self.SearchBest.pv[1+0] = move;
         self.SearchBest.pv[1+1] = self.MoveNone;

         self:search_update_best();

         onComplete();
         return;
      end
   end

   -- self.SearchInput

   self:gen_legal_moves(self.SearchInput.list,self.SearchInput.board);

   if( self.SearchInput.list.size <= 1) then
      self.SearchInput.depth_is_limited = true;
      self.SearchInput.depth_limit = 4;        -- was 1
   end

   -- Step-based iterative deepening with per-root-move yielding.
   -- Each loopFn tick processes ONE root move (or handles depth
   -- setup/teardown). This keeps the UI responsive even within a
   -- single depth where each root move search can take hundreds of ms.

   self.setjmp = false;
   local depth = 0;
   local needsInit = true;

   -- Root move loop state (inlined from full_root).
   -- These persist across steps while processing one depth.
   local inRootLoop = false;
   local rootMoveIdx = 0;
   local root_alpha, root_beta, root_old_alpha, root_best_value;
   local root_search_type;
   local root_undo = self:undo_t();
   local root_new_pv = {};

   local function stepFn()

      -- Handle setjmp (search interrupted from deep in the tree)
      if(self.setjmp) then
         self.setjmp = false;
         inRootLoop = false;
         --self:ASSERT(635, self.SearchInfo.can_stop);
         --self:ASSERT(636, self.SearchBest.move~=self.MoveNone);
         self:search_update_current();
         return false;
      end

      -- ================================================================
      -- If we are inside the root move loop, process one move per step
      -- ================================================================
      if(inRootLoop) then

         local list = self.SearchRoot.list;
         local board = self.SearchCurrent.board;

         -- All root moves exhausted: finish this depth
         if(rootMoveIdx >= list.size) then
            inRootLoop = false;

            --self:ASSERT(666, self:value_is_ok(root_best_value));

            self:list_sort(list);

            --self:ASSERT(667, self.SearchBest.move==list.move[1+0]);
            --self:ASSERT(668, self.SearchBest.value==root_best_value);

            if(self.UseTrans  and  root_best_value > root_old_alpha  and  root_best_value < root_beta) then
               self:pv_fill(self.SearchBest.pv, 0, board);
            end

            -- === post-depth logic ===

            self:search_update_current();

            if(self.DispDepthEnd) then
               self:send_ndtm(6);
            end

            if(depth >= 1) then
              self.SearchInfo.can_stop = true;
            end

            if(depth == 1
              and  list.size >= 2
              and  list.value[1+0] >= list.value[1+1] + self.EasyThreshold) then
               self.SearchRoot.easy = true;
            end

            if(self.UseBad  and  depth > 1) then
               self.SearchRoot.bad_2 = self.SearchRoot.bad_1;
               self.SearchRoot.bad_1 = false;
               --self:ASSERT(637, self.SearchRoot.bad_2==(self.SearchBest.value<=self.SearchRoot.last_value-self.BadThreshold));
            end

            self.SearchRoot.last_value = self.SearchBest.value;

            -- stop search?

            if(self.SearchInput.depth_is_limited
              and  depth >= self.SearchInput.depth_limit) then
               self.SearchRoot.flag = true;
            end

            if(self.SearchInput.time_is_limited
              and  self.SearchCurrent.time >= self.SearchInput.time_limit_1
              and  not self.SearchRoot.bad_2) then
               self.SearchRoot.flag = true;
            end

            if(self.UseEasy
              and  self.SearchInput.time_is_limited
              and  self.SearchCurrent.time >= self.SearchInput.time_limit_1 * self.EasyRatio
              and  self.SearchRoot.easy) then
               --self:ASSERT(638, not self.SearchRoot.bad_2);
               --self:ASSERT(639, not self.SearchRoot.change);
               self.SearchRoot.flag = true;
            end

            if(self.UseEarly
              and  self.SearchInput.time_is_limited
              and  self.SearchCurrent.time >= self.SearchInput.time_limit_1 * self.EarlyRatio
              and  not self.SearchRoot.bad_2
              and  not self.SearchRoot.change) then
               self.SearchRoot.flag = true;
            end

            if(self.SearchInfo.can_stop
              and(self.SearchInfo.stop  or(self.SearchRoot.flag  and  not self.SearchInput.infinite))) then
               return false;  -- search done
            end

            return true;  -- yield before next depth
         end

         -- === search one root move ===

         local i = rootMoveIdx;
         local move = list.move[1+i];
         local value;

         self.SearchRoot.depth = depth;
         self.SearchRoot.move = move;
         self.SearchRoot.move_pos = i;
         self.SearchRoot.move_nb = list.size;

         self:search_update_root();

         local new_depth = self:full_new_depth(depth,move,board,self:board_is_check(board) and list.size==1,true);

         self:move_do(board,move,root_undo);

         if(root_search_type == self.SearchShort  or  root_best_value == self.ValueNone) then
            -- first move or short search: full window
            value = -self:full_search(board,-root_beta,-root_alpha,new_depth,1,root_new_pv,self.NodePV);
            if(self.setjmp) then
               -- Search interrupted (longjmp simulation).
               -- Board is left modified - matches original full_root behaviour.
               -- Next step will detect setjmp and end the search.
               return true;
            end
         else
            -- other moves: zero-window first
            value = -self:full_search(board,-root_alpha-1,-root_alpha,new_depth,1,root_new_pv,self.NodeCut);
            if(self.setjmp) then
               return true;
            end
            if(value > root_alpha) then
               -- re-search with full window
               self.SearchRoot.change = true;
               self.SearchRoot.easy = false;
               self.SearchRoot.flag = false;
               self:search_update_root();
               value = -self:full_search(board,-root_beta,-root_alpha,new_depth,1,root_new_pv,self.NodePV);
               if(self.setjmp) then
                  return true;
               end
            end
         end

         self:move_undo(board,move,root_undo);

         -- Score the move
         if(value <= root_alpha) then
            list.value[1+i] = root_old_alpha;
         else
          if(value >= root_beta) then
            list.value[1+i] = root_beta;
          else
            list.value[1+i] = value;
          end
         end

         -- Update best
         if(value > root_best_value  and(root_best_value == self.ValueNone  or  value > root_alpha)) then

            self.SearchBest.move = move;
            self.SearchBest.value = value;
            if(value <= root_alpha) then
               self.SearchBest.flags = self.SearchUpper;
            else
             if(value >= root_beta) then
               self.SearchBest.flags = self.SearchLower;
             else
               self.SearchBest.flags = self.SearchExact;
             end
            end
            self.SearchBest.depth = depth;
            self:pv_cat(self.SearchBest.pv,root_new_pv,move);

            self:search_update_best();
         end

         if(value > root_best_value) then
            root_best_value = value;
            if(value > root_alpha) then
               if(root_search_type == self.SearchNormal) then
                 root_alpha = value;
               end
               if(value >= root_beta) then
                  -- Beta cutoff: skip remaining moves, finish depth on next step
                  rootMoveIdx = list.size;
                  return true;
               end
            end
         end

         rootMoveIdx = rootMoveIdx + 1;
         return true;  -- yield before next root move
      end

      -- ================================================================
      -- Not in root loop: depth setup / init
      -- ================================================================

      -- (Re-)init at start of each setjmp-loop iteration
      if(needsInit) then

         self:list_copy(self.SearchRoot.list,self.SearchInput.list);

         self:board_copy(self.SearchCurrent.board,self.SearchInput.board);
         self:my_timer_reset(self.SearchCurrent.timer);
         self:my_timer_start(self.SearchCurrent.timer);

         self:trans_inc_date(self.Trans);

         self:sort_init1();
         self:search_full_init(self.SearchRoot.list,self.SearchCurrent.board);

         depth = 0;
         needsInit = false;
      end

      depth = depth + 1;

      -- for-loop exhausted (depth > DepthMax-1): loop back to setjmp check
      if(depth > self.DepthMax - 1) then
         needsInit = true;
         return true;  -- continue (next step will check setjmp and re-init)
      end

      -- === start new depth: init root move loop ===

      if(self.DispDepthStart) then
         self:send("info depth " .. self:formatd(depth));
      end

      self.SearchRoot.bad_1 = false;
      self.SearchRoot.change = false;

      self:board_copy(self.SearchCurrent.board,self.SearchInput.board);

      -- Init root loop state (from full_root / search_full_root)
      local list = self.SearchRoot.list;
      root_search_type = (self.UseShortSearch and depth <= self.ShortSearchDepth)
                         and self.SearchShort or self.SearchNormal;
      root_alpha = -self.ValueInf;
      root_beta = self.ValueInf;
      root_old_alpha = root_alpha;
      root_best_value = self.ValueNone;

      self.SearchCurrent.node_nb = self.SearchCurrent.node_nb + 1;
      self.SearchInfo.check_nb = self.SearchInfo.check_nb - 1;

      for i = 0, list.size-1, 1 do
        list.value[1+i] = self.ValueNone;
      end

      rootMoveIdx = 0;
      inRootLoop = true;

      return true;  -- yield before first root move
   end

   loopFn(stepFn, onComplete)

end

-- self:search_update_best()

function self:search_update_best() -- void

   local move = 0;      -- int
   local value = 0;     -- int
   local flags = 0;     -- int
   local depth = 0;     -- int
   local max_depth = 0; -- int
   local pv = nil;
   local time = 0.0;
   local node_nb = 0;                  -- sint64
   local mate = 0;                     -- int
   local move_string = self:string_t();     -- string
   local pv_string = self:string_t();       -- string

   self:search_update_current();

   if(self.DispBest) then

      move = self.SearchBest.move;
      value = self.SearchBest.value;
      flags = self.SearchBest.flags;
      depth = self.SearchBest.depth;
      pv = self.SearchBest.pv;

      max_depth = self.SearchCurrent.max_depth;
      time = self.SearchCurrent.time;
      node_nb = self.SearchCurrent.node_nb;

      self:move_to_string(move,move_string);
      self:pv_to_string(pv,pv_string);

      mate = self:value_to_mate(value);
      self.SearchCurrent.mate = mate;

      if(mate == 0) then

         -- normal evaluation

         if(flags == self.SearchExact) then
            self:send_ndtm(10);
         else
            if(flags == self.SearchLower) then
              self:send_ndtm(11);
            else
              if(flags == self.SearchUpper) then
                 self:send_ndtm(12);
              end
            end
         end

      else

         -- mate announcement

         if(flags == self.SearchExact) then
            self:send_ndtm(20);
         else
            if(flags == self.SearchLower) then
              self:send_ndtm(21);
            else
              if(flags == self.SearchUpper) then
                 self:send_ndtm(22);
              end
            end
         end

      end
   end

   -- update time-management info

   if(self.UseBad  and  self.SearchBest.depth > 1) then
      if(self.SearchBest.value <= self.SearchRoot.last_value - self.BadThreshold) then
         self.SearchRoot.bad_1 = true;
         self.SearchRoot.easy = false;
         self.SearchRoot.flag = false;
      else
         self.SearchRoot.bad_1 = false;
      end
   end

end

-- self:search_update_root()

function self:search_update_root() -- void

   local move = 0;       -- int
   local move_pos = 0;   -- int

   local move_string = self:string_t();  -- string

   if(self.DispRoot) then

      self:search_update_current();

      if(self.SearchCurrent.time >= 1.0) then

         move = self.SearchRoot.move;
         move_pos = self.SearchRoot.move_pos;

         self:move_to_string(move,move_string);

         self:send("info currmove " .. move_string.v .. " currmovenumber ".. self:formatd(move_pos+1));
      end

   end
end

-- self:search_update_current()

function self:search_update_current() -- void

   local timer = nil;
   local node_nb = 0;
   local etime = 0.0;
   local speed = 0.0;

   timer = self.SearchCurrent.timer;
   node_nb = self.SearchCurrent.node_nb;

   etime = self:my_timer_elapsed_real(timer);
   speed = self:iif(etime >= 1.0, node_nb / etime, 0.0 );

   self.SearchCurrent.time = etime;
   self.SearchCurrent.speed = speed;

end

-- self:search_check()

function self:search_check()  -- void

   -- self:search_send_stat();

   -- event();

   if(self.SearchInput.depth_is_limited
     and  self.SearchRoot.depth > self.SearchInput.depth_limit) then
      self.SearchRoot.flag = true;
   end

   if(self.SearchInput.time_is_limited
     and  self.SearchCurrent.time >= self.SearchInput.time_limit_2) then
      self.SearchRoot.flag = true;
   end

   if(self.SearchInput.time_is_limited
     and  self.SearchCurrent.time >= self.SearchInput.time_limit_1
     and  not self.SearchRoot.bad_1
     and  not self.SearchRoot.bad_2
     and(not self.UseExtension  or  self.SearchRoot.move_pos == 0)) then
      self.SearchRoot.flag = true;
   end

   if(self.SearchInfo.can_stop
     and(self.SearchInfo.stop  or(self.SearchRoot.flag  and  not self.SearchInput.infinite))) then
      self.setjmp = true;  -- the same as  longjmp(self.SearchInfo.buf,1);
   end

end

-- self:search_send_stat()

function self:search_send_stat()  -- void

   local node_nb = 0;
   local time = 0.0;
   local speed = 0.0;

   self:search_update_current();

   if(self.DispStat  and  self.SearchCurrent.time >= self.SearchInfo.last_time + 1.0) then  -- at least one-second gap

      self.SearchInfo.last_time = self.SearchCurrent.time;

      time = self.SearchCurrent.time;
      speed = self.SearchCurrent.speed;
      node_nb = self.SearchCurrent.node_nb;

      self:send_ndtm(3);

      self:trans_stats(self.Trans);
   end

end

-- end of search.cpp



-- search_full.cpp

-- functions

-- self:search_full_init()

function self:search_full_init( list, board )

   local str1 = "";     -- string
   local tmove = 0;     -- int

   --self:ASSERT(640, self:list_is_ok(list));
   --self:ASSERT(641, self:board_is_ok(board));

   -- nil-move options

   str1 = self:option_get_string("nilMove Pruning");

   if(self:string_equal(str1,"Always")) then
      self.Usenil = true;
      self.UsenilEval = false;
   else
    if(self:string_equal(str1,"Fail High")) then
      self.Usenil = true;
      self.UsenilEval = true;
    else
     if(self:string_equal(str1,"Never")) then
       self.Usenil = false;
       self.UsenilEval = false;
     else
       --self:ASSERT(642, false);
       self.Usenil = true;
       self.UsenilEval = true;
     end
    end
   end

   self.nilReduction = self:option_get_int("nilMove Reduction");

   str1 = self:option_get_string("Verification Search");

   if(self:string_equal(str1,"Always")) then
      self.UseVer = true;
      self.UseVerEndgame = false;
   else
    if(self:string_equal(str1,"self.Endgame")) then
      self.UseVer = true;
      self.UseVerEndgame = true;
    else
     if(self:string_equal(str1,"Never")) then
      self.UseVer = false;
      self.UseVerEndgame = false;
     else
      --self:ASSERT(643, false);
      self.UseVer = true;
      self.UseVerEndgame = true;
     end
    end
   end

   self.VerReduction = self:option_get_int("Verification Reduction");

   -- history-pruning options

   self.UseHistory = self:option_get_bool("self.History Pruning");
   self.HistoryValue =(self:option_get_int("self.History Threshold") * 16384 + 50) / 100;

   -- futility-pruning options

   self.UseFutility = self:option_get_bool("Futility Pruning");
   self.FutilityMargin = self:option_get_int("Futility Margin");

   -- delta-pruning options

   self.UseDelta = self:option_get_bool("Delta Pruning");
   self.DeltaMargin = self:option_get_int("Delta Margin");

   -- quiescence-search options

   self.CheckNb = self:option_get_int("Quiescence Check Plies");
   self.CheckDepth = 1 - self.CheckNb;

   -- standard sort

   self:list_note(list);
   self:list_sort(list);

   -- basic sort

   tmove = self.MoveNone;
   if(self.UseTrans) then
     self:trans_retrieve(self.Trans, board.key, self.TransRv);
     tmove = self.TransRv.trans_move;
   end

   self:note_moves(list,board,0,tmove);
   self:list_sort(list);
end

-- self:search_full_root()

function self:search_full_root( list, board, depth, search_type )  -- int

   local value = 0;   -- int

   --self:ASSERT(644, self:list_is_ok(list));
   --self:ASSERT(645, self:board_is_ok(board));
   --self:ASSERT(646, self:depth_is_ok(depth));
   --self:ASSERT(647, search_type==self.SearchNormal or search_type==self.SearchShort);

   --self:ASSERT(648, list==self.SearchRoot.list);
   --self:ASSERT(649, not(list.size==0));
   --self:ASSERT(650, board==self.SearchCurrent.board);
   --self:ASSERT(651, self:board_is_legal(board));
   --self:ASSERT(652, depth>=1);

   value = self:full_root(list,board,-self.ValueInf, self.ValueInf,depth,0,search_type);
   if( self.setjmp ) then
     return 0;
   end

   --self:ASSERT(653, self:value_is_ok(value));
   --self:ASSERT(654, list.value[1+0]==value);

   return value;
end

-- self:full_root()

function self:full_root( list, board, alpha, beta, depth, height, search_type )  -- int

   local old_alpha = 0;    -- int
   local value = 0;        -- int
   local best_value = 0;   -- int
   local i = 0;            -- int
   local move = 0;         -- int
   local new_depth;        -- int
   local undo = self:undo_t();  -- undo_t[1]
   local new_pv = {};      -- int[self.HeightMax];

   --self:ASSERT(655, self:list_is_ok(list));
   --self:ASSERT(656, self:board_is_ok(board));
   --self:ASSERT(657, self:range_is_ok(alpha,beta));
   --self:ASSERT(658, self:depth_is_ok(depth));
   --self:ASSERT(659, self:height_is_ok(height));
   --self:ASSERT(660, search_type==self.SearchNormal or search_type==self.SearchShort);

   --self:ASSERT(661, list.size==self.SearchRoot.list.size);
   --self:ASSERT(662, not(list.size==0));
   --self:ASSERT(663, board.key==self.SearchCurrent.board.key);
   --self:ASSERT(664, self:board_is_legal(board));
   --self:ASSERT(665, depth>=1);

   -- init

   self.SearchCurrent.node_nb = self.SearchCurrent.node_nb + 1;
   self.SearchInfo.check_nb = self.SearchInfo.check_nb - 1;

   for i = 0, list.size-1, 1 do
     list.value[1+i] = self.ValueNone;
   end

   old_alpha = alpha;
   best_value = self.ValueNone;

   -- move loop

   for i = 0, list.size-1, 1 do

      move = list.move[1+i];

      self.SearchRoot.depth = depth;
      self.SearchRoot.move = move;
      self.SearchRoot.move_pos = i;
      self.SearchRoot.move_nb = list.size;

      self:search_update_root();

      new_depth = self:full_new_depth(depth,move,board,self:board_is_check(board) and list.size==1,true);

      self:move_do(board,move,undo);

      if(search_type == self.SearchShort  or  best_value == self.ValueNone) then   -- first move
         value = -self:full_search(board,-beta,-alpha,new_depth,height+1,new_pv,self.NodePV);
         if( self.setjmp ) then
           return 0;
         end
      else  -- other moves
         value = -self:full_search(board,-alpha-1,-alpha,new_depth,height+1,new_pv,self.NodeCut);
         if( self.setjmp ) then
           return 0;
         end
         if(value > alpha) then   --  and  value < beta
            self.SearchRoot.change = true;
            self.SearchRoot.easy = false;
            self.SearchRoot.flag = false;
            self:search_update_root();
            value = -self:full_search(board,-beta,-alpha,new_depth,height+1,new_pv,self.NodePV);
            if( self.setjmp ) then
              return 0;
            end
         end
      end

      self:move_undo(board,move,undo);

      if(value <= alpha) then    -- upper bound
         list.value[1+i] = old_alpha;
      else
       if(value >= beta) then    -- lower bound
         list.value[1+i] = beta;
       else      -- alpha < value < beta => exact value
         list.value[1+i] = value;
       end
      end

      if(value > best_value  and(best_value == self.ValueNone  or  value > alpha)) then

         self.SearchBest.move = move;
         self.SearchBest.value = value;
         if(value <= alpha) then    -- upper bound
            self.SearchBest.flags = self.SearchUpper;
         else
          if(value >= beta) then    -- lower bound
            self.SearchBest.flags = self.SearchLower;
          else      -- alpha < value < beta => exact value
            self.SearchBest.flags = self.SearchExact;
          end
         end
         self.SearchBest.depth = depth;
         self:pv_cat(self.SearchBest.pv,new_pv,move);

         self:search_update_best();
      end

      if(value > best_value) then
         best_value = value;
         if(value > alpha) then
            if(search_type == self.SearchNormal) then
              alpha = value;
            end
            if(value >= beta) then
              break;
            end
         end
      end
   end

   --self:ASSERT(666, self:value_is_ok(best_value));

   self:list_sort(list);

   --self:ASSERT(667, self.SearchBest.move==list.move[1+0]);
   --self:ASSERT(668, self.SearchBest.value==best_value);

   if(self.UseTrans  and  best_value > old_alpha  and  best_value < beta) then
      self:pv_fill(self.SearchBest.pv, 0, board);
   end

   return best_value;

end

-- self:full_search()

function self:full_search( board, alpha, beta, depth, height, pv, node_type )  -- int

   local in_check = false;       -- bool
   local single_reply = false;   -- bool
   local tmove = 0;       -- int
   local tdepth = 0;      -- int

   local min_value = 0;   -- int
   local max_value = 0;   -- int
   local old_alpha = 0;   -- int
   local value = 0;       -- int
   local best_value = 0;  -- int

   local bmove = self:int_t(); -- int
   local move = 0;        -- int

   local best_move = 0;   -- int
   local new_depth = 0;   -- int
   local played_nb = 0;   -- int
   local i = 0;           -- int
   local opt_value = 0;   -- int
   local reduced = false;      -- bool
   local attack = self:attack_t();  -- attack_t[1]
   local sort = self:sort_t();      -- sort_t[1]
   local undo = self:undo_t();      -- undo_t[1]
   local new_pv = {};          -- int[self.HeightMax]
   local played = {};          -- int[256]
   local gotocut = false;
   local cont = false;

   --self:ASSERT(669, board.sp~=nil);
   --self:ASSERT(670, self:range_is_ok(alpha,beta));
   --self:ASSERT(671, self:depth_is_ok(depth));
   --self:ASSERT(672, self:height_is_ok(height));
   -- --self:ASSERT(673, pv[1+0]~=nil);
   --self:ASSERT(674, node_type==self.NodePV or node_type==self.NodeCut or node_type==self.NodeAll);

   --self:ASSERT(675, self:board_is_legal(board));

   -- horizon?

   if(depth <= 0) then
     return self:full_quiescence(board,alpha,beta,0,height,pv);
   end

   -- init

   self.SearchCurrent.node_nb = self.SearchCurrent.node_nb + 1;
   self.SearchInfo.check_nb = self.SearchInfo.check_nb - 1;
   pv[1+0] = self.MoveNone;

   if(height > self.SearchCurrent.max_depth) then
     self.SearchCurrent.max_depth = height;
   end

   if(self.SearchInfo.check_nb <= 0) then
      self.SearchInfo.check_nb = self.SearchInfo.check_nb + self.SearchInfo.check_inc;
      self:search_check();
      if( self.setjmp ) then
        return 0;
      end
   end

   -- draw?

   if(self:board_is_repetition(board)  or  self:recog_draw(board)) then
     return self.ValueDraw;
   end

   -- mate-distance pruning

   if(self.UseDistancePruning) then

      -- lower bound

      value =(height+2-self.ValueMate); -- does not work if the current position is mate
      if(value > alpha  and  self:board_is_mate(board)) then
         value =(height-self.ValueMate);
      end

      if(value > alpha) then
         alpha = value;
         if(value >= beta) then
           return value;
         end
      end

      -- upper bound

      value = -(height+1-self.ValueMate);

      if(value < beta) then
         beta = value;
         if(value <= alpha) then
           return value;
         end
      end
   end

   -- transposition table

   tmove = self.MoveNone;

   if(self.UseTrans  and  depth >= self.TransDepth) then

     if( self:trans_retrieve(self.Trans, board.key, self.TransRv)) then

         tmove = self.TransRv.trans_move;

         -- trans_move is now updated

         if(node_type ~= self.NodePV) then

            if(self.UseMateValues) then

               if(self.TransRv.trans_min_value > self.ValueEvalInf  and  self.TransRv.trans_min_depth < depth) then
                  self.TransRv.trans_min_depth = depth;
               end

               if(self.TransRv.trans_max_value < -self.ValueEvalInf  and  self.TransRv.trans_max_depth < depth) then
                  self.TransRv.trans_max_depth = depth;
               end
            end

            min_value = -self.ValueInf;

            if( self.TransRv.trans_min_depth >= depth ) then
               min_value = self:value_from_trans(self.TransRv.trans_min_value,height);
               if(min_value >= beta) then
                 return min_value;
               end
            end

            max_value = self.ValueInf;

            if( self.TransRv.trans_max_depth >= depth ) then
               max_value = self:value_from_trans(self.TransRv.trans_max_value,height);
               if(max_value <= alpha) then
                 return max_value;
               end
            end

            if(min_value == max_value) then
              return min_value; -- exact match
            end
         end
      end
   end

   -- height limit

   if(height >= self.HeightMax-1) then
     return self:evalpos(board);
   end

   -- more init

   old_alpha = alpha;
   best_value = self.ValueNone;
   best_move = self.MoveNone;
   played_nb = 0;

   self:attack_set(attack,board);
   in_check = self:ATTACK_IN_CHECK(attack);

   -- nil-move pruning

   if(self.Usenil  and  depth >= self.nilDepth  and  node_type ~= self.NodePV) then

      if(not in_check
        and  not self:value_is_mate(beta)
        and  self:do_nil(board)
        and(not self.UsenilEval  or  depth <= self.nilReduction+1  or  self:evalpos(board) >= beta)) then

         -- nil-move search

         new_depth = depth - self.nilReduction - 1;

         self:move_do_nil(board,undo);
         value = -self:full_search(board,-beta,-beta+1,new_depth,height+1,new_pv,-node_type);
         if( self.setjmp ) then
           return 0;
         end
         self:move_undo_nil(board,undo);

         -- verification search

         if(self.UseVer  and  depth > self.VerReduction) then

            if(value >= beta  and(not self.UseVerEndgame  or  self:do_ver(board))) then

               new_depth = depth - self.VerReduction;
               --self:ASSERT(676, new_depth>0);

               value = self:full_no_nil(board,alpha,beta,new_depth,height,new_pv,self.NodeCut,tmove, bmove);
               move = bmove.v;

               if( self.setjmp ) then
                 return 0;
               end
               if(value >= beta) then
                  --self:ASSERT(677, move==new_pv[1+0]);
                  played[1+played_nb] = move;
                  played_nb = played_nb + 1;
                  best_move = move;
                  best_value = value;
                  self:pv_copy(pv,new_pv);
                  gotocut = true;
               end
            end
         end

         -- pruning

         if((not gotocut) and value >= beta) then

            if(value > self.ValueEvalInf) then
              value = self.ValueEvalInf; -- do not return unproven mates
            end
            --self:ASSERT(678, not self:value_is_mate(value));

            -- self:pv_cat(pv,new_pv,self.Movenil);

            best_move = self.MoveNone;
            best_value = value;
            gotocut = true;
         end
      end
   end

 if(not gotocut) then  -- [1]

   -- Internal Iterative Deepening

   if(self.UseIID  and  depth >= self.IIDDepth  and  node_type == self.NodePV  and  tmove == self.MoveNone) then

      new_depth = depth - self.IIDReduction;
      --self:ASSERT(679, new_depth>0);

      value = self:full_search(board,alpha,beta,new_depth,height,new_pv,node_type);
      if( self.setjmp ) then
        return 0;
      end
      if(value <= alpha) then
         value = self:full_search(board,-self.ValueInf,beta,new_depth,height,new_pv,node_type);
         if( self.setjmp ) then
           return 0;
         end
      end

      tmove = new_pv[1+0];
   end

   -- move generation

   self:sort_init2(sort,board,attack,depth,height,tmove);

   single_reply = false;
   if(in_check  and  sort.list.size == 1) then
     single_reply = true; -- HACK
   end

   -- move loop

   opt_value = self.ValueInf;

   while(true) do

      move = self:sort_next(sort);
      if(move == self.MoveNone) then
        break
      end

      -- extensions

      new_depth = self:full_new_depth(depth,move,board,single_reply,node_type==self.NodePV);

      -- history pruning

      reduced = false;

      if(self.UseHistory  and  depth >= self.HistoryDepth  and  node_type ~= self.NodePV) then
         if(not in_check  and  played_nb >= self.HistoryMoveNb  and  new_depth < depth) then
            --self:ASSERT(680, best_value~=self.ValueNone);
            --self:ASSERT(681, played_nb>0);
            --self:ASSERT(682, sort.pos>0 and move==sort.list.move[1+sort.pos-1]);
            value = sort.value; -- history score
            if(value < self.HistoryValue) then
               --self:ASSERT(683, value>=0 and value<16384);
               --self:ASSERT(684, move~=tmove);
               --self:ASSERT(685, not self:move_is_tactical(move,board));
               --self:ASSERT(686, not self:move_is_check(move,board));
               new_depth = new_depth - 1;
               reduced = true;
            end
         end
      end

      -- futility pruning

      if(self.UseFutility  and  depth == 1  and  node_type ~= self.NodePV) then

         if((not in_check)  and  new_depth == 0  and(not self:move_is_tactical(move,board))
                   and(not self:move_is_dangerous(move,board))) then

            --self:ASSERT(687, not self:move_is_check(move,board));

            -- optimistic evaluation

            if(opt_value == self.ValueInf) then
               opt_value = self:evalpos(board) + self.FutilityMargin;
               --self:ASSERT(688, opt_value<self.ValueInf);
            end

            value = opt_value;

            -- pruning

            if(value <= alpha) then

               if(value > best_value) then
                  best_value = value;
                  pv[1+0] = self.MoveNone;
               end

               cont = true;
            end
         end
      end

     if(cont) then  -- continue [1]
       cont = false;
     else

      -- recursive search

      self:move_do(board,move,undo);

      if(node_type ~= self.NodePV  or  best_value == self.ValueNone) then    -- first move
         value = -self:full_search(board,-beta,-alpha,new_depth,height+1,new_pv,-node_type);
         if( self.setjmp ) then
           return 0;
         end
      else       -- other moves
         value = -self:full_search(board,-alpha-1,-alpha,new_depth,height+1,new_pv,self.NodeCut);
         if( self.setjmp ) then
           return 0;
         end
         if(value > alpha) then    --  and  value < beta
            value = -self:full_search(board,-beta,-alpha,new_depth,height+1,new_pv,self.NodePV);
            if( self.setjmp ) then
              return 0;
            end
         end
      end

      -- history-pruning re-search

      if(self.HistoryReSearch  and  reduced  and  value >= beta) then

         --self:ASSERT(689, node_type~=self.NodePV);

         new_depth = new_depth + 1;
         --self:ASSERT(690, new_depth==depth-1);

         value = -self:full_search(board,-beta,-alpha,new_depth,height+1,new_pv,-node_type);
         if( self.setjmp ) then
           return 0;
         end
      end

      self:move_undo(board,move,undo);

      played[1+played_nb] = move;
      played_nb = played_nb + 1;

      if(value > best_value) then
         best_value = value;
         self:pv_cat(pv,new_pv,move);
         if(value > alpha) then
            alpha = value;
            best_move = move;
            if(value >= beta) then
              gotocut = true;
              break;
            end
         end
      end

      if(node_type == self.NodeCut) then
        node_type = self.NodeAll;
      end

     end  -- continue [1]

   end


 if(not gotocut) then  -- [2]

   -- ALL node

   if(best_value == self.ValueNone) then    -- no legal move
      if(in_check) then
         --self:ASSERT(691, self:board_is_mate(board));
         return(height-self.ValueMate);
      else
         --self:ASSERT(692, self:board_is_stalemate(board));
         return self.ValueDraw;
      end
   end

 end -- goto cut [2]
 end -- goto cut [1]

-- cut:

   --self:ASSERT(693, self:value_is_ok(best_value));

   -- move ordering

   if(best_move ~= self.MoveNone) then

      self:good_move(best_move,board,depth,height);

      if(best_value >= beta  and(not self:move_is_tactical(best_move,board))) then

         --self:ASSERT(694, played_nb>0 and played[1+played_nb-1]==best_move);

         for i = 0, played_nb-2, 1 do
            move = played[1+i];
            --self:ASSERT(695, move~=best_move);
            self:history_bad(move,board);
         end

         self:history_good(best_move,board);
      end
   end

   -- transposition table

   if(self.UseTrans  and  depth >= self.TransDepth) then

      tmove = best_move;
      tdepth = depth;
      self.TransRv.trans_min_value = self:iif( best_value > old_alpha,  self:value_to_trans(best_value,height) , -self.ValueInf );
      self.TransRv.trans_max_value = self:iif( best_value < beta , self:value_to_trans(best_value,height) , self.ValueInf );

      self:trans_store(self.Trans,board.key, tmove, tdepth, self.TransRv);
   end

   return best_value;

end


-- self:full_no_nil()

function self:full_no_nil( board, alpha,  beta, depth, height, pv, node_type, tmove,  b_move )  -- int

   local value = 0;            -- int
   local best_value = 0;       -- int
   local move = 0;             -- int
   local new_depth = 0;        -- int

   local attack = self:attack_t();  -- attack_t[1]
   local sort = self:sort_t();      -- sort_t[1]
   local undo = self:undo_t();      -- undo_t[1]
   local new_pv = {};          -- int[self.HeightMax]
   local gotocut = false;

   --self:ASSERT(696, board.sp~=nil);
   --self:ASSERT(697, self:range_is_ok(alpha,beta));
   --self:ASSERT(698, self:depth_is_ok(depth));
   --self:ASSERT(699, self:height_is_ok(height));
   -- --self:ASSERT(700, pv[1+0]~=nil);
   --self:ASSERT(701, node_type==self.NodePV or node_type==self.NodeCut or node_type==self.NodeAll);
   --self:ASSERT(702, tmove==self.MoveNone or self:move_is_ok(tmove));
   --self:ASSERT(703, best_move~=nil);

   --self:ASSERT(704, self:board_is_legal(board));
   --self:ASSERT(705, not self:board_is_check(board));
   --self:ASSERT(706, depth>=1);

   -- init

   self.SearchCurrent.node_nb = self.SearchCurrent.node_nb + 1;
   self.SearchInfo.check_nb = self.SearchInfo.check_nb - 1;
   pv[1+0] = self.MoveNone;

   if(height > self.SearchCurrent.max_depth) then
     self.SearchCurrent.max_depth = height;
   end

   if(self.SearchInfo.check_nb <= 0) then
      self.SearchInfo.check_nb = self.SearchInfo.check_nb + self.SearchInfo.check_inc;
      self:search_check();
      if( self.setjmp ) then
        return 0;
      end
   end

   self:attack_set(attack,board);
   --self:ASSERT(707, not self:ATTACK_IN_CHECK(attack));

   b_move.v = self.MoveNone;
   best_value = self.ValueNone;

   -- move loop

   self:sort_init2(sort,board,attack,depth,height,tmove);


   while(true) do

      move = self:sort_next(sort);
      if(move == self.MoveNone) then
        break
      end

      new_depth = self:full_new_depth(depth,move,board,false,false);

      self:move_do(board,move,undo);
      value = -self:full_search(board,-beta,-alpha,new_depth,height+1,new_pv,-node_type);
      if( self.setjmp ) then
         return 0;
      end
      self:move_undo(board,move,undo);

      if(value > best_value) then
         best_value = value;
         self:pv_cat(pv,new_pv,move);
         if(value > alpha) then
            alpha = value;
            b_move.v = move;
            if(value >= beta) then
              gotocut = true;
              break;
            end
         end
      end

   end

 if(not gotocut) then  -- [1]

   -- ALL node

   if(best_value == self.ValueNone) then     -- no legal move => stalemate
      --self:ASSERT(708, self:board_is_stalemate(board));
      best_value = self.ValueDraw;
   end

 end -- goto cut [1]

-- cut:

   --self:ASSERT(709, self:value_is_ok(best_value));

   return best_value;

end

-- self:full_quiescence()

function self:full_quiescence( board, alpha, beta, depth, height, pv ) -- int

   local in_check = false;     -- bool
   local old_alpha = 0;        -- int

   local value = 0;            -- int
   local best_move = 0;        -- int
   local best_value = 0;       -- int
   local opt_value = 0;        -- int
   local move = 0;             -- int

   local to = 0;               -- int
   local capture = 0;          -- int

   local attack = self:attack_t();  -- attack_t[1]
   local sort = self:sort_t();      -- sort_t[1]
   local undo = self:undo_t();      -- undo_t[1]
   local new_pv = {};          -- int[self.HeightMax]

   local gotocut = false;
   local cont = false;

   --self:ASSERT(710, board.sp~=nil);
   --self:ASSERT(711, self:range_is_ok(alpha,beta));
   --self:ASSERT(712, self:depth_is_ok(depth));
   --self:ASSERT(713, self:height_is_ok(height));
   -- --self:ASSERT(714, pv[1+0]~=nil);

   --self:ASSERT(715, self:board_is_legal(board));
   --self:ASSERT(716, depth<=0);

   -- init

   self.SearchCurrent.node_nb = self.SearchCurrent.node_nb + 1;
   self.SearchInfo.check_nb = self.SearchInfo.check_nb - 1;
   pv[1+0] = self.MoveNone;

   if(height > self.SearchCurrent.max_depth) then
     self.SearchCurrent.max_depth = height;
   end

   if(self.SearchInfo.check_nb <= 0) then
      self.SearchInfo.check_nb = self.SearchInfo.check_nb + self.SearchInfo.check_inc;
      self:search_check();
      if( self.setjmp ) then
        return 0;
      end
   end

   -- draw?

   if(self:board_is_repetition(board)  or  self:recog_draw(board)) then
     return self.ValueDraw;
   end

   -- mate-distance pruning

   if(self.UseDistancePruning) then

      -- lower bound

      value =(height+2-self.ValueMate); -- does not work if the current position is mate
      if(value > alpha  and  self:board_is_mate(board)) then
        value =(height-self.ValueMate);
      end

      if(value > alpha) then
         alpha = value;
         if(value >= beta) then
           return value;
         end
      end

      -- upper bound

      value = -(height+1-self.ValueMate);

      if(value < beta) then
         beta = value;
         if(value <= alpha) then
           return value;
         end
      end
   end

   -- more init

   self:attack_set(attack,board);
   in_check = self:ATTACK_IN_CHECK(attack);

   if(in_check) then
      --self:ASSERT(717, depth<0);
      depth = depth + 1; -- in-check extension
   end

   -- height limit

   if(height >= self.HeightMax-1) then
     return self:evalpos(board);
   end

   -- more init

   old_alpha = alpha;
   best_value = self.ValueNone;
   best_move = self.MoveNone;

   -- if(self.UseDelta)
   opt_value = self.ValueInf;

   if(not in_check) then

      -- lone-king stalemate?

      if(self:simple_stalemate(board)) then
        return self.ValueDraw;
      end

      -- stand pat

      value = self:evalpos(board);

      --self:ASSERT(718, value>best_value);
      best_value = value;
      if(value > alpha) then
         alpha = value;
         if(value >= beta) then
           gotocut = true;
         end
      end

      if((not gotocut) and self.UseDelta) then
         opt_value = value + self.DeltaMargin;
         --self:ASSERT(719, opt_value<self.ValueInf);
      end
   end

 if(not gotocut) then  -- [1]

   -- move loop

   self:sort_init_qs(sort,board,attack,depth>=self.CheckDepth);


   while(true) do

      move = self:sort_next_qs(sort);
      if(move == self.MoveNone) then
        break
      end


      -- delta pruning

      if(self.UseDelta  and  beta == old_alpha+1) then

         if((not in_check) and(not self:move_is_check(move,board)) and(not self:capture_is_dangerous(move,board))) then

            --self:ASSERT(720, self:move_is_tactical(move,board));

            -- optimistic evaluation

            value = opt_value;

            to = self:MOVE_TO(move);
            capture = board.square[1+to];

            if(capture ~= self.Empty) then
               value = value + self.ValuePiece[1+capture];
            else
             if(self:MOVE_IS_EN_PASSANT(move)) then
               value = value + self.ValuePawn;
             end
            end

            if(self:MOVE_IS_PROMOTE(move)) then
              value = value + self.ValueQueen - self.ValuePawn;
            end

            -- pruning

            if(value <= alpha) then

               if(value > best_value) then
                  best_value = value;
                  pv[1+0] = self.MoveNone;
               end

               cont = true;
            end
         end
      end

     if(cont) then  -- continue [1]
       cont = false;
     else

      self:move_do(board,move,undo);
      value = -self:full_quiescence(board,-beta,-alpha,depth-1,height+1,new_pv);
      if( self.setjmp ) then
        return 0;
      end
      self:move_undo(board,move,undo);

      if(value > best_value) then
         best_value = value;
         self:pv_cat(pv,new_pv,move);
         if(value > alpha) then
            alpha = value;
            best_move = move;
            if(value >= beta) then
              gotocut = true;
              break;
            end
         end
      end

     end  -- continue [1]

   end

 if(not gotocut) then  -- [2]

   -- ALL node

   if(best_value == self.ValueNone) then        -- no legal move
      --self:ASSERT(721, self:board_is_mate(board));
      return(height-self.ValueMate);
   end

 end -- goto cut [2]
 end -- goto cut [1]

-- cut:

   --self:ASSERT(722, self:value_is_ok(best_value));

   return best_value;

end

-- self:full_new_depth()

function self:full_new_depth( depth, move, board, single_reply, in_pv )  -- int
   local new_depth = 0;   -- int
   local b = false;       -- bool

   --self:ASSERT(723, self:depth_is_ok(depth));
   --self:ASSERT(724, self:move_is_ok(move));
   --self:ASSERT(725, board.sp~=nil);
   --self:ASSERT(726, single_reply==true or single_reply==false);
   --self:ASSERT(727, in_pv==true or in_pv==false);

   --self:ASSERT(728, depth>0);

   new_depth = depth - 1;

   b = b or(single_reply  and  self.ExtendSingleReply);
   b = b or(in_pv  and  self:MOVE_TO(move) == board.cap_sq and  self:see_move(move,board) > 0)  -- recapture
   b = b or(in_pv  and  self:PIECE_IS_PAWN(self:MOVE_PIECE(move,board))
               and  self:PAWN_RANK(self:MOVE_TO(move),board.turn) == self.Rank7
               and  self:see_move(move,board) >= 0);
   b = b or self:move_is_check(move,board);
   if(b) then
      new_depth = new_depth + 1;
   end

   --self:ASSERT(729, new_depth>=0 and new_depth<=depth);

   return new_depth;
end

-- self:do_nil()

function self:do_nil( board )  -- bool

   --self:ASSERT(730, board.sp~=nil);

   -- use nil move if the side-to-move has at least one piece

   return(board.piece_size[1+board.turn] >= 2); -- king + one piece
end

-- self:do_ver()

function self:do_ver( board )  -- bool

   --self:ASSERT(731, board.sp~=nil);

   -- use verification if the side-to-move has at most one piece

   return(board.piece_size[1+board.turn] <= 2); -- king + one piece
end

-- self:pv_fill()

function self:pv_fill( pv, at, board ) -- void

   local move = 0;   -- int
   local tmove = 0;  -- int
   local tdepth = 0; -- int

   local undo = self:undo_t();      -- undo_t[1]

   --self:ASSERT(732, pv[1+at]~=nil);
   --self:ASSERT(733, board.sp~=nil);

   --self:ASSERT(734, self.UseTrans);

   move = pv[1+at];

   if(move ~= self.MoveNone  and  move ~= self.Movenil) then

      self:move_do(board,move,undo);
      self:pv_fill(pv, at+1,board);
      self:move_undo(board,move,undo);

      tmove = move;
      tdepth = -127; -- HACK
      self.TransRv.trans_min_value = -self.ValueInf;
      self.TransRv.trans_max_value = self.ValueInf;

      self:trans_store(self.Trans, board.key, tmove, tdepth, self.TransRv);
   end
end

-- self:move_is_dangerous()

function self:move_is_dangerous( move, board )  -- bool

   local piece = 0;   -- int

   --self:ASSERT(735, self:move_is_ok(move));
   --self:ASSERT(736, board.sp~=nil);

   --self:ASSERT(737, not self:move_is_tactical(move,board));

   piece = self:MOVE_PIECE(move,board);

   if(self:PIECE_IS_PAWN(piece) and  self:PAWN_RANK(self:MOVE_TO(move),board.turn) >= self.Rank7) then
      return true;
   end

   return false;
end

-- self:capture_is_dangerous()

function self:capture_is_dangerous( move, board )  -- bool

   local piece = 0;     -- int
   local capture = 0;   -- int

   --self:ASSERT(738, self:move_is_ok(move));
   --self:ASSERT(739, board.sp~=nil);

   --self:ASSERT(740, self:move_is_tactical(move,board));

   piece = self:MOVE_PIECE(move,board);

   if(self:PIECE_IS_PAWN(piece) and  self:PAWN_RANK(self:MOVE_TO(move),board.turn) >= self.Rank7) then
      return true;
   end

   capture = self:move_capture(move,board);

   if(self:PIECE_IS_QUEEN(capture)) then
     return true;
   end

   if(self:PIECE_IS_PAWN(capture) and  self:PAWN_RANK(self:MOVE_TO(move),board.turn) <= self.Rank2) then
      return true;
   end

   return false;
end

-- self:simple_stalemate()

function self:simple_stalemate( board )  -- bool

   local me = 0          -- int
   local opp = 0;        -- int
   local king = 0;       -- int
   local opp_flag = 0;   -- int
   local from = 0;       -- int
   local to = 0;         -- int
   local capture = 0;    -- int
   local inc_ptr = 0;    -- int
   local inc = 0;        -- int

   --self:ASSERT(741, board.sp~=nil);

   --self:ASSERT(742, self:board_is_legal(board));
   --self:ASSERT(743, not self:board_is_check(board));

   -- lone king?

   me = board.turn;
   if(board.piece_size[1+me] ~= 1  or  board.pawn_size[1+me] ~= 0) then
     return false; -- no
   end

   -- king in a corner?

   king = self:KING_POS(board,me);
   if(king ~= self.A1  and  king ~= self.H1  and  king ~= self.A8  and  king ~= self.H8) then
     return false; -- no
   end

   -- init

   opp = self:COLOUR_OPP(me);
   opp_flag = self:COLOUR_FLAG(opp);

   -- king can move?

   from = king;

   inc_ptr = 0;
   while(true) do
      inc = self.KingInc[1+inc_ptr];
      if( inc == self.IncNone ) then
        break;
      end

      to = from + inc;
      capture = board.square[1+to];
      if(capture == self.Empty  or  self:FLAG_IS(capture,opp_flag)) then
         if(not self:is_attacked(board,to,opp)) then
           return false; -- legal king move
         end
      end

      inc_ptr = inc_ptr + 1;
   end


   -- no legal move

   --self:ASSERT(744, self:board_is_stalemate( board ));

   return true;
end

-- end of search_full.cpp



-- see.cpp

-- types

-- functions

-- self:see_move()

function self:see_move( move, board )  -- int

   local att = 0;              -- int
   local def = 0;              -- int
   local from = 0;             -- int
   local to = 0;               -- int
   local value = 0;            -- int
   local piece_value = 0;      -- int
   local piece = 0;            -- int
   local capture = 0;          -- int
   local pos = 0;              -- int
   local alists = self:alists_t();  -- alists_t[1]
   local alist = nil;          -- alist_t *

   --self:ASSERT(745, self:move_is_ok(move));
   --self:ASSERT(746, board.sp~=nil);

   -- init

   from = self:MOVE_FROM(move);
   to = self:MOVE_TO(move);

   -- move the piece

   piece_value = 0;

   piece = board.square[1+from];
   --self:ASSERT(747, self:piece_is_ok(piece));

   att = self:PIECE_COLOUR(piece);
   def = self:COLOUR_OPP(att);

   -- promote

   if(self:MOVE_IS_PROMOTE(move)) then
      --self:ASSERT(748, self:PIECE_IS_PAWN(piece));
      piece = self:move_promote(move);
      --self:ASSERT(749, self:piece_is_ok(piece));
      --self:ASSERT(750, self:COLOUR_IS(piece,att));
   end

   piece_value = piece_value + self.ValuePiece[1+piece];

   -- clear attacker lists

   self:alist_clear(alists.alist[1+self.Black]);
   self:alist_clear(alists.alist[1+self.White]);

   -- find hidden attackers

   self:alists_hidden(alists,board,from,to);

   -- capture the piece

   value = 0;

   capture = board.square[1+to];

   if(capture ~= self.Empty) then

      --self:ASSERT(751, self:piece_is_ok(capture));
      --self:ASSERT(752, self:COLOUR_IS(capture,def));

      value = value + self.ValuePiece[1+capture];
   end

   -- promote

   if(self:MOVE_IS_PROMOTE(move)) then
      value = value + self.ValuePiece[1+piece] - self.ValuePawn;
   end

   -- en-passant

   if(self:MOVE_IS_EN_PASSANT(move)) then
      --self:ASSERT(753, value==0);
      --self:ASSERT(754, self:PIECE_IS_PAWN(board.square[1+self:SQUARE_EP_DUAL(to)]));
      value = value + self.ValuePawn;
      self:alists_hidden(alists,board,self:SQUARE_EP_DUAL(to),to);
   end

   -- build defender list

   alist = alists.alist[1+def];

   self:alist_build(alist,board,to,def);
   if(alist.size == 0) then
     return value; -- no defender => stop SEE
   end

   -- build attacker list

   alist = alists.alist[1+att];

   self:alist_build(alist,board,to,att);

   -- remove the moved piece(if it's an attacker)

   pos = 0;
   while( pos<alist.size  and  alist.square[1+pos] ~= from ) do
     pos = pos + 1;
   end

   if(pos < alist.size) then
     self:alist_remove(alist,pos);
   end

   -- SEE search

   value = value - self:see_rec(alists,board,def,to,piece_value);

   return value;

end

-- self:see_square()

function self:see_square( board, to, colour )  -- int

   local att = 0;              -- int
   local def = 0;              -- int
   local piece_value = 0;      -- int
   local piece = 0;            -- int
   local alists = self:alists_t();  -- alists_t[1]
   local alist = nil;          -- alist_t *

   --self:ASSERT(755, board.sp~=nil);
   --self:ASSERT(756, self:SQUARE_IS_OK(to));
   --self:ASSERT(757, self:COLOUR_IS_OK(colour));

   --self:ASSERT(758, self:COLOUR_IS(board.square[1+to],self:COLOUR_OPP(colour)));

   -- build attacker list

   att = colour;

   alist = alists.alist[1+att];

   self:alist_clear(alist);

   self:alist_build(alist,board,to,att);

   if(alist.size == 0) then
     return 0; -- no attacker => stop SEE
   end

   -- build defender list

   def = self:COLOUR_OPP(att);
   alist = alists.alist[1+def];

   self:alist_clear(alist);

   self:alist_build(alist,board,to,def);

   -- captured piece

   piece = board.square[1+to];
   --self:ASSERT(759, self:piece_is_ok(piece));
   --self:ASSERT(760, self:COLOUR_IS(piece,def));

   piece_value = self.ValuePiece[1+piece];

   -- SEE search

   return self:see_rec(alists,board,att,to,piece_value);

end

-- self:see_rec()

function self:see_rec( alists, board, colour, to, piece_value )  -- int

   local from = 0;    -- int
   local piece = 0;   -- int
   local value = 0;   -- int

   --self:ASSERT(761, alists.alist[1+colour]~=nil);
   --self:ASSERT(762, board.sp~=nil);
   --self:ASSERT(763, self:COLOUR_IS_OK(colour));
   --self:ASSERT(764, self:SQUARE_IS_OK(to));
   --self:ASSERT(765, piece_value>0);

   -- find the least valuable attacker

   from = self:alist_pop(alists.alist[1+colour],board);
   if(from == self.SquareNone) then
     return 0; -- no more attackers
   end

   -- find hidden attackers

   self:alists_hidden(alists,board,from,to);

   -- calculate the capture value

   value = piece_value; -- captured piece
   if(value == self.ValueKing) then
     return value; -- do not allow an answer to a king capture
   end

   piece = board.square[1+from];
   --self:ASSERT(766, self:piece_is_ok(piece));
   --self:ASSERT(767, self:COLOUR_IS(piece,colour));
   piece_value = self.ValuePiece[1+piece];

   -- promote

   if(piece_value == self.ValuePawn  and  self.SquareIsPromote[1+to]) then    -- HACK: self:PIECE_IS_PAWN(piece)
      --self:ASSERT(768, self:PIECE_IS_PAWN(piece));
      piece_value = self.ValueQueen;
      value = value + self.ValueQueen - self.ValuePawn;
   end

   value = value - self:see_rec(alists,board,self:COLOUR_OPP(colour),to,piece_value);

   if(value < 0) then
     value = 0;
   end

   return value;

end

-- self:alist_build()

function self:alist_build( alist, board, to, colour )  -- int

   local ptr = 0;    -- int
   local from = 0;   -- int
   local piece = 0;  -- int
   local delta = 0;  -- int
   local inc = 0;    -- int
   local sq = 0;     -- int
   local pawn = 0;   -- int

   --self:ASSERT(769, alist.size~=nil);
   --self:ASSERT(770, board.sp~=nil);
   --self:ASSERT(771, self:SQUARE_IS_OK(to));
   --self:ASSERT(772, self:COLOUR_IS_OK(colour));

   -- piece attacks

   ptr = 0;
   while(true) do

      from = board.piece[1+colour][1+ptr];

      if(from==self.SquareNone) then
        break;
      end

      piece = board.square[1+from];
      delta = to - from;

      if(self:PSEUDO_ATTACK(piece,delta)) then

         inc = self:DELTA_INC_ALL(delta);
         --self:ASSERT(773, inc~=self.IncNone);

         sq = from;
         while(true) do

            sq = sq + inc;
            if(sq == to) then  -- attack
               self:alist_add(alist,from,board);
               break;
            end

            if(board.square[1+sq] ~= self.Empty) then
               break;
            end

         end
      end

      ptr = ptr + 1;
   end

   -- pawn attacks

   inc = self.PawnMoveInc[1+colour];
   pawn = self.PawnMake[1+colour];

   from = to -(inc-1);
   if(board.square[1+from] == pawn) then
     self:alist_add(alist,from,board);
   end

   from = to -(inc+1);
   if(board.square[1+from] == pawn) then
     self:alist_add(alist,from,board);
   end

end

-- self:alists_hidden()

function self:alists_hidden( alists, board, from, to )  -- int

   local inc = 0;     -- int
   local sq = 0;      -- int
   local piece = 0;   -- int

   --self:ASSERT(775, board.sp~=nil);
   --self:ASSERT(776, self:SQUARE_IS_OK(from));
   --self:ASSERT(777, self:SQUARE_IS_OK(to));

   inc = self:DELTA_INC_LINE(to-from);

   if(inc ~= self.IncNone)  then  -- line

      sq = from;

      while(true) do
        sq = sq - inc;
        piece = board.square[1+sq];
        if( piece~= self.Empty) then
          break;
        end
      end

      if(self:SLIDER_ATTACK(piece,inc)) then

         --self:ASSERT(778, self:piece_is_ok(piece));
         --self:ASSERT(779, self:PIECE_IS_SLIDER(piece));

         self:alist_add(alists.alist[1+self:PIECE_COLOUR(piece)],sq,board);
      end
   end

end

-- self:alist_clear()

function self:alist_clear( alist )

   --self:ASSERT(780, alist.size~=nil);

   alist.size = 0;
   alist.square = {};

end


-- self:alist_add()

function self:alist_add( alist, square, board )  -- int

   local piece = 0;   -- int
   local size = 0;    -- int
   local pos = 0;     -- int

   --self:ASSERT(781, alist.size~=nil);
   --self:ASSERT(782, self:SQUARE_IS_OK(square));
   --self:ASSERT(783, board.sp~=nil);

   -- insert in MV order

   piece = board.square[1+square];

   alist.size = alist.size + 1; -- HACK
   size = alist.size;

   --self:ASSERT(784, size>0 and size<16);

   pos = size-1;
   while( pos > 0  and  piece > board.square[1+alist.square[1+pos-1]]) do    -- HACK
      --self:ASSERT(785, pos>0 and pos<size);
      alist.square[1+pos] = alist.square[1+pos-1];
      pos = pos - 1;
   end

   --self:ASSERT(786, pos>=0 and pos<size);
   alist.square[1+pos] = square;

end

-- self:alist_remove()

function self:alist_remove( alist, pos )  -- int

   local size = 0;  -- int
   local i = 0;     -- int

   --self:ASSERT(787, alist.size~=nil);
   --self:ASSERT(788, pos>=0 and pos<alist.size);

   size = alist.size;
   alist.size = alist.size - 1;     -- HACK

   --self:ASSERT(789, size>=1);

   --self:ASSERT(790, pos>=0 and pos<size);

   for i = pos, size-2, 1 do
      --self:ASSERT(791, i>=0 and i<size-1);
      alist.square[1+i] = alist.square[1+i+1];
   end

end

-- self:alist_pop()

function self:alist_pop( alist, board )  -- int

   local sq = 0;     -- int
   local size = 0;   -- int

   --self:ASSERT(792, alist.size~=nil);
   --self:ASSERT(793, board.sp~=nil);

   sq = self.SquareNone;

   size = alist.size;

   if(size ~= 0) then
      size = size - 1;
      --self:ASSERT(794, size>=0);
      sq = alist.square[1+size];
      alist.size = size;
   end

   return sq;

end

-- end of see.cpp



-- sort.cpp



-- functions

-- sort_init()

function self:sort_init1() -- void

   local i = 0;        -- int
   local height = 0;   -- int
   local pos = 0;      -- int

   -- killer

   for height = 0, self.HeightMax-1, 1 do
      self.Killer[1+height] = {};
      for i = 0, 1, 1 do
        self.Killer[1+height][1+i] = self.MoveNone;
      end
   end

   -- history

   for i = 0, self.HistorySize-1, 1 do
      self.History[1+i] = 0;
      self.HistHit[1+i] = 1;
      self.HistTot[1+i] = 1;
   end

   -- self.Code[]

   for pos = 0, self.CODE_SIZE-1, 1 do
     self.Code[1+pos] = self.GEN_ERROR;
   end

   pos = 0;

   -- main search

   self.PosLegalEvasion = pos;
   self.Code[1+0] = self.GEN_LEGAL_EVASION;
   self.Code[1+1] = self.GEN_END;

   self.PosSEE = 2;
   self.Code[1+2] = self.GEN_TRANS;
   self.Code[1+3] = self.GEN_GOOD_CAPTURE;
   self.Code[1+4] = self.GEN_KILLER;
   self.Code[1+5] = self.GEN_QUIET;
   self.Code[1+6] = self.GEN_BAD_CAPTURE;
   self.Code[1+7] = self.GEN_END;

   -- quiescence search

   self.PosEvasionQS = 8;
   self.Code[1+8] = self.GEN_EVASION_QS;
   self.Code[1+9] = self.GEN_END;

   self.PosCheckQS = 10;
   self.Code[1+10] = self.GEN_CAPTURE_QS;
   self.Code[1+11] = self.GEN_CHECK_QS;
   self.Code[1+12] = self.GEN_END;

   self.PosCaptureQS = 13;
   self.Code[1+13] = self.GEN_CAPTURE_QS;
   self.Code[1+14] = self.GEN_END;

   pos = 15;

   --self:ASSERT(795, pos<self.CODE_SIZE);

end

-- sort_init()

function self:sort_init2( sort, board, attack, depth, height, trans_killer ) -- void
   --self:ASSERT(796, sort.depth~=nil);
   --self:ASSERT(797, board.sp~=nil);
   --self:ASSERT(798, attack~=nil);
   --self:ASSERT(799, self:depth_is_ok(depth));
   --self:ASSERT(800, self:height_is_ok(height));
   --self:ASSERT(801, trans_killer==self.MoveNone or self:move_is_ok(trans_killer));

   sort.board = board;
   sort.attack = attack;

   sort.depth = depth;
   sort.height = height;

   sort.trans_killer = trans_killer;
   sort.killer_1 = self.Killer[1+sort.height][1+0];
   sort.killer_2 = self.Killer[1+sort.height][1+1];

   if(self:ATTACK_IN_CHECK(sort.attack)) then

      self:gen_legal_evasions(sort.list,sort.board,sort.attack);
      self:note_moves(sort.list,sort.board,sort.height,sort.trans_killer);
      self:list_sort(sort.list);

      sort.gen = self.PosLegalEvasion + 1;
      sort.test = self.TEST_NONE;

   else  -- not in check

      sort.list.size = 0;
      sort.gen = self.PosSEE;

   end

   sort.pos = 0;
end

-- self:sort_next()

function self:sort_next( sort )  -- int

   local move = 0;   -- int
   local gen = 0;    -- int
   local nocont = false;
   local ifelse = false;

   --self:ASSERT(802, sort.pos~=nil);

   while(true) do

      while(sort.pos < sort.list.size) do

         nocont = true;

         -- next move

         move = sort.list.move[1+sort.pos];
         sort.value = 16384; -- default score
         sort.pos = sort.pos + 1;

         --self:ASSERT(803, move~=self.MoveNone);

         -- test

         ifelse = true;
         if(ifelse and(sort.test == self.TEST_NONE)) then
		    ifelse = false;
         end

         if(ifelse and(sort.test == self.TEST_TRANS_KILLER)) then

            if(nocont and not self:move_is_pseudo(move,sort.board)) then
              nocont = false;
            end
            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            ifelse = false;
         end

         if(ifelse and(sort.test == self.TEST_GOOD_CAPTURE)) then

            --self:ASSERT(804, self:move_is_tactical(move,sort.board));

            if(nocont and move == sort.trans_killer) then
              nocont = false;
            end

            if(nocont and not self:capture_is_good(move,sort.board)) then
              self:LIST_ADD(sort.bad,move);
              nocont = false;
            end

            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            ifelse = false;
         end

         if(ifelse and(sort.test == self.TEST_BAD_CAPTURE)) then

            --self:ASSERT(805, self:move_is_tactical(move,sort.board));
            --self:ASSERT(806, not self:capture_is_good(move,sort.board));

            --self:ASSERT(807, move~=sort.trans_killer);
            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            ifelse = false;
         end

         if(ifelse and(sort.test == self.TEST_KILLER)) then

            if(nocont and move == sort.trans_killer) then
              nocont = false;
            end
            if(nocont and not self:quiet_is_pseudo(move,sort.board)) then
              nocont = false;
            end
            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            --self:ASSERT(808,(not nocont) or(not self:move_is_tactical(move,sort.board)));

            ifelse = false;
         end

         if(ifelse and(sort.test == self.TEST_QUIET)) then

            --self:ASSERT(809, not self:move_is_tactical(move,sort.board));

            if(nocont and move == sort.trans_killer) then
              nocont = false;
            end
            if(nocont and move == sort.killer_1) then
              nocont = false;
            end
            if(nocont and move == sort.killer_2) then
              nocont = false;
            end
            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            if(nocont) then
              sort.value = self:history_prob(move,sort.board);
            end

            ifelse = false;
         end

         if(ifelse) then

            --self:ASSERT(810, false);

            return self.MoveNone;
         end

         if(nocont) then

           --self:ASSERT(811, self:pseudo_is_legal(move,sort.board));
           return move;

         end -- otherwise continue

      end

      -- next stage

      gen = self.Code[1+sort.gen];
      sort.gen = sort.gen + 1;

      ifelse = true;

      if(ifelse and(gen == self.GEN_TRANS)) then

         self:LIST_CLEAR(sort.list);
         if(sort.trans_killer ~= self.MoveNone) then
           self:LIST_ADD(sort.list,sort.trans_killer);
         end

         sort.test = self.TEST_TRANS_KILLER;

         ifelse = false;
      end

      if(ifelse and(gen == self.GEN_GOOD_CAPTURE)) then

         self:gen_captures(sort.list,sort.board);
         self:note_mvv_lva(sort.list,sort.board);
         self:list_sort(sort.list);

         self:LIST_CLEAR(sort.bad);

         sort.test = self.TEST_GOOD_CAPTURE;

         ifelse = false;
      end

      if(ifelse and(gen == self.GEN_BAD_CAPTURE)) then

         self:list_copy(sort.list,sort.bad);

         sort.test = self.TEST_BAD_CAPTURE;

         ifelse = false;
      end

      if(ifelse and(gen == self.GEN_KILLER)) then

         self:LIST_CLEAR(sort.list);
         if(sort.killer_1 ~= self.MoveNone) then
           self:LIST_ADD(sort.list,sort.killer_1);
         end
         if(sort.killer_2 ~= self.MoveNone) then
           self:LIST_ADD(sort.list,sort.killer_2);
         end

         sort.test = self.TEST_KILLER;

         ifelse = false;
      end

      if(ifelse and(gen == self.GEN_QUIET)) then

         self:gen_quiet_moves(sort.list,sort.board);
         self:note_quiet_moves(sort.list,sort.board);
         self:list_sort(sort.list);

         sort.test = self.TEST_QUIET;

         ifelse = false;
      end

      if(ifelse) then

         --self:ASSERT(812, gen==self.GEN_END);

         return self.MoveNone;
      end

      sort.pos = 0;

   end

   return self.MoveNone;
end

-- self:sort_init_qs()

function self:sort_init_qs( sort, board, attack, check )  -- bool

   --self:ASSERT(813, sort.pos~=nil);
   --self:ASSERT(814, board.sp~=nil);
   --self:ASSERT(815, attack~=nil);
   --self:ASSERT(816, check==true or check==false);

   sort.board = board;
   sort.attack = attack;

   if(self:ATTACK_IN_CHECK(sort.attack)) then
      sort.gen = self.PosEvasionQS;
   else
    if(check) then
      sort.gen = self.PosCheckQS;
    else
      sort.gen = self.PosCaptureQS;
    end
   end

   self:LIST_CLEAR(sort.list);
   sort.pos = 0;

end

-- self:sort_next_qs()

function self:sort_next_qs( sort )  -- int

   local move = 0;   -- int
   local gen = 0;    -- int
   local nocont = false;
   local ifelse = false;

   --self:ASSERT(817, sort.pos~=nil);

   while(true) do

      while(sort.pos < sort.list.size) do

         nocont = true;

         -- next move

         move = sort.list.move[1+sort.pos];
         sort.pos = sort.pos + 1;

         --self:ASSERT(818, move~=self.MoveNone);

         -- test

         ifelse = true;

         if(ifelse and(sort.test == self.TEST_LEGAL)) then

            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            ifelse = false;
         end

         if(ifelse and(sort.test == self.TEST_CAPTURE_QS)) then

            --self:ASSERT(819, self:move_is_tactical(move,sort.board));

            if(nocont and not self:capture_is_good(move,sort.board)) then
              nocont = false;
            end
            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            ifelse = false;
         end

         if(ifelse and(sort.test == self.TEST_CHECK_QS)) then

            --self:ASSERT(820, not self:move_is_tactical(move,sort.board));
            --self:ASSERT(821, self:move_is_check(move,sort.board));

            if(nocont and self:see_move(move,sort.board) < 0) then
              nocont = false;
            end
            if(nocont and not self:pseudo_is_legal(move,sort.board)) then
              nocont = false;
            end

            ifelse = false;
         end

         if(ifelse) then

            --self:ASSERT(822, false);
            return self.MoveNone;

         end

         if(nocont) then

           --self:ASSERT(823, self:pseudo_is_legal(move,sort.board));
           return move;

         end

      end

      -- next stage

      gen = self.Code[1+sort.gen];
      sort.gen = sort.gen + 1;

      ifelse = true;

      if(ifelse and(gen == self.GEN_EVASION_QS)) then

         self:gen_pseudo_evasions(sort.list,sort.board,sort.attack);
         self:note_moves_simple(sort.list,sort.board);
         self:list_sort(sort.list);

         sort.test = self.TEST_LEGAL;

         ifelse = false;
      end

      if(ifelse and(gen == self.GEN_CAPTURE_QS)) then

         self:gen_captures(sort.list,sort.board);
         self:note_mvv_lva(sort.list,sort.board);
         self:list_sort(sort.list);

         sort.test = self.TEST_CAPTURE_QS;

         ifelse = false;
      end

      if(ifelse and(gen == self.GEN_CHECK_QS)) then

         self:gen_quiet_checks(sort.list,sort.board);

         sort.test = self.TEST_CHECK_QS;

         ifelse = false;
      end

      if(ifelse) then

         --self:ASSERT(824, gen==self.GEN_END);

         return self.MoveNone;
      end

      sort.pos = 0;
   end

   --self:ASSERT(1824, false);
   return self.MoveNone;
end

-- self:good_move()

function self:good_move( move, board, depth, height )  -- int

   local index = 0;   -- int
   local i = 0;       -- int

   --self:ASSERT(825, self:move_is_ok(move));
   --self:ASSERT(826, board.sp~=nil);
   --self:ASSERT(827, self:depth_is_ok(depth));
   --self:ASSERT(828, self:height_is_ok(height));

   if(self:move_is_tactical(move,board)) then
     return;
   end

   -- killer

   if(self.Killer[1+height][1+0] ~= move) then
      self.Killer[1+height][1+1] = self.Killer[1+height][1+0];
      self.Killer[1+height][1+0] = move;
   end

   --self:ASSERT(829, self.Killer[1+height][1+0]==move);
   --self:ASSERT(830, self.Killer[1+height][1+1]~=move);

   -- history

   index = self:history_index(move,board);

   self.History[1+index] = self.History[1+index] +( depth * depth );          -- HISTORY_INC()

   if(self.History[1+index] >= self.HistoryMax) then
      for i = 0, self.HistorySize-1, 1 do
         self.History[1+i] =(self.History[1+i] + 1) / 2;
      end
   end

end

-- self:history_good()

function self:history_good( move, board )  -- void

   local index = 0;   -- int

   --self:ASSERT(831, self:move_is_ok(move));
   --self:ASSERT(832, board.sp~=nil);

   if(self:move_is_tactical(move,board)) then
     return;
   end

   -- history

   index = self:history_index(move,board);

   self.HistHit[1+index] = self.HistHit[1+index] + 1;
   self.HistTot[1+index] = self.HistTot[1+index] + 1;

   if(self.HistTot[1+index] >= self.HistoryMax) then
      self.HistHit[1+index] =(self.HistHit[1+index] + 1) / 2;
      self.HistTot[1+index] =(self.HistTot[1+index] + 1) / 2;
   end

   --self:ASSERT(833, self.HistHit[1+index]<=self.HistTot[1+index]);
   --self:ASSERT(834, self.HistTot[1+index]<self.HistoryMax);
end

-- self:history_bad()

function self:history_bad( move, board )  -- void

   local index = 0;   -- int

   --self:ASSERT(835, self:move_is_ok(move));
   --self:ASSERT(836, board.sp~=nil);

   if(self:move_is_tactical(move,board)) then
     return;
   end

   -- history

   index = self:history_index(move,board);

   self.HistTot[1+index] = self.HistTot[1+index] + 1;

   if(self.HistTot[1+index] >= self.HistoryMax) then
      self.HistHit[1+index] =(self.HistHit[1+index] + 1) / 2;
      self.HistTot[1+index] =(self.HistTot[1+index] + 1) / 2;
   end

   --self:ASSERT(837, self.HistHit[1+index]<=self.HistTot[1+index]);
   --self:ASSERT(838, self.HistTot[1+index]<self.HistoryMax);

end

-- self:note_moves()

function self:note_moves( list, board, height,  trans_killer )  -- void

   local size = 0;   -- int
   local i = 0;      -- int
   local move = 0;   -- int

   --self:ASSERT(839, self:list_is_ok(list));
   --self:ASSERT(840, board.sp~=nil);
   --self:ASSERT(841, self:height_is_ok(height));
   --self:ASSERT(842, trans_killer==self.MoveNone or self:move_is_ok(trans_killer));

   size = list.size;

   if(size >= 2) then
      for i = 0, size-1, 1 do
         move = list.move[1+i];
         list.value[1+i] = self:move_value(move,board,height,trans_killer);
      end
   end

end

-- self:note_captures()

function self:note_captures( list, board ) -- void

   local size = 0;   -- int
   local i = 0;      -- int
   local move = 0;   -- int

   --self:ASSERT(843, self:list_is_ok(list));
   --self:ASSERT(844, board.sp~=nil);

   size = list.size;

   if(size >= 2) then
      for i = 0, size-1, 1 do
         move = list.move[1+i];
         list.value[1+i] = self:capture_value(move,board);
      end
   end

end

-- self:note_quiet_moves()

function self:note_quiet_moves( list, board ) -- void

   local size = 0;   -- int
   local i = 0;      -- int
   local move = 0;   -- int

   --self:ASSERT(845, self:list_is_ok(list));
   --self:ASSERT(846, board.sp~=nil);

   size = list.size;

   if(size >= 2) then
      for i = 0, size-1, 1 do
         move = list.move[1+i];
         list.value[1+i] = self:quiet_move_value(move,board);
      end
   end

end

-- self:note_moves_simple()

function self:note_moves_simple( list, board ) -- void

   local size = 0;   -- int
   local i = 0;      -- int
   local move = 0;   -- int

   --self:ASSERT(847, self:list_is_ok(list));
   --self:ASSERT(848, board.sp~=nil);

   size = list.size;

   if(size >= 2) then
      for i = 0, size-1, 1 do
         move = list.move[1+i];
         list.value[1+i] = self:move_value_simple(move,board);
      end
   end

end

-- self:note_mvv_lva()

function self:note_mvv_lva( list, board ) -- void

   local size = 0;   -- int
   local i = 0;      -- int
   local move = 0;   -- int

   --self:ASSERT(849, self:list_is_ok(list));
   --self:ASSERT(850, board.sp~=nil);

   size = list.size;

   if(size >= 2) then
      for i = 0, size-1, 1 do
         move = list.move[1+i];
         list.value[1+i] = self:mvv_lva(move,board);
      end
   end

end

-- self:move_value()

function self:move_value( move, board, height, trans_killer )  -- int

   local value = 0;   -- int

   --self:ASSERT(851, self:move_is_ok(move));
   --self:ASSERT(852, board.sp~=nil);
   --self:ASSERT(853, self:height_is_ok(height));
   --self:ASSERT(854, trans_killer==self.MoveNone or self:move_is_ok(trans_killer));

   if(move == trans_killer) then    -- transposition table killer
      value = self.TransScore;
   else
    if(self:move_is_tactical(move,board)) then   -- capture or promote
      value = self:capture_value(move,board);
    else
     if(move == self.Killer[1+height][1+0]) then   -- killer 1
       value = self.KillerScore;
     else
      if(move == self.Killer[1+height][1+1]) then  -- killer 2
       value = self.KillerScore - 1;
      else      -- quiet move
       value = self:quiet_move_value(move,board);
      end
     end
    end
   end

   return value;

end

-- self:capture_value()

function self:capture_value( move, board )  -- int

   local value = 0;   -- int

   --self:ASSERT(855, self:move_is_ok(move));
   --self:ASSERT(856, board.sp~=nil);

   --self:ASSERT(857, self:move_is_tactical(move,board));

   value = self:mvv_lva(move,board);

   if(self:capture_is_good(move,board)) then
      value = value + self.GoodScore;
   else
      value = value + self.BadScore;
   end

   --self:ASSERT(858, value>=-30000 and value<=30000);

   return value;

end

-- self:quiet_move_value()

function self:quiet_move_value( move, board )  -- int

   local value = 0;   -- int
   local index = 0;   -- int

   --self:ASSERT(859, self:move_is_ok(move));
   --self:ASSERT(860, board.sp~=nil);

   --self:ASSERT(861, not self:move_is_tactical(move,board));

   index = self:history_index(move,board);

   value = self.HistoryScore + self.History[1+index];
   --self:ASSERT(862, value>=self.HistoryScore and value<=self.KillerScore-4);

   return value;

end

-- self:move_value_simple()

function self:move_value_simple( move, board )  -- int

   local value = 0;   -- int

   --self:ASSERT(863, self:move_is_ok(move));
   --self:ASSERT(864, board.sp~=nil);

   value = self.HistoryScore;
   if(self:move_is_tactical(move,board)) then
     value = self:mvv_lva(move,board);
   end

   return value;

end

-- self:history_prob()

function self:history_prob( move, board )  -- int

   local value = 0;   -- int
   local index = 0;   -- int

   --self:ASSERT(865, self:move_is_ok(move));
   --self:ASSERT(866, board.sp~=nil);

   --self:ASSERT(867, not self:move_is_tactical(move,board));

   index = self:history_index(move,board);

   --self:ASSERT(868, self.HistHit[1+index]<=self.HistTot[1+index]);
   --self:ASSERT(869, self.HistTot[1+index]<self.HistoryMax);

   value =(self.HistHit[1+index] * 16384) / self.HistTot[1+index];
   --self:ASSERT(870, value>=0 and value<=16384);

   return value;

end

-- self:capture_is_good()

function self:capture_is_good( move, board )  -- bool

   local piece = 0;     -- int
   local capture = 0;   -- int

   --self:ASSERT(871, self:move_is_ok(move));
   --self:ASSERT(872, board.sp~=nil);

   --self:ASSERT(873, self:move_is_tactical(move,board));

   -- special cases

   if(self:MOVE_IS_EN_PASSANT(move)) then
     return true;
   end
   if(self:move_is_under_promote(move)) then
     return false; -- REMOVE ME?
   end

   -- captures and queen promotes

   capture = board.square[1+self:MOVE_TO(move)];

   if(capture ~= self.Empty) then

      -- capture

      --self:ASSERT(874, self:move_is_capture(move,board));

      if(self:MOVE_IS_PROMOTE(move)) then
        return true; -- promote-capture
      end

      piece = board.square[1+self:MOVE_FROM(move)];
      if(self.ValuePiece[1+capture] >= self.ValuePiece[1+piece]) then
        return true;
      end
   end

   return(self:see_move(move,board) >= 0);

end

-- self:mvv_lva()

function self:mvv_lva( move, board )  -- int

   local piece = 0;     -- int
   local capture = 0;   -- int
   local promote = 0;   -- int
   local value = 0;     -- int

   --self:ASSERT(875, self:move_is_ok(move));
   --self:ASSERT(876, board.sp~=nil);

   --self:ASSERT(877, self:move_is_tactical(move,board));

   if(self:MOVE_IS_EN_PASSANT(move)) then   -- en-passant capture

      value = 5; -- PxP

   else

    capture = board.square[1+self:MOVE_TO(move)];

    if(capture~= self.Empty) then   -- normal capture

      piece = board.square[1+self:MOVE_FROM(move)];

      value =(self.PieceOrder[1+capture] * 6) - self.PieceOrder[1+piece] + 5;
      --self:ASSERT(878, value>=0 and value<30);

    else   -- promote

      --self:ASSERT(879, self:MOVE_IS_PROMOTE(move));

      promote = self:move_promote(move);

      value = self.PieceOrder[1+promote] - 5;
      --self:ASSERT(880, value>=-4 and value<0);
    end
   end

   --self:ASSERT(881, value>=-4 and value<30);

   return value;

end

-- self:history_index()

function self:history_index( move, board )  -- int

   local index = 0;   -- int

   --self:ASSERT(882, self:move_is_ok(move));
   --self:ASSERT(883, board.sp~=nil);

   --self:ASSERT(884, not self:move_is_tactical(move,board));

   index =(self.PieceTo12[1+board.square[1+self:MOVE_FROM(move)]] * 64) + self.SquareTo64[1+self:MOVE_TO(move)];

   --self:ASSERT(885, index>=0 and index<self.HistorySize);

   return index;

end

-- end of sort.cpp


-- square.cpp

-- functions

-- self:square_init()

function self:square_init() -- void

   local sq = 0;   -- int

   -- self.SquareTo64[]

   for sq = 0, self.SquareNb-1, 1 do
     self.SquareTo64[1+sq] = -1;
   end

   for sq = 0, 63, 1 do
      self.SquareTo64[1+self.SquareFrom64[1+sq]] = sq;
   end

   -- self.SquareIsPromote[]

   for sq = 0, self.SquareNb-1, 1 do
      self.SquareIsPromote[1+sq] = self:SQUARE_IS_OK(sq)  and(self:SQUARE_RANK(sq) == self.Rank1  or  self:SQUARE_RANK(sq) == self.Rank8);
   end

end

-- self:file_from_char()

function self:file_from_char(c)  -- int

   --self:ASSERT(886, c>="a" and c<="h");

   return self.FileA +(string.byte(c,1) - string.byte("a",1));
end

-- self:rank_from_char()

function self:rank_from_char(c)  -- int

   --self:ASSERT(887, c>="1" and c<="8");

   return self.Rank1 +(string.byte(c,1) - string.byte("1",1));
end

-- self:file_to_char()

function self:file_to_char( file )  -- char

   --self:ASSERT(888, file>=self.FileA and file<=self.FileH);

   return string.format("%c", string.byte("a",1) +(file - self.FileA));
end

-- self:rank_to_char()

function self:rank_to_char( rank )  -- int

   --self:ASSERT(889, rank>=self.Rank1 and rank<=self.Rank8);

   return string.format("%c", string.byte("1",1) +(rank - self.Rank1));

end

-- self:square_to_string()

function self:square_to_string( square, str1 )  -- bool

   --self:ASSERT(890, self:SQUARE_IS_OK(square));
   --self:ASSERT(891, str1.v~=nil);

   str1.v = "";
   str1.v = str1.v .. self:file_to_char(self:SQUARE_FILE(square));
   str1.v = str1.v .. self:rank_to_char(self:SQUARE_RANK(square));

   return true;
end

-- self:square_from_string()

function self:square_from_string( str1 )  -- int

   local file = 0;   -- int
   local rank = 0;   -- int
   local c1 = " ";   -- char
   local c2 = " ";   -- char

   --self:ASSERT(893, str1.v~=nil);

   c1 = string.sub( str1.v, 1, 1 );
   if(c1 < "a"  or  c1 > "h") then
     return self.SquareNone;
   end
   c2 = string.sub( str1.v, 2, 2 );
   if(c2 < "1"  or  c2 > "8") then
     return self.SquareNone;
   end

   file = self:file_from_char(c1);
   rank = self:rank_from_char(c2);

   return self:SQUARE_MAKE(file,rank);
end

-- end of square.cpp


-- trans.cpp

-- functions

-- self:trans_is_ok()

function self:trans_is_ok( trans )  -- bool

   local date = 0;   -- int

   if((trans.table == nil) or(trans.size == 0)) then
     return false;
   end

   if((trans.mask == 0)  or(trans.mask >= trans.size)) then
     return false;
   end

   if(trans.date >= self.DateSize) then
     return false;
   end

   for date = 0, self.DateSize-1, 1 do
      if(trans.age[1+date] ~= self:trans_age(trans,date)) then
        return false;
      end
   end

   return true;

end


-- self:trans_alloc()

function self:trans_alloc( trans )

   trans.size = self.TransSize;
   trans.mask = trans.size - 1;   -- 2^x -1

   self:trans_clear(trans);

   --self:ASSERT(900, self:trans_is_ok(trans));
end


-- self:trans_clear()

function self:trans_clear( trans ) -- void

   local clear_entry = nil;     -- entry_t *

   local index = 0;                   -- uint32

   --self:ASSERT(902, trans.size~=nil);

   self:trans_set_date(trans,0);
   trans.table = {};            -- will define objects while searching

end


-- self:trans_cl_I()

function self:trans_cl_I( trans, index ) -- void

   local clear_entry = nil;     -- entry_t *

   trans.table[1+index] = self:entry_t();

   clear_entry = trans.table[1+index];

   clear_entry.lock = 0;
   clear_entry.move = self.MoveNone;
   clear_entry.depth = self.DepthNone;
   clear_entry.date = trans.date;
   clear_entry.move_depth = self.DepthNone;
   clear_entry.flags = 0;
   clear_entry.min_depth = self.DepthNone;
   clear_entry.max_depth = self.DepthNone;
   clear_entry.min_value = -self.ValueInf;
   clear_entry.max_value = self.ValueInf;

   --self:ASSERT(903, self:entry_is_ok(clear_entry));

end


-- self:trans_inc_date()

function self:trans_inc_date( trans )  -- void

   --self:ASSERT(904, trans.size~=nil);

   self:trans_set_date(trans,(trans.date+1)%self.DateSize);
end

-- self:trans_set_date()

function self:trans_set_date( trans, date )  -- void

   local date1 = 0;

   --self:ASSERT(905, trans.size~=nil);
   --self:ASSERT(906, date>=0 and date<self.DateSize);

   trans.date = date;

   for date1 = 0, self.DateSize-1, 1 do
      trans.age[1+date1] = self:trans_age(trans,date1);
   end

   trans.used = 0;
   trans.read_nb = 0;
   trans.read_hit = 0;
   trans.write_nb = 0;
   trans.write_hit = 0;
   trans.write_collision = 0;

end

-- self:trans_age()

function self:trans_age( trans, date )  -- int

   local age = 0;   -- int

   --self:ASSERT(907, trans.size~=nil);
   --self:ASSERT(908, date>=0 and date<self.DateSize);

   age = trans.date - date;
   if(age < 0) then
     age = age + self.DateSize;
   end

   --self:ASSERT(909, age>=0 and age<self.DateSize);

   return age;

end

-- self:trans_store()

function self:trans_store( trans, key, move, depth, Tset )  -- void

   local entry = nil;        -- entry_t *
   local best_entry = nil;   -- entry_t *
   local ei = 0;             -- int
   local i = 0;              -- int
   local score = 0;          -- int
   local best_score = 0;     -- int
   local nw_rc = false;

   --self:ASSERT(910, self:trans_is_ok(trans));
   --self:ASSERT(911, move>=0 and move<65536);
   --self:ASSERT(912, depth>=-127 and depth<=127);
   --self:ASSERT(913, Tset.trans_min_value>=-self.ValueInf and Tset.trans_min_value<=self.ValueInf);
   --self:ASSERT(914, Tset.trans_max_value>=-self.ValueInf and Tset.trans_max_value<=self.ValueInf);
   --self:ASSERT(915, Tset.trans_min_value<=Tset.trans_max_value);

   -- init

   trans.write_nb = trans.write_nb + 1;

   -- probe

   best_entry = nil;
   best_score = -32767;

   ei = self:trans_entry(trans,key);

   for i = 0, self.ClusterSize-1 , 1 do

      entry = trans.table[1+ei+i];

      if(entry ~= nil and entry.lock ~= nil) then

       if(entry.lock == self:KEY_LOCK(key)) then

         -- hash hit => update existing entry

         trans.write_hit = trans.write_hit + 1;
         if(entry.date ~= trans.date) then
           trans.used = trans.used + 1;
         end

         entry.date = trans.date;

         if(depth > entry.depth) then
           entry.depth = depth; -- for replacement scheme
         end

         if(move ~= self.MoveNone  and  depth >= entry.move_depth) then
            entry.move_depth = depth;
            entry.move = move;
         end

         if(Tset.trans_min_value > -self.ValueInf  and  depth >= entry.min_depth) then
            entry.min_depth = depth;
            entry.min_value = Tset.trans_min_value;
         end

         if(Tset.trans_max_value < self.ValueInf  and  depth >= entry.max_depth) then
            entry.max_depth = depth;
            entry.max_value = Tset.trans_max_value;
         end

         --self:ASSERT(916, self:entry_is_ok(entry));

         return;
       end

      else

        self:trans_cl_I( trans, ei+i );   -- create a new entry record
        nw_rc = true;

        entry = trans.table[1+ei+i];

      end

      -- evaluate replacement score

      score =(trans.age[1+entry.date] * 256) - entry.depth;
      --self:ASSERT(917, score>-32767);

      if(score > best_score) then
         best_entry = entry;
         best_score = score;
      end

      if(nw_rc) then
        break;
      end

   end

   -- "best" entry found

   entry = best_entry;
   --self:ASSERT(918, entry.lock~=nil);
   --self:ASSERT(919, entry.lock~=self:KEY_LOCK(key));

   if(entry.lock ~= 0) then     -- originally entry.date == trans.date
      trans.write_collision = trans.write_collision + 1;
   else
      trans.used = trans.used + 1;
   end

   -- store

   entry.lock = self:KEY_LOCK(key);
   entry.date = trans.date;

   entry.depth = depth;

   entry.move_depth = self:iif( move ~= self.MoveNone, depth, self.DepthNone );
   entry.move = move;

   entry.min_depth = self:iif(Tset.trans_min_value > -self.ValueInf, depth, self.DepthNone );
   entry.max_depth = self:iif(Tset.trans_max_value < self.ValueInf, depth, self.DepthNone );
   entry.min_value = Tset.trans_min_value;
   entry.max_value = Tset.trans_max_value;

   --self:ASSERT(921, self:entry_is_ok(entry));

end

-- self:trans_retrieve()

function self:trans_retrieve( trans, key, Ret )  -- bool

   local entry = nil;   -- entry_t *
   local ei = 0;        -- int
   local i = 0;         -- int

   --self:ASSERT(922, self:trans_is_ok(trans));

   -- init

   trans.read_nb = trans.read_nb + 1;

   -- probe

   ei = self:trans_entry(trans,key);

   for i = 0, self.ClusterSize-1 , 1 do

      entry = trans.table[1+ei+i];
	  
	  if(entry ~= nil and entry.lock ~= nil) then

       if(entry.lock == self:KEY_LOCK(key)) then

         -- found

         trans.read_hit = trans.read_hit + 1;
         if(entry.date ~= trans.date) then
           entry.date = trans.date;
         end

         Ret.trans_move = entry.move;

         Ret.trans_min_depth = entry.min_depth;
         Ret.trans_max_depth = entry.max_depth;
         Ret.trans_min_value = entry.min_value;
         Ret.trans_max_value = entry.max_value;

         return true;
	   end
	   
	  else
         return false;
      end
   end

   -- not found

   return false;
end

-- self:trans_stats()

function self:trans_stats( trans ) -- void

   local full = 0.0;       -- double
   local hit = 0.0;        -- double
   local collision = 0.0;  -- double
   local s = "";

   --self:ASSERT(928, self:trans_is_ok(trans));

   full = self:iif(trans.size>0, trans.used / trans.size, 0);
   hit = self:iif(trans.read_nb>0, trans.read_hit / trans.read_nb, 0);
   collision = self:iif(trans.write_nb>0, trans.write_collision / trans.write_nb, 0);

   s = s .. "\n" .. "hash trans info";
   s = s .." hashfull " .. self:formatd(full*100.0) .. "%";
   s = s .." hits " .. self:formatd(hit*100.0) .. "%";
   s = s .." collisions " .. self:formatd(collision*100.0) .. "%";

   full = self:iif(self.Material.size>0, self.Material.used / self.Material.size, 0);
   hit = self:iif(self.Material.read_nb>0, self.Material.read_hit / self.Material.read_nb, 0);
   collision = self:iif(self.Material.write_nb>0, self.Material.write_collision / self.Material.write_nb, 0);

   s = s .. "\n" .. "hash material info";
   s = s .." hashfull " .. self:formatd(full*100.0) .. "%";
   s = s .." hits " .. self:formatd(hit*100.0) .. "%";
   s = s .." collisions " .. self:formatd(collision*100.0) .. "%";

   full = self:iif(self.Pawn.size>0, self.Pawn.used / self.Pawn.size, 0);
   hit = self:iif(self.Pawn.read_nb>0, self.Pawn.read_hit /self.Pawn.read_nb, 0);
   collision = self:iif(self.Pawn.write_nb>0, self.Pawn.write_collision / self.Pawn.write_nb, 0);

   s = s .. "\n" .. "hash pawn info";
   s = s .." hashfull " .. self:formatd(full*100.0) .. "%";
   s = s .." hits " .. self:formatd(hit*100.0) .. "%";
   s = s .." collisions " .. self:formatd(collision*100.0) .. "%";
   s = s .. "\n";

   self:send( s );
end

-- self:trans_entry()

function self:trans_entry( trans, key ) -- int - index to entry_t

   local index = 0;  -- uint32

   --self:ASSERT(929, self:trans_is_ok(trans));

   if(self.UseModulo) then
      index = self:KEY_INDEX(key) %(trans.mask + 1);
   else
      index =  bit.band( self:KEY_INDEX(key) , trans.mask);
   end

   --self:ASSERT(930, index<=trans.mask);

   return index;

end

-- self:entry_is_ok()

function self:entry_is_ok( entry )  -- bool

   if(entry.date ~= nil and entry.date >= self.DateSize) then
     return false;
   end

   if(entry.move == self.MoveNone  and  entry.move_depth ~= self.DepthNone) then
     return false;
   end
   if(entry.move ~= self.MoveNone  and  entry.move_depth == self.DepthNone) then
     return false;
   end

   if(entry.min_value == -self.ValueInf  and  entry.min_depth ~= self.DepthNone) then
     return false;
   end
   if(entry.min_value >  -self.ValueInf  and  entry.min_depth == self.DepthNone) then
     return false;
   end

   if(entry.max_value == self.ValueInf  and  entry.max_depth ~= self.DepthNone) then
     return false;
   end
   if(entry.max_value <  self.ValueInf  and  entry.max_depth == self.DepthNone) then
     return false;
   end

   return true;
end

-- end of trans.cpp



-- util.cpp

-- self:my_timer_reset()

function self:my_timer_reset( timer )

   --self:ASSERT(944, timer.start_real~=nil);

   timer.start_real = 0.0;
   timer.elapsed_real = 0.0;
   timer.running = false;

end

-- self:my_timer_start()

function self:my_timer_start( timer )

   --self:ASSERT(945, timer.start_real~=nil);

   --self:ASSERT(946, timer.start_real==0.0);
   --self:ASSERT(948, not timer.running);

   timer.running = true;
   timer.start_real = self:clock();

end

-- self:my_timer_stop()

function self:my_timer_stop( timer )

   --self:ASSERT(949, timer.start_real~=nil);

   --self:ASSERT(950, timer.running);

   timer.elapsed_real = timer.elapsed_real + self:clock() - timer.start_real;
   timer.start_real = 0.0;
   timer.running = false;

end

-- self:my_timer_elapsed_real()

function self:my_timer_elapsed_real( timer ) -- int

   --self:ASSERT(951, timer.start_real~=nil);

   if(timer.running) then
     timer.elapsed_real =(self:clock() - timer.start_real);
   end

   return timer.elapsed_real;
end


-- end of util.cpp



-- value.cpp

-- functions

-- self:value_init()

function self:value_init()

   local piece = 0;   -- int

   -- self.ValuePiece[]

   for piece = 0, 1, 1 do
    self.ValuePiece[1+piece] = -1;
   end

   self.ValuePiece[1+self.Empty] = 0; -- needed?
   self.ValuePiece[1+self.Edge]  = 0; -- needed?

   self.ValuePiece[1+self.WP] = self.ValuePawn;
   self.ValuePiece[1+self.WN] = self.ValueKnight;
   self.ValuePiece[1+self.WB] = self.ValueBishop;
   self.ValuePiece[1+self.WR] = self.ValueRook;
   self.ValuePiece[1+self.WQ] = self.ValueQueen;
   self.ValuePiece[1+self.WK] = self.ValueKing;

   self.ValuePiece[1+self.BP] = self.ValuePawn;
   self.ValuePiece[1+self.BN] = self.ValueKnight;
   self.ValuePiece[1+self.BB] = self.ValueBishop;
   self.ValuePiece[1+self.BR] = self.ValueRook;
   self.ValuePiece[1+self.BQ] = self.ValueQueen;
   self.ValuePiece[1+self.BK] = self.ValueKing;
end

-- self:value_is_ok()

function self:value_is_ok( value )  -- bool

   if(value < -self.ValueInf  or  value > self.ValueInf) then
     return false;
   end

   return true;
end

-- self:range_is_ok()

function self:range_is_ok( min, max )  -- bool

   if(not self:value_is_ok(min)) then
     return false;
   end
   if(not self:value_is_ok(max)) then
     return false;
   end

   if(min >= max) then
     return false; -- alpha-beta-like ranges cannot be nil
   end

   return true;
end

-- self:value_is_mate()

function self:value_is_mate( value )  -- bool

   --self:ASSERT(954, self:value_is_ok(value));

   if(value < -self.ValueEvalInf  or  value > self.ValueEvalInf) then
     return true;
   end

   return false;
end

-- self:value_to_trans()

function self:value_to_trans( value, height )  -- int

   --self:ASSERT(955, self:value_is_ok(value));
   --self:ASSERT(956, self:height_is_ok(height));

   if(value < -self.ValueEvalInf) then
      value = value - height;
   else
    if(value > self.ValueEvalInf) then
      value = value + height;
    end
   end

   --self:ASSERT(957, self:value_is_ok(value));

   return value;

end

-- self:value_from_trans()

function self:value_from_trans( value, height )  -- int

   --self:ASSERT(958, self:value_is_ok(value));
   --self:ASSERT(959, self:height_is_ok(height));

   if(value < -self.ValueEvalInf) then
      value = value + height;
   else
    if(value > self.ValueEvalInf) then
      value = value - height;
    end
   end

   --self:ASSERT(960, self:value_is_ok(value));

   return value;

end

-- self:value_to_mate()

function self:value_to_mate( value )  -- int

   local dist = 0;   -- int

   --self:ASSERT(961, self:value_is_ok(value));

   if(value < -self.ValueEvalInf) then

      dist =(self.ValueMate + value) / 2;
      --self:ASSERT(962, dist>0);

      return -dist;

   else
    if(value > self.ValueEvalInf) then

      dist =(self.ValueMate - value + 1) / 2;
      --self:ASSERT(963, dist>0);

      return dist;
    end
   end

   return 0;
end

-- end of value.cpp



-- vector.cpp

-- functions

function self:vector_init() -- void

   local delta = 0;   -- int
   local x = 0;       -- int
   local y = 0;       -- int
   local dist = 0;    -- int
   local tmp = 0;     -- int

   -- self.Distance[]

   for delta = 0, self.DeltaNb-1, 1 do
     self.Distance[1+delta] = -1;
   end

   for y = -7, 7, 1 do

      for x = -7, 7, 1 do

         delta = y * 16 + x;
         --self:ASSERT(964, self:delta_is_ok(delta));

         dist = 0;

         tmp = x;
         if(tmp < 0) then
           tmp = -tmp;
         end
         if(tmp > dist) then
           dist = tmp;
         end

         tmp = y;
         if(tmp < 0) then
           tmp = -tmp;
         end
         if(tmp > dist) then
           dist = tmp;
         end

         self.Distance[1+self.DeltaOffset+delta] = dist;
      end
   end

end


-- self:delta_is_ok()

function self:delta_is_ok( delta )  -- bool

   if(delta < -119  or  delta > 119) then
     return false;
   end

   if( bit.band(delta,0xF) == 8) then
     return false;     -- HACK: delta % 16 would be ill-defined for negative numbers
   end

   return true;
end


-- self:inc_is_ok()

function self:inc_is_ok( inc )  -- bool

   local dir = 0;   -- int

   for dir = 0, 7, 1 do
      if(self.KingInc[1+dir] == inc) then
        return true;
      end
   end

   return false;
end

-- end of vector.cpp


-- main.cpp

-- functions

-- self:main()

function self:main()

   -- init

   -- suppressed: self:log( self.VERSION );

   self:option_init();

   self:square_init();
   self:piece_init();
   self:pawn_init_bit();
   self:value_init();
   self:vector_init();
   self:attack_init();
   self:move_do_init();

   self:random_init();
   self:hash_init();

   self:inits();
   self:setstartpos();

end

-- end of main.cpp

function self:ClearAll()    -- just clear all to be sure that nothing left

   self:search_clear();
   self:trans_clear(self.Trans);
   self:pawn_clear();
   self:material_clear();

end


    return self
end


return M
end


-- randomized simplest opening case...
local function randomopening( mvlist )   -- bool

   local tm = "";   -- string
   local l = 0;     -- int
   local i = 0;  -- int
   local j = 0;  -- int
   local mv_l = string.len( mvlist );  -- int
   local fmv = {};  -- move 1
   local m = "";    -- string
   
   if(mv_l<6) then

     tm = string.format( "%s", engine:time() );
     l = string.len( tm );
     i = tonumber( string.sub( tm, l, l ) );

     if( mv_l == 0 ) then
       fmv = { "e2-e4", "d2-d4", "Ng1-f3", "Nb1-c3", "c2-c4", "g2-g3", "e2-e4", "c2-c3", "e2-e4", "d2-d4" };
     else
       fmv = { "e7-e5", "d7-d5", "Ng8-f6", "Nb8-c6", "c7-c5", "g7-g6", "c7-c5", "c7-c6", "e7-e6", "g7-g6" };
     end

     m = fmv[1+i];
     j = engine:iif( string.len( m )>5, 1, 0 );
     engine.bestmv = string.sub(m,1+j,2+j) .. string.sub(m,4+j,5+j);
     engine.bestmv2 = m;

     return true;
   end

   return false;

end

-- AI vs AI game for testing...
local function autogame()

  local pgn = "";
  local mc = 0;
  local mlist = "";

  self:log("Autogame!");

  engine:printboard();

  while(true) do

    if( not randomopening( mlist ) ) then
      engine:do_input( "go movetime 1");
      engine:log("nodes: " .. engine.SearchCurrent.node_nb);	-- to see performance
    end

    if(mc%2==0) then
      pgn = pgn .. engine:formatd(math.floor(mc/2)+1 )..".";
    end
    pgn = pgn .. engine.bestmv2 .." ";

    mlist = mlist .. " " .. engine.bestmv;

    engine:do_input( "position moves" .. mlist );
    engine:printboard();

    engine:log(pgn);

    if( engine:board_is_mate(  engine.SearchInput.board ) ) then
      engine:log("Checkmate! " .. engine:iif( engine.SearchInput.board.turn == engine.White, "0-1", "1-0" ));
      break;
    end
    if( engine:board_is_stalemate( engine.SearchInput.board ) ) then
      engine:log("Stalemate  1/2-1/2");
      break;
    end

    mc = mc + 1;
  end
end

-- .......................
--
-- here it starts...
--
-- .......................

--  engine:main();  -- initialize and set up starting position

--  self:do_input( "help" );
--  self:do_input( "position moves e2e4 e7e5 g1f3 g8f6 f1c4 f8c5 e1g1 e8g8" );
--  self:do_input( "position moves h2h3 a7a5 h3h4 a5a4 b2b4" );
--  self:do_input( "position moves b2b4 a7a5 a2a3 a5b4 c1b2 b4a3 b1c3 a3b2 h2h3 b2a1n h3h4 a1c2" );
--  self:do_input( "position moves b2b4 g7g6 b4b5 c7c5 b5c6" );
--  self:do_input( "position fen 7k/Q7/2P2K2/8/8/8/8/8 w - - 70 1" );
--  self:printboard();
--  self:do_input( "go");
--  self:do_input( "go depth 5");
--  self:do_input( "go movetime 5");


-- checkmate in 3 moves    1.Bf7+ Kxf7 2.Qxg6+ Ke7 3.Qe6#
--  ShowInfo = true;
--  self:do_input( "position fen r3kr2/pbq5/2pRB1p1/8/4QP2/2P3P1/PP6/2K5 w q - 0 36" );
--  self:printboard();
--  self:do_input( "go movetime 15");


--  autogame();


local Fruit21 = CreateEngine()

DeltaChess = DeltaChess or {}
DeltaChess.LibFruit21 = Fruit21

return Fruit21
