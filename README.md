# DeltaChess Engine Framework

A lightweight, asynchronous chess engine framework for Lua that provides a unified interface for multiple chess engines. The framework is designed to be **stateless**, **single-threaded**, and **World of Warcraft addon compatible**, making it perfect for embedded environments with limited resources.

This framework powers the [Delta Chess](https://github.com/Deltachaos/deltachess) World of Warcraft addon, bringing playable chess with adjustable AI difficulty directly into the game.

## Overview

Single-threaded, async interface for Lua chess engines. Engines are **stateless**; position and options are passed per calculation. Supports ELO-based difficulty (100-2600). Tests validate engines by self-play: each move is checked for legality.

## Requirements

- **Lua 5.1+** (tested with 5.1, 5.2, 5.3, 5.4)
- No external dependencies for core functionality
- Optional: `curl` for downloading test suites via scripts

The framework is designed to work in constrained environments, including the World of Warcraft Lua sandbox, which has no file I/O, limited standard library, and restricted execution time per frame.

## Installation

### Standalone Lua Project

```bash
# Clone the repository
git clone https://github.com/Deltachaos/deltachess-engine-framework.git
cd deltachess-engine-framework

# Run tests to verify installation
lua5.1 Libs/LibUnitWow/run.lua tests Libs/LibChessOpeningBook/tests

# Try a sample game
lua bin/play.lua sunfish 1200
```

### World of Warcraft Addon

1. Copy the entire framework into your addon directory
2. Include `DeltachessFramework.toc` in your addon list. The packager fetches LibStub.
3. The TOC loads LibStub, then `lib.xml`
4. Use `LibStub("DeltachessFramework-1.0")`

See the [Delta Chess addon](https://github.com/Deltachaos/deltachess) for a complete integration example.

### As a Lua Module

Load `DeltachessFramework.toc` (LibUnitWow does this outside WoW), then take the library from LibStub. Nothing is written into the global namespace.

```lua
local DeltaChess = LibStub("DeltachessFramework-1.0")
local Runner = DeltaChess.EngineRunner
Runner.Create("sunfish")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :Elo(1200)
    :OnComplete(function(result, err)
        if result then
            print("Best move:", result.move)
        end
    end)
    :Run()
```

## Architecture

The framework is built around three core components:

1. **Engine Interface** (`EngineInterface.lua`) - Registry and contract definition for chess engines
2. **Move Generator** (`MoveGen.lua`) - FEN parsing, legal move generation, position validation
3. **Engine Runner** (`EngineRunner.lua`) - Async execution with builder pattern API

Engines implement a simple interface and remain completely stateless. The runner handles all state management, time limits, cancellation, and move validation.

```
┌─────────────────┐
│  Your Code      │
└────────┬────────┘
         │ Builder Pattern API
         v
┌─────────────────┐      ┌──────────────┐
│  EngineRunner   │─────>│  MoveGen     │
│  (Orchestrator) │      │  (Validator) │
└────────┬────────┘      └──────────────┘
         │ Async Callbacks
         v
┌─────────────────┐
│  Chess Engine   │
│  (Sunfish, etc) │
└─────────────────┘
```

## Engine Interface

Engines register with **DeltaChess.Engines** and implement:

| Requirement | Description |
|-------------|-------------|
| `.id` | Unique identifier (string), e.g. `"beat_highest_piece"`. |
| `.name` | Display name (string). |
| `.description` | Optional short description. |
| `GetEloRange()` | Returns `{min, max}` (numbers). |
| `Calculate(state, loopFn, stepFn, onComplete)` | Search via `DeltaChess.Slice.Run`. `loopFn(step, done)` resumes time slices; call `onComplete(result, err)` when done. |

**State** (per calculation):

- `state.fen` (required) – current position in FEN.
- `state.moves` (optional) – list of moves that led to this position (UCI or SAN).
- `state.elo` – requested strength.
- `state.time_limit_ms` (optional) – max compute time in ms (idle time between slices is not counted).
- `state.cancelled` – set by `session:Cancel()`; engine should unwind and keep the best move so far.

**Return:** `(resultTable, err)`. On success, `resultTable.move` is UCI (e.g. `e2e4`, `e7e8q`).

**Optional:** `GetAverageCpuTime(elo)` for UI sorting.

## Registry API (DeltaChess.Engines)

- `Register(engine)` – register engine (must have `.id`, `.name`, `GetEloRange`, `Calculate`).
- `Get(id)` – get engine by id (or effective default if id is nil).
- `GetEngineList()` – list `{id, name, description, maxElo}` sorted by max ELO.
- `GetEloRange(engineId)` – returns `{min, max}` or nil.
- `GetGlobalEloRange()` – global min/max across all engines.
- `GetEnginesForElo(elo)` – engines supporting that ELO, sorted by efficiency.
- `SetDefaultId(id)` / `GetDefaultId()` / `GetEffectiveDefaultId()`.
- `Unregister(id)`.

## Async runner (builder pattern)

Search runs in a coroutine (`DeltaChess.Slice`). `Slice.Poll()` yields when the current resume's millisecond budget is spent. `time_limit_ms` is accumulated compute time across slices.

- **Builder:** `Runner.Create(engineId)` returns a builder. Chain state/options then `Run()` or `Start()`:
  - `:Fen(fen)` – position. Default: build from `:Moves()` (if set), else initial position.
  - `:Moves(moves)` – optional. Moves that led to this position (UCI). Used to build FEN if `:Fen()` not set.
  - `:Elo(elo)` – optional. Requested difficulty. Default: average of engine's ELO range.
  - `:TimeLimitMs(ms)` – optional. Max compute time in ms. Default: 20000 (20 seconds).
  - `:OnComplete(cb)` – `function(result, err)`. Required before `Run()`. If the engine returns an illegal move, the runner calls `onComplete(nil, err)` with `err` an object `{ message = "illegal move", move = "<uci>" }`.
  - `:DelayFn(fn)` – optional. `function(next)`. Default: call `next()` immediately.
  - `:LoopFn(fn)` – optional. `function(stepFn, doneFn)` loop driver. Each `step()` is one time slice. Default: runs all steps synchronously with an unlimited slice.
  - `:SliceMs(ms)` – optional. Milliseconds per `:LoopFn` step. Default: 8 when a LoopFn is set.
  - `:UseOpeningBook(on)` – probe `LibChessOpeningBook` before search. Default: on. `:UseOpeningBook(false)` searches from scratch. A hit completes immediately with `{ move, san, book, weight }`. A miss falls through to search. An unknown book id completes with an error. If the library is missing, books are skipped unless `:OpeningBooks()` was set.
  - `:OpeningBooks(idOrList)` – optional. One book id or a list of ids. Default: pick books whose ELO range matches `:Elo()`.
  - `:BookDepth(n)` – optional. Use the book only while the position ply is `< n` (start position is ply 0). Default: derived from `:Elo()` (1 ply at 100, 16 plies at 2600).
  - `:Run()` – start calculation and drive it with LoopFn (or one unlimited slice).
  - `:Start()` – prepare calculation and return a session. Call `session:Compute(ms)` each frame (returns `true` while thinking). `session:Cancel()` stops at the next poll. The finishing `Compute` calls `OnComplete`.

## Library registration

Modules attach themselves to the LibStub library `DeltachessFramework-1.0`. There is no global `DeltaChess` table.

- `DeltaChess.Engines` – registry and engine interface
- `DeltaChess.MoveGen` – FEN, legal moves, move validation
- `DeltaChess.Slice` – coroutine time slices (`Run`, `Poll`, `Stopped`, `AllowStop`)
- `DeltaChess.EngineRunner` – `Create(engineId)` (builder)
- `DeltaChess.OpeningBook` – `IsAvailable()`, `GetBookList()`, `GetBooksForElo(elo)`, `DepthForElo(elo)`, `SelectMove(fen, bookIds, depth)` (uses `LibChessOpeningBook-1.0` when loaded)

**World of Warcraft:** `DeltachessFramework.toc` loads LibStub, then `lib.xml`.

## Usage

```lua
local DeltaChess = LibStub("DeltachessFramework-1.0")
local Runner = DeltaChess.EngineRunner
Runner.Create("beat_highest_piece")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :Elo(500)
    :OnComplete(function(result, err)
        if not err then -- use result.move
        end
    end)
    :Run()
```

**Opening book (on by default):** `LibChessOpeningBook-1.0` ships `first_moves` (ELO 100–600), `open_games`, and `closed_games` (ELO 500–1800). With no `:OpeningBooks()`, the runner picks books by `:Elo()` and shortens book depth at lower ELO. Other addons call `LibStub("LibChessOpeningBook-1.0"):Register({...})` (`elo = { min, max }`) to add more. Convert a Polyglot `.bin` with `lua Libs/LibChessOpeningBook/bin/polyglot2lua.lua book.bin out.lua --id gm --name "GM" --min-elo 1800 --max-elo 2400`.

```lua
-- Auto-select book and depth from ELO
DeltaChess.EngineRunner.Create("sunfish")
    :Elo(800)
    :OnComplete(function(result, err)
        if result and result.book then
            -- result.move came from result.book
        end
    end)
    :Run()

-- Or name the book and cap depth yourself
DeltaChess.EngineRunner.Create("sunfish")
    :OpeningBooks("open_games")
    :BookDepth(4)
    :OnComplete(function(result, err) end)
    :Run()
```

**Per-frame Compute (e.g. WoW OnUpdate):**
```lua
local session = DeltaChess.EngineRunner.Create("sunfish")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :Elo(1200)
    :OnComplete(function(result, err)
        if err then return end
        -- use result.move
    end)
    :Start()

-- each frame:
if session and not session:Compute(8) then
    session = nil  -- OnComplete has already run
end
```

**LoopFn still owns pacing** (one 8 ms slice per step, then you decide when to call the next `step`):
```lua
DeltaChess.EngineRunner.Create("sunfish")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :SliceMs(8)
    :LoopFn(function(step, done)
        local ticker
        ticker = C_Timer.NewTicker(0, function()
            if step() == false then
                ticker:Cancel()
                done()
            end
        end)
    end)
    :OnComplete(function(result, err)
        if err then return end
        -- use result.move
    end)
    :Run()
```

**CLI game (bin/play.lua):** Run from repo root. Engine vs itself or engine1 vs engine2, with optional ELOs:
```bash
lua bin/play.lua <engine1> [elo1] [engine2] [elo2]
# Examples:
lua bin/play.lua beat_highest_piece
lua bin/play.lua beat_highest_piece 500
lua bin/play.lua beat_highest_piece 500 other_engine 600
```

**UCI interface (bin/uci.lua):** Provides a UCI (Universal Chess Interface) compatible interface to any engine. Specify the engine via command-line argument, the `ENGINE` environment variable, or interactively:
```bash
# Via command-line argument (preferred):
lua bin/uci.lua fruit21
# Or make it executable and run directly:
./bin/uci.lua sunfish

# Via environment variable:
ENGINE=fruit21 lua bin/uci.lua

# Interactively - select engine after starting:
lua bin/uci.lua
# Then use: engine <engine_id>
```

Supported UCI commands:
- `engine <engine_id>` – (custom) select engine interactively
- `uci` – identify as UCI engine
- `isready` – respond with readyok
- `ucinewgame` – reset state
- `position [fen <fenstring> | startpos] [moves <move1> ... <moveN>]` – set position
- `go [wtime <x>] [btime <x>] [movetime <x>] [depth <x>] [nodes <x>]` – start calculating
- `quit` – exit

Example session (with interactive engine selection):
```bash
$ lua bin/uci.lua
uci
id name DeltaChess UCI Interface
id author DeltaChess
info string No engine selected. Use: engine <engine_id>
uciok
engine fruit21
uci
id name Fruit 2.1 (DeltaChess)
id author Fabien Letouzey
uciok
position startpos moves e2e4
go movetime 1000
bestmove e7e5
quit
```

## Quick Start

```lua
local DeltaChess = LibStub("DeltachessFramework-1.0")

DeltaChess.EngineRunner.Create("sunfish")
    :Fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
    :Elo(1200)  -- Intermediate difficulty
    :TimeLimitMs(5000)  -- 5 seconds max
    :OnComplete(function(result, err)
        if err then
            print("Error:", err.message)
        else
            print("Best move:", result.move)  -- UCI format, e.g., "e7e5"
        end
    end)
    :Run()
```

## Features

- **Stateless Architecture:** Engines receive position and options per calculation, no internal state management
- **Async/Yield Support:** Coroutine time slices via `Start`/`Compute(ms)` or `:LoopFn`, so hosts can search a few milliseconds per frame
- **Multiple Engines:** Choose from 4 different chess engines with varying strengths (ELO 100-2600)
- **Builder Pattern API:** Fluent, chainable interface for configuring calculations
- **FEN & Move Support:** Full FEN parsing, legal move generation, and UCI move notation
- **UCI Interface:** Standard UCI protocol support for compatibility with chess GUIs
- **Self-Play Testing:** Automated validation ensures all engines produce only legal moves
- **Zero Dependencies:** Pure Lua implementation (except for optional WoW integration)
- **Embedded-Friendly:** Works in resource-constrained environments like WoW's Lua sandbox

## Available Engines

The framework includes four chess engines with different characteristics:

| Engine | License | Description |
|--------|---------|-------------|
| **Dumb Goblin** | GPL-3.0 | Simple engine: prefers checkmate, then highest-value capture, else first legal move. Deterministic. |
| **Sunfish** | GPL-3.0 | Minimalist MTD-bi search with iterative deepening and transposition tables. Strong tactical play. |
| **GarboChess** | BSD-3-Clause | Classic alpha-beta with bitboard move generation and positional evaluation. |
| **Fruit 2.1** | GPL-3.0 | Highly influential engine with null-move pruning, late-move reductions, history heuristics, and sophisticated evaluation. |

### Adding Custom Engines

Add new engines in `src/Engines/` (with `.id`, `.name`, `GetEloRange`, `Calculate`) and add the script to `lib.xml`. See the included engines for implementation examples.

## Testing

The framework includes comprehensive tests to ensure correctness and reliability:

### Test Suite

```bash
# Run all tests except the engine snapshot suite
TEST_IGNORE_SLUGS=engine_consistency_integration lua5.1 ../LibUnitWow/run.lua tests Libs/LibChessOpeningBook/tests

# Run one category
lua5.1 ../LibUnitWow/run.lua tests/unit
lua5.1 ../LibUnitWow/run.lua tests/integration
lua5.1 ../LibUnitWow/run.lua Libs/LibChessOpeningBook/tests
```

### Test Categories

1. **Unit Tests** (`tests/unit/`, `Libs/LibChessOpeningBook/tests/`)
   - Engine interface compliance
   - Move generation correctness
   - FEN parsing and validation
   - Runner state management
   - Opening-book hashing, lookup, and Polyglot conversion

2. **Integration Tests** (`tests/integration/`)
   - Engine self-play validation
   - Move legality verification
   - Time limit enforcement
   - Cancellation handling

3. **Engine Validation**
   - Each engine plays against itself
   - Runner validates every move for legality
   - Illegal moves are caught and reported
   - Ensures engines never return invalid moves

### Continuous Integration

GitHub Actions (`.github/workflows/ci.yml`) automatically runs all tests on:
- Push to `main`/`master` branches
- Pull requests
- Multiple Lua versions (5.1, 5.2, 5.3, 5.4)

### Benchmarking

```bash
# Benchmark all engines at various ELO levels
lua bin/benchmark.lua

# Run engine duels
lua bin/duel.lua sunfish garbochess

# Calculate ELO ratings
lua bin/rate.lua
```

## Contributing

Contributions are welcome! Here's how you can help:

1. **Add New Engines:** Implement the engine interface and submit a PR
2. **Improve Documentation:** Fix typos, add examples, clarify explanations
3. **Report Bugs:** Open issues with reproducible test cases
4. **Optimize Performance:** Profile and improve calculation speed
5. **Expand Test Coverage:** Add more test scenarios

### Engine Development Guidelines

When adding a new engine:

1. Implement the required interface (`.id`, `.name`, `GetEloRange()`, `Calculate()`)
2. Make it stateless - all state passed via `state` parameter
3. Run search through `DeltaChess.Slice.Run` and call `Slice.Poll()` in the node loop
4. Honor `Slice.Stopped()` / `state.cancelled`; keep the best move when stopping
5. Call `onComplete(result, err)` exactly once when done
6. Return moves in UCI format (e.g., `e2e4`, `e7e8q`)
7. Add comprehensive tests including self-play validation

See existing engines in `src/Engines/` for reference implementations.

## Layout

```
DeltachessFramework.toc  # Loads lib.xml
lib.xml                  # Libraries and engines, in load order
bin/
  play.lua         # CLI: engine vs itself or engine1 vs engine2 (optional ELOs)
  uci.lua          # UCI interface: ENGINE=<id> lua bin/uci.lua
  benchmark.lua    # Performance benchmarking
  duel.lua         # Engine vs engine matches
  rate.lua         # ELO rating calculations
src/
  EngineInterface.lua  # DeltaChess.Engines registry and interface contract
  EngineRunner.lua    # DeltaChess.EngineRunner (Start/Compute or Run/LoopFn)
  OpeningBook.lua     # DeltaChess.OpeningBook (opt-in LibChessOpeningBook lookup)
  Slice.lua           # DeltaChess.Slice (coroutine time slices)
  MoveGen.lua         # DeltaChess.MoveGen (legal move generation)
  Board.lua           # Board representation and utilities
  BoardMove.lua       # Move generation and validation
  Cache.lua           # Caching utilities
  Constants.lua       # Shared constants
  EngineDuel.lua      # Engine vs engine testing
  Util.lua            # Utility functions
  Engines/
    DumbGoblin.lua  # Simple capture-focused engine
    Sunfish.lua     # MTD-bi search engine
    GarboChess.lua  # Alpha-beta with bitboards
    Fruit21.lua     # Advanced alpha-beta engine
Libs/
  bit.lua            # Bitwise operations (Lua 5.1 compatibility)
  fruit21.lua        # Fruit 2.1 core implementation
  json.lua           # JSON utilities
  LibChessOpeningBook/  # LibChessOpeningBook-1.0 (Lua books, Polyglot hash, converter)
    tests/           # Library and Polyglot converter tests
tests/
  unit/              # Unit tests (*_test.lua), run with LibUnitWow/run.lua
  integration/       # Integration tests (*_test.lua)
.github/workflows/
  ci.yml             # Continuous integration
```

## License

This project is licensed under the **GNU General Public License v3.0** (GPL-3.0). See the [LICENSE](LICENSE) file for details.

### Engine Licenses

The chess engines included in the `src/Engines/` directory follow different licenses:

- **Dumb Goblin** (`DumbGoblin.lua`) - GPL-3.0 
  - Author: Deltachaos
  - Repository: https://github.com/Deltachaos/deltachess-engine-framework

- **Sunfish** (`Sunfish.lua`) - GPL-3.0
  - Original Author: Thomas Ahle
  - Lua Port: Soumith Chintala
  - Repository: https://github.com/soumith/sunfish.lua

- **GarboChess** (`GarboChess.lua`) - BSD-3-Clause
  - Author: Gary Linscott
  - Lua Port: Chessforeva
  - Repository: https://github.com/glinscott/Garbochess-JS

- **Fruit 2.1** (`Fruit21.lua`) - GPL-3.0
  - Original Author: Fabien Letouzey
  - Lua Port: Chessforeva
  - Repository: https://github.com/Chessforeva/Lua4chess

When using this framework, please ensure you comply with the license requirements of any engines you include. The GPL-3.0 license of the framework itself applies to the core infrastructure (move generation, engine interface, runner, etc.).

## Performance

### Performance Tips

- **Use appropriate ELO levels:** Higher ELO = longer calculation time
- **Set time limits:** Use `:TimeLimitMs()` to prevent excessive calculations
- **Slice per frame:** In constrained environments (like WoW), call `session:Compute(ms)` with a few milliseconds per frame, or set `:SliceMs()` with a `:LoopFn`
- **Warm up engines:** First calculation may be slower due to transposition table initialization

## Known Limitations

- **Single-threaded:** No parallel search across multiple cores
- **Opening books are on by default:** `EngineRunner` consults `LibChessOpeningBook` unless `:UseOpeningBook(false)` is set. A miss still searches from scratch. Lower ELO uses a shallower book. Other addons register extra books with `LibStub("LibChessOpeningBook-1.0"):Register(...)`.
- **No endgame tablebases:** Pure search-based play in all positions
- **Memory constraints:** Designed for embedded environments with limited RAM
- **No persistent learning:** Engines don't learn from previous games

These limitations are intentional design choices to keep the framework simple, portable, and suitable for embedded environments like World of Warcraft.

## Troubleshooting

### Engine returns illegal moves

This should never happen! If it does:
1. Check that you're using the latest version
2. Verify the FEN string is valid
3. Run the self-play tests: `lua5.1 ../LibUnitWow/run.lua tests/integration`
4. Report a bug with the FEN and engine ID

### Calculation takes too long

1. Lower the ELO level
2. Set a time limit with `:TimeLimitMs()`
3. Use a faster engine (Dumb Goblin < Sunfish < GarboChess < Fruit)
4. Check if the yield function is being called

### Memory usage too high

1. Reduce the number of concurrent calculations
2. Use simpler engines (avoid Fruit 2.1 for memory-constrained environments)
3. Clear transposition tables between games if needed

### WoW addon frame rate drops

1. Spend less time per frame with `session:Compute(ms)` or `:SliceMs(ms)`
2. Use lower ELO settings
3. Limit concurrent calculations to 1
4. Consider using Dumb Goblin or Sunfish instead of Fruit 2.1

## Used In

- **[Delta Chess](https://github.com/Deltachaos/deltachess)** - A World of Warcraft addon that brings fully playable chess into the game with AI opponents at various difficulty levels.

## Credits

### Framework

- **Author:** Deltachaos
- **Repository:** https://github.com/Deltachaos/deltachess-engine-framework

### Included Engines

- **Fruit 2.1:** Original by Fabien Letouzey, Lua port by Chessforeva
- **Sunfish:** Original by Thomas Ahle, Lua port by Soumith Chintala
- **GarboChess:** Original by Gary Linscott, Lua port by Chessforeva
- **Dumb Goblin:** Original implementation by Deltachaos

Special thanks to all contributors and the chess programming community!
