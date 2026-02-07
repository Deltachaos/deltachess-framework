# Delta Chess Engine

Single-threaded, async interface for Lua chess engines. Engines are **stateless**; position and options are passed per calculation. Supports ELO-based difficulty. Tests validate engines by self-play: each move is checked for legality.

## Interface

Engines register with **DeltaChess.Engines** and implement:

| Requirement | Description |
|-------------|-------------|
| `.id` | Unique identifier (string), e.g. `"beat_highest_piece"`. |
| `.name` | Display name (string). |
| `.description` | Optional short description. |
| `GetEloRange()` | Returns `{min, max}` (numbers). |
| `Calculate(state, yieldFn, onComplete)` | Async: call `yieldFn(next)` to yield (runner calls `next()` later); call `onComplete(result, err)` when done. |

**State** (per calculation):

- `state.fen` (required) – current position in FEN.
- `state.moves` (optional) – list of moves that led to this position (UCI or SAN).
- `state.elo` – requested strength.
- `state.time_limit_ms` (optional) – max time in ms.
- `state.cancelled` – set by runner; engine should abort when true.

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

## Async runner (WoW-compatible, builder pattern)

- **Callback-based:** engines receive `yieldFn(next)` and `onComplete(result, err)`. Use `yieldFn(next)` to yield; the runner calls `next()` later (e.g. via `C_Timer.After(0, next)` in WoW).
- **Builder:** `Runner.Create(engineId)` returns a builder. Chain state/options then `Run()`:
  - `:Fen(fen)` – position. Default: build from `:Moves()` (if set), else initial position.
  - `:Moves(moves)` – optional. Moves that led to this position (UCI). Used to build FEN if `:Fen()` not set.
  - `:Elo(elo)` – optional. Requested difficulty. Default: average of engine's ELO range.
  - `:TimeLimitMs(ms)` – optional. Max time in ms. Default: 20000 (20 seconds).
  - `:OnComplete(cb)` – `function(result, err)`. Required before `Run()`. If the engine returns an illegal move, the runner calls `onComplete(nil, err)` with `err` an object `{ message = "illegal move", move = "<uci>" }`.
  - `:DelayFn(fn)` – optional. `function(next)`. Default: call `next()` immediately.
  - `:LoopFn(fn)` – optional. `function(stepFn, doneFn)` loop driver for async iteration. Default: runs all steps synchronously.
  - `:Run()` – start calculation.

## Global registration (WoW addon compatible)

All functionality is registered on the global **DeltaChess** table:

- `DeltaChess.Engines` – registry and engine interface
- `DeltaChess.MoveGen` – FEN, legal moves, move validation
- `DeltaChess.EngineRunner` – `Create(engineId)` (builder)
- `DeltaChess.Engines.BeatHighestPiece` – built-in engine (after load)

**Single init file (e.g. World of Warcraft):** Use `lua/Init.lua` as the only script in your TOC. Before loading, set `DeltaChess.AddonPath` to the addon directory (e.g. `"Interface\\AddOns\\DeltaChess\\"`). Init will load the other Lua files via `loadfile` and register engines. If `AddonPath` is not set, Init only registers engines (assume other files were loaded by the TOC in order).

**Load order** when using multiple TOC entries: `MoveGen.lua` → `EngineInterface.lua` → `Runner.lua` → `Engines/BeatHighestPiece.lua` → `Init.lua`.

## Usage

**With require (standalone):**
```lua
package.path = "lua/?.lua;lua/?/init.lua;" .. (package.path or "")
require("engines.init")  -- register all engines

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

**With global (after init.lua or TOC load):**
```lua
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

**World of Warcraft (async with C_Timer.After):**
```lua
DeltaChess.EngineRunner.Create("beat_highest_piece")
    :Fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    :Elo(500)
    :OnComplete(function(result, err)
        if err then return end
        -- use result.move
    end)
    :DelayFn(function(next) C_Timer.After(0, next) end)
    :Run()
```
Engines receive `yieldFn(next)` and call it when they want to yield; the delay function runs `next()` on the next frame so the game stays responsive.

**CLI game (bin/play.lua):** Run from repo root. Engine vs itself or engine1 vs engine2, with optional ELOs:
```bash
lua bin/play.lua <engine1> [elo1] [engine2] [elo2]
# Examples:
lua bin/play.lua beat_highest_piece
lua bin/play.lua beat_highest_piece 500
lua bin/play.lua beat_highest_piece 500 other_engine 600
```

## Engines

- **beat_highest_piece** – prefers checkmate, then highest-value capture, else random; ELO 0–1500.

Add new engines in `src/Engines/` (with `.id`, `.name`, `GetEloRange`, `Calculate`) and load/register them in `init.lua`.

## Tests and CI

- **Interface tests:** `lua tests/test_interface.lua` (from repo root)
- **Engine self-play:** `lua tests/test_engine_selfplay.lua` – each engine plays against itself; runner validates every move (illegal moves reported via `onComplete(nil, err)`).

GitHub Actions (`.github/workflows/ci.yml`) runs both on push/PR to `main`/`master`.

## Layout

```
init.lua           # Single entry: loadfile's all modules, registers engines (WoW addon)
bin/
  play.lua         # CLI: engine vs itself or engine1 vs engine2 (optional ELOs)
src/
  EngineInterface.lua  # DeltaChess.Engines registry and interface contract
  EngineRunner.lua    # DeltaChess.Runner (callback-based, WoW-compatible)
  MoveGen.lua         # DeltaChess.MoveGen (legal move generation)
  Engines/
    BeatHighestPiece.lua  # DeltaChess.Engines.BeatHighestPiece
tests/
  test_interface.lua
  test_engine_selfplay.lua
.github/workflows/
  ci.yml
```
