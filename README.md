# ~ Tetris ~ 🎮

A cute Tetris game written in Haskell with pastel-colored graphics, powered by the [Gloss](https://hackage.haskell.org/package/gloss) 2D rendering library.

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=flat&logo=haskell&logoColor=white)

## Prerequisites

- **GHC** >= 9.6 (Glasgow Haskell Compiler)
- **Cabal** >= 3.10

If you don't have Haskell installed, the easiest way is via [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Then restart your terminal or run:

```bash
source ~/.ghcup/env
```

## Build & Run

```bash
# Build (first run downloads dependencies — takes a couple minutes)
cabal build

# Run the game
cabal run tetris
```

## Controls

| Key | Action |
|---|---|
| **←** / **→** | Move piece left / right |
| **↓** | Soft drop (+1 point per cell) |
| **↑** | Rotate clockwise |
| **Z** | Rotate counter-clockwise |
| **Space** | Hard drop (+2 points per cell) |
| **R** | Restart (after game over) |

## Gameplay

- **Board**: 10 × 20 grid
- **Pieces**: All 7 standard tetrominoes — I, J, L, O, S, T, Z
- **Ghost piece**: A faded preview shows where your piece will land
- **Next piece**: Displayed in the sidebar
- **Leveling**: Every 10 lines cleared increases the level and speed
- **Scoring** (NES-style, multiplied by level + 1):

| Lines Cleared | Base Score |
|---|---|
| 1 (Single) | 100 |
| 2 (Double) | 300 |
| 3 (Triple) | 500 |
| 4 (Tetris) | 800 |

## Project Structure

```
.
├── app/
│   └── Main.hs          # All game logic + rendering
├── tetris.cabal          # Cabal project file
├── main.py               # Original design notes
├── image.png             # Tetromino reference image
└── README.md
```

## How It Works

The game is built on a few core ideas from the design notes in `main.py`:

- **Board** — A 2D array where each cell is either empty (`Nothing`) or occupied by a piece type (`Just PieceType`).
- **Shapes** — Each tetromino is a list of `(row, col)` coordinates plus a rotation origin.
- **Rotation** — Uses matrix math: subtract the origin vector to center at `(0,0)`, multiply by a 90° rotation matrix `(dr, dc) → (dc, -dr)`, then add the origin back. Wall kicks try 7 offset positions to find a valid placement.
- **Collision** — Before any move or rotation, the new coordinates are checked against the board bounds and occupied cells.
- **Row clearing** — Full rows are filtered out and new empty rows are prepended to the top.

## Color Palette

Each piece has a cute pastel color with a darker shadow for depth:

| Piece | Color |
|---|---|
| I | Baby blue |
| J | Lavender |
| L | Peach |
| O | Butter yellow |
| S | Mint green |
| T | Pink lilac |
| Z | Rose |

## License

MIT
