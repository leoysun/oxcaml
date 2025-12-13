# Rummikub in OCaml

A web-based implementation of the classic tile game Rummikub built with OCaml, featuring local multiplayer, computer opponent, and online multiplayer via Firebase.

**Play online:** [https://leoysun.github.io/oxcaml](https://leoysun.github.io/oxcaml)

## 🎮 Game Rules

Rummikub is a tile-based game where players try to be the first to play all tiles from their rack.

### Objective
Be the first player to empty your rack by playing all tiles in valid melds.

### Setup
- Each player starts with 14 tiles
- Remaining tiles form the draw pile
- Players take turns in order

### Valid Melds
The following define possible melds, or groups of tiles, in Rummikub. These melds reside on the table and tiles can be 
taken from or added to existing melds in future moves, as long as all melds on the table remain valid.

**Groups (Sets):** 3-4 tiles of the same number in different colors
- Example: Red 5, Blue 5, Black 5, Orange 5

**Runs (Sequences):** 3+ consecutive numbers in the same color
- Example: Red 3, Red 4, Red 5, Red 6
- Do not wrap around (ie. 13,1,2 not allowed)

### Special Rules

1. **First Play (30-Point Rule):**
   - On your first turn, you must play melds totaling at least 30 points without using existing melds
   - That is, NO REARRANGING on first turn.
   - Tile values: Numbers 1-13 = face value, Jokers = points of whatever it represents
   - After your first play, you can play any valid melds and use existing table tiles

2. **Jokers:**
   - Can substitute for any tile in a meld
   - Can be taken from the board if you can replace it with the actual tile

3. **Table Manipulation:**
   - You can rearrange existing melds on the board
   - You can split, combine, or add tiles to existing melds
   - All melds on the board must remain valid after your turn

### Turn Actions

1. **Play Melds:** Select tiles from your hand and play them as valid groups or runs
2. **Rearrange:** Manipulate existing melds on the board (after your first play)
3. **Draw:** Take a tile from the draw pile and end your turn
4. **End Turn:** Pass your turn (after playing at least one meld)

### Winning
The first player to play all tiles from their rack wins the game!

---

## Quick Start

### Play Online
Simply visit [https://leoysun.github.io/oxcaml](https://leoysun.github.io/oxcaml) - no installation needed!

### Build and Run Locally

#### Prerequisites
- OCaml 5.1.0+ with OPAM
- Dune build system
- Bonsai web framework

#### Setup Development Environment

```bash
# Initialize OPAM
opam init -a --disable-sandboxing --yes --bare
opam update -a

# Create switch and install dependencies
opam switch create 5.1.0 --yes
eval $(opam env)
opam install --yes ocamlformat merlin ocaml-lsp-server bonsai js_of_ocaml-compiler
```

#### Build the Game

```bash
# Build the web app
make rummikub

# Or manually:
dune build ui/rummikub_app.bc.js
cp _build/default/ui/rummikub_app.bc.js generated_js/
```

#### Run Locally

1. **Simple HTTP Server:**
   ```bash
   python3 -m http.server 8000
   # Or use any static file server
   ```

2. **Open in Browser:**
   - Navigate to `http://localhost:8000`
   - Open `rummikub_game.html`

3. **Firebase Setup (for multiplayer):**
   - Copy `firebase-config.example.js` to `firebase-config.js`
   - Add your Firebase project credentials (see Firebase Console)

---

## 🎯 How to Play

### Local Games

**vs Computer:**
1. Click "vs Computer"
2. Game starts automatically
3. Select tiles from your hand and click "Play"
4. Computer moves automatically after your turn

**Pass-and-Play (2-4 Players):**
1. Select number of players (2, 3, or 4)
2. Game starts automatically
3. Pass the device between players
4. Each player takes their turn when it's their time

### Online Multiplayer

**Create a Game:**
1. Sign in (email/password or guest)
2. Click "Online Multiplayer"
3. Select number of players (2, 3, or 4)
4. Click "Create Game"
5. Share the game code with friends

**Join a Game:**
1. Sign in or play as Guest
2. Click "Online Multiplayer"
3. Enter the game code
4. Click "Join"

### Game Controls

- **Select Tiles:** Click tiles in your hand to select them
- **Play Selected:** Play selected tiles as a meld (must be valid)
- **Draw Tile:** Take a tile from the draw pile
- **End Turn:** Pass your turn (after playing melds)
- **Rearrange Table:** Enter rearrange mode to manipulate board melds
- **Menu Button (☰):** Return to main menu (local games only)

---

## 🏗️ Project Structure

```
.
├── src/                    # Core game logic
│   ├── rummikub.ml         # Main game rules and state
│   ├── state.ml            # Game state management
│   ├── meld.ml             # Meld validation
│   ├── tile.ml             # Tile definitions
│   ├── rules.ml            # Game rules engine
│   ├── firebase.ml         # Firebase initialization
│   ├── firestore.ml        # Firestore operations
│   ├── auth.ml             # Authentication
│   └── game_sync.ml        # State serialization
├── ui/                     # User interface
│   └── rummikub_ui_main.ml # Main Bonsai UI component
├── test/                   # Tests
├── generated_js/           # Compiled JavaScript (for deployment)
├── index.html              # Main HTML entry point
├── firebase-config.js      # Firebase credentials (not in repo)
└── Makefile                # Build commands
```
---

## Tech Stack

- **OCaml** - Functional programming language
- **Bonsai** - Declarative UI framework
- **js_of_ocaml** - OCaml to JavaScript compiler
- **Firebase** - Backend services (Auth, Firestore for multiplayer with real-time listeners)
- **Dune** - Build system