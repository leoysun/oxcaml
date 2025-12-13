# UI Tests for Rummikub

This directory contains Playwright UI tests for the Rummikub game.

## Setup

1. Install Node.js and npm (if not already installed)
2. Install dependencies:
   ```bash
   make install-ui-deps
   ```
   Or manually:
   ```bash
   npm install
   npx playwright install chromium
   ```

## Running Tests

Run all UI tests:
```bash
make test-ui
```

Or directly with npm:
```bash
npm test
```

Run tests in headed mode (see the browser):
```bash
npm run test:headed
```

Run tests with UI mode (interactive):
```bash
npm run test:ui
```

## Test Structure

Tests are located in `tests/rummikub-ui.spec.js` and cover:
- Main menu display
- Starting a local game
- Displaying player hands
- Selecting tiles
- Game controls
- Game state management

## Notes

- Tests use a local HTTP server (port 8000) to serve the game
- The server is automatically started before tests run
- Tests may need adjustment based on actual UI selectors and structure
- Some tests are basic and may need refinement as the UI evolves
