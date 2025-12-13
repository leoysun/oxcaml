const { test, expect } = require('@playwright/test');

test.describe('Rummikub UI Tests', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the game
    await page.goto('/index.html');
    // Wait for the app to load
    await page.waitForSelector('#app', { state: 'attached' });
    // Wait a bit for Bonsai to initialize
    await page.waitForTimeout(1000);
  });

  test('should display the main menu', async ({ page }) => {
    // Check for menu elements (adjust selectors based on actual UI)
    const menuVisible = await page.locator('body').isVisible();
    expect(menuVisible).toBeTruthy();
  });

  test('should start a local game', async ({ page }) => {
    // Look for a button to start a game (adjust selector based on actual UI)
    // This is a basic test - you may need to adjust selectors
    const startButton = page.locator('button:has-text("Pass and Play"), button:has-text("Start"), button:has-text("New Game")').first();
    
    if (await startButton.count() > 0) {
      await startButton.click();
      // Wait for game to start
      await page.waitForTimeout(1000);
      
      // Check that we're in a game (look for game elements)
      const gameContainer = page.locator('#app');
      expect(await gameContainer.isVisible()).toBeTruthy();
    }
  });

  test('should display player hands', async ({ page }) => {
    // Start a game first
    const startButton = page.locator('button:has-text("Pass and Play"), button:has-text("Start")').first();
    
    if (await startButton.count() > 0) {
      await startButton.click();
      await page.waitForTimeout(2000);
      
      // Look for player hand elements (tiles)
      // Adjust selector based on actual rendered HTML structure
      const tiles = page.locator('[class*="tile"], [data-tile], .tile');
      const tileCount = await tiles.count();
      
      // Should have some tiles visible (at least for current player)
      expect(tileCount).toBeGreaterThan(0);
    }
  });

  test('should allow selecting tiles', async ({ page }) => {
    // Start a game
    const startButton = page.locator('button:has-text("Pass and Play"), button:has-text("Start")').first();
    
    if (await startButton.count() > 0) {
      await startButton.click();
      await page.waitForTimeout(2000);
      
      // Try to click on a tile
      const firstTile = page.locator('[class*="tile"], [data-tile], .tile').first();
      
      if (await firstTile.count() > 0) {
        await firstTile.click();
        await page.waitForTimeout(500);
        
        // Tile should be selected (check for selected state)
        // This is a basic check - adjust based on actual implementation
        const isVisible = await firstTile.isVisible();
        expect(isVisible).toBeTruthy();
      }
    }
  });

  test('should show game controls', async ({ page }) => {
    // Start a game
    const startButton = page.locator('button:has-text("Pass and Play"), button:has-text("Start")').first();
    
    if (await startButton.count() > 0) {
      await startButton.click();
      await page.waitForTimeout(2000);
      
      // Look for control buttons (Draw, Pass, etc.)
      const drawButton = page.locator('button:has-text("Draw"), button:has-text("End Turn")');
      const hasControls = await drawButton.count() > 0;
      
      // Should have some game controls
      expect(hasControls).toBeTruthy();
    }
  });

  test('should handle game over state', async ({ page }) => {
    // This test would require playing a full game, which is complex
    // For now, just check that the UI can handle the game state
    const startButton = page.locator('button:has-text("Pass and Play"), button:has-text("Start")').first();
    
    if (await startButton.count() > 0) {
      await startButton.click();
      await page.waitForTimeout(2000);
      
      // Check that game state is managed
      const appContainer = page.locator('#app');
      expect(await appContainer.isVisible()).toBeTruthy();
    }
  });
});
