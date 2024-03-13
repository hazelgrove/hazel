const { test, expect } = require('@playwright/test');

test('measure time from key press to page finish changing', async ({ page }) => {
  await page.goto('http://0.0.0.0:8000', { waitUntil: 'domcontentloaded' });

  // The selector for a div with the ID "page"
  const pageDivSelector = '#page'; 
  // Wait for the div to be present in the DOM and to be visible.
  await page.waitForSelector(pageDivSelector, { state: 'visible' });
  await page.click(pageDivSelector);
  
  // Start the timer by injecting performance.now() before the last key press
  const startTime = await page.evaluate(() => performance.now());

  // Press the keys '1', '+', '1' individually
  await page.keyboard.press('1');
  await page.keyboard.press('NumpadAdd'); // For the '+' key on the numpad
  await page.keyboard.press('1');

  // Selector that matches the div with class "ok result"
  const resultSelector = 'div.ok.result';
  await page.waitForSelector(resultSelector);

  // Stop the timer
  const endTime = await page.evaluate(() => performance.now());
  const duration = endTime - startTime;
  console.log(`Time elapsed: ${duration} milliseconds`);

  // Verify the duration is within an expected range: 5s
  expect(duration).toBeLessThan(5000);
});