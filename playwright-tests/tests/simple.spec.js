import { test, expect } from '@playwright/test';

const program1 = `1+1`;
const program2 = `let double_recursively : Int -> Int =
fun n ->
if n == 0 then 0
else double_recursively(n - 1) + 2
in
let empty_list : [Int] = [] in
let non_empty_list : [Int] = 1::2::3::[] in
let list_literals : [Int] = [1, 2, 3] in
let length : [Int] -> Int =
fun xs ->
case xs
| [] => 0
| hd::tl => 1 + length(tl)
end
in
let has_at_least_two_elements : [Int] -> Bool =
fun xs ->
case xs
| [] => false
| hd::[] => false
| a::b::[] => true
end
in
let string_lits = "Hello, world!" in
let string_equality = string_lits $== "Hello, world!" in
let non_empty_hole : Int = true in
2 + 2`;

const testTemplate = async (page, program, expectedTime) => {
  await page.goto('http://0.0.0.0:8000', { waitUntil: 'domcontentloaded' });

  // The selector for a div with the ID "page"
  const pageDivSelector = '#page'; 
  // Wait for the div to be present in the DOM and to be visible.
  await page.waitForSelector(pageDivSelector, { state: 'visible' });
  await page.click(pageDivSelector);
  
  // Start the timer by injecting performance.now() before the last key press
  const startTime = await page.evaluate(() => performance.now());

  const resultSelector = 'div.ok.status';
  for (let c of program) {
    await page.keyboard.press(c);
    // Selector that matches the div with class "ok result"
    await page.waitForSelector(resultSelector);
  }

  // Stop the timer
  const endTime = await page.evaluate(() => performance.now());
  const elapsedTime = endTime - startTime;
  console.log(`Time elapsed: ${elapsedTime} milliseconds`);
  
  expect(elapsedTime).toBeLessThan(expectedTime);
};

test('measure program 1', async ({ page }) => {
  test.setTimeout(10000); // 10 sec timeout
  await testTemplate(page, program1, 5000);
});

test('measure program 2', async ({ page }) => {
  test.setTimeout(120000); // 120 sec timeout
  await testTemplate(page, program2, 120000);
});