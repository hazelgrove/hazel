# Playwright Stepper Testing

This procedure was calibrated against Hazel at `http://127.0.0.1:8000` with a
`1280 x 1000` viewport. It exercises the SVG/hidden-input stepper workflow,
runs JSCoq, and applies the validated written step.

## Prerequisites

1. Run `make`.
2. Serve `_build/default/src/web/www` on port 8000.
3. Hard-refresh or create a new browser context after rebuilding.
4. Use a timeout of at least 70 seconds around JSCoq validation. A current
   integer check takes about 11 seconds in headless Chromium.

Playwright is installed at `/Users/nishant/node_modules/playwright`.

If Playwright reports that its version-specific cached Chromium executable is
missing, use the installed system Chrome instead of reinstalling browsers:

```javascript
const browser = await chromium.launch({
  headless: true,
  executablePath: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  args: [
    "--no-first-run",
    "--no-default-browser-check",
    "--disable-crash-reporter",
    "--disable-session-crashed-bubble",
  ],
});
```

Those flags also prevent the stale “Chrome did not shut down correctly” banner
from contaminating screenshots and intercepting calibrated clicks. On macOS,
launching system Chrome from a sandboxed test process may require escalation.

## Calibrated Algebra Check

Use this smoke case:

```text
source: x * (x + 1)
target: x*x + x
level: Algebra
mode: Check result
expected verdict: Valid
```

### Enter the source

Hazel does not expose the main expression editor as an input. Click the visible
expression hole near `(20, 60)`, then use `page.keyboard.type(...)`.

Verify that `body.innerText` contains `x * (x + 1)` before continuing.

### Open the stepper

Click the toggle with `title="Show Stepper"`. With the calibrated viewport this
is initially near `(1218, 108)`.

The stepper toolbar should then contain a settings icon near `(1150, 151)`.

### Configure math automation

1. Open the stepper settings icon.
2. Click `[title="choose math automation"]`.
3. Click exact text `Algebra`.
4. Click exact text `Check result`.
5. Close the modal by clicking its backdrop, for example `(100, 400)`.

Escape did not reliably close the nested chooser. A remaining `.modal-back`
will intercept later pointer events, so assert that `.modal-back` has count 0.

### Select the expression

A normal click only places a red caret and does not expose proof actions.
Structurally select the complete final-row expression by dragging horizontally
across it:

```javascript
await page.mouse.move(55, 154);
await page.mouse.down();
await page.mouse.move(175, 154, {steps: 12});
await page.mouse.up();
```

Assert that exact text `Search ▼` is visible. This is the key calibration step
that earlier automated runs missed.

### Enter and validate the target

1. Click exact text `Search ▼`.
2. Click the target expression hole near `(115, 335)`.
3. Type `x*x + x` with `page.keyboard.type(...)`.
4. Assert that exact text `Run Rocq Search` is visible.
5. Click it and wait until `body.innerText.split("\n")` contains `Valid`.

Do not treat `Equivalent, outside profile` as success for this case. The
profile-valid proof uses distribution followed by structural `* 1` cleanup.

Useful console events are:

```text
[Hazel JSCoq] starting check
[Hazel JSCoq] finished check
```

Capture `window.HazelJSCoq.stats()` after completion. A successful check should
have `activeChecks: 0`, one completed check with `ok: true`, and no hidden check
hosts.

### Apply the step

Validation alone does not modify the derivation. Click exact text `Replace`,
wait for recalculation, and assert that the stepper contains:

```text
reparenthesize
Rocq tactic search (1 exportable rule)
x * x + x
```

The target should also appear in `.cell-item.code-editor` text contents.

## Calibrated One Step Identity Check

The following case exercises a named algebra identity without JSCoq fallback:

```text
source: a ** 2 - b ** 2
target: (a + b) * (a - b)
level: Algebra
mode: One step
expected verdict: Valid
expected derivation: algebra identity one step (1 exportable rule)
```

Do not hard-code the settings or final-row Y coordinate. Read the rendered
boxes, then use mouse events; `locator.click()` was unreliable for the
unlabelled gear control:

```javascript
const gear = await page.locator(".stepper-controls .icon").nth(1).boundingBox();
await page.mouse.click(gear.x + 10, gear.y + 10);

const row = /* highest-Y .code-container whose text equals the source */;
await page.mouse.move(row.x + 1, row.y + row.height / 2);
await page.mouse.down();
await page.mouse.move(row.x + row.width - 1, row.y + row.height / 2, {steps: 20});
await page.mouse.up();
```

The target editor auto-inserts matching parentheses. Enter a parenthesized
term by typing the opening parenthesis and contents, then press `ArrowRight` to
move past the generated closing parenthesis. Typing `)` creates an extra
delimiter and leaves a broken expression:

```javascript
await page.keyboard.type("(");
await page.keyboard.type("a + b", {delay: 40});
await page.keyboard.press("ArrowRight");
await page.keyboard.type(" * ", {delay: 40});
await page.keyboard.type("(");
await page.keyboard.type("a - b", {delay: 40});
await page.keyboard.press("ArrowRight");
```

Use a fresh context and a small typing delay. Assert that
`body.innerText.split("\n")` contains `Valid`, click `Replace`, and verify the
named identity derivation rather than accepting a generic equivalence result.

## Calibrated Profile Toggle Checks

Profile-boundary tests must change controls through the rendered Profile pane;
do not mutate the profile model from JavaScript. After structurally selecting
the expression:

1. Open exact text `Profile ▼`.
2. Expand the relevant visible-rule subgroup. Trigonometry and Calculus
   subgroups start collapsed.
3. Locate the rule's `label.profile-board-toggle` and click its checkbox.
4. For `Normalize affine arithmetic`, expand the last Arithmetic level toggle;
   that toggle belongs to Check Result operations, while the first Arithmetic
   toggle belongs to visible operations.
5. Wait about 700 ms after the final toggle, close exact text `Profile ▲`, wait
   for `.profile-board-layer` to detach, and wait another 700 ms before clicking
   `Search ▼`. Without both settles, the SVG selection may collapse instead of
   opening Search.

For the target hole, use its rendered bounding box and a mouse click, then wait
about 250 ms before typing. A forced SVG click can resolve to the hole while the
overlying code container keeps editor focus, causing the first target character
to be lost. Always assert the rendered target text before running Rocq.

For a valid result, also assert the displayed `Profile route:` text. This catches
unexpected alternate routes—for example, an affine equality can remain valid
through `Normalize affine arithmetic` even when its more visually obvious
factoring rule is disabled. For disabled Trigonometry routes, assert both the
`Invalid` verdict and the concise `No proof is available using the active
Profile.` message; raw `Pp_*` output belongs only in the browser console.

## Known Failure Modes

- Clicking the wrong right-side tab opens API, probes, or problems settings.
  Prefer the `Show Stepper` title and the stepper toolbar coordinates above.
- Clicking an operator places a caret. Dragging across the expression creates
  the structural selection required for Search controls.
- A forced click on `svg.empty-hole` can leave focus on the overlying code
  container. Click the hole's coordinates with `page.mouse` and pause before
  typing the target.
- Nested settings backdrops intercept clicks even after pressing Escape.
- Typing closing parentheses into the hidden target editor duplicates Hazel's
  auto-inserted delimiters; use `ArrowRight` to leave each pair.
- JSCoq's manager uses a `ProviderContainer`. Its editor is
  `manager.provider.snippets[0].editor`, not `manager.provider.editor`.
- Reusing the JSCoq worker currently preserves correctness but not speed. Hazel
  still reparses the complete generated prelude for each validation.
- Unbound scratch variables may show red static diagnostics while the generated
  Rocq theorem correctly quantifies them. Assert the checker verdict separately
  from the scratch statics indicator.

## Confidence

DOM-only automation is insufficient for this workflow. A reliable test must
combine semantic selectors for menus/buttons with calibrated mouse interaction
for the expression editor and structural selection. Always retain a screenshot
and JSCoq console timings when diagnosing failures.
