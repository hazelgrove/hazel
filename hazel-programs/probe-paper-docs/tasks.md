# Study Tasks — Capsules

The shipped tasks are tutorial slides 26–34 in `hazel-programs/tutorial/`
(code embedded in each `.hzt`). D = debugging (find/fix a seeded bug),
W = writing. All debugging programs are mini-MVU (`Model` record + `Action`
ADT + `fold_left(update)`); night-garden themed.

## Shipped

| Slide | Task | Kind | The bug / the job | Probe showcase |
|---|---|---|---|---|
| 26 | Dew ledger | D | `Spill` halves `jars` instead of `dew` (wrong record field) | first full read-the-types-then-probe walkthrough; small enough to read whole |
| 27 | Grove name | W | write `grove_name`: first segment of a path string | `string_split` argument-order + `nth` off-by-one discovered via probe values |
| 28 | Watering timer | D | `minutes > 60` should be `>= 60` — exact hours format wrong | boundary-condition bug; probe the branch condition at the failing input |
| 29 | Running sum | W | running totals of a list via fold | accumulator evolution visible across fold iterations |
| 30 | Misplanted seeds | D | inner grid index compares `j == row` instead of `j == col` (wrong variable; `_col` sits unused) | probe inside nested `mapi`; which call/cell went wrong |
| 31 | Moonphase log | W | write `clean_entry`: trim, strip entry numbers, normalize dashes, collapse spaces | shadowed-`let` pipeline; one sample per stage; `string_replace`/`string_match` behavior discovery |
| 32 | Harvest streak | D | streak compares against freshly-updated `newLast` instead of the previous `lastQuality` — streak never resets | stale-vs-fresh value bug; probing both sides of the comparison shows them always equal |
| 33 | Plant a column | W (modify) | add `PlantCol` action + finish the `setCol` stub, mirroring `PlantRow` | three-site modification; tests fill with live values as the helper is written |
| 34 | Growth plotter | D | `Growing => Growing` case arm — crops stuck, never mature | probe `advanceStage` over repeated waterings; the repeated `Growing` samples are the tell |

The task programs originated as full-size study programs (150–500 lines,
below) and were trimmed to tutorial-embedded size (~60–120 lines).

## Unshipped task families (design-space breadth)

Debugging (each: working version + bug variant(s) + probe strategy notes):

- **companion-plotter** — neighbor effects: `companionEffect(cell.crop,
  cell.crop)` compares a crop with itself, so effects never fire (right
  variable in scope, wrong one used; silently does nothing).
- **soil-plotter / crop-plotter (grove)** — the shipped `i`/`j` wrong-index
  family at full size.
- **lunar-growth** — a moon phase that is not its own opposite.
- **nutrient-rotation** — wrong field restored after rotation.
- **tamagotchi** (haunted-toaster pet, ~505 lines) — three bug variants: wrong
  stat checked in decay; `neglectScore > 0` instead of `> careScore`
  (evolution priority); sleep applies `- bonus` instead of `+ bonus`.
- **tictactoe** — anti-diagonal indices (2,5,6 for 2,4,6); `nextPlayer`
  returning the same player.
- Pre-garden generation (`study-old`): emojipaint (grid paint, setCell index
  bug), game-of-life (orthogonal-only neighbors; survival 2–4 for 2–3;
  sequential vs simultaneous update), calculator (flat precedence;
  right-associativity).

Writing (sketch/solution pairs): mentions extractor (split → filter → map;
`@name` extraction), garden-survey (`string_match` "yes" matches
"yesterday"), night-bloom, crop-tally, garden-path (shipped as 27),
log-cleaner v1/v2 (shipped as 31), running-sum (shipped as 29),
harvest-streak-extend, crop-plotter-extend (shipped as 33), clamp / safe-head
(warmups; clamp became the tutorial's `if` slide).
