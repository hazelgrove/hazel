# Study-Write Task Ideas

This document contains brainstormed ideas for medium and modification tasks, with details on domain, relevant functions, types, potential errors, and research question aspects.

---

## Medium Task Concepts (5-10 lines to write)

### 1. Recipe Scaler
- **Domain**: Cooking apps (scale recipes for different serving sizes)
- **Task**: Scale ingredient quantities by a factor
- **Functions**: `map`, `fold_left`, multiplication/division
- **Types**: `type Ingredient = (name: String, amount: Float, unit: String)`
- **Errors**: Integer division truncation, scaling factor applied wrong (multiply vs divide)
- **RQ aspect**: Arithmetic intermediate values - do users catch `4 / 2 = 2` but `3 / 2 = 1` (int division)?

### 2. Password Strength Checker
- **Domain**: Security/forms
- **Task**: Check multiple password rules (length, has digit, has uppercase)
- **Functions**: `string_length`, `fold_left` over characters, boolean combinators
- **Types**: `type Rule = LengthMin(Int) + HasDigit + HasUpper + HasSpecial`
- **Errors**: Off-by-one in length (`>= 8` vs `> 8`), combining conditions with wrong operator (`&&` vs `||`)
- **RQ aspect**: Multiple boolean conditions - probes show which specific rule passed/failed

### 3. Star Rating Aggregator
- **Domain**: Reviews (Yelp, Amazon)
- **Task**: Compute average rating, count by star level
- **Functions**: `fold_left`, `length`, `filter`, division
- **Types**: `type Review = (stars: Int, text: String)`
- **Errors**: Integer division for average, empty list division by zero, off-by-one in star filtering
- **RQ aspect**: Accumulator tuple `(sum, count)` - probes show both evolving

### 4. Playlist Duration Calculator
- **Domain**: Music apps (Spotify-like)
- **Task**: Sum song durations, format as "Xh Ym"
- **Functions**: `fold_left`, `map`, record access, modulo/division
- **Types**: `type Song = (title: String, artist: String, seconds: Int)`
- **Errors**: Time arithmetic (60 seconds vs 60 minutes), accumulator logic
- **RQ aspect**: Record field access + arithmetic pipeline

### 5. Leaderboard Formatter
- **Domain**: Games / competitions
- **Task**: Take scores, output ranked list with positions like "1. Alice: 100"
- **Functions**: `mapi`, `string_concat`, `sort` (if available) or assume pre-sorted
- **Types**: `type Entry = (name: String, score: Int)`
- **Errors**: Off-by-one in rank (0-indexed internally, 1-indexed for display)
- **RQ aspect**: Index manipulation - probes show `i` vs `i + 1`

### 6. Notification Triage
- **Domain**: Social apps (Facebook/Twitter notifications)
- **Task**: Filter and group notifications by type, mark important ones
- **Functions**: `filter`, `partition` (or two filters), `map`
- **Types**: `type Notif = Like(PostId) + Comment(PostId, String) + Follow(UserId) + Mention(PostId)`
- **Errors**: Pattern match coverage, wrong constructor matched
- **RQ aspect**: Sum type pattern matching - probes show which variant matched

### 7. Expense Categorizer
- **Domain**: Finance apps (budget tracking)
- **Task**: Categorize expenses, compute totals per category
- **Functions**: `fold_left` with map accumulator, `filter`
- **Types**: `type Category = Food + Transport + Entertainment + Other`, `type Expense = (amount: Int, cat: Category)`
- **Errors**: Accumulator structure (map vs list), category matching
- **RQ aspect**: Grouped aggregation - more complex fold accumulator

### 8. Event Schedule Validator
- **Domain**: Calendar apps
- **Task**: Check for overlapping events, find gaps
- **Functions**: `fold_left`, comparison operators, `filter`
- **Types**: `type Event = (name: String, start: Int, end: Int)` (times as minutes since midnight)
- **Errors**: Overlap condition logic (`start1 < end2 && start2 < end1`), boundary cases
- **RQ aspect**: Complex boolean conditions with multiple comparisons

### 9. CSV Row Parser
- **Domain**: Data processing
- **Task**: Parse comma-separated values, handle quoted fields
- **Functions**: `string_split`, `map`, `string_sub`, `filter`
- **Errors**: Empty fields, wrong split character, trimming whitespace
- **RQ aspect**: String manipulation pipeline, similar to mentions but different structure

### 10. Simple Markdown Stripper
- **Domain**: Text processing
- **Task**: Remove markdown formatting (`**bold**` → `bold`, `*italic*` → `italic`)
- **Functions**: `string_split`, `fold_left`, `string_concat`, `string_sub`
- **Errors**: Handling nested formatting, off-by-one in substring
- **RQ aspect**: State machine in fold (inside bold? inside italic?)

---

## Modification Task Concepts (extend existing program)

### 1. Calculator - Add Modulo
- **Base**: `calculator.hz` (existing ~60 lines)
- **Modification**: Add `%` modulo operator
- **Changes needed**: Add token case, add eval case (~5 lines)
- **Errors**: Forgetting to update tokenizer OR evaluator (need both)
- **RQ aspect**: Understanding multi-phase pipeline (tokenize → parse → eval)

### 2. Emojipaint - Add Diagonal Fill
- **Base**: `emojipaint.hz` (existing ~100 lines)
- **Modification**: Add `PaintDiagonal(Bool)` action (top-left to bottom-right, or reverse)
- **Changes needed**: Type, helper, case (~10 lines)
- **Errors**: Coordinate math (row == col for main diagonal), bounds checking
- **RQ aspect**: 2D coordinate reasoning

### 3. Tamagotchi - Add Sleep State
- **Base**: `tamagotchi.hz` (existing)
- **Modification**: Add `Sleep`/`Wake` actions, `awake: Bool` field
- **Changes needed**: Model field, action type, update logic that blocks feeding while asleep
- **Errors**: State interaction bugs (feeding sleeping pet should fail/no-op)
- **RQ aspect**: State machine with preconditions - probes show blocked actions

### 4. Tic-tac-toe - Add Move History
- **Base**: `tictactoe.hz` (existing)
- **Modification**: Add `Undo` action, track move history
- **Changes needed**: History list in model, undo logic
- **Errors**: Empty history handling, forgetting to push moves to history
- **RQ aspect**: History management pattern - probes show history growing/shrinking

### 5. Counter App - Add Reset to Value
- **Base**: Simple MVU counter (new, ~30 lines)
- **Modification**: Add `ResetTo(Int)` action alongside `Increment`/`Decrement`
- **Changes needed**: New action case, simple handler
- **Errors**: Trivial but good intro to modification pattern
- **RQ aspect**: Gentlest modification task, learning the workflow

### 6. Todo List - Add Priority Filter
- **Base**: Simple todo list MVU (new, ~50 lines)
- **Types**: `type Priority = Low + Medium + High`, `type Todo = (text: String, priority: Priority, done: Bool)`
- **Modification**: Add `FilterByPriority(Priority)` action
- **Changes needed**: Filter state in model, filter logic in view
- **Errors**: Filter predicate wrong, forgetting to handle "show all" case
- **RQ aspect**: Filtering with sum types

### 7. Quiz App - Add Skip Question
- **Base**: Simple quiz app (new, ~60 lines)
- **Modification**: Add `Skip` action that moves to next question without answering
- **Changes needed**: Handle skipped questions in scoring, track skips
- **Errors**: Skip count vs wrong count, end-of-quiz boundary
- **RQ aspect**: Multiple counters in state

### 8. Emoji Reaction Counter - Add Top N
- **Base**: Count reactions by emoji (new, ~40 lines)
- **Modification**: Add function to get top N most common reactions
- **Changes needed**: Sorting or fold-based top-N extraction
- **Errors**: Comparison direction (ascending vs descending), off-by-one in N
- **RQ aspect**: Aggregation + ranking

### 9. Chat Log - Add Search
- **Base**: Simple chat message list (new, ~40 lines)
- **Modification**: Add `Search(String)` that filters messages containing query
- **Changes needed**: Search state, filter predicate with `string_contains` or similar
- **Errors**: Case sensitivity, empty query handling
- **RQ aspect**: String matching in filter

### 10. Game of Life - Add Wraparound
- **Base**: `gameoflife.hz` (existing)
- **Modification**: Change neighbor counting to wrap around edges (torus topology)
- **Changes needed**: Modify coordinate lookups to use modulo
- **Errors**: Off-by-one in modulo, negative coordinate handling
- **RQ aspect**: Modifying existing algorithm, coordinate edge cases

---

## Task Selection Matrix

### By Domain Appeal

| Task | Domain | Relatability |
|------|--------|--------------|
| Star Rating Aggregator | Reviews/e-commerce | Very high - everyone uses ratings |
| Notification Triage | Social media | High - familiar UX pattern |
| Recipe Scaler | Cooking | High - common real task |
| Playlist Duration | Music streaming | High - familiar app type |
| Password Checker | Security | Medium-high - everyone encounters these |
| Expense Categorizer | Finance | Medium - less universal |

### By FP Concept Coverage

| Task | Primary Concept | Secondary |
|------|----------------|-----------|
| Star Rating Aggregator | Fold with tuple accumulator | Arithmetic |
| Notification Triage | Sum type pattern matching | Filter |
| Leaderboard Formatter | mapi (indexed map) | String formatting |
| Expense Categorizer | Fold with complex accumulator | Sum types |
| Markdown Stripper | Fold as state machine | String manipulation |

### By Probe Benefit

| Task | What Probes Reveal |
|------|-------------------|
| Star Rating | Accumulator (sum, count) evolving step-by-step |
| Password Checker | Which specific rule passed/failed |
| Notification Triage | Which sum type variant matched |
| Leaderboard | Index values (catch off-by-one) |
| Event Validator | Complex boolean condition evaluation |

### Suggested Priority Picks

**Medium tasks to implement next:**
1. **Star Rating Aggregator** - Very relatable, shows fold tuple accumulator
2. **Notification Triage** - Modern domain, sum type matching

**Modification tasks to implement next:**
1. **Tamagotchi + Sleep** - Fun/nostalgic, state preconditions
2. **Calculator + Modulo** - Simple, shows multi-phase understanding

---

## Implementation Notes

When implementing these tasks:

1. **Test formatting**: Use multi-line test format for maximum probe visibility
   ```hazel
   test
     function_call(args)
     == expected
   end
   ```

2. **Intermediate bindings**: Encourage let bindings on separate lines
   ```hazel
   let step1 = ... in
   let step2 = ... in
   result
   ```

3. **Error case coverage**: Include tests that exercise error-prone cases
   - Empty lists
   - Boundary values (0, 1, max)
   - Both branches of conditionals

4. **Storyboard common mistakes**: Document 2-3 typical errors and how probes reveal them
