# Mention Extractor Storyboard

## Task Overview
- **Category**: Medium (5-10 lines)
- **Domain**: Social media / chat (relatable!)
- **Error patterns**: Parameter order, off-by-one, pipeline composition
- **Probe insight**: See each transformation step in the pipeline

## Setup
- 3 helper functions to write
- 4 tests covering: single mention, multiple mentions, no mentions, just a mention

## CLI Development Session

### Step 1: Start with just the split

**User writes:**
```hazel
let extract_mentions = fun message ->
  let words = string_split(" ", message) in
  words
in
extract_mentions("Hey @alice")
```

**Probe output:**
```
let extract_mentions = fun ⟦message⟧ ->     ≡ "Hey @alice"
  let words = ⟦string_split(" ", message)⟧ in     ≡ ["Hey", "@alice"]
  ⟦words⟧     ≡ ["Hey", "@alice"]
```

**Insight**: Can see the split worked - `["Hey", "@alice"]`.

### Step 2: Add the filter

**User writes:**
```hazel
let starts_with_at = fun word ->
  string_sub(word, 0, 1) == "@"
in

let extract_mentions = fun message ->
  let words = string_split(" ", message) in
  let mentions = filter(words, starts_with_at) in
  mentions
```

**Probe output (--many with multiple tests):**
```
let starts_with_at = fun ⟦word⟧ ->     ≡ "Hey" ⫽ "@alice" ⫽ "@bob" ⫽ "hello"
  ⟦string_sub(word, 0, 1) == "@"⟧     ≡ false ⫽ true ⫽ true ⫽ false
...
  let mentions = ⟦filter(words, starts_with_at)⟧ in     ≡ ["@alice"] ⫽ ["@bob", "@carol"]
```

**Insight**: Filter keeps only @-words. Can see predicate returning true/false for each word.

### Step 3: Add the map to strip @

**User writes:**
```hazel
let strip_at = fun word ->
  string_sub(word, 1, string_length(word) - 1)
in
...
  let usernames = map(mentions, strip_at) in
```

**Probe output:**
```
let strip_at = fun ⟦word⟧ ->     ≡ "@alice" ⫽ "@bob" ⫽ "@carol"
  ⟦string_sub(word, 1, string_length(word) - 1)⟧     ≡ "alice" ⫽ "bob" ⫽ "carol"
```

**Insight**: Each @-word gets its prefix removed.

## Common Mistake Paths

### Mistake A: Wrong `string_split` parameter order

**User writes:**
```hazel
let words = string_split(message, " ") in  # WRONG ORDER
```

**Probe shows:**
```
  let words = ⟦string_split(message, " ")⟧ in     ≡ [" "]
```

**How probe helps**: Result is `[" "]` (just the separator!) instead of the words. Immediately obvious something is wrong.

### Mistake B: Off-by-one in `strip_at`

**User writes:**
```hazel
let strip_at = fun word ->
  string_sub(word, 1, string_length(word))  # Length should be len-1
```

**Probe shows:**
```
  ⟦string_sub(word, 1, string_length(word))⟧     ≡ string_sub("@alice")
```

**How probe helps**: Stuck expression `string_sub("@alice")` indicates out-of-bounds. Compare to correct version showing `"alice"`.

### Mistake C: Wrong substring indices for starts_with_at

**User writes:**
```hazel
let starts_with_at = fun word ->
  string_sub(word, 1, 1) == "@"  # Starts at 1, not 0
```

**Probe shows:**
```
  ⟦string_sub(word, 1, 1) == "@"⟧     ≡ false  # For "@alice"
```

**How probe helps**: Returns false even for "@alice" because it's checking the second character ('a'), not the first ('@').

### Mistake D: Forgetting a pipeline step

**User writes:**
```hazel
let extract_mentions = fun message ->
  let words = string_split(" ", message) in
  let mentions = filter(words, starts_with_at) in
  mentions  # Forgot to strip the @!
```

**Probe shows:**
```
  ⟦mentions⟧     ≡ ["@alice"]  # Still has @ prefix
```

**How probe helps**: Test fails with `["@alice"] == ["alice"] ≡ false`, and probe shows the @ is still there.

## Key Probe Benefits

1. **Pipeline visibility**: Each let binding shows its intermediate result
2. **Multiple samples**: `--many` shows how each function handles different inputs
3. **Helper function tracing**: See `starts_with_at` returning true/false for each word
4. **Parameter order bugs**: Wrong arguments produce visibly wrong results
5. **Stuck expressions**: Out-of-bounds access shows as stuck `string_sub(...)` rather than confusing error

## Domain Appeal

This task feels like "real programming":
- Extracting data from text is ubiquitous
- @mentions are familiar from Twitter, Slack, Discord
- The split → filter → map pipeline is a common pattern
- Errors have clear, relatable consequences ("why does it return [' '] instead of words?")
