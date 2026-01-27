# Real Incremental Writing Session: clamp

This is Claude's actual command-line session attempting to write `clamp` incrementally,
with real outputs and thoughts as they occurred.

---

## Setup

I'm starting with the sketch and will try to write the solution incrementally,
checking probe output after each step.


## Step 1: Read the sketch

```
$ cat clamp-sketch.hz
```

I see:
- Function signature: `fun (x, lo, hi) -> ?`
- Need to return lo if x < lo, hi if x > hi, else x
- 5 tests including boundary cases (0 and 10)

**My thought**: This is straightforward nested if-else. I'll start with the first condition.

---

## Step 2: Write first condition, leave rest as hole

```
$ cat > /tmp/clamp-wip.hz << 'EOF'
let clamp = fun (x, lo, hi) ->
  if x < lo then lo
  else ?
in
test clamp(5, 0, 10) == 5 end;
test clamp(-3, 0, 10) == 0 end;
test clamp(15, 0, 10) == 10 end;
test clamp(0, 0, 10) == 0 end;
test clamp(10, 0, 10) == 10 end
EOF
```


**Output:**
```
let clamp = fun (x, lo, hi) ->
      if x < lo then lo
      else ?
in
test clamp(5, 0, 10) == 5 end;
...
```

**My thought**: No probe values shown! Right - CLI doesn't auto-probe. I need to add `^^probe()`.
Let me add a probe on the condition to see which tests hit it.

---

## Step 3: Add explicit probe on the condition

```
$ cat > /tmp/clamp-wip.hz << 'EOF'
let clamp = fun (x, lo, hi) ->
  if ^^probe(x < lo) then lo
  else ?
in
...
EOF

$ ./hazel probe --many /tmp/clamp-wip.hz
```


**Output:**
```
if ^^probe(x < lo) then lo     ≡ false ⫽ true ⫽ false ⫽ false ⫽ false
```

**My thought**: 5 boolean values for 5 tests. Let me map them:
- test 1: clamp(5, 0, 10) → x < lo is **false** (5 is not < 0) ✓
- test 2: clamp(-3, 0, 10) → x < lo is **true** (-3 < 0) ✓
- test 3: clamp(15, 0, 10) → x < lo is **false** (15 is not < 0) ✓
- test 4: clamp(0, 0, 10) → x < lo is **false** (0 is not < 0) ✓
- test 5: clamp(10, 0, 10) → x < lo is **false** (10 is not < 0) ✓

Good, the first condition is correct. Only test 2 takes the "then lo" branch.

---

## Step 4: Add second condition

```
$ cat > /tmp/clamp-wip.hz << 'EOF'
let clamp = fun (x, lo, hi) ->
  if ^^probe(x < lo) then lo
  else if ^^probe(x > hi) then hi
  else ?
in
...
EOF

$ ./hazel probe --many /tmp/clamp-wip.hz
```


**Output:**
```
if ^^probe(x < lo) then lo     ≡ false ⫽ true ⫽ false ⫽ false ⫽ false
else if ^^probe(x > hi) then hi     ≡ false ⫽ true ⫽ false ⫽ false
```

**My thought**: Wait - the second probe only shows 4 values, not 5!

**[ANNOTATION]** This is interesting and took me a second to understand. Test 2 
(`clamp(-3, 0, 10)`) took the first branch (`x < lo` was true), so it never 
evaluated the second condition. The probe only fires when that code path executes.

So the 4 values for `x > hi` correspond to tests 1, 3, 4, 5:
- test 1: clamp(5, 0, 10) → x > hi is **false** (5 is not > 10) ✓
- test 3: clamp(15, 0, 10) → x > hi is **true** (15 > 10) ✓
- test 4: clamp(0, 0, 10) → x > hi is **false** (0 is not > 10) ✓
- test 5: clamp(10, 0, 10) → x > hi is **false** (10 is not > 10) ✓

Good - only test 3 takes the "then hi" branch.

---

## Step 5: Complete with else branch

```
$ cat > /tmp/clamp-wip.hz << 'EOF'
let clamp = fun (x, lo, hi) ->
  if ^^probe(x < lo) then lo
  else if ^^probe(x > hi) then hi
  else x
in
...
EOF

$ ./hazel probe --many /tmp/clamp-wip.hz
```


**Output:**
```
if ^^probe(x < lo) then lo     ≡ false ⫽ true ⫽ false ⫽ false ⫽ false
else if ^^probe(x > hi) then hi     ≡ false ⫽ true ⫽ false ⫽ false
else x
```

**My thought**: Same probe output as before - the `else x` doesn't have a probe so I don't see 
those values. Let me run the tests to see if this is correct.

---

## Step 6: Run tests

```
$ ./hazel test /tmp/clamp-wip.hz
```


**Output:**
```
Test Results: All tests passing!
```

**My thought**: Done! All 5 tests pass.

---

## Reflection

### What I actually did:
1. Read the spec
2. Wrote the first `if` condition with a hole for the rest
3. Added a probe to see which tests hit the first branch
4. Added the second `if` condition
5. Noticed the probe count dropped (4 not 5) - had to think about why
6. Completed with `else x`
7. Ran tests to confirm

### What surprised me:
- **The probe count change**: When I added the second condition, I expected 5 values 
  but got 4. Understanding this requires knowing that probes only fire on executed paths.
  A human might find this confusing at first.

### What I didn't do that a human might:
- I didn't make any mistakes. I knew the algorithm from the start.
- I didn't try wrong operators like `<=` instead of `<`.
- I didn't forget parameter order or anything like that.

### Honest assessment:
This felt somewhat artificial. I was "playing" at being incremental rather than 
genuinely exploring. A real beginner would:
- Maybe try `<=` first and see different probe output
- Maybe confuse the parameter order (which is lo, which is hi?)
- Maybe forget the `else` case entirely
- Take longer to interpret the probe output

**[ANNOTATION]** The key insight from this exercise: **probes on conditions show 
control flow**, but interpreting "4 values instead of 5" requires understanding 
that unreached code doesn't produce samples. This is non-obvious and might need 
to be taught.

