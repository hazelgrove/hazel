# CLI Auto-Probe Proposal

## Proposed API

```bash
# Current behavior - only shows manually placed ^^probe() wrappers
./hazel probe program.hz

# Proposed: --auto flag enables auto-probe for entire program
./hazel probe --auto program.hz
./hazel probe -a program.hz

# Can combine with --many for multiple samples
./hazel probe --auto --many program.hz
```

## Mockup: What Output Would Look Like

### Input: clamp.hz (without any ^^probe wrappers)

**Note**: Tests are formatted with line breaks to expose intermediate values (see "Test expression formatting" above).

```hazel
let clamp = fun (x, lo, hi) ->
  if x < lo then lo
  else if x > hi then hi
  else x
in

test
  clamp(5, 0, 10)
  == 5
end;

test
  clamp(-3, 0, 10)
  == 0
end;

test
  clamp(15, 0, 10)
  == 10
end
```

### Current Output (no --auto)

```
$ ./hazel probe clamp.hz
```
No probe values shown - no manual `^^probe()` wrappers in the code.

### Proposed Output (with --auto --many)

```
$ ./hazel probe --auto --many clamp.hz

let clamp = fun (x, lo, hi) ->
  if x < lo then lo     ≡ false ⫽ true ⫽ false
  else if x > hi then hi     ≡ false ⫽ true
  else x     ≡ 5 ⫽ 10
in

test
  clamp(5, 0, 10)     ≡ 5
  == 5     ≡ true
end;

test
  clamp(-3, 0, 10)     ≡ 0
  == 0     ≡ true
end;

test
  clamp(15, 0, 10)     ≡ 10
  == 10     ≡ true
end
```

**What this shows:**
- Function body: multiple samples (one per test invocation) showing which branch taken
- Test expressions: single value each (tests at top level run once)
- `clamp(...)` line: actual return value
- `== expected` line: whether test passes

**Note**: Auto-probe uses the "one probe per line" heuristic. The exact expressions probed depend on the AutoProbe module's selection logic, which prefers:
- Rightmost ending position on each line
- Largest term at that position
- Avoids holes, function-typed terms, redundant parens

### Another Example: base_route

```hazel
let base_route = fun path ->
  let parts = string_split("/", path) in
  nth(parts, 1)
in

test
  base_route("/api/v1")
  == "api"
end
```

With `--auto --many`:

```
$ ./hazel probe --auto --many base_route.hz

let base_route = fun path ->
  let parts = string_split("/", path) in     ≡ ["", "api", "v1"]
  nth(parts, 1)     ≡ "api"
in

test
  base_route("/api/v1")     ≡ "api"
  == "api"     ≡ true
end
```

The line-break formatting gives us:
- What `base_route` actually returned
- Whether it matched the expected value

---

## Important Notes on Auto-Probe Behavior

### Line breaks determine probe placement

Auto-probe uses a "one probe per line" heuristic - it probes the **terminal expression** on each line. This means:

- Where you put line breaks determines what gets probed
- If you want to see a sub-expression's value, it needs to be the last thing on its line
- Multi-expression lines will only show the rightmost/outermost value

### GUI placement logic vs "probe everything"

In the web UI, there's logic that when you auto-probe a let definition, it goes to just the definition expression (not the body). This makes sense for focusing on one definition at a time.

For CLI `--auto` mode, we want to probe **everything** - all top-level definitions plus trailing expressions. This means we may need to:
- Call a slightly lower-level function than the GUI uses
- Or call `ids_to_autoprobe` on the root of the entire program (which should traverse everything)
- Bypass the "focus on one definition" placement logic

### Test expression formatting

The `test expr end` form returns **unit**, not the boolean result. So auto-probe on a test line won't show true/false unless formatted carefully.

**Problem format** (one line):
```hazel
test clamp(5, 0, 10) == 5 end
```
Auto-probe would probe the whole test expression, which returns `()`.

**Better format** (line breaks expose intermediate values):
```hazel
test clamp(5, 0, 10)
  == 5
end
```
This gives THREE probed lines:
1. `clamp(5, 0, 10)` → shows actual result (e.g., `5`)
2. `== 5` → shows comparison result (`true` or `false`)
3. `end` → (no useful value, but test is recorded)

This format provides both:
- What the actual result was (useful if it differs from expected)
- Whether the test passed

---

## Implementation Plan

### Step 1: Understand the data flow

Current `probe_hazel` function in `Cli.re` (lines 349-399):
1. Parse to zipper
2. Get `refractors.manuals` (manual probes from ^^probe syntax)
3. Get `refractors.autos.ephemerals` (currently empty for CLI since no UI interaction)
4. Build `sample_map` from combined probe IDs
5. Evaluate with probe targets
6. Format output with `ProbeText.of_segment`

For `--auto`, we need to insert step 2.5:
- Call `AutoProbe.ids_to_autoprobe` to get IDs for the whole program
- Add those IDs to the probe set

### Step 2: Proposed code change

In `src/CLI/Cli.re`, modify `probe_hazel`:

```reason
let probe_hazel = (auto: bool, many: bool, path: string): unit => {
  let program = read_input(path);
  switch (parse_to_zipper(program)) {
  | None => prerr_endline("Failed to parse program")
  | Some(zipper) =>
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term;

    open Util;
    open Language;

    /* Run statics */
    let info_map =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);

    /* Get manual probe IDs */
    let manual_ids = Id.Map.map(_ => (), zipper.refractors.manuals);

    /* If --auto, compute auto-probe IDs */
    let auto_ids = if (auto) {
      /* Build syntax cache for AutoProbe */
      let syntax = Haz3lcore.CachedSyntax.mk(zipper, ~info_map, ~dyn_map=Id.Map.empty);
      let root_id = Haz3lcore.Segment.root_id(
        Haz3lcore.Segment.skel(segment),
        segment
      );

      switch (Haz3lcore.AutoProbe.ids_to_autoprobe(
        root_id,
        syntax.term_data,
        syntax.terms,
        syntax.measured,
        info_map,
      )) {
      | Some(ids) =>
        List.fold_left(
          (acc, id_opt) => switch (id_opt) {
            | Some(id) => Id.Map.add(id, (), acc)
            | None => acc
          },
          Id.Map.empty,
          ids
        )
      | None => Id.Map.empty
      };
    } else {
      Id.Map.empty;
    };

    /* Combine manual and auto probes */
    let probe_ids = Id.Map.union((_, _, _) => Some(), manual_ids, auto_ids);

    /* Build sample map and evaluate */
    let sample_map =
      Haz3lcore.CachedStatics.compute_targets(
        ~settings=CoreSettings.on,
        ~info_map,
        ~probe_ids,
      );
    let (_, sample_map) = Run.evaluate_with_probe_map(~sample_map, term);

    /* Format output */
    let window = many ? Sample.Window.Many : Sample.Window.Single;

    /* For auto-probe, we need to pass the auto IDs as refractors for rendering */
    let refractors = if (auto) {
      /* Build a refractor map that includes auto IDs */
      Id.Map.fold(
        (id, (), acc) => Id.Map.add(id, Haz3lcore.Refractors.default_entry, acc),
        auto_ids,
        zipper.refractors.manuals
      );
    } else {
      zipper.refractors.manuals;
    };

    let output =
      Haz3lcore.ProbeText.of_segment(
        ~window,
        ~probe_map=sample_map,
        ~refractors,
        segment,
      );
    print_endline(output);
  };
};
```

### Step 3: Add CLI flag

In `probe_cmd`:

```reason
let probe_cmd = {
  let doc = "Run a Hazel program and display probe values inline.";
  let many_arg = {
    let doc = "Show multiple sample values per probe (many mode).";
    Arg.(value & flag & info(["many", "m"], ~doc));
  };
  let auto_arg = {
    let doc = "Auto-probe all expressions (one per line).";
    Arg.(value & flag & info(["auto", "a"], ~doc));
  };
  let info = Cmd.info("probe", ~doc);
  Cmd.v(info, Term.(const(probe_hazel) $ auto_arg $ many_arg $ input_arg));
};
```

### Step 4: Handle ProbeText rendering

The `ProbeText.of_segment` function needs to know which IDs have probes to render values for them. Currently it uses the `refractors` map.

**Refractor entry format for probes:**
```reason
{
  kind: ProjectorCore.Kind.Probe,
  model: "()"  /* unit serialized as sexp */
}
```

The probe projector model is `type model = unit`, so the serialized model is just `"()"`.

---

## Questions/Decisions

1. **Should `--auto` include or exclude manual probes?**
   - For now: Use either `--auto` OR manual `^^probe()` wrappers, not both
   - Don't worry about interaction between them for initial implementation

2. **What about test expressions?**
   - AutoProbe currently operates on a "root term"
   - For a program with multiple top-level definitions + tests, we need to probe all of them
   - May need to iterate over all top-level terms

3. **Should there be a way to auto-probe just one definition?**
   - Could add `--auto-def <name>` to probe a specific definition
   - Matches the web UI's "auto-def" mode behavior
   - Lower priority for initial implementation

4. **Output alignment**
   - Current ProbeText aligns values with the probed expression
   - With many probes, output could get visually dense
   - May want to consider alternative formatting for CLI (e.g., separate section for values?)

---

## Files to Modify

1. `src/CLI/Cli.re` - Main changes to `probe_hazel` function and `probe_cmd`
2. Possibly `src/haz3lcore/ProbeText.re` - If rendering needs adjustment for auto IDs

## Dependencies

The implementation relies on:
- `AutoProbe.ids_to_autoprobe` (already exists)
- `CachedSyntax.mk` (already exists)
- `CachedStatics.compute_targets` (already exists)
- `ProbeText.of_segment` (may need adjustment)

## Testing

1. Run on existing study programs to verify output looks reasonable
2. Compare auto-probe selections to web UI to ensure consistency
3. Test edge cases: empty programs, programs with holes, multi-definition programs
