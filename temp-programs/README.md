# Pretty Printer Test Programs

## Formatting Command

```bash
# From repo root:
node _build/default/src/CLI/cli.bc.js format -w WIDTH INPUT_FILE

# Example: format emojipaint.hz at width 60, save to .fmt.hz
node _build/default/src/CLI/cli.bc.js format -w 60 temp-programs/emojipaint.hz > temp-programs/emojipaint.fmt.hz
```

- `WIDTH`: target line width (default 80; editor uses 60)
- Stdin (`-`) is broken in js_of_ocaml; always use file input
- Build first with `dune build` if source has changed

## Files

- `*.hz` — original source programs
- `*.fmt.hz` — formatted output from the CLI
