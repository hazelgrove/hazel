# Study Programs

Programs for user studies on Hazel's probe debugging mechanism.

Each program has two versions:
- `program.hz` - Working version with comprehensive tests
- `program-bug.hz` - Buggy version with minimal failing test(s)

## Programs

| Program | Lines | Bug Difficulty | Description |
|---------|-------|----------------|-------------|
| emojipaint | ~150 | Easy (1 char) | MVU paint app with emoji brush, grid operations |

## Bug Difficulty Scale

- **Easy**: Single character or token fix
- **Medium**: 1-2 lines in one location
- **Hard**: Multiple locations or subtle logic

## Debugging with Probes

1. Run `./hazel test program-bug.hz` to see failing test
2. Add `^^probe(expr)` to inspect values
3. Run `./hazel probe program-bug.hz` to see probe output
4. Use `--many` flag to see all samples: `./hazel probe -m program-bug.hz`

See `plans/study-programs.md` for full documentation.
