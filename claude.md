# Claude Worktree Info

## Quick Reference

**Worktree path**:
```
/Users/andrewblinn/.claude-worktrees/hazel/sorted-insertion-modules
```

**Browser URL**: http://localhost:8002

**To open in editor**:
```bash
code ~/.claude-worktrees/hazel/sorted-insertion-modules
# or
cursor ~/.claude-worktrees/hazel/sorted-insertion-modules
```

---

## Current Session

**Branch**: `sorted-insertion-modules` (based on `sorted-insertion`)

**Task**: Phase 1.1 - Module Syntax Foundation

**Status**: Starting

---

## Why This Setup Works

- **Separate `_build/` directories**: Each worktree has its own `_build/`, so Dune's lock files don't conflict.
- **Separate ports**: You use `make serve` (port 8000), I use port 8002.
- **Independent work**: Git worktrees share the same repo but have separate working directories.
