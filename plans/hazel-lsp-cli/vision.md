# Vision: Semantic Views for AI-Assisted Development

This document outlines the long-term direction for Hazel's CLI/LSP capabilities, focusing on providing AI agents with rich, semantically-augmented views of code.

## Core Concept: Augmented Source Views

The central idea is giving agents the ability to request **special-purpose views** on code. These views:

1. **Resemble actual source syntax** - familiar, readable representation
2. **Are augmented with derived semantic data** - types, errors, runtime values
3. **Can be structurally queried** - request specific definitions or subterms, not just line ranges

This parallels how probes augment source code with dynamic information - we extend this pattern to static information (types, errors) and structural navigation.

## View Augmentations

### Dynamic Information (Probes)
Already implemented via `^^probe(expr)` and `./hazel probe`:
```
let f = fun x -> ^^probe(x * 2) in f(3)
# Output: let f = fun x -> x * 2 ≡ 6 in f(3)
```

### Static Errors
Augment source with inline error markers using special brackets:
```hazel
let x = true in x + 3
# Augmented view:
let x = true in ⟦x⟧ + 3
               ↑ Type mismatch: expected Int, got Bool
```

Or with surrounding context:
```
Line 3: let x = true in ⟦x⟧ + 3
                        ~~~
        Error: Type mismatch: expected Int, got Bool
```

### Type Annotations
Since Hazel has bidirectional type inference, inferred types aren't always visible. Views could expose them:
```hazel
# Original:
let f = fun x -> x + 1 in f(5)

# With type annotations:
let f : Int -> Int = fun x : Int -> x + 1 in f(5) : Int
```

## Structural Queries

Beyond line-number ranges, agents can request code structurally:

### By Definition
```
> hazel view --definition "factorial"
let factorial : Int -> Int =
fun n ->
  if n == 0 then 1
  else n * factorial(n - 1)
```

### By Subterm with Context
```
> hazel view --around-error 1 --context-lines 2
  let x = 5 in
  let y = true in
  ⟦y⟧ + x        <- Error here

  Error: Type mismatch...
```

### By Structural Path
```
> hazel view --path "let.body.case.branch[0]"
| [] => 0
```

## Command Design Options

Could be multiple commands or a single command with options:

```bash
# Option A: Multiple commands
./hazel view --definition "foo"
./hazel errors --inline
./hazel types --annotate

# Option B: Single command with view options
./hazel view program.hz --errors --types --probes
./hazel view program.hz --definition "foo" --with-types
```

Key options to support:
- `--line-numbers` / `--no-line-numbers`
- `--errors` (inline error markers)
- `--types` (show inferred types)
- `--probes` (show probe values)
- `--definition NAME` (extract specific definition)
- `--context-lines N` (lines around errors/selections)

## Bidirectional Editing (Future)

A more ambitious extension: views that support targeted edits.

Instead of rewriting entire files, agents issue structural edit actions:
```bash
# Edit a specific definition
./hazel edit --definition "factorial" --replace "..."

# Fix an error at a location
./hazel edit --at-error 1 --replace "x : Int"

# Rename across scope
./hazel edit --rename "oldName" "newName" --in "moduleName"
```

This prevents agents from having to regenerate entire files and reduces the chance of unintended changes.

## Implementation Approach

### Phase 1: Rich Error Views (Current)
- [x] Line numbers in errors
- [x] Show erroneous term
- [ ] Inline error markers in source
- [ ] Context lines around errors

### Phase 2: Type Views
- [ ] Show inferred types on demand
- [ ] Type annotations at bindings
- [ ] Type at cursor/selection

### Phase 3: Structural Queries
- [ ] Extract definition by name
- [ ] Extract subterm by path
- [ ] Context-aware excerpts

### Phase 4: Edit Actions
- [ ] Replace definition
- [ ] Targeted fixes
- [ ] Structural refactoring

## Design Principles

1. **Familiarity** - Output should look like Hazel code, just augmented
2. **Composability** - Options combine naturally (errors + types + line numbers)
3. **Structural over textual** - Prefer structural queries when possible, but support line ranges too
4. **Reversibility** - Augmented views should be parseable back (or clearly marked as display-only)
5. **Progressive disclosure** - Simple commands for common cases, options for power users

## Related Work

- LSP (Language Server Protocol) - provides similar capabilities but via JSON-RPC
- Tree-sitter queries - structural code queries
- Sourcegraph - code intelligence and navigation
- GitHub Copilot workspace - AI-assisted code views

The key differentiator for Hazel is that our structure-aware parser and typed holes enable richer semantic information even for incomplete/erroneous programs.
