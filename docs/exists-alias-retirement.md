# Retired builtin alias `exists(list, predicate)` (D83)

Decision: D83, panel answer `dq-D83-5ccc282c` (quantifier). `exists` is the existential
keyword (`exists (x : T) -> P`), so the list builtin alias `exists`, which was defined as
`any`, is removed from `src/language/builtins/BuiltinsList.re`.

## Migration

Replace `exists(xs, p)` with `any(xs, p)`. The behavior is identical (the alias was
implemented as `any`).

| Before (retired) | After |
|---|---|
| `exists([1, 2], fun (x : Int) -> x > 1)` | `any([1, 2], fun (x : Int) -> x > 1)` |

`exists2` / `any2`, `all` / `for_all` and the other aliases are unchanged.

## Uses migrated

None. A targeted search of the tracked library, docs, fixtures and examples found no
actual Hazel call to `exists(list, predicate)`. The only occurrence is the intentional
retired negative control: `alias_call` in `test/Test_ResetExistentials.re`, and the new
`test/Test_ResetExistsAliasRetirement.re`.

The search excluded:
- Reason/OCaml `List.exists` and other utility functions named `exists`;
- `Exp`/`Pat` constructors and the existential syntax itself;
- historical reports.

## Hint

**Omitted.** There is no existing diagnostic path that maps a removed builtin name to a
replacement. The old call now parses as an `exists` expression and is rejected by statics.
This file documents the migration instead.
