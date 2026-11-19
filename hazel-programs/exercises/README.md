# Exercises-mode exercises (authored as text)

These `.hzt` files ARE the bundled **code** and **theorem** exercises of
Exercises mode. They are embedded at compile time (ppx_blob, listed in
`src/web/exercises/settings/ExerciseSettings_base.re`) and parsed by
`src/web/exercises/ExerciseText.re` at startup, the way the tutorial lessons
in `../tutorial/` are. Editing an exercise means editing its file and
rebuilding.

Exercise order is the order of the `sources` list in
`ExerciseSettings_base.re`, so **adding, removing or renaming** an exercise
means updating that list too: one `[%blob]` line per file.

Derivation exercises (`src/web/exercises/examples/Ex_EvaluationDerivation.ml`,
and the derivation slides of `src/web/derivation/examples/`) have no text
form yet: their proof trees are not a sequence of sections.

## Example

```
@title
Oddly Recursive

@flags
id=3335e34d-d211-4332-91e2-815e9e183885 required_tests=6 provided_tests=2

@points
test_validation=1 mutation_testing=1 impl_grading=2

@prompt
Write a recursive function that determines whether the given integer is odd.

@prelude
let not : Bool -> Bool =
  fun x ->
    if x then false else true
in ¿

@code
let odd: Int -> Bool =
  fun n ->
    ¿
in ¿

@your-tests
test not(false) end;
¿

@solution
let odd: Int -> Bool =
  fun x -> ...
in ¿

@bug always returns false
let odd: Int -> Bool =
  fun x -> false
in ¿

@test
test not(odd(0)) end;
test odd(1) end

@hints
zero

@syntax odd is recursive
(IsRecursive odd)
```

## Sections

A marker line, *exactly* `@name` (or `@name <hint>` for `@bug` and
`@syntax`), opens a section that runs to the next marker line. The syntax is
the tutorial lessons' (`HztText.re`); the blank lines around a program are
not part of it, its indentation is. Sections may come in any order, except
that `@bug`s and `@syntax`es keep theirs. Text before the first marker is
ignored.

| marker | code exercise field | notes |
|---|---|---|
| `@title` | `title` | required |
| `@flags` | `id`, `module_name`, `your_tests.required`, `your_tests.provided` | `id=<uuid>` (required), `module=<Name>`, `required_tests=N`, `provided_tests=N` |
| `@points` | `point_distribution` | `test_validation=N mutation_testing=N impl_grading=N` |
| `@prompt` | `prompt` | markdown |
| `@prelude` | `prelude` | shown read-only above the student's cells |
| `@code` | `your_impl` | the student's starting implementation (as in a tutorial lesson) |
| `@your-tests` | `your_tests.tests` | the student's starting tests; `provided_tests` counts the ones given |
| `@solution` | `correct_impl` | instructor-only (as in a tutorial lesson) |
| `@bug <hint>` | one of `hidden_bugs` | instructor-only; one section per bug, the hint on the marker line |
| `@test` | `hidden_tests.tests` | instructor-only (as in a tutorial lesson) |
| `@hints` | `hidden_tests.hints` | one hint per non-empty line, in test order |
| `@syntax <hint>` | one of `syntax_tests` | the predicate as an s-expression: `(IsRecursive f)`, `(IsNotRecursive f)`, `(IsTailRecursive f)`, `(VarApplied f)` |

A file with `@lemmas` or `@theorem` is a **theorem** exercise instead:
`@title`, `@flags` (`id=`, `module=`), `@points` (`max=N`), `@prompt`,
`@prelude`, `@lemmas`, `@theorem`.

- Programs are Hazel text as the editor prints it: holes are `¿` (see
  `../tutorial/README.md`), and the trailing `in ¿` of a prelude or
  implementation is where the next cell is stitched in.
- **The `id` keys the student's saved progress** (and the instructor's
  edits) in browser storage and in exported submissions. Never change it;
  give a new exercise a fresh one (`uuidgen | tr A-Z a-z`).
- `module=` is only needed when the module name (the name an instructor
  export is saved under) is not the one the filename gives:
  `oddly-recursive.hzt` is `Ex_OddlyRecursive`.
- A source that is malformed (no `id=`, an unknown flag, a bad number or
  predicate) fails at startup; the tests load every bundled exercise.

## Authoring in the app

In an instructor build (`make dev`), Exercises mode's File menu has
**Export Exercise Module**, which downloads the current exercise, with any
edits made in instructor mode, as a `.hzt` file in this format. Drop it in
here (and add it to `ExerciseSettings_base.re` if it is new).

## Tests

`test/Test_ExerciseText.re` checks that every file here loads and prints
back to itself, that the solution earns every point and the hidden tests
catch every hidden bug, and that the format round-trips.
