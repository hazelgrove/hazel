# REFACTOR:

## TypeSlice.re: 
As compared to `witnesses` branch:
- [x] Remove Join type
- [x] Add global code slice and context slices (for ana context): `type slice_ana'
- [x] Impement in direct correspondance to Slice.re
- [x] Implement as a SUM TYPE. With empty slices being exactly as Typ.
- [x] Therefore: we can combine slices and types in the same tree to get partially computed slices.
- [x] Add syntactic destructuring functions (see in Unboxing.re) to TypSlice.re
- [ ] Simplify and remove redundancy from join_using
- [ ] Preserve global slices on variables during substitution

## Use of Typ:
- [x] Replace all (relevant) use of Typ with TypSlice.
- [ ] Decide on use of Typ vs TypSlice in constructor maps

## Self.re:
- [x] Term id logic for synthesis slices
- [x] Ctx slice logic for synthesis slices
- [x] Display ctx slices.
- [x] Don't highlight let bindings when var left unused.

## Mode.re:
- [x] Term id logic for analysis slices _(bugchecking todo)_
- [x] Ctx slice logic for analysis slices

## Info.re:
- [x] Add slice-related functions. i.e. getting synthesis slice, analysis slice (from mode and self respectively) and fixed slice (real `Info.ty`).
- [ ] Calculate ctx_used in typslices from Co_ctx and Ctx?

## Statics.re & Elaboration.re:
- [ ] Separate use of Slice.re to allow disabling. i.e. using Typ only. Could do this by passing a bool through (also allows calculating slices only for specific _regions_ of the code: which could be linked directly to the). Or could create a (let.slice) binding operator which skips slice computation dependent on bool. _Make sure this still maintains the old slice!_
- [x] Separate Self.annot into utyp\_to\_info\_map

## Parsing
- [ ] Make decision on full use of TypSlice or use of Typ for this stage
- [x] Attach slices correctly to types/casts in this stage if using TypSlice.
- [x] Type aliases should be slices. Currently converted in Statics.re.

## Other
- [ ] Remove redundancy in uinfo_of_typ and also consider using Typ.t for Info.typ
- [ ] More ergonomic use of TypSlice (better versions of map & apply)

# Slicing
- [X] Add slices to inserted casts to least specific compound types, i.e. [ ]
- [x] HIGH PRIORITY: UI for synthesis slices AND analysis slices AND their joins.
- [ ] Allow turning slicing off.


## Bugs
### High Priority
- [X] Types in stepper share ids which messes with selecting them/cursor movement
- [ ] Cast slicing in stepper does not highlight in editor
- [x] FAILURE: Not a product in `let (x, y) = ? in x`

## Middle Priority
- [x] Constructor names not highlighted in slices

## Low Priority
- [X] Deferrals: See failed test
- [X] Parentheses not highlighted in slices (likely due to type normliasation?)
- [ ] Type application not highlighted in slice, e.g: let f : forall A -> A -> A = typfun B -> fun x -> x in f@<Int>**(** 2 **)**. This is because (probably): type slice substitution drops all slices (as `Var(_)` appears only in Typ. and Typ.subst is used here)
- [X] Cast slice stack overflows see: `let f : forall A -> A -> A = typfun B -> fun x -> x in f@<Int>(2)`
- [ ] Forall Type Checking failure (see failed test)
- [ ] Labeled Tuple Elaboration failures (see tests)
- [ ] Pattern NoTyp(FreeConstructor) unnecessarily acting (see tests)
