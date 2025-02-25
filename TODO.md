# REFACTOR:

## TypeSlice.re: 
As compared to `witnesses` branch:
- [x] Remove Join type
- [x] Add global code slice and context slices (for ana context): `type slice_ana'
- [x] Impement in direct correspondance to Slice.re
- [x] Implement as a SUM TYPE. With empty slices being exactly as Typ.
- [x] Therefore: we can combine slices and types in the same tree to get partially computed slices.
- [ ] Add syntactic destructuring functions (see in Unboxing.re) to TypSlice.re
- [ ] Simplify and remove redundancy from join_using
- [ ] Remove slc_incr
- [ ] Preserve global slices on variables during substitution

## Use of Typ:
- [x] Replace all (relevant) use of Typ with TypSlice.
- [ ] Decide on use of Typ vs TypSlice in constructor maps

## Self.re:
- [x] Term id logic for synthesis slices
- [x] Ctx slice logic for synthesis slices
- [x] Display ctx slices.
- [ ] Don't highlight let bindings when var left unused.

## Mode.re:
- [x] Term id logic for analysis slices _(bugchecking todo)_
- [x] Ctx slice logic for analysis slices

## Info.re:
- [ ] Add slice-related functions. i.e. getting synthesis slice, analysis slice (from mode and self respectively) and fixed slice (real `Info.ty`).
- [ ] Also add slice with no syn switches??
- [ ] Calculate ctx_used in typslices from Co_ctx and Ctx?

## Statics.re & Elaboration.re:
- [ ] Separate use of Slice.re to allow disabling. i.e. using Typ only. Could do this by passing a bool through (also allows calculating slices only for specific _regions_ of the code: which could be linked directly to the). Or could create a (let.slice) binding operator which skips slice computation dependent on bool. _Make sure this still maintains the old slice!_
- [x] Separate Self.annot into utyp\_to\_info\_map

## Parsing
- [ ] Make decision on full use of TypSlice or use of Typ for this stage
- [x] Attach slices correctly to types/casts in this stage if using TypSlice.
- [ ] Type aliases should be slices. Currently converted in Statics.re.

## Other
- [ ] Remove redundancy in uinfo_of_typ and also consider using Typ.t for Info.typ
- [ ] More ergonomic use of TypSlice (better versions of map & apply)
- [ ] Improve performance: Likely issues due to overuse of TypSlice.typ_of? Hopefully not due to TypSlice.wrap_global or wrap_incr.....

# SEARCH PROC/INDET EVAL:
- [ ] Cast Transitions for terms like 0 : Int -> ? -> Int, (or otherwise 0 : ? -> Int)
- [ ] Insert the correct casts when instantiating terms like an (Int, Int) to (?, ?), i.e. a cast from (?, ?) -> ? -> (Int, Int). Also make sure cast transitions work for these
- [ ] Deal with holes with no immediate cast. These seem to appear during pattern matching expressions i.e. in the unnanotated map function, casts are instead placed on the branches (it might be equivalent placing on scrutinant)
- [ ] Implement Sum type instantiation
- [X] Check that closure substitution during instantiation doesn't replace hole ids
- [X] Check that function application etc. doesn't replace hole ids
- [X] Check that instantiations work with pattern matches (i.e. let x::y = ? in x)
- [ ] Analyse how hole substitute could explode the stack when closures are deeply nested (i.e. in FixF terms)
- [ ] Add depth limits to search (i.e. iterative deepening)
- [ ] Implement Type Function instantiation

## Bugs
### High Priority
- [X] Types in stepper share ids which messes with selecting them/cursor movement
- [X] Multiple holes not instantiated, i.e. ? + ?
- [ ] Cast slicing in stepper does not highlight in editor
- [ ] FAILURE: Not a product in `let (x, y) = ? in x`
- [ ] Unboxing bug when annotating with explicit dynamic type ?, i.e. in: 
```let x : ? = [1] in
case x
  |[] => true
  | _ => false
end```

## Middle Priority
- [ ] Constructor names not highlighted in slices
- [ ] Unbound vars not instantiated in IndetEvaluator

## Low Priority
- [ ] Deferrals: See failed test
- [X] Parentheses not highlighted in slices (likely due to type normliasation?)
- [ ] Type application not highlighted in slice, e.g: let f : forall A -> A -> A = typfun B -> fun x -> x in f@<Int>**(** 2 **)**. This is because (probably): type slice substitution drops all slices (as `Var(_)` appears only in Typ. and Typ.subst is used here)
- [X] Cast slice stack overflows see: `let f : forall A -> A -> A = typfun B -> fun x -> x in f@<Int>(2)`
