# Small Step Evaluation and Evaluation Filter

## Small Step Evaluator

Prior work: [PR #462](https://github.com/hazelgrove/hazel/pull/462)

See
[POPL'19 Live Functional Programming with Typed Holes](https://doi.org/10.1145/3291622)
for semantics on the small step evaluator.

PR #462 use substitution for function application, while Hazel 3 turns
to a environment based evaluator.
[PR #902](https://github.com/hazelgrove/hazel/pull/902) adapted the
older small step evaluator to Hazel 3, i.e. use environment for
evaluation. The adaptation work is mostly done by now (Apr, 2023), but
there are many possible improvements, both on the semantics and the UI.

### Semantics

The updated small step evaluator deploys the environment-based
evaluation scheme, which means we have to consider environment during
decomposition and composition. The duplication of closure can be
eliminated by careful consideration, but due to the lack of binding
information, we cannot really solve the problem of nested closure. For
example:
```ocaml
let a = ... in
let b = ... in
let c = ... in
let y = fun x -> x in
let z = y(fun x -> x) in
```
We noticed that in the closure of the second function literal
`fun x -> x`, there is an entry from variable `y` to closed function
literal `Closure(env_y, fun x -> x)`. These two closure both contains
variable `a, b, c, d` and can be somehow shared.

This problem can be partially (or mostly) solution by close the function
literal during elaboration, but this should better goes directly into
*the* evaluator, not the small-step one.

### User Interface

For now the UI is very primitive, and we cannot really handle overlapped
steps. This is OK with current semantics, but if we want to allow user
goes beyond the call-by-value or strict evaluation, we might need more
flexible UI. For example:
```ocaml
let id = fun x -> x in
(id (fun x -> id (x))) (3)
```
User can choose one of the following options:
1. Evaluate `id (x)`, turning the expression into
```ocaml
(id (fun x -> x)) (3)
```
2. Evaluate `id (fun x -> id (x))`, turning the expression into
```ocaml
(fun x -> id (x)) (3)
```
3. User cannot evaluate the outer-most function application.

We noticed that option 1 and option 2 are overlapped -- they both
contains the region `id (x)`. One possible approach is to only set the
*operator* as the clickable area. This seems can be easily done to
binary operators like `+` or `*`, but might be difficult for operator
like function application.

## Evaluation Filter

Based on the small-step evaluator, we enable user to choose whatever
order they would like to evaluate their expressions in. However, it
might be tedious to do all the stepping by hand, and it might be helpful
to skip the stepping for some *kind* of expressions entirely.

This is an ongoing PR working on this:
[PR #1006](https://github.com/hazelgrove/hazel/pull/1006)

There many ways to design the filter language. There many famous
examples, like:
- SQL for database
- Rules for network packet (iptables or the wireshark filter)
- Regex for string (sort of)

There are some common patterns in these languages, i.e.
- They both have match part:
  - The `WHERE` clause in SQL
  - The `ipv4.src == 192.168.1.1` in the Wireshark filter
  - The matching group `\(...\)` in regex
- Most of them have (some sort of) action:
  - `DROP TABLE`, `SELECT`, etc.
  - `DROP`, `FORWARD` in iptables rule.
  - default dropped, but use `\1` to add the matched group back.
- Some of them have mechanism of breaking ties or selecting:
  - "maximal munch" in parsing.
  - "function overloading" in C++

The most important application of the filter language in functional
programming is pattern matching. It matches things, allow user to write
code on certain condition. It solve the problem of possible multiple
matching by:
- Arrange priority from top to down, i.e. the pattern appears the first
   has the highest priority.
- Use static checking to ensure that there are no redundant pattern
   and no unspecified cases.

The filter language I have in my mind, matches against the source
program, issue command for change the evaluation mode (big-step or
small-step, lazy or eager), and a narrower filter has a higher priority.

For the matching part, I choose the Hazel language it's self as the
pattern language. `UPat` and `DHPat` won't work since they only matches
against *values*, indicating I have to extend them somehow so that they
can match against *expressions*. The empty hole is take as the
match all filter, i.e. `*` in many other matching languages. It will
make much more sense to use `_` as the pattern, but this seems to be
used by Alan Yang in this PR to add partial application to Hazel
([PR #957](https://github.com/hazelgrove/hazel/pull/957)).

Stepping (step through every reduction step during the evaluation) and
skipping (directly evaluate the expression to value) are the only two
actions visible to users. Since the third filter Pausing is use
internally, it might be necessary to prove the evaluation process with
filters are always making progress.

The tie-breaking is done by specifying that the inner-most filter has
the highest priority. The overlapping pattern is the trickiest in
specifying the semantics of the filter. In those case, blending the
effect of the two filter is more appropriate than just picking the one
with higher priority, hence the difficult in distributing sub-filter,
etc.

The semantics of the filter language can be implemented statically
or dynamically. Statically we have all the binding and type information,
which means we can match against the type of a variable, etc;
Dynamically we can match against the value of a variable, etc. Now the
semantics is implemented dynamically and the future plan is to have both
of them effective.
