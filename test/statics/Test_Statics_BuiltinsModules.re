open Language;
open Test_Statics_Prelude;
open FTemp;
open Typ;

/* Signature-type builders, as in Test_Statics_Modules: items are compared
   positionally, so they are listed in source order. */
/* The standard library's modules and instances, from the type side: the
   signature a module builtin carries is derived from the same records as its
   value, so what these check is that the signature reaches the program, that
   a member's type is the function's type, and that the shipped instances say
   enough about themselves for resolution to pick them. */

let tests = (
  "Statics.BuiltinsModules",
  [
    fully_consistent_typecheck(
      "A member's type is the function's type",
      {|List.length|},
      Some(arrow(list(unknown(Internal)), int())),
    ),
    fully_consistent_typecheck(
      "A member is applied like the flat function",
      {|(List.length([1, 2]), String.length("ab"), Int.abs(-1))|},
      Some(prod([int(), int(), int()])),
    ),
    fully_consistent_typecheck(
      "A constant member keeps its type",
      {|Float.pi|},
      Some(float()),
    ),
    fully_consistent_typecheck(
      "A conversion member reads as a member",
      {|(String.of_int(1), Int.of_string("1"), Float.of_int(1))|},
      Some(prod([string(), int(), float()])),
    ),
    inconsistent_typecheck(
      "A member is checked against its type",
      parse_exp({|String.length(1)|}),
    ),
    has_mark_test(
      "A member the module does not have is reported",
      {|String.nosuch("a")|},
      fun
      | Mark.ModuleMemberNotFound(_) => true
      | _ => false,
    ),
    /* The signatures are type aliases, so a program can annotate with them
       and write its own instances against them. */
    /* An annotation keeps the alias it was written with; that these are
       accepted as module parameter types at all is what says they are
       signatures. */
    fully_consistent_typecheck(
      "SHOW and ORD are in scope as signature types",
      {|let f = fun (s : SHOW, o : ORD) -> 1 in f|},
      Some(arrow(prod([var("SHOW"), var("ORD")]), int())),
    ),
    fully_consistent_typecheck(
      "A signature alias names its members",
      {|fun (s : SHOW) -> s.show|},
      Some(arrow(var("SHOW"), arrow(escaped("s.T"), string()))),
    ),
    /* An instance is an ordinary module until a program marks it implicit;
       its type member is manifest, which is what lets resolution choose it
       by the argument's type. */
    fully_consistent_typecheck(
      "A shipped instance is a module with a manifest type member",
      {|ShowInt.show|},
      Some(arrow(int(), string())),
    ),
    fully_consistent_typecheck(
      "A shipped instance resolves once it is marked implicit",
      {|let implicit A = ShowInt in
        let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in
        show(3)|},
      Some(string()),
    ),
    /* Until then it is not a candidate: nothing in the standard library is
       ambient, so a program that defines its own instances is not made
       ambiguous by ours. */
    has_mark_test(
      "A shipped instance is not a candidate until marked",
      {|let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in show(3)|},
      fun
      | Mark.ImplicitNotFound({candidates: [], _}) => true
      | _ => false,
    ),
  ],
);
