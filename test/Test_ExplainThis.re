open Haz3lcore;
open Language;

module ET = Web.ExplainThis;

/* Property-based test ensuring ExplainThis never raises while producing
   documentation for any sub-term of an expression. The documentation for
   each form substitutes term ids into its explanation string via a format,
   so a mismatch between the number of `%s` placeholders and the number of
   supplied arguments crashes at runtime. This test guards against that. */

let globals = Web.Globals.Model.init(~settings=Web.Settings.Model.init, ());
let docs = Web.ExplainThisModel.init;

let statics = term =>
  fst(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    ),
  );

let qcheck_explainthis_does_not_crash =
  QCheck.Test.make(
    ~name="ExplainThis.decide does not crash",
    ~count=1000,
    QCheck_Util.arb_exp(~minimal_idents=true, 12),
    exp => {
    /* Statics failures are out of scope; we only assert that ExplainThis
       itself does not raise for any sub-term it is asked to document. The
       color map is harvested too, since that is where the explanation's
       markdown is parsed. */
    switch (statics(exp)) {
    | exception _ => true
    | info_map =>
      Id.Map.iter(
        (_id, info: Info.t) => {
          let _ = ET.color_map_of(~globals, ET.decide(~docs, Some(info)));
          ();
        },
        info_map,
      );
      true;
    }
  });

/* The ExplainThis section title and the cursor inspector label both come
   from Info.cls_label, which must reflect the statics-re-kinded negation
   op rather than the user term's (always-Int) op. */
let unop_label = (program: string): option(string) => {
  let exp =
    switch (Haz3lcore.Parser.to_term(program, ~root=Exp)) {
    | Some(e) => e
    | None => Alcotest.fail("Failed to parse expression: " ++ program)
    };
  Id.Map.fold(
    (_id, info: Info.t, acc) =>
      switch (acc, Info.cls_of(info)) {
      | (None, Exp(UnOp(_))) => Some(Info.cls_label(info))
      | _ => acc
      },
    statics(exp),
    None,
  );
};

let negation_labels = () => {
  Alcotest.(check(option(string)))(
    "float negation label",
    Some("Float Negation"),
    unop_label("-1.5"),
  );
  Alcotest.(check(option(string)))(
    "integer negation label",
    Some("Integer Negation"),
    unop_label("-5"),
  );
};

/* ===================== Characterization (golden) test =====================

   The property test above only asserts that `decide` doesn't raise. Nothing
   pinned down *which* sub-term each explanation actually links to, which is
   exactly what a refactor of the coloring/specificity plumbing moves around.

   The color map is no use as a fingerprint: it is re-derived from the
   explanation's markdown links and never reads `colorings` at all, so it is
   blind to the very mapping being moved. `decide` returns the decision itself —
   group, selected form, and colorings — with no rendering in the way.

   `Id.t` is a UUID, freshly generated per parse, so raw ids can't appear in a
   golden. Each id is canonicalized to the printed text of the sub-term it
   belongs to, looked up in the statics info map. */

let print_any = (any: Any.t): string =>
  switch (
    any
    |> ExpToSegment.any_to_segment(
         ~settings=ExpToSegment.Settings.editable(~inline=true),
       )
    |> Printer.of_segment(~holes="?", _)
  ) {
  | s => s
  | exception _ => "<unprintable>"
  };

let canonical_id = (info_map, id: Id.t): string =>
  switch (Id.Map.find_opt(id, info_map)) {
  | Some(info) =>
    switch (Info.any_of(info)) {
    | Some(any) => print_any(any)
    | None => "<secondary>"
    }
  | None => "<unmapped>"
  };

/* Only a `Doc` decision names a group and a form; prose and derivation terms
   carry no colorings to characterize. */
let doc_of = (~docs, info: Info.t): option(ET.doc) =>
  switch (ET.decide(~docs, Some(info))) {
  | ET.Doc(d) => Some(d)
  | ET.NoDoc
  | ET.Prose(_)
  | ET.Markdown(_)
  | ET.DrvSyntax(_) => None
  };

let render_doc = (info_map, d: ET.doc): string => {
  let colorings =
    d.colorings
    |> List.map(~f=((_sf_id, code_id)) => canonical_id(info_map, code_id))
    |> List.sort(~compare=String.compare)
    |> String.concat(~sep=",");
  Web.ExplainThisForm.show_form_id(d.form.id)
  ++ " colorings=["
  ++ colorings
  ++ "]";
};

/* `ExplainThisModel.init` records no group selections, so
   `get_selected_option` always returns the *most specific* form — meaning a
   harness built only on `init` never reaches a single fallback form, which is
   exactly what the specificity ladders select. So for each documented sub-term
   we sweep every form its group offers by planting a selection for it. */
let swept_docs = (info: Info.t): list(ET.doc) =>
  switch (doc_of(~docs, info)) {
  | None => []
  | Some(d) =>
    d.group.forms
    |> List.filter_map(~f=(form: Web.ExplainThisForm.form) => {
         let docs': Web.ExplainThisModel.t = {
           ...Web.ExplainThisModel.init,
           groups: [
             {
               group: d.group.id,
               selected: form.id,
             },
           ],
         };
         doc_of(~docs=docs', info);
       })
  };

let fingerprint_of_info = (info_map, info: Info.t): list(string) =>
  switch (swept_docs(info)) {
  | [] => ["(no group doc)"]
  | ds => List.map(~f=render_doc(info_map), ds)
  };

let info_map_of = (src: string) =>
  switch (Haz3lcore.Parser.to_term(src, ~root=Exp)) {
  | Some(e) => statics(e)
  | None => failwith("corpus entry failed to parse: " ++ src)
  };

let doc_fingerprint = (src: string): string => {
  let info_map = info_map_of(src);
  Id.Map.fold(
    (_id, info: Info.t, acc) =>
      switch (Info.any_of(info)) {
      | None => acc /* Secondary — no doc */
      | Some(any) =>
        let cursor = print_any(any);
        List.map(
          ~f=l => cursor ++ " => " ++ l,
          fingerprint_of_info(info_map, info),
        )
        @ acc;
      },
    info_map,
    [],
  )
  |> List.dedup_and_sort(~compare=Poly.compare)
  |> String.concat(~sep="\n");
};

/* Each entry is chosen so that its root, plus the sub-terms it contains,
   exercise a doc form the refactor touches. Hand-written because
   AST.gen_exp_sized cannot produce Asc, SInt/Nat, module forms or pipelines. */
let corpus = [
  ("fun-var", "fun x -> x"),
  ("fun-parens-var", "fun (x) -> x"),
  ("fun-tuple2", "fun (a, b) -> a"),
  ("fun-tuple3", "fun (a, b, c) -> a"),
  ("fun-cons", "fun h::t -> h"),
  ("fun-intlit", "fun 1 -> 2"),
  ("fun-wild", "fun _ -> 3"),
  ("let-var", "let x = 1 in x"),
  ("let-tuple2", "let (a, b) = (1, 2) in a"),
  ("let-cons", "let h::t = [1] in h"),
  ("binop-plus", "1 + 2"),
  ("ascription", "1 : Int"),
  ("if", "if true then 1 else 2"),
  ("case", "case 1 | 1 => 2 | _ => 3 end"),
  ("listlit", "[1, 2]"),
  ("tuple2", "(1, 2)"),
  ("arrow3", "let f : Int -> Bool -> Int = f in f"),
  ("pipeline", "1 |> fun x -> x"),
  ("test", "test true end"),
  ("seq", "1; 2"),
  /* Bypass shapes: an annotated pattern diverges between the specific and
     fallback forms the same way a parenthesized one does. */
  ("fun-annot-pat", "fun x : Int -> x"),
  ("let-annot-pat", "let x : Int = 1 in x"),
  /* Application patterns — reaches AppPat.funaps/conaps. */
  ("ap-pat", "let f(x) = x * 2 in f(3)"),
  /* Type functions — reaches TypFunctionExp. */
  ("typfun", "typfun a -> fun x : a -> x"),
  /* Labeled tuple pattern — reaches FunctionExp.functions_tuplabel. */
  ("fun-tuplabel", "fun (x=y) -> y"),
  /* Two-level cons pattern — reaches ListPat.cons2. */
  ("cons2-pat", "let a::b::c = [1, 2] in a"),
  ("tuple3", "(1, 2, 3)"),
  ("unop-not", "!true"),
  ("power", "2 ** 4"),
  /* Partial application — its doc links a synthesized id that is not a real
     term, which shows up as <unmapped>. */
  ("deferred-ap", "let plus = fun (x, y) -> x + y in plus(1, _)"),
  /* These reach forms whose coloring function is named differently from the
     form itself, where a dropped ~colorings argument regresses silently. */
  ("tuplabel-exp", "(x=1)"),
  ("dot", "(x=1, y=2).x"),
  ("tyalias", "type T = Int in 1"),
  /* The list *expression* forms. Without these the pattern-side cons docs are
     covered but the expression-side ones are not, so a coloring pointing at the
     wrong piece in either would go unnoticed. */
  ("cons-exp", "1::[]"),
  ("concat-exp", "[1] @ [2]"),
];

/* Captured from the current implementation. A refactor of the coloring or
   specificity plumbing must leave every line byte-identical; a diff here is
   either a regression or a deliberate, reviewed behavior change.

   Note `fun (x) -> x`: the Base (fallback) form links the *parenthesized*
   pattern while the Var form links the inner `x`. That asymmetry is real and
   easy to erase by accident, so it is pinned here. */
let golden = [
  (
    "fun-var",
    {|fun x -> x => (FunctionExp Base) colorings=[x,x]
fun x -> x => (FunctionExp Var) colorings=[x,x]
x => VarExp colorings=[]
x => VarPat colorings=[]|},
  ),
  (
    "fun-parens-var",
    {|(x) => VarPat colorings=[]
fun (x) -> x => (FunctionExp Base) colorings=[(x),x]
fun (x) -> x => (FunctionExp Var) colorings=[x,x]
x => VarExp colorings=[]
x => VarPat colorings=[]|},
  ),
  (
    "fun-tuple2",
    {|(a, b) => Tuple2Pat colorings=[a,b]
(a, b) => TuplePat colorings=[]
a => VarExp colorings=[]
a => VarPat colorings=[]
b => VarPat colorings=[]
fun (a, b) -> a => (FunctionExp Base) colorings=[(a, b),a]
fun (a, b) -> a => (FunctionExp Tuple) colorings=[(a, b),a]
fun (a, b) -> a => (FunctionExp Tuple2) colorings=[a,a,b]|},
  ),
  (
    "fun-tuple3",
    {|(a, b, c) => Tuple3Pat colorings=[a,b,c]
(a, b, c) => TuplePat colorings=[]
a => VarExp colorings=[]
a => VarPat colorings=[]
b => VarPat colorings=[]
c => VarPat colorings=[]
fun (a, b, c) -> a => (FunctionExp Base) colorings=[(a, b, c),a]
fun (a, b, c) -> a => (FunctionExp Tuple) colorings=[(a, b, c),a]
fun (a, b, c) -> a => (FunctionExp Tuple3) colorings=[a,a,b,c]|},
  ),
  (
    "fun-cons",
    {|fun h:: t -> h => (FunctionExp Base) colorings=[h,h:: t]
fun h:: t -> h => (FunctionExp ListCons) colorings=[h,h,t]
h => VarExp colorings=[]
h => VarPat colorings=[]
h:: t => ConsPat colorings=[h,t]
t => VarPat colorings=[]|},
  ),
  (
    "fun-intlit",
    {|1 => IntPat colorings=[]
2 => IntExp colorings=[]
fun 1 -> 2 => (FunctionExp Base) colorings=[1,2]
fun 1 -> 2 => (FunctionExp Int) colorings=[1,2]|},
  ),
  (
    "fun-wild",
    {|3 => IntExp colorings=[]
_ => WildPat colorings=[]
fun _ -> 3 => (FunctionExp Base) colorings=[3,_]
fun _ -> 3 => (FunctionExp Wild) colorings=[3]|},
  ),
  (
    "let-var",
    {|1 => IntExp colorings=[]
let x = 1 in x => (LetExp Base) colorings=[1,x]
let x = 1 in x => (LetExp Var) colorings=[1,x,x]
x => VarExp colorings=[]
x => VarPat colorings=[]|},
  ),
  (
    "let-tuple2",
    {|(1, 2) => Tuple2Exp colorings=[1,2]
(1, 2) => TupleExp colorings=[]
(a, b) => Tuple2Pat colorings=[a,b]
(a, b) => TuplePat colorings=[]
1 => IntExp colorings=[]
2 => IntExp colorings=[]
a => VarExp colorings=[]
a => VarPat colorings=[]
b => VarPat colorings=[]
let (a, b) = (1, 2) in a => (LetExp Base) colorings=[(1, 2),(a, b)]
let (a, b) = (1, 2) in a => (LetExp Tuple) colorings=[(1, 2),(a, b)]
let (a, b) = (1, 2) in a => (LetExp Tuple2) colorings=[(1, 2),a,b]|},
  ),
  (
    "let-cons",
    {|1 => IntExp colorings=[]
[1] => ListExp colorings=[]
h => VarExp colorings=[]
h => VarPat colorings=[]
h:: t => ConsPat colorings=[h,t]
let h:: t = [1] in h => (LetExp Base) colorings=[[1],h:: t]
let h:: t = [1] in h => (LetExp ListCons) colorings=[[1],h,t]
t => VarPat colorings=[]|},
  ),
  (
    "binop-plus",
    {|1 + 2 => (BinOpExp (Int Plus)) colorings=[1,2]
1 => IntExp colorings=[]
2 => IntExp colorings=[]|},
  ),
  (
    "ascription",
    {|1 => IntExp colorings=[]
1:Int => AscExp colorings=[1,Int]
Int => IntTyp colorings=[]|},
  ),
  (
    "if",
    {|1 => IntExp colorings=[]
2 => IntExp colorings=[]
if true then 1 else 2 => IfExp colorings=[1,2,true]
true => BoolExp colorings=[]|},
  ),
  (
    "case",
    {|1 => IntExp colorings=[]
1 => IntPat colorings=[]
2 => IntExp colorings=[]
3 => IntExp colorings=[]
_ => WildPat colorings=[]
case 1 | 1 => 2| _ => 3 end => CaseExp colorings=[1]|},
  ),
  (
    "listlit",
    {|1 => IntExp colorings=[]
2 => IntExp colorings=[]
[1, 2] => ListExp colorings=[]|},
  ),
  (
    "tuple2",
    {|(1, 2) => Tuple2Exp colorings=[1,2]
(1, 2) => TupleExp colorings=[]
1 => IntExp colorings=[]
2 => IntExp colorings=[]|},
  ),
  (
    "arrow3",
    {|Bool -> Int => ArrowTyp colorings=[Bool,Int]
Bool => BoolTyp colorings=[]
Int -> Bool -> Int => Arrow3Typ colorings=[Bool,Int,Int]
Int -> Bool -> Int => ArrowTyp colorings=[Bool -> Int,Int]
Int => IntTyp colorings=[]
f => VarExp colorings=[]
f => VarPat colorings=[]
f:(Int -> Bool -> Int) => TypAnnPat colorings=[Int -> Bool -> Int,f]
let f:(Int -> Bool -> Int) = f in f => (LetExp Base) colorings=[f,f]
let f:(Int -> Bool -> Int) = f in f => (LetExp Var) colorings=[f,f,f]|},
  ),
  (
    "pipeline",
    {|1 => IntExp colorings=[]
1 |> (fun x -> x) => PipelineExp colorings=[1,fun x -> x]
fun x -> x => (FunctionExp Base) colorings=[x,x]
fun x -> x => (FunctionExp Var) colorings=[x,x]
x => VarExp colorings=[]
x => VarPat colorings=[]|},
  ),
  (
    "test",
    {|test true end => TestExp colorings=[true]
true => BoolExp colorings=[]|},
  ),
  (
    "seq",
    {|1 => IntExp colorings=[]
1; 2 => SeqExp colorings=[1,2]
2 => IntExp colorings=[]|},
  ),
  (
    "fun-annot-pat",
    {|Int => IntTyp colorings=[]
fun x:(Int) -> x => (FunctionExp Base) colorings=[x,x:(Int)]
fun x:(Int) -> x => (FunctionExp Var) colorings=[x,x]
x => VarExp colorings=[]
x => VarPat colorings=[]
x:(Int) => TypAnnPat colorings=[Int,x]|},
  ),
  (
    "let-annot-pat",
    {|1 => IntExp colorings=[]
Int => IntTyp colorings=[]
let x:(Int) = 1 in x => (LetExp Base) colorings=[1,x]
let x:(Int) = 1 in x => (LetExp Var) colorings=[1,x,x]
x => VarExp colorings=[]
x => VarPat colorings=[]
x:(Int) => TypAnnPat colorings=[Int,x]|},
  ),
  (
    "ap-pat",
    {|2 => IntExp colorings=[]
3 => IntExp colorings=[]
f => VarExp colorings=[]
f => VarPat colorings=[]
f(3) => FunApExp colorings=[3,f]
f(x) => ApFuncPat colorings=[f,x]
fun x -> x * 2 => (FunctionExp Base) colorings=[x,x * 2]
fun x -> x * 2 => (FunctionExp Var) colorings=[x,x * 2]
let f(x) = x * 2 in f(3) => (LetExp ApFunc) colorings=[f,x,x * 2]
let f(x) = x * 2 in f(3) => (LetExp Base) colorings=[f(x),x * 2]
x * 2 => (BinOpExp (Int Times)) colorings=[2,x]
x => VarExp colorings=[]
x => VarPat colorings=[]|},
  ),
  (
    "typfun",
    {|a => VarTPat colorings=[]
a => VarTyp colorings=[]
fun x:(a) -> x => (FunctionExp Base) colorings=[x,x:(a)]
fun x:(a) -> x => (FunctionExp Var) colorings=[x,x]
typfun a -> fun x:(a) -> x => TypFunctionExp colorings=[a,fun x:(a) -> x]
x => VarExp colorings=[]
x => VarPat colorings=[]
x:(a) => TypAnnPat colorings=[a,x]|},
  ),
  (
    "fun-tuplabel",
    {|(x=y) => TuplePat colorings=[]
`x` => Label colorings=[]
fun (x=y) -> y => (FunctionExp Base) colorings=[(x=y),y]
fun (x=y) -> y => (FunctionExp TupLabel) colorings=[`x`,y,y]
x=y => LabeledPat colorings=[`x`,y]
y => VarExp colorings=[]
y => VarPat colorings=[]|},
  ),
  /* The ConsPat fallback shows the *outer* tail `b:: c`, supplied by `decide`'s
     override rather than by the form, which carries the inner tail. */
  (
    "cons2-pat",
    {|1 => IntExp colorings=[]
2 => IntExp colorings=[]
[1, 2] => ListExp colorings=[]
a => VarExp colorings=[]
a => VarPat colorings=[]
a:: b:: c => Cons2Pat colorings=[a,b,c]
a:: b:: c => ConsPat colorings=[a,b:: c]
b => VarPat colorings=[]
b:: c => ConsPat colorings=[b,c]
c => VarPat colorings=[]
let a:: b:: c = [1, 2] in a => (LetExp Base) colorings=[[1, 2],a:: b:: c]
let a:: b:: c = [1, 2] in a => (LetExp ListCons) colorings=[[1, 2],a,b:: c]|},
  ),
  (
    "tuple3",
    {|(1, 2, 3) => Tuple3Exp colorings=[1,2,3]
(1, 2, 3) => TupleExp colorings=[]
1 => IntExp colorings=[]
2 => IntExp colorings=[]
3 => IntExp colorings=[]|},
  ),
  (
    "unop-not",
    {|! true => (UnOpExp (Bool Not)) colorings=[true]
true => BoolExp colorings=[]|},
  ),
  (
    "power",
    {|2 ** 4 => (BinOpExp (Int Power)) colorings=[2,4]
2 => IntExp colorings=[]
4 => IntExp colorings=[]|},
  ),
  (
    "deferred-ap",
    {|(x, y) => Tuple2Pat colorings=[x,y]
(x, y) => TuplePat colorings=[]
1 => IntExp colorings=[]
_ => DeferralExp colorings=[]
fun (x, y) -> x + y => (FunctionExp Base) colorings=[(x, y),x + y]
fun (x, y) -> x + y => (FunctionExp Tuple) colorings=[(x, y),x + y]
fun (x, y) -> x + y => (FunctionExp Tuple2) colorings=[x,x + y,y]
let plus = fun (x, y) -> x + y in plus(1, _) => (LetExp Base) colorings=[fun (x, y) -> x + y,plus]
let plus = fun (x, y) -> x + y in plus(1, _) => (LetExp Var) colorings=[fun (x, y) -> x + y,plus,plus(1, _)]
plus => VarExp colorings=[]
plus => VarPat colorings=[]
plus(1, _) => DeferredApExp colorings=[_,plus]
x + y => (BinOpExp (Int Plus)) colorings=[x,y]
x => VarExp colorings=[]
x => VarPat colorings=[]
y => VarExp colorings=[]
y => VarPat colorings=[]|},
  ),
  (
    "tuplabel-exp",
    {|(x=1) => TupleExp colorings=[]
1 => IntExp colorings=[]
`x` => Label colorings=[]
x=1 => LabeledExp colorings=[1,`x`]|},
  ),
  (
    "dot",
    {|(x=1, y=2) => Tuple2Exp colorings=[x=1,y=2]
(x=1, y=2) => TupleExp colorings=[]
(x=1, y=2).x => DotExp colorings=[(x=1, y=2),`x`]
1 => IntExp colorings=[]
2 => IntExp colorings=[]
`x` => Label colorings=[]
`y` => Label colorings=[]
x=1 => LabeledExp colorings=[1,`x`]
y=2 => LabeledExp colorings=[2,`y`]|},
  ),
  (
    "cons-exp",
    {|1 => IntExp colorings=[]
1:: [] => ConsExp colorings=[1,[]]
[] => ListExp colorings=[]|},
  ),
  (
    "concat-exp",
    {|1 => IntExp colorings=[]
2 => IntExp colorings=[]
[1] => ListExp colorings=[]
[1] @ [2] => ListConcatExp colorings=[[1],[2]]
[2] => ListExp colorings=[]|},
  ),
  (
    "tyalias",
    {|1 => IntExp colorings=[]
Int => IntTyp colorings=[]
T => VarTPat colorings=[]
type T = Int in 1 => TyAliasExp colorings=[Int,T]|},
  ),
];

/* Which doc groups the corpus actually reaches. A golden fingerprint only
   guards the docs it visits, so the reached set is asserted exactly: losing
   coverage fails rather than going quiet, and "not covered" stays
   distinguishable from "not reachable at all". */
/* Folds `f` over every doc decision the corpus produces, with every form of
   every reached group selected in turn. */
let over_corpus_docs =
    (f: (Id.Map.t(Info.t), ET.doc) => list('a)): list('a) =>
  corpus
  |> List.concat_map(~f=((_name, src)) => {
       let info_map = info_map_of(src);
       Id.Map.fold(
         (_id, info: Info.t, acc) =>
           List.concat_map(~f=f(info_map), swept_docs(info)) @ acc,
         info_map,
         [],
       );
     });

let reached_groups = () =>
  over_corpus_docs((_info_map, d: ET.doc) =>
    [Web.ExplainThisForm.show_group_id(d.group.id)]
  )
  |> List.dedup_and_sort(~compare=Poly.compare);

/* Recorded from the corpus above. Add to this when a new corpus entry reaches a
   new doc; a drop means a doc silently stopped being exercised.

   Note what is absent. `FunctionExp(Base)` and `LetExp(Base)` never appear as a
   *group*: they name the shared least-specific form, and `decide` always
   dispatches to a more specific group that contains it. `FunctionExp(Tuple)` and
   `LetExp(Tuple)` are only dispatched for tuples of size other than 2 or 3. */
let expected_groups = [
  "(BinOpExp (Int Plus))",
  "(BinOpExp (Int Power))",
  "(BinOpExp (Int Times))",
  "(FunctionExp Int)",
  "(FunctionExp ListCons)",
  "(FunctionExp TupLabel)",
  "(FunctionExp Tuple2)",
  "(FunctionExp Tuple3)",
  "(FunctionExp Var)",
  "(FunctionExp Wild)",
  "(LetExp ApFunc)",
  "(LetExp ListCons)",
  "(LetExp Tuple2)",
  "(LetExp Var)",
  "(UnOpExp (Bool Not))",
  "ApFuncPat",
  "Arrow3Typ",
  "ArrowTyp",
  "AscExp",
  "BoolExp",
  "BoolTyp",
  "CaseExp",
  "Cons2Pat",
  "ConsExp",
  "ConsPat",
  "DeferralExp",
  "DeferredApExp",
  "DotExp",
  "FunApExp",
  "IfExp",
  "IntExp",
  "IntPat",
  "IntTyp",
  "Label",
  "LabeledExp",
  "LabeledPat",
  "ListConcatExp",
  "ListExp",
  "PipelineExp",
  "SeqExp",
  "TestExp",
  "Tuple2Exp",
  "Tuple2Pat",
  "Tuple3Exp",
  "Tuple3Pat",
  "TupleExp",
  "TuplePat",
  "TyAliasExp",
  "TypAnnPat",
  "TypFunctionExp",
  "VarExp",
  "VarPat",
  "VarTPat",
  "VarTyp",
  "WildPat",
];

/* Groups that no corpus entry can reach because nothing in the surface language
   dispatches to them. Asserted unreached so that if one ever becomes reachable
   the test says so rather than staying silent.

   - SInt/Nat binary operators: Operators.op_bin has these arms and form_id
     admits the groups, but OpExp defines no docs for them, so they fall through
     to "No docs available".
   - SIntPat: MakeTerm only ever builds Atom(Int) literal patterns from surface
     syntax, so the SInt pattern doc appears unreachable from the editor. */
let known_unreachable = [
  "(BinOpExp (SInt Plus))",
  "(BinOpExp (Nat Plus))",
  "SIntPat",
];

let coverage_case =
  Alcotest.test_case(
    "group coverage",
    `Quick,
    () => {
      let reached = reached_groups();
      Alcotest.check(
        Alcotest.list(Alcotest.string),
        "doc groups reached by the corpus",
        expected_groups,
        reached,
      );
      List.iter(
        ~f=
          g =>
            Alcotest.check(
              Alcotest.bool,
              "expected to be unreachable: " ++ g,
              false,
              List.mem(reached, g, ~equal=Poly.equal),
            ),
        known_unreachable,
      );
    },
  );

/* A coloring pairs a piece of the form's *own* syntactic form with a term in the
   user's code. Naming a piece that the form does not contain is always a bug, and
   a silent one: the pair simply never matches anything while the map is built, so
   the explanation renders with that link unhighlighted and nothing complains.

   This is the invariant a builder that created each placeholder and recorded its
   pairing in one step would give for free (issue #1170). Checking it here gets the
   same guarantee over every form the corpus reaches without rewriting all 154 of
   them, and it is what makes the *_coloring_ids functions safe to leave in place:
   they cannot drift from the form they belong to undetected. */
let stray_colorings = (info_map, d: ET.doc): list(string) => {
  /* Segment.ids recurses into tile children, so template-built forms count. */
  let sf_ids = Segment.ids(d.form.syntactic_form);
  d.colorings
  |> List.filter(~f=((sf_id, _)) =>
       !List.mem(sf_ids, sf_id, ~equal=Poly.equal)
     )
  |> List.map(~f=((_sf_id, code_id)) =>
       Web.ExplainThisForm.show_form_id(d.form.id)
       ++ " links "
       ++ canonical_id(info_map, code_id)
       ++ " to a piece it does not contain"
     );
};

let pairing_case =
  Alcotest.test_case("colorings name the form's own pieces", `Quick, () =>
    Alcotest.check(
      Alcotest.list(Alcotest.string),
      "colorings referring to a piece outside the form",
      [],
      over_corpus_docs(stray_colorings)
      |> List.dedup_and_sort(~compare=Poly.compare),
    )
  );

let golden_case = ((name, src)) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let expected =
        switch (List.Assoc.find(golden, name, ~equal=Poly.equal)) {
        | Some(g) => g
        | None => "<no golden recorded for " ++ name ++ ">"
        };
      Alcotest.check(Alcotest.string, name, expected, doc_fingerprint(src));
    },
  );

let tests = (
  "ExplainThis",
  [
    QCheck_alcotest.to_alcotest(qcheck_explainthis_does_not_crash),
    Alcotest.test_case("negation labels re-kind by class", `Quick, () =>
      negation_labels()
    ),
    coverage_case,
    pairing_case,
    ...List.map(~f=golden_case, corpus),
  ],
);
