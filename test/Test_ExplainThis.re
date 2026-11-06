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
    |> List.map(((_sf_id, code_id)) => canonical_id(info_map, code_id))
    |> List.sort(String.compare)
    |> String.concat(",");
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
    |> List.filter_map((form: Web.ExplainThisForm.form) => {
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
  | ds => List.map(render_doc(info_map), ds)
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
          l => cursor ++ " => " ++ l,
          fingerprint_of_info(info_map, info),
        )
        @ acc;
      },
    info_map,
    [],
  )
  |> List.sort_uniq(String.compare)
  |> String.concat("\n");
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
  |> List.concat_map(((_name, src)) => {
       let info_map = info_map_of(src);
       Id.Map.fold(
         (_id, info: Info.t, acc) =>
           List.concat_map(f(info_map), swept_docs(info)) @ acc,
         info_map,
         [],
       );
     });

let reached_groups = () =>
  over_corpus_docs((_info_map, d: ET.doc) =>
    [Web.ExplainThisForm.show_group_id(d.group.id)]
  )
  |> List.sort_uniq(String.compare);

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
        g =>
          Alcotest.check(
            Alcotest.bool,
            "expected to be unreachable: " ++ g,
            false,
            List.mem(g, reached),
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
  |> List.filter(((sf_id, _)) => !List.mem(sf_id, sf_ids))
  |> List.map(((_sf_id, code_id)) =>
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
      over_corpus_docs(stray_colorings) |> List.sort_uniq(String.compare),
    )
  );

let golden_case = ((name, src)) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let expected =
        switch (List.assoc_opt(name, golden)) {
        | Some(g) => g
        | None => "<no golden recorded for " ++ name ++ ">"
        };
      Alcotest.check(Alcotest.string, name, expected, doc_fingerprint(src));
    },
  );

/* --- prover-form documentation completeness ------------------------------ */

/* Every form of the Proof sort must map to a real message, not a generic
   "Proof term" fallback. `ProofDoc.single` is total over `Proof.term`, so
   the compiler already guarantees an arm exists; what this test guarantees
   is that each arm carries prose, that the prose references every child it
   colors (a coloring whose id never appears in the message paints
   nothing), that no `%s` placeholder was left unfilled, and that each
   example's source really parses. */

let contains = (hay: string, needle: string): bool => {
  let (nh, nn) = (String.length(hay), String.length(needle));
  let rec go = (i: int) =>
    i + nn > nh ? false : String.sub(hay, i, nn) == needle || go(i + 1);
  go(0);
};

let hole_proof = () => Proof.fresh(EmptyHole);
let some_exp = (): Exp.t => Exp.fresh(EmptyHole);
let some_pat = (): Pat.t => Pat.fresh(EmptyHole);
let inst = () => Some((some_exp(), some_exp()));

/* Representative terms, one per constructor (and one per shape of the
   optional `with` clause, which selects a different message). Exhaustive
   by construction: a new proof form fails to compile here. */
let representatives = (cls: Proof.cls): list(Proof.t) =>
  switch (cls) {
  | Invalid => [Proof.fresh(Invalid("~"))]
  | EmptyHole => [Proof.fresh(EmptyHole)]
  | MultiHole => [Proof.fresh(MultiHole([]))]
  | Seq => [Proof.fresh(Seq(hole_proof(), hole_proof()))]
  | AxiomStep =>
    [Util.Direction.Right, Util.Direction.Left]
    |> List.concat_map(direction =>
         [None, inst()]
         |> List.map(instantiation =>
              Proof.fresh(
                AxiomStep({
                  at_idx: some_exp(),
                  at_exp: some_exp(),
                  direction,
                  equality: some_exp(),
                  instantiation,
                }),
              )
            )
       )
  | AlgebriteStep => [
      Proof.fresh(
        AlgebriteStep({
          at_idx: some_exp(),
          at_exp: some_exp(),
          with_exp: some_exp(),
        }),
      ),
    ]
  | EvalStep => [
      Proof.fresh(
        EvalStep({
          at_idx: some_exp(),
          at_exp: some_exp(),
        }),
      ),
    ]
  | Induction => [
      Proof.fresh(
        Induction(some_exp(), None, [(some_pat(), hole_proof())]),
      ),
      Proof.fresh(
        Induction(
          some_exp(),
          Some(some_pat()),
          [(some_pat(), hole_proof())],
        ),
      ),
    ]
  | Forall => [Proof.fresh(Forall(some_pat(), hole_proof()))]
  | Funext => [Proof.fresh(Funext(some_pat(), hole_proof()))]
  | Assume => [
      Proof.fresh(Assume(some_exp(), None, hole_proof())),
      Proof.fresh(Assume(some_exp(), Some(some_pat()), hole_proof())),
    ]
  | Alias => [Proof.fresh(Alias(some_pat(), some_exp(), hole_proof()))]
  | Generalize => [Proof.fresh(Generalize(some_exp(), hole_proof()))]
  | Revert => [
      Proof.fresh(Revert(some_exp(), None, hole_proof())),
      Proof.fresh(Revert(some_exp(), inst(), hole_proof())),
    ]
  | Contradiction => [
      Proof.fresh(Contradiction(some_exp(), None)),
      Proof.fresh(Contradiction(some_exp(), inst())),
    ]
  | Have => [Proof.fresh(Have(some_exp(), hole_proof(), hole_proof()))]
  /* Parens are transparent: their doc is the inner form's, so the
     representative wraps a form that has a worked example. */
  | Parens => [
      Proof.fresh(Parens(Proof.fresh(Funext(some_pat(), hole_proof())))),
    ]
  };

/* The degenerate forms carry a message but no worked example. */
let example_free = (cls: Proof.cls): bool =>
  switch (cls) {
  | Invalid
  | EmptyHole
  | MultiHole => true
  | Seq
  | AxiomStep
  | AlgebriteStep
  | EvalStep
  | Induction
  | Forall
  | Funext
  | Assume
  | Generalize
  | Revert
  | Contradiction
  | Have
  | Alias
  | Parens => false
  };

let check_doc =
    (~what: string, ~needs_example: bool, doc: Web.ExplainThisForm.Simple.t) => {
  let msg = doc.explanation;
  Alcotest.check(
    Alcotest.bool,
    what ++ ": message is real prose",
    true,
    String.length(msg) > 80,
  );
  Alcotest.check(
    Alcotest.bool,
    what ++ ": message is not the generic fallback",
    false,
    contains(msg, "Proof term"),
  );
  Alcotest.check(
    Alcotest.bool,
    what ++ ": no unfilled format placeholder",
    false,
    contains(msg, "%s"),
  );
  /* Every colored child must be referenced by the message. */
  let (_, colorings) = doc.abstract;
  List.iter(
    ((_, id)) =>
      Alcotest.check(
        Alcotest.bool,
        what ++ ": message references its colored child " ++ Id.to_string(id),
        true,
        contains(msg, Id.to_string(id)),
      ),
    colorings,
  );
  if (needs_example) {
    Alcotest.check(
      Alcotest.bool,
      what ++ ": has at least one example",
      true,
      doc.examples != [],
    );
  };
  /* An example whose source failed to parse comes back as an empty
     segment, so this checks the example programs are real syntax. */
  List.iter(
    (ex: Web.ExplainThisForm.example) =>
      Alcotest.check(
        Alcotest.bool,
        what ++ ": example parses",
        true,
        Lazy.force(ex.term) != [],
      ),
    doc.examples,
  );
};

let test_proof_forms_documented = () =>
  List.iter(
    (cls: Proof.cls) =>
      List.iter(
        p =>
          check_doc(
            ~what=Proof.show_cls(cls),
            ~needs_example=!example_free(cls),
            Web.ProofDoc.single(p),
          ),
        representatives(cls),
      ),
    Proof.all_of_cls,
  );

/* The two restricted binders are Exp forms, but they are prover syntax. */
let test_restricted_binders_documented = () => {
  let ids = () => (Id.mk(), Id.mk(), Id.mk());
  let (pat_id, guard_id, body_id) = ids();
  check_doc(
    ~what="forall where",
    ~needs_example=true,
    Web.ForallExp.where_single(~pat_id, ~guard_id, ~body_id),
  );
  let (pat_id, guard_id, body_id) = ids();
  check_doc(
    ~what="fun where",
    ~needs_example=true,
    Web.FunWhereExp.single(~pat_id, ~guard_id, ~body_id),
  );
};

/* End-to-end: the cursor inspector's own path. `color_map_of` gives an
   empty color map for plain prose and a populated one for a form whose
   message references its children, so a non-empty map on every InfoProof
   in these programs asserts that the dispatch reaches ProofDoc rather than
   a stub. Between them the sources cover every step form that takes
   children. */
let proof_sources = [
  "theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end; eval 5 == 5 at 0 end in 0",
  "theorem t = 1 + 1 == 2 proof rewrite 1 + 1 with 2 at 0 end in 0",
  "theorem t = (true && false) == (false && true) proof axiom and_comm at 0 on true && false end in 0",
  "theorem t = (true && false) == (false && true) proof axiomrev and_comm at 0 on false && true end in 0",
  "theorem t = forall x -> x == x proof forall x => axiom refl_eq at 0 on x == x end in 0",
  "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => axiom assume at 0 on n == 1 end in 0",
  "theorem t = forall n: Int -> false ==> n == n proof assume false => contradiction false end in 0",
  "theorem t = forall n: Int -> n == 1 ==> n == 2 ==> false proof assume n == 1 => assume n == 2 => contradiction n == 1 with n = 2 end in 0",
  "theorem t = forall x -> x == 1 proof have 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end => ? in 0",
  "theorem inv = forall w where w != 0 -> w / w == 1 proof ? in theorem t = forall n where n != 0 -> n / n == 1 proof generalize n => forall n => axiom inv at 0 on n / n end in 0",
  "let f = fun x where x != 0 -> 100 / x in theorem t = forall y -> f(y) == f(y) proof ? in 0",
  "let f = fun x -> x + 1 in theorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof ? in theorem g = f(2) == 3 proof axiom lem with n = 1 at 0 on f(2) end in 0",
  "let f = fun x -> x + 1 in theorem p = forall n: Int -> n + 0 == n proof ? in theorem g = f(2) == 3 proof revert p with n = 5 => ? in 0",
  "theorem t = forall n: Int -> forall m: Int -> n == n proof induction n > 0 as hn | true => induction m > 0 | true => revert hn => ? | false => ? end | false => ? end in 0",
  "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x == x proof assume x == 1 => assume y == 2 as hy => revert hy => ? in 0",
  "theorem t = forall n: Int -> n == 1 ==> n == n proof assume n == 1 => alias hn = assume => axiom refl_eq at 0 on n == n end in 0",
  "type Nt = +Z+S(Nt) in let pos = fun e -> case e | Z => true | S(b) => true end in theorem t = forall e: Nt -> pos(e) proof induction e | Z => eval pos(Z) at 0 end | S(b) => revert pos(b) => axiom ih at 0 on pos(b) end end in 0",
];

let parse_exp = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse: " ++ s)
  };

let test_cursor_on_proof_step_explains = () => {
  let seen = ref(0);
  /* Assert the sources really parse into the naming forms rather than
     degrading to holes, which the loop below would skip in silence. */
  let named = ref([]);
  List.iter(
    src => {
      let info_map = statics(parse_exp(src));
      Id.Map.iter(
        (_id, info: Info.t) =>
          switch (info) {
          | InfoProof({user_term, _}) =>
            switch (user_term.term) {
            /* The degenerate forms have no children to color. */
            | EmptyHole
            | MultiHole(_)
            | Invalid(_) => ()
            | _ =>
              incr(seen);
              switch (user_term.term) {
              | Induction(_, Some(_), _) =>
                named := ["induction as", ...named^]
              | Assume(_, Some(_), _) => named := ["assume as", ...named^]
              | Alias(_) => named := ["alias", ...named^]
              | _ => ()
              };
              let (color_map, _) =
                ET.color_map_of(~globals, ET.decide(~docs, Some(info)));
              Alcotest.check(
                Alcotest.bool,
                Proof.show_cls(Proof.cls_of_term(user_term.term))
                ++ ": cursor gets a message referencing its children",
                false,
                Id.Map.is_empty(color_map),
              );
            }
          | _ => ()
          },
        info_map,
      );
    },
    proof_sources,
  );
  Alcotest.check(
    Alcotest.bool,
    "the sources really contain proof steps",
    true,
    seen^ >= 20,
  );
  List.iter(
    shape =>
      Alcotest.check(
        Alcotest.bool,
        shape ++ ": source parsed into the named form",
        true,
        List.mem(shape, named^),
      ),
    ["induction as", "assume as", "alias"],
  );
};

let tests = (
  "ExplainThis",
  [
    QCheck_alcotest.to_alcotest(qcheck_explainthis_does_not_crash),
    Alcotest.test_case("negation labels re-kind by class", `Quick, () =>
      negation_labels()
    ),
    coverage_case,
    pairing_case,
    Alcotest.test_case(
      "every prover form has a real message",
      `Quick,
      test_proof_forms_documented,
    ),
    Alcotest.test_case(
      "restricted binders documented",
      `Quick,
      test_restricted_binders_documented,
    ),
    Alcotest.test_case(
      "cursor on a proof step gets a real message",
      `Quick,
      test_cursor_on_proof_step_explains,
    ),
    ...List.map(golden_case, corpus),
  ],
);
