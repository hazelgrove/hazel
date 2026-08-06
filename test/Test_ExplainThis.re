open Haz3lcore;
open Language;
open MenhirParser;

/* Property-based test ensuring ExplainThis never raises while producing
   documentation for any sub-term of an expression. The documentation for
   each form substitutes term ids into its explanation string via a format,
   so a mismatch between the number of `%s` placeholders and the number of
   supplied arguments crashes at runtime. This test guards against that. */

let globals = Web.Globals.Model.init();
let docs = Web.ExplainThisModel.init;

let arb_drv_exp = (~minimal_idents, size) => {
  open QCheck.Gen;
  let base = AST.gen_exp_sized(~minimal_idents, size);
  let to_core = menhir_exp =>
    Conversion.Exp.of_menhir_ast(menhir_exp)
    |> Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh());
  let show = exp =>
    switch (
      exp
      |> ExpToSegment.exp_to_segment(
           ~settings=ExpToSegment.Settings.editable(~inline=true),
           _,
         )
      |> Printer.of_segment(~holes="?", _)
    ) {
    | s => s
    | exception _ => "<unprintable expression>"
    };
  QCheck.make(~print=show, map(to_core, base));
};

let statics = term =>
  fst(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));

let qcheck_explainthis_does_not_crash =
  QCheck.Test.make(
    ~name="ExplainThis does not crash",
    ~count=1000,
    arb_drv_exp(~minimal_idents=true, 12),
    exp => {
    /* Statics failures are out of scope; we only assert that ExplainThis
       itself does not raise for any sub-term it is asked to document. */
    switch (statics(exp)) {
    | exception _ => true
    | info_map =>
      Id.Map.iter(
        (_id, info: Info.t) => {
          let _ =
            Web.ExplainThis.get_doc(
              ~globals,
              ~docs,
              Some(info),
              Web.ExplainThis.Colorings,
            );
          ();
        },
        info_map,
      );
      true;
    }
  });

/* ===================== Characterization (golden) test =====================

   The property test above only asserts that `get_doc` doesn't raise. Nothing
   pinned down *which* sub-term each explanation actually links to, which is
   exactly what a refactor of the coloring/specificity plumbing moves around.

   `Colorings` mode is not usable for this: it only re-derives a color map from
   the explanation's markdown links and never reads `colorings` at all, so it
   is blind to the very mapping being moved. `get_doc`'s `Probe` mode reports
   the decision itself — group, selected form, and colorings — without
   rendering.

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

/* `get_doc` reports its decision without rendering. */
let probes_of = (~docs, info: Info.t): list(Web.ExplainThis.probe) => {
  let acc = ref([]);
  let _ =
    Web.ExplainThis.get_doc(
      ~globals,
      ~docs,
      Some(info),
      Web.ExplainThis.Probe(p => acc := [p, ...acc^]),
    );
  List.rev(acc^);
};

let render_probe = (info_map, p: Web.ExplainThis.probe): string => {
  let colorings =
    p.colorings
    |> List.map(((_sf_id, code_id)) => canonical_id(info_map, code_id))
    |> List.sort(String.compare)
    |> String.concat(",");
  Web.ExplainThisForm.show_form_id(p.form)
  ++ " colorings=["
  ++ colorings
  ++ "]";
};

/* `ExplainThisModel.init` records no group selections, so
   `get_selected_option` always returns the *most specific* form — meaning a
   harness built only on `init` never reaches a single fallback form, which is
   exactly what the specificity ladders select. So for each documented sub-term
   we sweep every form its group offers by planting a selection for it. */
let fingerprint_of_info = (info_map, info: Info.t): list(string) =>
  switch (probes_of(~docs, info)) {
  | [] => ["(no group doc)"]
  | ps =>
    ps
    |> List.concat_map((p: Web.ExplainThis.probe) =>
         p.forms
         |> List.concat_map(form_id => {
              let docs': Web.ExplainThisModel.t = {
                ...Web.ExplainThisModel.init,
                groups: [
                  {
                    group: p.group,
                    selected: form_id,
                  },
                ],
              };
              probes_of(~docs=docs', info)
              |> List.map(q => render_probe(info_map, q));
            })
       )
  };

let doc_fingerprint = (src: string): string => {
  let term =
    switch (Haz3lcore.Parser.to_term(src, ~root=Exp)) {
    | Some(e) => e
    | None => failwith("corpus entry failed to parse: " ++ src)
    };
  let info_map = statics(term);
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
  /* `f(x) => ApConsPat` is wrong: this is a *function* application pattern, so
     its group is ApFuncPat. The form's id disagrees with its group. */
  (
    "ap-pat",
    {|2 => IntExp colorings=[]
3 => IntExp colorings=[]
f => VarExp colorings=[]
f => VarPat colorings=[]
f(3) => FunApExp colorings=[3,f]
f(x) => ApConsPat colorings=[f,x]
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
  /* The ConsPat fallback shows the *outer* tail `b:: c`, supplied by `get_doc`'s
     override — the form itself was built with the inner tail. Moving colorings
     onto the form must preserve the outer reading. */
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
];

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

let tests = (
  "ExplainThis",
  [
    QCheck_alcotest.to_alcotest(qcheck_explainthis_does_not_crash),
    ...List.map(golden_case, corpus),
  ],
);
