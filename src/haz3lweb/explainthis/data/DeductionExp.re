open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open ExplainThisForm;

open DrvSyntax;

let highlight = (msg: list(Node.t), id: Id.t, mapping: ColorSteps.t): Node.t => {
  let (c, _) = ColorSteps.get_color(id, mapping);
  let classes = Attr.class_("highlight-" ++ c);
  let attrs = [classes];
  Node.span(~attrs, msg);
};
let rec repr = (p: int, prop: t, ~color_map: ColorSteps.t): list(Node.t) => {
  let p' = precedence(prop);
  let mk = x => [Node.text(x)];
  let repr = repr(p', ~color_map);
  // let insert_space = (a: list(Node.t), b: list(Node.t)) =>
  //   List.concat([a, mk(" "), b]);
  let repr_aba = (as_: list(string), bs: list(t)): list(Node.t) =>
    Aba.mk(as_, bs)
    |> Aba.join(mk, repr)
    |> Aba.mk(
         _,
         List.init(List.length(bs) + List.length(as_) - 1, _ =>
           Unicode.nbsp
         ),
       )
    |> Aba.join(Fun.id, mk)
    |> List.concat;
  let repr_aba_tight = (as_: list(string), bs: list(t)) =>
    Aba.mk(as_, bs) |> Aba.join(mk, repr) |> List.concat;
  let repr_binop = (op: string, a: t, b: t) =>
    [repr(a), [Node.text(Unicode.nbsp ++ op ++ Unicode.nbsp)], repr(b)]
    |> List.concat;
  let repr_postop = (op, a: t) =>
    [repr(a), [Node.text(Unicode.nbsp ++ op)]] |> List.concat;
  let repr_preop = (op, a: t) =>
    [[Node.text(op ++ Unicode.nbsp)], repr(a)] |> List.concat;
  (
    switch (IdTagged.term_of(prop)) {
    | Hole(s) => Printf.sprintf("[%s]", s) |> mk
    | Atom(s) => s |> mk
    | And(a, b) => repr_binop("∧", a, b)
    | Or(a, b) => repr_binop("∨", a, b)
    | Impl(a, b) when IdTagged.term_of(b) == Falsity => repr_postop("¬", a)
    | Impl(a, b) => repr_binop("⊃", a, b)
    | Truth => "⊤" |> mk
    | Falsity => "⊥" |> mk
    | Ctx(ctx) =>
      if (List.length(ctx) == 0) {
        "·" |> mk;
      } else {
        ctx |> List.map(repr) |> List.concat;
      }
    | Entail(a, b) => repr_binop("⊢", a, b)
    | NumLit(i) => string_of_int(i) |> mk
    | Val(a) => repr_postop("val", a)
    | Neg(a) => repr_preop("-", a)
    | Plus(a, b) => repr_binop("+", a, b)
    | Minus(a, b) => repr_binop("-", a, b)
    | Times(a, b) => repr_binop("*", a, b)
    | Lt(a, b) => repr_binop("<", a, b)
    | Gt(a, b) => repr_binop(">", a, b)
    | Eq(a, b) => repr_binop("==", a, b)
    | Eval(a, b) => repr_binop("⇓", a, b)
    | Num => "Num" |> mk
    | Bool => "Bool" |> mk
    | Arrow(a, b) => repr_binop("→", a, b)
    | Prod(a, b) => repr_binop("×", a, b)
    | Unit => "Unit" |> mk
    | Sum(a, b) => repr_binop("+", a, b)
    | TVar(x) => x |> mk
    | Rec(x, a) => repr_aba(["rec", "→", ""], [x, a])
    | True => "True" |> mk
    | False => "False" |> mk
    | If(a, b, c) => repr_aba(["if", "then", "else", ""], [a, b, c])
    | Var(x) => x |> mk
    | Let(x, a, b) => repr_aba(["let", "=", "in", ""], [x, a, b])
    | LetAnn(x, t, a, b) =>
      repr_aba(["let", ":", "=", "in", ""], [x, t, a, b])
    | Fix(x, a) => repr_aba(["fix", "→", ""], [x, a])
    | FixAnn(x, t, a) => repr_aba(["fix", ":", "→", ""], [x, t, a])
    | Fun(x, a) => repr_aba(["fun", "→", ""], [x, a])
    | FunAnn(x, t, a) => repr_aba(["fun", ":", "→", ""], [x, t, a])
    | Ap(a, b) => repr_aba_tight(["", "(", ")"], [a, b])
    | Pair(a, b) => repr_aba_tight(["(", ",", ")"], [a, b])
    | Triv => "()" |> mk
    | PrjL(a) => repr_postop(".fst", a)
    | PrjR(a) => repr_postop(".snd", a)
    | LetPair(x, y, a, b) =>
      repr_aba(["let (", ",", ") =", "in", ""], [x, y, a, b])
    | InjL(a) => repr_preop("L", a)
    | InjR(a) => repr_preop("R", a)
    | Case(a, x, b, y, c) =>
      repr_aba(
        ["case", "of L", "→", "else R", "→", ""],
        [a, x, b, y, c],
      )
    | Roll(a) => repr_aba_tight(["roll(", ")"], [a])
    | Unroll(a) => repr_aba_tight(["unroll(", ")"], [a])
    | TPat(x) => x |> mk
    | Pat(x) => x |> mk
    | HasTy(a, b) => repr_binop(":", a, b)
    | Syn(a, b) => repr_binop("⇒", a, b)
    | Ana(a, b) => repr_binop("⇐", a, b)
    }
  )
  |> (x => p < p' ? List.concat([mk("("), x, mk(")")]) : x)
  |> (
    x =>
      switch (
        Haz3lcore.Id.Map.find_opt(IdTagged.rep_id(prop), fst(color_map))
      ) {
      | None => x
      | Some(_) => [highlight(x, IdTagged.rep_id(prop), color_map)]
      }
  );
};

let repr = repr(P.min);

let copy_color_map =
    (terms: list(RuleVerify.t), (map, idx): ColorSteps.t): ColorSteps.t => {
  (
    List.fold_left(
      (new_map, RuleVerify.{self, ghost}) => {
        let id = IdTagged.rep_id(self);
        switch (ghost) {
        | None => new_map
        | Some(ghost) =>
          let ghost_id = ghost |> IdTagged.rep_id;
          let color = Haz3lcore.Id.Map.find_opt(id, map);
          switch (color) {
          | None => new_map
          | Some(color) => Haz3lcore.Id.Map.add(ghost_id, color, new_map)
          };
        };
      },
      Haz3lcore.Id.Map.empty,
      terms,
    ),
    idx,
  );
};

let copy_color_map =
    (failure: RuleVerify.failure, color_map: ColorSteps.t): ColorSteps.t => {
  let terms: list(RuleVerify.t) =
    switch (failure) {
    | PremiseMismatch(_) => []
    | FailUnbox(_, p) => [p]
    | NotAList(p) => [p]
    | NotEqual(p1, p2) => [p1, p2]
    | FailTest(p, op) =>
      [p]
      @ (
        switch (op) {
        | Neg(p) => [p]
        | Plus(p1, p2) => [p1, p2]
        | Minus(p1, p2) => [p1, p2]
        | Times(p1, p2) => [p1, p2]
        | Lt(p) => [p]
        | NotLt(p) => [p]
        | Gt(p) => [p]
        | NotGt(p) => [p]
        | Eq(p) => [p]
        | NotEq(p) => [p]
        | Subst((p1, p2), p3) => [p1, p2, p3]
        | Subst2((p1, p2), (p3, p4), p5) => [p1, p2, p3, p4, p5]
        | SubstTy((p1, p2), p3) => [p1, p2, p3]
        | Cons(p1, p2) => [p1, p2]
        | ConsHasTy((p1, p2), p3) => [p1, p2, p3]
        | ConsHasTy2((p1, p2), (p3, p4), p5) => [p1, p2, p3, p4, p5]
        | Mem(p) => [p]
        | MemHasTy(p1, p2) => [p1, p2]
        }
      )
    };
  copy_color_map(terms, color_map);
};

let conclusion_view = (~syntax: DrvSyntax.t, ~color_map: ColorSteps.t) =>
  Node.div(
    ~attrs=[Attr.class_("deduction-concl")],
    repr(syntax, ~color_map),
  );

let rule_to_label =
  fun
  | Some(rule) => Rule.repr(rule)
  | None => "?";

let label_view = (~label) =>
  Node.div(~attrs=[Attr.class_("deduction-label")], [Node.text(label)]);

let premises_view =
    (~syntaxes: list(DrvSyntax.t), ~rule, ~color_map: ColorSteps.t) => {
  let label = rule_to_label(rule);
  Node.div(
    ~attrs=[Attr.class_("deduction-prems-label")],
    [
      Node.div(
        ~attrs=[
          Attr.class_("deduction-prems"),
          Attr.class_("drv-explainthis"),
        ],
        List.map(
          syntax =>
            Node.div([
              Node.span(repr(syntax, ~color_map)),
              Node.span([
                Node.text(Unicode.nbsp ++ Unicode.nbsp ++ Unicode.nbsp),
              ]),
            ]),
          syntaxes,
        ),
      ),
    ]
    @ [label_view(~label)],
  );
};

let rule_example_view =
    (
      ~info: Haz3lschool.ProofGrade.VerifiedTree.info,
      ~color_map: ColorSteps.t,
    )
    : Node.t => {
  let (rule, res) = (info.rule, info);
  let color_map =
    switch (res.res) {
    | Correct => color_map
    | Incorrect(failure) => copy_color_map(failure, color_map)
    | Pending(_) => color_map
    };
  Node.div(
    ~attrs=[Attr.class_("section"), Attr.class_("syntactic-form")],
    switch (res.ghost) {
    | Some({prems, concl}) => [
        premises_view(~syntaxes=prems, ~rule, ~color_map),
        conclusion_view(~syntax=concl, ~color_map),
      ]
    | None => []
    },
  );
};

let rule_example_view =
    (
      ~info: option(Haz3lschool.ProofGrade.VerifiedTree.info),
      ~color_map: ColorSteps.t,
    ) =>
  switch (info) {
  | Some(info) => rule_example_view(~info, ~color_map)
  | None => Node.div([])
  };

let mk_explanation_title = () =>
  Node.div(
    ~attrs=[Attr.class_("section-title")],
    [Node.text("Verification Result")],
  );

let show_ghost = (t: option(t)) =>
  switch (t) {
  | Some(t) => DrvSyntax.repr(t)
  | None => "?"
  };

let premise_mismatch: group = {
  id: DrvPremiseMismatch,
  forms: [
    {
      id: DrvPremiseMismatch,
      syntactic_form: [],
      expandable_id: None,
      explanation: "",
      examples: [],
    },
  ],
};

let failunbox: (DrvSyntax.cls, RuleVerify.t) => group =
  (cls, {ghost: g, _}) => {
    id: DrvFailUnbox,
    forms: [
      {
        id: DrvFailUnbox,
        syntactic_form: [],
        expandable_id: None,
        explanation:
          "Failed to unbox [*"
          ++ (g |> show_ghost)
          ++ "*](%s), expected "
          ++ DrvSyntax.show_cls(cls)
          ++ ".",
        examples: [],
      },
    ],
  };

let notalist: RuleVerify.t => group =
  ({ghost: g, _}) => {
    id: DrvNotAList,
    forms: [
      {
        id: DrvNotAList,
        syntactic_form: [],
        expandable_id: None,
        explanation: "[*" ++ (g |> show_ghost) ++ "*](%s) is not a list.",
        examples: [],
      },
    ],
  };

let notequal: (RuleVerify.t, RuleVerify.t) => group =
  ({ghost: g1, _}, {ghost: g2, _}) => {
    id: DrvNotEqual,
    forms: [
      {
        id: DrvNotEqual,
        syntactic_form: [],
        expandable_id: None,
        explanation:
          "[*"
          ++ (g1 |> show_ghost)
          ++ "*](%s) is not equal to [*"
          ++ (g2 |> show_ghost)
          ++ "*](%s).",
        examples: [],
      },
    ],
  };

let failtest: (test_id, string) => group =
  (id, explanation) => {
    id: DrvFailTest(id),
    forms: [
      {
        id: DrvFailTest(id),
        syntactic_form: [],
        expandable_id: None,
        explanation,
        examples: [],
      },
    ],
  };

// type operation =
//   | Neg(t)
//   | Plus(t, t)
//   | Minus(t, t)
//   | Times(t, t)
//   | Lt(t)
//   | NotLt(t)
//   | Gt(t)
//   | NotGt(t)
//   | Eq(t)
//   | NotEq(t)
//   | Subst((t, t), t)
//   | Subst2((t, t), (t, t), t)
//   | SubstTy((t, t), t)
//   | Cons(t, t)
//   | ConsHasTy((t, t), t)
//   | ConsHasTy2((t, t), (t, t), t)
//   | Mem(t)
//   | MemHasTy(t, t);

let neg = (s, n) =>
  failtest(
    Neg,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be the negation of [*"
    ++ (n |> show_ghost)
    ++ "*](%s).",
  );

let plus = (s, n1, n2) =>
  failtest(
    Plus,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (n1 |> show_ghost)
    ++ "*](%s) plus [*"
    ++ (n2 |> show_ghost)
    ++ "*](%s).",
  );

let minus = (s, n1, n2) =>
  failtest(
    Minus,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (n1 |> show_ghost)
    ++ "*](%s) minus [*"
    ++ (n2 |> show_ghost)
    ++ "*](%s).",
  );

let times = (s, n1, n2) =>
  failtest(
    Times,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (n1 |> show_ghost)
    ++ "*](%s) times [*"
    ++ (n2 |> show_ghost)
    ++ "*](%s).",
  );

let lt = (s, n) =>
  failtest(
    Lt,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be less than [*"
    ++ (n |> show_ghost)
    ++ "*](%s).",
  );

let notlt = (s, n) =>
  failtest(
    NotLt,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be not less than [*"
    ++ (n |> show_ghost)
    ++ "*](%s).",
  );

let gt = (s, n) =>
  failtest(
    Gt,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be greater than [*"
    ++ (n |> show_ghost)
    ++ "*](%s).",
  );

let notgt = (s, n) =>
  failtest(
    NotGt,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be not greater than [*"
    ++ (n |> show_ghost)
    ++ "*](%s).",
  );

let eq = (s, n) =>
  failtest(
    Eq,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be equal to [*"
    ++ (n |> show_ghost)
    ++ "*](%s).",
  );

let noteq = (s, n) =>
  failtest(
    NotEq,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be not equal to [*"
    ++ (n |> show_ghost)
    ++ "*](%s).",
  );

let subst = (s, (v, x), e) =>
  failtest(
    Subst,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (e |> show_ghost)
    ++ "*](%s) after substituting [*"
    ++ (x |> show_ghost)
    ++ "*](%s) by [*"
    ++ (v |> show_ghost)
    ++ "*](%s).",
  );

let subst2 = (s, (v1, x), (v2, y), e) =>
  failtest(
    Subst2,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (e |> show_ghost)
    ++ "*](%s) after substituting [*"
    ++ (x |> show_ghost)
    ++ "*](%s) by [*"
    ++ (v1 |> show_ghost)
    ++ "*](%s) and [*"
    ++ (y |> show_ghost)
    ++ "*](%s) by [*"
    ++ (v2 |> show_ghost)
    ++ "*](%s).",
  );

let substty = (s, (t, a), e) =>
  failtest(
    SubstTy,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (e |> show_ghost)
    ++ "*](%s) after substituting [*"
    ++ (a |> show_ghost)
    ++ "*](%s) by [*"
    ++ (t |> show_ghost)
    ++ "*](%s).",
  );

let cons = (s, p, l) =>
  failtest(
    Cons,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (l |> show_ghost)
    ++ "*](%s) extended by [*"
    ++ (p |> show_ghost)
    ++ "*](%s).",
  );

let conshasty = (s, (x, t), l) =>
  failtest(
    ConsHasTy,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (l |> show_ghost)
    ++ "*](%s) extended by [*"
    ++ (x |> show_ghost)
    ++ "*](%s) : [*"
    ++ (t |> show_ghost)
    ++ "*](%s).",
  );

let conshasty2 = (s, (x, t1), (y, t2), l) =>
  failtest(
    ConsHasTy2,
    "Expect [*"
    ++ (s |> show_ghost)
    ++ "*](%s) to be [*"
    ++ (l |> show_ghost)
    ++ "*](%s) extended by [*"
    ++ (x |> show_ghost)
    ++ "*](%s) : [*"
    ++ (t1 |> show_ghost)
    ++ "*](%s) and [*"
    ++ (y |> show_ghost)
    ++ "*](%s) : [*"
    ++ (t2 |> show_ghost)
    ++ "*](%s).",
  );

let mem = (s, p) =>
  failtest(
    Mem,
    "Expect [*"
    ++ (p |> show_ghost)
    ++ "*](%s) to be a member of [*"
    ++ (s |> show_ghost)
    ++ "*](%s).",
  );

let memhasty = (s, (x, t)) =>
  failtest(
    MemHasTy,
    "Expect [*"
    ++ (x |> show_ghost)
    ++ "*](%s) : [*"
    ++ (t |> show_ghost)
    ++ "*](%s) to be a member of [*"
    ++ (s |> show_ghost)
    ++ "*](%s).",
  );
