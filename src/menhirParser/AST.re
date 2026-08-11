open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type filter_action =
  | Pause
  | Debug
  | Hide
  | Eval;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_float =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_int =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_string =
  | Concat;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_poly =
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type bin_op =
  | IntOp(op_bin_int)
  | FloatOp(op_bin_float)
  | StringOp(op_bin_string)
  | BoolOp(op_bin_bool)
  | PolyOp(op_bin_poly);

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_un_int =
  | Minus;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_un =
  | Int(op_un_int)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type typ_provenance =
  | Internal
  | EmptyHole;

/* CONSTRUCTOR_IDENT: ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
   Only the minimal alphabet is generated, which keeps sum types from
   colliding on duplicate constructor names. */
let gen_constructor_ident: QCheck.Gen.t(string) =
  QCheck.Gen.(oneof([pure("A"), pure("B")]));

/**
 * Generates a random IDENT string. Used for IDENT in the lexer.
 *
 * @return A QCheck generator for Identifier.
 *
 * ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
 */
let gen_ident: (~minimal_idents: bool) => QCheck.Gen.t(string) =
  (~minimal_idents) =>
    // Currently there is an issue if the keyword is a prefix of another word.
    // `let ? = ina in ?`
    // Temporarily doing single char identifiers as a fix
    QCheck.Gen.(
      if (minimal_idents) {
        oneof([pure("x"), pure("y")]);
      } else {
        string_size(~gen=char_range('a', 'z'), int_range(1, 1));
      }
    );

/**
 * Generates a string literal for use in the program.
 * This generator produces strings that match the `string` pattern in the lexer.
 */
let gen_string_literal: QCheck.Gen.t(string) =
  // TODO This should be anything printable other than `"`
  QCheck.Gen.(string_small_of(char_range('a', 'z')));

/* Payload generators for the [@deriving qcheck] annotations below, which
 * derive generators covering every constructor of the grammar.
 *
 * String payloads stay lexically valid via [@gen ...]. List payloads are
 * overridden because the ppx default can reach ~10k elements and explode
 * recursive terms. */

/* Consulted at generation time so suites can request x/y (true) vs single
 * random letters (false). Safe because Gen.t is Random.State.t -> 'a. */
let ppx_minimal_idents: ref(bool) = ref(false);

let gen_ppx_ident: QCheck.Gen.t(string) =
  st => gen_ident(~minimal_idents=ppx_minimal_idents^, st);

/* Constructor names are always minimal ("A"/"B"); ppx_minimal_idents only
 * controls value identifiers. */
let gen_ppx_constructor_ident: QCheck.Gen.t(string) = gen_constructor_ident;

/* LivelitName stores the lexeme including its leading caret, so the generated
 * name must match Lexer.livelit_ident: '^' ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*. */
let gen_ppx_livelit_ident: QCheck.Gen.t(string) =
  QCheck.Gen.map(name => "^" ++ name, gen_ppx_ident);

/* Round-trip tests canonicalize Nat/SInt → Int (same digits, no distinct
 * literal syntax). Crash PBTs should still see every atom kind. */
let gen_ppx_atom: QCheck.Gen.t(Language.Atom.t) =
  QCheck.Gen.(
    oneof([
      map(x => Language.Atom.Int(Bigint.of_int(x)), small_nat),
      map(x => Language.Atom.SInt(x), small_nat),
      map(x => Language.Atom.Nat(Bigint.of_int(x)), small_nat),
      map(x => Language.Atom.Float(x), QCheck.pos_float.gen),
      map(x => Language.Atom.Bool(x), bool),
      map(x => Language.Atom.String(x), gen_string_literal),
    ])
  );

/* DynamicErrorHole's payload must parse as an InvalidOperationError sexp:
 * Conversion.Exp.of_menhir_ast feeds it to t_of_sexp. */
let gen_ppx_error: QCheck.Gen.t(string) = QCheck.Gen.pure("DivideByZero");

/* Junk tokens MakeTerm classifies as Invalid: single operand-shaped tiles
 * that aren't a real form. Operator-shaped junk (`!!!`) molds as an infix
 * with holes instead. Lexer.invalid_face recognizes exactly this set, so
 * print→parse round-trips. */
let invalid_token_examples: list(string) = [
  "^o^",
  "^_^",
  "^w^",
  "o^o",
  "?_?",
  "$_$",
];

let gen_invalid_token: QCheck.Gen.t(string) =
  QCheck.Gen.oneof(List.map(QCheck.Gen.pure, invalid_token_examples));

let gen_ppx_small_list = (gen: QCheck.Gen.t('a)): QCheck.Gen.t(list('a)) =>
  QCheck.Gen.(list_size(int_range(0, 3), gen));

let gen_ppx_small_list1 = (gen: QCheck.Gen.t('a)): QCheck.Gen.t(list('a)) =>
  QCheck.Gen.(list_size(int_range(1, 3), gen));

/* Tuple fields: mostly plain, sometimes `lab=e` or `_=e`. */
let gen_tuple_fields =
    (
      ~labeled: (string, 'a) => 'a,
      ~unlabeled: 'a => 'a,
      gen: QCheck.Gen.t('a),
    )
    : QCheck.Gen.t(list('a)) =>
  QCheck.Gen.(
    gen_ppx_small_list(
      frequency([
        (3, gen),
        (
          1,
          {
            let* l = gen_ppx_ident
            and* x = gen;
            return(labeled(l, x));
          },
        ),
        (1, map(unlabeled, gen)),
      ]),
    )
  );

/* A TupLabel's left side must be a Label or ExplicitNonlabel — the only forms
 * that print and parse as `lab=…` / `_=…`. */
let gen_tup_label_lhs =
    (~label: string => 'a, ~nonlabel: 'a): QCheck.Gen.t('a) =>
  QCheck.Gen.(
    frequency([(3, map(label, gen_ppx_ident)), (1, pure(nonlabel))])
  );

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type tpat =
  | InvalidTPat([@gen gen_invalid_token] string)
  | EmptyHoleTPat
  | VarTPat([@gen gen_ppx_ident] string);

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type typ =
  | ParenTyp(typ)
  | IntType
  | SIntType
  | StringType
  | FloatType
  | BoolType
  | NatType
  | VoidType
  | SumTyp([@gen gen_ppx_small_list1(gen_sumterm_sized(n / 2))] sumtype)
  /* Internal/EmptyHole both print as `?`; only generate the printable form. */
  | UnknownType(
      [@gen QCheck.Gen.pure(EmptyHole: typ_provenance)] typ_provenance,
    )
  | TupleType(
      [@gen
        gen_tuple_fields(
          ~labeled=(l, t) => TupLabelType(LabelType(l), t),
          ~unlabeled=t => TupLabelType(ExplicitNonlabelType, t),
          gen_typ_sized(n / 2),
        )
      ]
      list(typ),
    )
  | ArrayType(typ)
  | ArrowType(typ, typ)
  | TypVar([@gen gen_ppx_ident] string)
  | InvalidTyp([@gen gen_invalid_token] string)
  | PolyType(tpat, typ)
  | RecType(tpat, typ)
  | ProofOfType(exp)
  /* Also injected as tuple fields via TupleType above. */
  | LabelType([@gen gen_ppx_ident] string)
  | ExplicitNonlabelType
  | TupLabelType(
      [@gen
        gen_tup_label_lhs(
          ~label=l => LabelType(l),
          ~nonlabel=ExplicitNonlabelType,
        )
      ] typ,
      typ,
    )
  | IndicationTyp(typ)
  | ProdProjection(typ, typ)
  | ProdExtension(typ, typ)
  | Sig(
      [@gen gen_ppx_small_list(gen_sig_item_sized(n / 2))] list(sig_item),
    )
and sumterm =
  | Variant([@gen gen_ppx_constructor_ident] string, option(typ))
  /* No distinct surface syntax — prints as the inner typ. Menhir classifies
     like MakeTerm: TypVar → Variant, anything else → BadEntry. */
  | BadEntry(typ)
and sumtype = list(sumterm)

and pat =
  | ParenPat(pat)
  | AscPat(pat, typ)
  | EmptyHolePat
  | WildPat
  | AtomPat([@gen gen_ppx_atom] Language.Atom.t)
  | VarPat([@gen gen_ppx_ident] string)
  /* Constructor type payloads are not printed; Canonicalize strips them. */
  | ConstructorPat(
      [@gen gen_ppx_constructor_ident] string,
      option(option(typ)),
    )
  | TuplePat(
      [@gen
        gen_tuple_fields(
          ~labeled=(l, p) => TupLabelPat(LabelPat(l), p),
          ~unlabeled=p => TupLabelPat(ExplicitNonlabelPat, p),
          gen_pat_sized(n / 2),
        )
      ]
      list(pat),
    )
  | ConsPat(pat, pat)
  | ListPat([@gen gen_ppx_small_list(gen_pat_sized(n / 2))] list(pat))
  | ApPat(pat, pat)
  | InvalidPat([@gen gen_invalid_token] string)
  /* Also injected as tuple fields via TuplePat above. */
  | TupLabelPat(
      [@gen
        gen_tup_label_lhs(
          ~label=l => LabelPat(l),
          ~nonlabel=ExplicitNonlabelPat,
        )
      ] pat,
      pat,
    )
  | LabelPat([@gen gen_ppx_ident] string)
  | IndicationPat(pat)
  | ExplicitNonlabelPat

and if_consistency =
  | Consistent
  | Inconsistent

and deferral_pos =
  | InAp
  | OutsideAp

and exp =
  | ParenExp(exp)
  | Atom([@gen gen_ppx_atom] Language.Atom.t)
  | Var([@gen gen_ppx_ident] string)
  | LivelitName([@gen gen_ppx_livelit_ident] string) /* lexeme with the leading caret */
  /* Constructor type payloads are not printed; Canonicalize strips them. */
  | Constructor(
      [@gen gen_ppx_constructor_ident] string,
      option(option(typ)),
    )
  | ListExp([@gen gen_ppx_small_list(gen_exp_sized(n / 2))] list(exp))
  | TupleExp(
      [@gen
        gen_tuple_fields(
          ~labeled=(l, e) => TupLabel(Label(l), e),
          ~unlabeled=e => TupLabel(ExplicitNonlabel, e),
          gen_exp_sized(n / 2),
        )
      ]
      list(exp),
    )
  | BinExp(exp, bin_op, exp)
  | UnOp(op_un, exp)
  | Let(pat, exp, exp)
  | Theorem(pat, exp, exp)
  | ProofObject(exp)
  /* Named funs print as plain `fun`; Canonicalize drops the name. */
  | Fun(pat, exp, [@gen QCheck.Gen.option(gen_ppx_ident)] option(string))
  | ForallExp(pat, exp)
  | CaseExp(
      exp,
      [@gen
        gen_ppx_small_list1(
          QCheck.Gen.pair(gen_pat_sized(n / 2), gen_exp_sized(n / 2)),
        )
      ]
      list((pat, exp)),
    )
  | Label([@gen gen_ppx_ident] string)
  /* Prints as `_`; Canonicalize rewrites a bare exp `_` to Deferral. */
  | ExplicitNonlabel
  /* Also injected as tuple fields via TupleExp above. */
  | TupLabel(
      [@gen
        gen_tup_label_lhs(~label=l => Label(l), ~nonlabel=ExplicitNonlabel)
      ] exp,
      exp,
    )
  | Dot(exp, exp)
  | ApExp(exp, exp)
  | PipelineExp(exp, exp) /* e1 |> e2 == Ap(Reverse, e2, e1) */
  | FixF(pat, exp)
  | Asc(exp, typ)
  | EmptyHole
  | Filter(filter_action, exp, exp)
  /* Prints as a bare name; Canonicalize rewrites to Var. */
  | BuiltinFun([@gen gen_ppx_ident] string)
  | Undefined
  | Seq(exp, exp)
  | Test(exp)
  | HintedTest(exp, exp)
  | Deferral
  | TypFun(tpat, exp)
  | Cons(exp, exp)
  | ListConcat(exp, exp)
  | If(exp, exp, exp)
  | InvalidExp([@gen gen_invalid_token] string)
  | TypAp(exp, typ)
  /* Stripped on print; Canonicalize unwraps to the inner expression. */
  | DynamicErrorHole(exp, [@gen gen_ppx_error] string)
  | TyAlias(tpat, typ, exp)
  | Use(typ, exp)
  | IndicationExp(exp)
  | TupleExtension(exp, exp)
  | Module(
      [@gen gen_ppx_small_list(gen_mod_item_sized(n / 2))] list(mod_item),
    )
  /* Menhir binders are IDENT/CTR (as VarPat), wild, or hole — keep to vars. */
  | ModuleExp(
      [@gen
        QCheck.Gen.(
          map(
            name => VarPat(name),
            oneof([gen_ppx_ident, gen_ppx_constructor_ident]),
          )
        )
      ] pat,
      exp,
      exp,
    )

and mod_item =
  | ModItemLet(pat, exp)
  | ModItemType(tpat, typ)
  | ModItemExp(exp)
  /* Menhir only accepts `module name [=|: typ =] …` with IDENT/CTR binders. */
  | ModItemModule(
      [@gen
        QCheck.Gen.(
          map(
            name => VarPat(name),
            oneof([gen_ppx_ident, gen_ppx_constructor_ident]),
          )
        )
      ] pat,
      exp,
    )

and sig_item =
  | SigItemLet(pat)
  | SigItemType(tpat, typ);

/* Memoize by fuel: constructing gen_*_sized(n) eagerly expands the frequency
 * tree to ~positions^log2(n) closures, so rebuilding it per suite OOMs Node. */
let memo_by_fuel =
    (derived: int => QCheck.Gen.t('a)): (int => QCheck.Gen.t('a)) => {
  let cache: Hashtbl.t(int, QCheck.Gen.t('a)) = Hashtbl.create(8);
  n =>
    switch (Hashtbl.find_opt(cache, n)) {
    | Some(g) => g
    | None =>
      let g = derived(n);
      Hashtbl.add(cache, n, g);
      g;
    };
};

let gen_typ_full_sized: int => QCheck.Gen.t(typ) =
  memo_by_fuel(gen_typ_sized);
let gen_exp_full_sized: int => QCheck.Gen.t(exp) =
  memo_by_fuel(gen_exp_sized);

let shrink_non_empty_string: QCheck.Shrink.t(string) =
  x => QCheck.Shrink.(filter(x => String.length(x) != 0, string, x));

let pat_typ_opt = (p: pat): option(typ) =>
  switch (p) {
  | AscPat(_, t) => Some(t)
  | _ => None
  };

let rec shrink_exp: QCheck.Shrink.t(exp) =
  QCheck.(
    (exp: exp) =>
      Iter.(
        switch (exp) {
        | ParenExp(e) => return(e)
        | Atom(a) =>
          return(TupleExp([]))
          <+> (
            switch (a) {
            | Int(i) =>
              switch (Bigint.to_int(i)) {
              | Some(i) =>
                Shrink.int(i)
                >|= ((i: int) => Atom(Int(Bigint.of_int(i))))
              | None => Iter.empty
              }
            | String(s) =>
              Shrink.string(s) >|= ((s: string) => Atom(String(s)))
            | Bool(b) => Shrink.bool(b) >|= ((b: bool) => Atom(Bool(b)))
            | Nat(n) =>
              if (Bigint.(<)(n, Bigint.of_int(2))) {
                Iter.empty;
              } else {
                return(Atom(Nat(Bigint.(/)(n, Bigint.of_int(2)))));
              }
            | SInt(i) => Shrink.int(i) >|= ((i: int) => Atom(SInt(i)))
            | _ => Iter.empty
            }
          )
        | Var(x) =>
          return(TupleExp([]))
          <+> (shrink_non_empty_string(x) >|= ((x: string) => Var(x))) // TODO This isn't great for vars
        | Constructor(_, _) => return(TupleExp([])) // Constructor ident format is preserved; only allow collapse to unit
        | ListExp(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_exp);
          switch (shrunk) {
          | [x] => return(ListExp(shrunk)) <+> Iter.return(x)
          | _ => return(ListExp(shrunk))
          };
        | TupleExp(l) =>
          if (List.length(l) <= 1) {
            Iter.empty;
          } else {
            let* shrunk = Shrink.list(l, ~shrink=shrink_exp);
            switch (shrunk) {
            | [] => Iter.return(TupleExp([]))
            | [x] =>
              switch (x) {
              | TupLabel(_, _) => Iter.return(TupleExp(shrunk))
              | _ => Iter.return(x)
              }
            | _ => return(TupleExp(shrunk))
            };
          }
        | BinExp(e1, op, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            shrink_exp(e1) >|= (e1 => BinExp(e1, op, e2));
          }
          <+> {
            shrink_exp(e2) >|= (e2 => BinExp(e1, op, e2));
          }
        | UnOp(op, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(UnOp(op, shrunk));
          }
        | Let(p, e1, e2) =>
          of_list([e1, e2])
          <+> (
            switch (pat_typ_opt(p)) {
            | Some(t) => of_list([Asc(e1, t), Asc(e2, t)])
            | None => Iter.empty
            }
          )
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Let(p, shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Let(p, e1, shrunk));
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(Let(shrunk, e1, e2));
          }
        | Theorem(p, e1, e2) =>
          of_list([e1, e2])
          <+> (
            switch (pat_typ_opt(p)) {
            | Some(t) => of_list([Asc(e1, t), Asc(e2, t)])
            | None => Iter.empty
            }
          )
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Theorem(p, shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Theorem(p, e1, shrunk));
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(Theorem(shrunk, e1, e2));
          }
        | ProofObject(t) =>
          let* shrunk = shrink_exp(t);
          return(ProofObject(shrunk));
        | ForallExp(pat, e) =>
          return(e)
          <+> (
            switch (pat_typ_opt(pat)) {
            | Some(t) => return(Asc(e, t))
            | None => Iter.empty
            }
          )
          <+> {
            let* shrunk = shrink_exp(e);
            return(ForallExp(pat, shrunk));
          }
          <+> {
            let* shrunk = shrink_pat(pat);
            return(ForallExp(shrunk, e));
          }
        | Fun(p, e, name) =>
          return(e)
          <+> (
            switch (pat_typ_opt(p)) {
            | Some(t) => return(Asc(e, t))
            | None => Iter.empty
            }
          )
          <+> {
            let* shrunk = shrink_exp(e);
            return(Fun(p, shrunk, name): exp);
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(Fun(shrunk, e, name): exp);
          }
        | CaseExp(e, cases) =>
          {
            return(e);
          }
          <+> {
            let* shrunk = shrink_exp(e);
            return(CaseExp(shrunk, cases));
          }
          <+> {
            let shrink_case: QCheck.Shrink.t((pat, exp)) =
              QCheck.(
                (
                  ((pat, exp)) =>
                    Iter.(
                      {
                        let* pat = shrink_pat(pat);
                        return((pat, exp));
                      }
                      <+> {
                        let* exp = shrink_exp(exp);
                        return((pat, exp));
                      }
                    )
                )
              );
            let* shrunk = Shrink.list(cases, ~shrink=shrink_case);
            return(CaseExp(e, shrunk));
          }
        | Label(l) =>
          shrink_non_empty_string(l) >|= ((l: string) => Label(l))
        | ExplicitNonlabel => Iter.empty
        | TupLabel(e1, e2) =>
          {
            return(
              e2 // e1 is a label
            );
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(TupLabel(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(TupLabel(e1, shrunk));
          }
        | Dot(e1, e2) =>
          {
            return(
              e1 // e2 is a label
            );
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Dot(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Dot(e1, shrunk));
          }
        | ApExp(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(ApExp(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(ApExp(e1, shrunk));
          }
        | PipelineExp(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(PipelineExp(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(PipelineExp(e1, shrunk));
          }
        | TypAp(e, t) =>
          {
            return(e);
          }
          <+> {
            let* shrunk = shrink_exp(e);
            return(TypAp(shrunk, t));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(TypAp(e, shrunk));
          }
        | FixF(p, e) =>
          return(e)
          <+> (
            switch (pat_typ_opt(p)) {
            | Some(t) => return(Asc(e, t))
            | None => Iter.empty
            }
          )
          <+> {
            let* shrunk = shrink_exp(e);
            return(FixF(p, shrunk));
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(FixF(shrunk, e));
          }
        | Asc(e, t) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(Asc(shrunk, t));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(Asc(e, shrunk));
          }
        | Filter(fa, e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Filter(fa, shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Filter(fa, e1, shrunk));
          }
        | Seq(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Seq(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Seq(e1, shrunk));
          }
        | Test(e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(Test(shrunk));
          }
        | HintedTest(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(HintedTest(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(HintedTest(e1, shrunk));
          }
        | Deferral => Iter.empty
        | TypFun(tpat, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(TypFun(tpat, shrunk));
          } // Not worth shrinking tpat
        | Cons(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Cons(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Cons(e1, shrunk));
          }
        | TupleExtension(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(TupleExtension(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(TupleExtension(e1, shrunk));
          }
        | ListConcat(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(ListConcat(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(ListConcat(e1, shrunk));
          }
        | DynamicErrorHole(e, s) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(DynamicErrorHole(shrunk, s));
          }
        | TyAlias(tpat, t, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(TyAlias(tpat, t, shrunk));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(TyAlias(tpat, shrunk, e));
          }
        | Use(t, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(Use(t, shrunk));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(Use(shrunk, e));
          }
        | If(e1, e2, e3) =>
          {
            of_list([e1, e2, e3]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(If(shrunk, e2, e3));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(If(e1, shrunk, e3));
          }
          <+> {
            let* shrunk = shrink_exp(e3);
            return(If(e1, e2, shrunk));
          }
        | ModuleExp(p, e1, e2) =>
          of_list([e1, e2])
          <+> {
            let* shrunk = shrink_exp(e1);
            return(ModuleExp(p, shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(ModuleExp(p, e1, shrunk));
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(ModuleExp(shrunk, e1, e2));
          }
        | IndicationExp(_)
        | EmptyHole
        | BuiltinFun(_)
        | Undefined
        | InvalidExp(_)
        | LivelitName(_)
        | Module(_) => Iter.empty
        }
      )
  )
and shrink_pat: QCheck.Shrink.t(pat) =
  QCheck.(
    (pat: pat) =>
      Iter.(
        switch (pat) {
        | ParenPat(p) => return(p)
        | AtomPat(a) =>
          return(WildPat)
          <+> (
            switch (a) {
            | Int(i) =>
              switch (Bigint.to_int(i)) {
              | Some(i) =>
                Shrink.int(i)
                >|= ((i: int) => AtomPat(Int(Bigint.of_int(i))))
              | None => Iter.empty
              }
            | String(s) =>
              Shrink.string(s) >|= ((s: string) => AtomPat(String(s)))
            | Bool(b) => Shrink.bool(b) >|= ((b: bool) => AtomPat(Bool(b)))
            | Nat(n) =>
              if (Bigint.(<)(n, Bigint.of_int(2))) {
                Iter.empty;
              } else {
                return(AtomPat(Nat(Bigint.(/)(n, Bigint.of_int(2)))));
              }
            | SInt(i) => Shrink.int(i) >|= ((i: int) => AtomPat(SInt(i)))
            | _ => Iter.empty
            }
          )
        | VarPat(x) =>
          return(WildPat)
          <+> (shrink_non_empty_string(x) >|= ((x: string) => VarPat(x)))
        | ConstructorPat(_) => return(WildPat) // Constructor ident format is preserved; only allow collapse to wild
        | ListPat(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_pat);
          switch (shrunk) {
          | [x] => return(ListPat(shrunk)) <+> Iter.return(x)
          | _ => return(ListPat(shrunk))
          };
        | AscPat(p, t) =>
          return(p)
          <+> {
            let* shrunk = shrink_pat(p);
            return(AscPat(shrunk, t));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(AscPat(p, shrunk));
          }
        | ApPat(p1, p2) =>
          {
            of_list([p1, p2]);
          }
          <+> {
            let* shrunk = shrink_pat(p1);
            return(ApPat(shrunk, p2));
          }
          <+> {
            let* shrunk = shrink_pat(p2);
            return(ApPat(p1, shrunk));
          }
        | TuplePat(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_pat);
          switch (shrunk) {
          | [] => Iter.return(TuplePat([]))
          | [x] => Iter.return(x)
          | _ => return(TuplePat(shrunk))
          };
        | ConsPat(p1, p2) =>
          {
            of_list([p1, p2]);
          }
          <+> {
            let* shrunk = shrink_pat(p1);
            return(ConsPat(shrunk, p2));
          }
          <+> {
            let* shrunk = shrink_pat(p2);
            return(ConsPat(p1, shrunk));
          }
        | TupLabelPat(p1, p2) =>
          {
            return(
              p2 // p1 is a label
            );
          }
          <+> {
            let* shrunk = shrink_pat(p1);
            return(TupLabelPat(shrunk, p2));
          }
          <+> {
            let* shrunk = shrink_pat(p2);
            return(TupLabelPat(p1, shrunk));
          }
        | LabelPat(l) =>
          shrink_non_empty_string(l) >|= ((l: string) => LabelPat(l))
        | ExplicitNonlabelPat
        | InvalidPat(_)
        | IndicationPat(_)
        | WildPat
        | EmptyHolePat => Iter.empty
        }
      )
  )
and shrink_typ: QCheck.Shrink.t(typ) =
  QCheck.(
    (typ: typ) =>
      Iter.(
        switch (typ) {
        | ParenTyp(t) => return(t)
        | SumTyp(l) =>
          let payloads =
            List.filter_map(
              fun
              | Variant(_, Some(t)) => Some(t)
              | BadEntry(t) => Some(t)
              | Variant(_, None) => None,
              l,
            );
          let shrink_sumterm: QCheck.Shrink.t(sumterm) =
            QCheck.(
              (
                (sumterm: sumterm) =>
                  Iter.(
                    switch (sumterm) {
                    | Variant(_) => Iter.empty
                    | BadEntry(t) =>
                      let* shrunk = shrink_typ(t);
                      return(BadEntry(shrunk));
                    }
                  )
              )
            );
          of_list(payloads)
          <+> {
            let* shrunk = Shrink.list(l, ~shrink=shrink_sumterm);
            switch (shrunk) {
            | [] => Iter.empty
            | _ => return(SumTyp(shrunk))
            };
          };
        | TupleType(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_typ);
          switch (shrunk) {
          | [x] => Iter.return(x)
          | _ => return(TupleType(shrunk))
          };
        | ArrayType(t) =>
          return(t)
          <+> {
            let* shrunk = shrink_typ(t);
            return(ArrayType(shrunk));
          }
        | ArrowType(t1, t2) =>
          of_list([t1, t2])
          <+> {
            let* shrunk1 = shrink_typ(t1);
            return(ArrowType(shrunk1, t2));
          }
          <+> {
            let* shrunk2 = shrink_typ(t2);
            return(ArrowType(t1, shrunk2));
          }
        | TypVar(x) =>
          shrink_non_empty_string(x) >|= ((x: string) => TypVar(x))
        | PolyType(tpat, t) =>
          return(t)
          <+> {
            let* shrunk = shrink_typ(t);
            return(PolyType(tpat, shrunk));
          }
        | RecType(tpat, t) =>
          return(t)
          <+> {
            let* shrunk = shrink_typ(t);
            return(RecType(tpat, shrunk));
          }
        | ExplicitNonlabelType => Iter.empty
        | ProofOfType(e) =>
          let* shrunk = shrink_exp(e);
          return(ProofOfType(shrunk));
        | LabelType(x) =>
          shrink_non_empty_string(x) >|= ((x: string) => LabelType(x))
        | TupLabelType(t1, t2) =>
          return(t2)
          <+> {
            let* shrunk1 = shrink_typ(t1);
            return(TupLabelType(shrunk1, t2));
          }
          <+> {
            let* shrunk2 = shrink_typ(t2);
            return(TupLabelType(t1, shrunk2));
          }
        | ProdProjection(t1, t2) =>
          return(t1)
          <+> {
            let* shrunk1 = shrink_typ(t1);
            return(ProdProjection(shrunk1, t2));
          }
          <+> {
            let* shrunk2 = shrink_typ(t2);
            return(ProdProjection(t1, shrunk2));
          }
        | ProdExtension(t1, t2) =>
          return(t1)
          <+> {
            let* shrunk1 = shrink_typ(t1);
            return(ProdExtension(shrunk1, t2));
          }
          <+> {
            let* shrunk2 = shrink_typ(t2);
            return(ProdExtension(t1, shrunk2));
          }
        | IntType
        | SIntType
        | StringType
        | FloatType
        | BoolType
        | NatType => return(TupleType([]))
        | VoidType
        | IndicationTyp(_)
        | UnknownType(_)
        | InvalidTyp(_)
        | Sig(_) => Iter.empty
        }
      )
  );
/* `size` is depth fuel, halved at each recursive step — not a node budget. */
let arb_typ_full = size =>
  QCheck.make(~print=show_typ, ~shrink=shrink_typ, gen_typ_full_sized(size));
let arb_exp_full = size =>
  QCheck.make(~print=show_exp, ~shrink=shrink_exp, gen_exp_full_sized(size));
