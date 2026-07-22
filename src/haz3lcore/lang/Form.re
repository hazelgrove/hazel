open Util;
open Sort;
module P = Precedence;

/* The form identity types (atomic_form, family, FormId.t) live in
 * FormId.re; included here so existing references (Form.family, bare
 * constructors, all_of_family, ...) keep resolving. In particular
 * Form.t = FormId.t: a form is its identity; the definition
 * record (label/mold/expansion) is Form.def below. */
include FormId;

/* This module determines the syntactic extent of the language; the
 * entire Syntax module is driven by the below definitions. To add
 * a new syntactic form add a case to either the atomic_form or
 * family types and follow the errors: each family needs its rows in
 * `defs_of` and a position per row in `priority` (checked at module
 * init). The definitions determine the shape, precedence, and
 * expansion behavior of each form. */

/* When you complete a token corresponding to a delimiter of a
 * compound form, that token might be 'expanded', which is to say,
 * converted from a monotile to a incomplete polytile aka shard
 * representing a part of specific form. All leading delimiters
 * of all forms trigger expansion, but not all trailing
 * delimiters, for example ambiguous trailing delimiters like
 * `in` or `>`. Thus we designate certain delimiters as expanding
 * on a per-form basis. Expanding delimiters should be globally
 * unique, and indeed it would probably be better if we dispensed
 * with this explicit classification and instead derived this
 * information from the grammar. Currently all forms fall into two
 * classes: Either only the leading delimiter triggers expansion,
 * or the leading and trailing delimiters trigger expansion */
[@deriving (show({with_path: false}), sexp, yojson)]
type expansion =
  | Non /* Non-expandind: Placeholder for atomic forms */
  | L /* Leading-only: All keyword forms and some special cases */
  | LT; /* Leading and trailing: Used for parethesis-like things */

[@deriving (show({with_path: false}), sexp, yojson)]
type expansions = list((Token.t, (Label.t, Direction.t)));

/* Sort-aware expansions include the form's sort for filtering */
[@deriving (show({with_path: false}), sexp, yojson)]
type sorted_expansions = list((Token.t, Sort.t, Label.t, Direction.t));

/* A label, a mold, and expansion behavior together determine a form. */
[@deriving (show({with_path: false}), sexp, yojson)]
type def = {
  label: Label.t,
  mold: Mold.t,
  expansion,
};

let mk = (expansion, label, mold) => {
  label,
  mold,
  expansion,
};

let op = Mold.mk_op(_, []);

let mk_atom = (t: Token.t, mold_of) => mk(Non, [t], mold_of([]));

let mk_atom_op = (sort: Sort.t, t: Token.t) => mk_atom(t, Mold.mk_op(sort));

let mk_infix = (t: Token.t, sort: Sort.t, ~l=?, ~r=?, prec) =>
  mk_atom(t, Mold.mk_bin(prec, sort, ~l?, ~r?));

let mk_prefix = (t: Token.t, sort: Sort.t, prec) =>
  mk_atom(t, Mold.mk_pre(prec, sort));

let mk_pre_c =
    (exp, label: Label.t, prec, sort: Sort.t, inner_sorts: list(Sort.t)) =>
  mk(exp, label, Mold.mk_pre(prec, sort, inner_sorts));

// Prefix form where the body (right operand) has a different sort than out
let mk_pre_c' =
    (
      exp,
      label: Label.t,
      prec,
      sort: Sort.t,
      inner_sorts: list(Sort.t),
      body_sort: Sort.t,
    ) =>
  mk(exp, label, Mold.mk_pre'(prec, sort, inner_sorts, body_sort));

let mk_op_c = (exp, label: Label.t, sort: Sort.t, inner_sorts: list(Sort.t)) =>
  mk(exp, label, Mold.mk_op(sort, inner_sorts));

let mk_post_c =
    (exp, label: Label.t, prec, sort: Sort.t, child_sorts: list(Sort.t)) =>
  mk(exp, label, Mold.mk_post(prec, sort, child_sorts));

let mk_parens = (sort: Sort.t) =>
  mk_op_c(LT, Token.tuple_lbl, sort, [sort]);

/* The definition rows of each family: every mold the family's label
 * takes, one row per out sort (the Dot family's duplicate Typ row is
 * the one exception, kept for candidate-multiplicity fidelity). All
 * rows of a family share its label and outer-nib shape-role, and
 * (out sort -> mold) is a function on them (machine-checked in
 * test/Test_FormId.re). Row order within a family follows the global
 * `priority` order below. */
let defs_of: family => list(def) =
  fun
  | TypeArrow => [
      mk_infix("->", Typ, P.type_arrow),
      mk_infix("->", Drv(Typ), P.type_arrow),
    ]
  | CellJoin => [
      mk_infix(";", Exp, P.semi),
      mk_infix(";", Mod, P.mod_seq),
      mk_infix(";", Sig, P.mod_seq),
    ]
  | Plus => [
      mk_infix("+", Exp, P.plus),
      mk_infix("+", Typ, P.type_plus),
      mk_infix("+", Drv(Exp), P.plus),
      mk_infix("+", Drv(Typ), P.type_plus),
    ]
  | Minus => [mk_infix("-", Exp, P.plus), mk_infix("-", Drv(Exp), P.plus)]
  | Times => [
      mk_infix("*", Exp, P.mult),
      mk_infix("*", Drv(Exp), P.mult),
      mk_infix("*", Drv(Typ), P.type_prod),
    ]
  | Power => [mk_infix("**", Exp, P.power)]
  | FPower => [mk_infix("**.", Exp, P.power)]
  | Divide => [mk_infix("/", Exp, P.mult)]
  | Equals => [mk_infix("==", Exp, P.eqs), mk_infix("==", Drv(Exp), P.eqs)]
  | StringConcat => [mk_infix("++", Exp, P.concat)]
  | Lt => [mk_infix("<", Exp, P.eqs), mk_infix("<", Drv(Exp), P.eqs)]
  | Gt => [mk_infix(">", Exp, P.eqs), mk_infix(">", Drv(Exp), P.eqs)]
  | NotEquals => [mk_infix("!=", Exp, P.eqs)]
  | Gte => [mk_infix(">=", Exp, P.eqs)]
  | Lte => [
      mk_infix("<=", Exp, P.eqs),
      /* derivation Ana judgment */
      mk(
        L,
        ["<="],
        Mold.mk_bin'(P.ann, Drv(Exp), Drv(Exp), [], Drv(Typ)),
      ),
    ]
  | FPlus => [mk_infix("+.", Exp, P.plus)]
  | FMinus => [mk_infix("-.", Exp, P.plus)]
  | FTimes => [mk_infix("*.", Exp, P.mult)]
  | FDivide => [mk_infix("/.", Exp, P.mult)]
  | FEquals => [mk_infix("==.", Exp, P.eqs)]
  | FLt => [mk_infix("<.", Exp, P.eqs)]
  | FGt => [mk_infix(">.", Exp, P.eqs)]
  | FNotEquals => [mk_infix("!=.", Exp, P.eqs)]
  | FGte => [mk_infix(">=.", Exp, P.eqs)]
  | FLte => [mk_infix("<=.", Exp, P.eqs)]
  | LogicalAnd => [mk_infix("&&", Exp, P.and_)]
  | LogicalOrLegacy => [
      mk_infix("\\/", Exp, P.or_),
      mk_infix("\\/", Drv(Exp), P.or_),
    ]
  | LogicalOr => [mk_infix("||", Exp, P.or_)]
  | ListConcat => [
      mk_infix("@", Exp, P.concat),
      mk_infix("@", Drv(Exp), P.plus),
    ]
  | Cons => [
      mk_infix("::", Exp, P.cons),
      mk_infix("::", Pat, P.cons),
      mk(
        L,
        ["::"],
        Mold.mk_bin'(P.cons, Drv(Exp), Drv(Exp), [], Drv(Exp)),
      ),
    ]
  | TypeAsc => [
      mk_infix(":", Pat, ~l=Pat, ~r=Typ, P.asc),
      mk_infix(":", Exp, ~l=Exp, ~r=Typ, P.asc),
      /* derivation HasType judgment */
      mk(
        L,
        [":"],
        Mold.mk_bin'(P.ann, Drv(Exp), Drv(Exp), [], Drv(Typ)),
      ),
      /* derivation Cast */
      mk(
        L,
        [":"],
        Mold.mk_bin'(P.asc, Drv(Pat), Drv(Pat), [], Drv(Typ)),
      ),
      mk_infix(":", MPat, ~l=MPat, ~r=Typ, P.asc),
    ]
  | TupleLabeled => [
      mk_infix("=", Exp, P.lab),
      mk_infix("=", Pat, P.lab),
      mk_infix("=", Typ, P.lab),
    ]
  | Dot => [
      mk_infix(".", Exp, P.dot),
      mk_infix(".", Typ, P.dot),
      /* duplicate Typ row: the pre-family DotTyp and ProdProjection
       * were byte-identical; both rows are kept so remold candidate
       * multiplicity is unchanged */
      mk_infix(".", Typ, P.dot),
      mk_infix(".", Drv(Exp), P.dot),
    ]
  | TupleExtension => [
      mk_infix("...", Exp, P.plus),
      mk_infix("...", Typ, P.ap),
    ]
  | Not => [
      mk_prefix("!", Exp, P.not_),
      mk_pre_c(L, ["!"], P.neg, Drv(Exp), []),
    ]
  | SumSingle => [mk_prefix("+", Typ, P.or_)]
  | UnaryMinus => [
      mk_prefix("-", Exp, P.neg),
      mk_pre_c(L, ["-"], P.neg, Drv(Exp), []),
    ]
  | Comma => [
      mk_infix(",", Exp, P.comma),
      mk_infix(",", Pat, P.comma),
      mk_infix(",", Typ, P.comma),
      mk_infix(",", Drv(Exp), P.comma),
      mk_infix(",", Drv(Pat), P.comma),
    ]
  | ListLit => [
      mk_op_c(LT, ["[", "]"], Exp, [Exp]),
      mk_op_c(LT, ["[", "]"], Pat, [Pat]),
      mk_op_c(LT, ["[", "]"], Typ, [Typ]),
      mk_op_c(LT, ["[", "]"], Drv(Exp), [Drv(Exp)]),
    ]
  | Parens => [
      mk_parens(Exp),
      mk_parens(Pat),
      mk_parens(Typ),
      mk_parens(TPat), // HACk (Issue #1913)
      mk_parens(Drv(Prop)),
      mk_parens(Drv(Exp)),
      mk_parens(Drv(Pat)),
      mk_parens(Drv(Typ)),
    ]
  | ApEmpty => [
      mk_post_c(LT, ["()"], P.ap, Exp, []),
      mk_post_c(LT, ["()"], P.ap, Pat, []),
      mk_post_c(LT, ["()"], P.ap, Drv(Exp), []),
    ]
  | Ap => [
      mk_post_c(LT, ["(", ")"], P.ap, Exp, [Exp]),
      mk_post_c(LT, ["(", ")"], P.ap, Pat, [Pat]),
      mk_post_c(LT, ["(", ")"], P.type_sum_ap, Typ, [Typ]),
      mk_post_c(LT, ["(", ")"], P.ap, Drv(Exp), [Drv(Exp)]),
      mk_post_c(LT, ["(", ")"], P.ap, Drv(Pat), [Drv(Pat)]),
    ]
  | ApExpTyp => [mk_post_c(L, ["@<", ">"], P.ap, Exp, [Typ])]
  | Case => [
      mk_op_c(L, ["case", "end"], Exp, [Rul]),
      mk_op_c(L, ["case", "end"], Drv(Exp), [Drv(Exp)]),
    ]
  | Test => [mk_op_c(L, ["test", "end"], Exp, [Exp])]
  | ProofOf => [mk_op_c(L, ["proof_of", "end"], Typ, [Exp])]
  | ProofObject => [mk_op_c(L, ["proof_object", "end"], Exp, [Exp])]
  | HintedTest => [mk_op_c(L, ["hint", "test", "end"], Exp, [Exp, Exp])]
  | Fun => [
      mk_pre_c(L, ["fun", "->"], P.fun_, Exp, [Pat]),
      mk_pre_c(L, ["fun", "->"], P.fun_, Drv(Exp), [Drv(Pat)]),
    ]
  | Fix => [
      mk_pre_c(L, ["fix", "->"], P.fun_, Exp, [Pat]),
      mk_pre_c(L, ["fix", "->"], P.fun_, Drv(Exp), [Drv(Pat)]),
    ]
  | TypFun => [mk_pre_c(L, ["typfun", "->"], P.fun_, Exp, [TPat])]
  | Poly => [mk_pre_c(L, ["poly", "->"], P.fun_, Typ, [TPat])]
  | Forall => [mk_pre_c(L, ["forall", "->"], P.fun_, Exp, [Pat])]
  | Rec => [
      mk_pre_c(L, ["rec", "->"], P.fun_, Typ, [TPat]),
      mk_pre_c(L, ["rec", "->"], P.fun_, Drv(Typ), [Drv(TPat)]),
    ]
  | Rule => [
      mk(L, ["|", "=>"], Mold.mk_bin'(P.rule_sep, Rul, Exp, [Pat], Exp)),
      mk(
        L,
        ["|", "=>"],
        Mold.mk_bin'(
          P.rule_sep,
          Drv(Exp),
          Drv(Exp),
          [Drv(Pat)],
          Drv(Exp),
        ),
      ),
    ]
  | Pipeline => [mk_infix("|>", Exp, P.eqs)] // in OCaml, pipeline precedence is in same class as '=', '<', etc.
  | FilterHide => [mk_pre_c(L, ["hide", "in"], P.let_, Exp, [Exp])]
  | FilterEval => [mk_pre_c(L, ["eval", "in"], P.let_, Exp, [Exp])]
  | FilterPause => [mk_pre_c(L, ["pause", "in"], P.let_, Exp, [Exp])]
  | FilterDebug => [mk_pre_c(L, ["debug", "in"], P.let_, Exp, [Exp])]
  | Use => [mk_pre_c(L, ["use", "in"], P.let_, Exp, [Typ])]
  | OfProp => [mk_op_c(L, ["of_prop", "end"], Exp, [Drv(Exp)])]
  | OfCtx => [mk_op_c(L, ["of_ctx", "end"], Exp, [Drv(Exp)])]
  | OfJdmt => [mk_op_c(L, ["of_jdmt", "end"], Exp, [Drv(Exp)])]
  | OfAlfaExp => [mk_op_c(L, ["of_alfa_exp", "end"], Exp, [Drv(Exp)])]
  | OfAlfaTyp => [mk_op_c(L, ["of_alfa_typ", "end"], Exp, [Drv(Typ)])]
  | OfAlfaPat => [mk_op_c(L, ["of_alfa_pat", "end"], Exp, [Drv(Pat)])]
  | OfAlfaTPat => [mk_op_c(L, ["of_alfa_tpat", "end"], Exp, [Drv(TPat)])]
  | Subst => [
      mk_pre_c(
        Non,
        ["[", "/", "]"],
        P.fun_,
        Drv(Exp),
        [Drv(Exp), Drv(Pat)],
      ),
      mk_pre_c(
        Non,
        ["[", "/", "]"],
        P.fun_,
        Drv(Typ),
        [Drv(Typ), Drv(TPat)],
      ),
    ]
  | Glb => [
      mk_op_c(Non, ["glb(", ",", ")"], Drv(Typ), [Drv(Typ), Drv(Typ)]),
    ]
  | Val => [mk_op_c(L, ["val", "end"], Drv(Exp), [Drv(Exp)])]
  | Eval => [mk_infix("\\=/", Drv(Exp), P.min)]
  | Entail => [mk_infix("|-", Drv(Exp), P.min)]
  | UnaryEntail => [mk_pre_c(L, ["|-"], P.min, Drv(Exp), [])]
  | Consistent => [
      mk(
        L,
        ["consistent", "~"],
        Mold.mk_pre'(P.fun_, Drv(Exp), [Drv(Typ)], Drv(Typ)),
      ),
    ]
  | MatchedArrow => [
      mk(
        L,
        ["matched_arrow", "with"],
        Mold.mk_pre'(P.fun_, Drv(Exp), [Drv(Typ)], Drv(Typ)),
      ),
    ]
  | MatchedProd => [
      mk(
        L,
        ["matched_prod", "with"],
        Mold.mk_pre'(P.fun_, Drv(Exp), [Drv(Typ)], Drv(Typ)),
      ),
    ]
  | MatchedSum => [
      mk(
        L,
        ["matched_sum", "with"],
        Mold.mk_pre'(P.fun_, Drv(Exp), [Drv(Typ)], Drv(Typ)),
      ),
    ]
  | Valid => [mk_op_c(L, ["valid", "end"], Drv(Exp), [Drv(Typ)])]
  | Syn => [
      mk(
        L,
        ["=>"],
        Mold.mk_bin'(P.ann, Drv(Exp), Drv(Exp), [], Drv(Typ)),
      ),
    ]
  | And => [mk_infix("/\\", Drv(Exp), P.and_)]
  | Impl => [mk_infix("==>", Drv(Exp), P.impl)]
  | If => [
      mk_pre_c(
        L,
        ["if", "then", "else"],
        P.if_,
        Drv(Exp),
        [Drv(Exp), Drv(Exp)],
      ),
      mk_pre_c(L, ["if", "then", "else"], P.if_, Exp, [Exp, Exp]),
    ]
  | Let => [
      mk_pre_c(
        L,
        ["let", "=", "in"],
        P.let_,
        Drv(Exp),
        [Drv(Pat), Drv(Exp)],
      ),
      mk_pre_c(L, ["let", "=", "in"], P.let_, Exp, [Pat, Exp]),
    ]
  | Theorem => [
      mk_pre_c(L, ["theorem", "=", "in"], P.let_, Exp, [Pat, Exp]),
    ]
  | TypeAlias => [
      mk_pre_c(L, ["type", "=", "in"], P.let_, Exp, [TPat, Typ]),
    ]
  | ModBody => [
      mk_op_c(LT, ["{", "}"], Exp, [Mod]),
      mk_op_c(LT, ["{", "}"], Typ, [Sig]),
    ]
  | ModLet => [mk_pre_c'(L, ["let", "="], P.let_, Mod, [Pat], Exp)]
  | ModType => [
      mk_pre_c'(L, ["type", "="], P.let_, Mod, [TPat], Typ),
      mk_pre_c'(L, ["type", "="], P.let_, Sig, [TPat], Typ),
    ]
  | ModuleExp => [
      mk_pre_c'(L, ["module", "=", "in"], P.let_, Exp, [MPat, Exp], Exp),
    ]
  | ModuleMod => [mk_pre_c'(L, ["module", "="], P.let_, Mod, [MPat], Exp)]
  | SigLet => [mk_pre_c'(L, ["let"], P.let_, Sig, [], Pat)];

/* Global classification/remolding priority: the flat row order of
 * the form table. Each occurrence of a family below is dealt that
 * family's next defs_of row, so a family with n rows appears n
 * times; a count mismatch fails loudly at module init. The
 * interleaving is load-bearing wherever families share a label with
 * rows at the same sort: at Exp/Pat/Typ the Parens row precedes the
 * Ap row while at Drv sorts the Ap row precedes the Parens row
 * (classify picks Ap there — see mk_parens_id); likewise
 * bin-vs-prefix `-` flips between Exp (Minus first) and Drv(Exp)
 * (UnaryMinus first), and bin `+` vs prefix `+` (SumSingle) and
 * Entail vs UnaryEntail depend on their relative positions. */
let priority: list(family) = [
  TypeArrow,
  CellJoin,
  Plus,
  Minus,
  Times,
  Power,
  FPower,
  Divide,
  Equals,
  StringConcat,
  Lt,
  Gt,
  NotEquals,
  Gte,
  Lte,
  FPlus,
  FMinus,
  FTimes,
  FDivide,
  FEquals,
  FLt,
  FGt,
  FNotEquals,
  FGte,
  FLte,
  LogicalAnd,
  LogicalOrLegacy,
  LogicalOr,
  ListConcat,
  Cons,
  Cons,
  TypeAsc,
  TupleLabeled,
  TupleLabeled,
  TupleLabeled,
  Dot,
  TupleExtension,
  Dot,
  TypeAsc,
  Plus,
  Dot,
  TupleExtension,
  Not,
  SumSingle,
  UnaryMinus,
  Comma,
  Comma,
  Comma,
  ListLit,
  ListLit,
  ListLit,
  Parens,
  Parens,
  Parens,
  Parens,
  ApEmpty,
  Ap,
  ApEmpty,
  Ap,
  Ap,
  ApExpTyp,
  Case,
  Test,
  ProofOf,
  ProofObject,
  HintedTest,
  Fun,
  Fix,
  TypFun,
  Poly,
  Forall,
  Rec,
  Rule,
  Pipeline,
  FilterHide,
  FilterEval,
  FilterPause,
  FilterDebug,
  Use,
  OfProp,
  OfCtx,
  OfJdmt,
  OfAlfaExp,
  OfAlfaTyp,
  OfAlfaPat,
  OfAlfaTPat,
  Subst,
  Subst,
  Glb,
  Val,
  Eval,
  Entail,
  UnaryEntail,
  Consistent,
  MatchedArrow,
  MatchedProd,
  MatchedSum,
  Valid,
  TypeAsc,
  Syn,
  Lte,
  And,
  LogicalOrLegacy,
  Impl,
  Not,
  Cons,
  ListConcat,
  ListLit,
  UnaryMinus,
  Plus,
  Minus,
  Times,
  Equals,
  Lt,
  Gt,
  If,
  Let,
  Fix,
  Fun,
  Dot,
  Case,
  Rule,
  TypeAsc,
  TypeArrow,
  Times,
  Plus,
  Rec,
  ApEmpty,
  Ap,
  Ap,
  Comma,
  Comma,
  Parens,
  Parens,
  Parens,
  Parens,
  Let,
  Theorem,
  TypeAlias,
  If,
  ModBody,
  CellJoin,
  ModLet,
  ModType,
  ModuleExp,
  ModuleMod,
  TypeAsc,
  ModBody,
  CellJoin,
  SigLet,
  ModType,
];

let forms: list((family, def)) = {
  let remaining: Hashtbl.t(family, list(def)) = Hashtbl.create(128);
  List.iter(f => Hashtbl.replace(remaining, f, defs_of(f)), all_of_family);
  let deal = (fam: family): def =>
    switch (Hashtbl.find(remaining, fam)) {
    | [] =>
      failwith(
        "Form.forms: priority lists "
        ++ show_family(fam)
        ++ " more often than defs_of has rows",
      )
    | [d, ...rest] =>
      Hashtbl.replace(remaining, fam, rest);
      d;
    };
  let rows = List.map(fam => (fam, deal(fam)), priority);
  List.iter(
    fam =>
      if (Hashtbl.find(remaining, fam) != []) {
        failwith(
          "Form.forms: defs_of("
          ++ show_family(fam)
          ++ ") has rows not listed in priority",
        );
      },
    all_of_family,
  );
  rows;
};

let delims: list(Token.t) =
  forms |> List.concat_map(((_, t)) => t.label) |> List.sort_uniq(compare);

/* These are tokens that have proven annoying as TyDi suggestions.
 * This category is doubly nominative in that it has proven hard
 * to derive automatically; typically these are annoying bacause
 * they have a prefix that occurs more commonly */
let annoying_delims = ["|>", "||", "::", "!=", "!=.", "**."];
let is_annoying_delim = List.mem(_, annoying_delims);

/* Returns a list of all strings which are proper prefixes of
 * a non-leading alphanumeric concave delimiter of a compount form.
 * These are assigned a special backup infix-op mode, so that
 * when you're entering e.g. the `in` in a let, you don't get
 * disruptive switching between a convex variable and concaved
 * delimiter */
let infix_delimiter_ops_prefixes: list(Token.t) =
  forms
  |> List.filter_map(((_, form: def)) => {
       switch ((form.mold.nibs |> snd).shape) {
       /* Could be pickier here, e.g. just trailing delimiters */
       | _ when List.length(form.label) >= 2 => Some(form.label)
       | _ => None
       }
     })
  |> List.concat
  |> List.filter(Token.is_potential_operand)
  |> List.sort_uniq(compare)
  |> List.map(Token.prefixes)
  |> List.concat;

let is_infix_delimiter_op_prefix = List.mem(_, infix_delimiter_ops_prefixes);

/* Tokens that appear both as single-token labels and in other forms labels.
 * These have special put-down behavior to make sure we can actually enter
 * the single-delimiter variant during left-to-right entry */
let amiguous_polymorphs: list(Token.t) = {
  let single_token_labels =
    forms
    |> List.filter_map(((_, {label, _})) =>
         switch (label) {
         | [token] => Some(token)
         | _ => None
         }
       )
    |> Token.sort_uniq;
  let appears_in_other_forms = (target_token: Token.t): bool => {
    forms
    |> List.exists(((_, {label, _})) =>
         switch (label) {
         | [token] when token == target_token => false
         | label => List.mem(target_token, label)
         }
       );
  };
  single_token_labels |> List.filter(appears_in_other_forms);
};

let is_ambiguous_polymorph = List.mem(_, amiguous_polymorphs);

let get_atomic_form: atomic_form => (Token.t => bool, list(Mold.t)) =
  fun
  | Var => (Token.is_var, [op(Exp), op(Pat)])
  | InfixDelimiterPrefix => (
      is_infix_delimiter_op_prefix,
      [
        Mold.mk_bin(Precedence.concave_grout, Exp, []),
        Mold.mk_bin(Precedence.concave_grout, Pat, []),
        Mold.mk_bin(Precedence.concave_grout, Typ, []),
        Mold.mk_bin(Precedence.concave_grout, TPat, []),
      ],
    )
  | ExplicitHole => (
      Token.is_explicit_hole,
      [op(Exp), op(Pat), op(Typ), op(TPat), op(Drv(Typ))],
    )
  | ImplicitHoleMarker => (
      Token.is_implicit_hole_marker,
      [op(Exp), op(Pat), op(Typ), op(TPat), op(Drv(Typ))],
    )
  | LLMHole => (Token.is_llm_hole, [op(Exp), op(Pat), op(Typ), op(TPat)])
  | Wild => (Token.is_wild, [op(Pat), op(Drv(Exp))])
  | String => (Token.is_string, [op(Exp), op(Pat)])
  | QuotedLabel => (Token.is_quoted_label, [op(Exp), op(Pat), op(Typ)])
  | IntLit => (
      Token.is_int,
      [op(Exp), op(Pat), op(Drv(Exp)), op(Drv(Typ))],
    )
  | FloatLit => (Token.is_float, [op(Exp), op(Pat)])
  | LivelitName => (Token.is_livelit, [op(Exp), op(Pat)])
  | ProjectorInvoke => (
      Token.is_projector_invoke,
      [op(Exp), op(Pat), op(Typ), op(TPat)],
    )
  | BoolLit => (Token.is_bool, [op(Exp), op(Pat), op(Drv(Exp))])
  | UndefinedLit => (Token.is_undefined, [op(Exp), op(Pat)])
  | EmptyList => (Token.is_empty_list, [op(Exp), op(Pat), op(Drv(Exp))])
  | EmptyTuple => (
      Token.is_empty_tuple,
      [op(Exp), op(Pat), op(Typ), op(Drv(Exp))],
    )
  | EmptyModule => (Token.is_empty_module, [op(Exp), op(Typ)])
  | Deferral => (Token.is_wild, [op(Exp)])
  | ExplicitNonlabel => (Token.is_wild, [op(Typ)])
  | TyVar => (Token.is_typ_var, [op(Typ)])
  | TyVarP => (Token.is_typ_var, [op(TPat)])
  | Ctr => (Token.is_ctr, [op(Exp), op(Pat)])
  | MPatName => ((t => Token.is_var(t) || Token.is_ctr(t)), [op(MPat)])
  | Type => (Token.is_base_typ, [op(Typ)])
  | DrvVar => (
      Token.is_typ_var,
      [op(Drv(Exp)), op(Drv(Pat)), op(Drv(Typ)), op(Drv(TPat))],
    );

module Expansion = {
  /* Sort-agnostic expansion info (for backward compatibility) */
  let expanding_of = ({expansion, label, _}: def): option(expansions) =>
    switch (expansion, label) {
    | (L, [hd, ..._]) => Some([(hd, (label, Direction.Left))])
    | (LT, [hd, ..._]) =>
      Some([(hd, (label, Left)), (ListUtil.last(label), (label, Right))])
    | _ => None
    };

  /* Sort-aware expansion info - uses nib sorts for context matching.
     Leading delimiters use left nib sort (the context you're in when typing).
     Trailing delimiters use right nib sort.

     Note: This uses nib sort rather than mold.out because the nib sort
     reflects what context you're typing in, not what the form produces.
     For example, Rule ["|", "=>"] has out=Rul but left nib=Exp, since
     you type | after an expression (the previous rule body).

     Limitation: Ascriptions (expr : Type) have Typ right nib even though
     they produce Exp. This causes issues for forms like | that can follow
     ascribed expressions. See Insert.re for the special case handling. */
  let sorted_expanding_of =
      ({expansion, label, mold}: def): option(sorted_expansions) => {
    let (l_nib, r_nib) = mold.nibs;
    switch (expansion, label) {
    | (L, [hd, ..._]) => Some([(hd, l_nib.sort, label, Direction.Left)])
    | (LT, [hd, ..._]) =>
      Some([
        (hd, l_nib.sort, label, Left),
        (ListUtil.last(label), r_nib.sort, label, Right),
      ])
    | _ => None
    };
  };

  /* Sort-agnostic expansions (kept for is_leading) */
  let expansions: expansions =
    List.filter_map(((_, form: def)) => expanding_of(form), forms)
    |> List.flatten
    |> List.sort_uniq(compare);

  /* Sort-aware expansions */
  let sorted_expansions: sorted_expansions =
    List.filter_map(((_, form: def)) => sorted_expanding_of(form), forms)
    |> List.flatten;

  /* Try to get expansion for a token in a specific sort context.
     Returns None if no expansion exists for this sort. */
  let try_get = (sort: Sort.t, t: Token.t): option((Label.t, Direction.t)) => {
    let matching =
      sorted_expansions
      |> List.find_opt(((tok, s, _, _)) => tok == t && s == sort);
    switch (matching) {
    | Some((_, _, lbl, dir)) => Some((lbl, dir))
    | None => None
    };
  };

  /* Get expansion for a token in a specific sort context.
     Returns monotile if no expansion exists for this sort.
     Exception: Rul context is permissive - falls back to any expansion.
     This is because Rul (case rules) contains Exp/Pat operands but has no
     direct forms for things like parens. Other sorts remain strict. */
  let get = (sort: Sort.t, t: Token.t): (Label.t, Direction.t) => {
    let matching =
      sorted_expansions
      |> List.find_opt(((tok, s, _, _)) => tok == t && s == sort);
    switch (matching) {
    | Some((_, _, lbl, dir)) => (lbl, dir)
    | None =>
      switch (sort) {
      | Rul =>
        /* Rul context: fall back to any expansion since rules contain
           Exp/Pat operands but have no direct operand forms. */
        let any_match =
          sorted_expansions |> List.find_opt(((tok, _, _, _)) => tok == t);
        switch (any_match) {
        | Some((_, _, lbl, dir)) => (lbl, dir)
        | None => ([t], Right)
        };
      | _ => ([t], Right)
      }
    };
  };

  /* Check if token would expand in ANY sort (sort-agnostic) */
  let will = (t: Token.t): bool =>
    List.exists(((tok, _, _, _)) => tok == t, sorted_expansions);

  /* Check if token is a leading delimiter in ANY sort (sort-agnostic) */
  let is_leading = (t: Token.t): bool =>
    switch (List.assoc_opt(t, expansions)) {
    | Some((_, Left)) => true
    | _ => false
    };
};

/* FormId lookup/classification layer (property-tested in
 * test/Test_FormId.re). */

let atomic_defs: list((atomic_form, (Token.t => bool, list(Mold.t)))) =
  List.map(a => (a, get_atomic_form(a)), all_of_atomic_form);

/* Atomic candidates for a token, in atomic_form declaration order
 * (classification/remolding priority): (form, mold) pairs. The
 * InfixDelimiterPrefix class carries the TokInfix shape-role; all
 * other classes are token-and-sort determined and collapse to Tok. */
let atomic_candidates = (t: Token.t): list((FormId.t, Mold.t)) =>
  List.concat_map(
    ((a, (pred, molds))) =>
      pred(t)
        ? List.map(
            (m: Mold.t) =>
              a == InfixDelimiterPrefix ? (TokInfix(t), m) : (Tok(t), m),
            molds,
          )
        : [],
    atomic_defs,
  );

/* label => (family, mold) pairs, memoized; per-label order =
 * priority order (remolding priority) */
let compound_defs: Label.t => list((family, Mold.t)) = {
  let tbl: Hashtbl.t(Label.t, list((family, Mold.t))) =
    Hashtbl.create(256);
  List.iter(
    ((fam, {label, mold, _}: def)) => {
      let prev = Option.value(Hashtbl.find_opt(tbl, label), ~default=[]);
      Hashtbl.replace(tbl, label, prev @ [(fam, mold)]);
    },
    forms,
  );
  label => Option.value(Hashtbl.find_opt(tbl, label), ~default=[]);
};

let base_candidates = (label: Label.t): list((FormId.t, Mold.t)) => {
  let compounds =
    compound_defs(label) |> List.map(((fam, m)) => (Compound(fam), m));
  switch (label) {
  | [t] => atomic_candidates(t) @ compounds
  | _ => compounds
  };
};

let label_of_family = (fam: family): Label.t =>
  switch (defs_of(fam)) {
  | [def, ..._] => def.label
  | [] => [] /* unreachable: every family has a row */
  };

let unmolded_mold = (label: Label.t): Mold.t =>
  switch (label) {
  | [t]
      when Token.is_potential_operator(t) && !Token.is_potential_operand(t) =>
    Mold.mk_bin(Precedence.max, Any, [])
  | _ => Mold.mk_op(Any, [])
  };

let label_of: FormId.t => Label.t =
  fun
  | Compound(fam) => label_of_family(fam)
  | Tok(t)
  | TokInfix(t) => [t];

/* Does this form spell the same label as family fam?
 * Label-family check: labels are shared between families (e.g.
 * ["(",")"] is both Parens and Ap), and Tok ids can spell the same
 * tokens as a registered form. */
let has_label_of = (f: FormId.t, fam: family): bool =>
  label_of(f) == label_of_family(fam);

/* The mold of a form at the tile's stored sort. Compound: the family
 * row with that out sort, else the Any-fallback (in particular
 * sort=Any always falls back — no form has out=Any). Tok: the first
 * atomic-candidate mold with that out sort (atomic_form declaration
 * order), else the Any-fallback. TokInfix: the InfixDelimiterPrefix
 * bin, uniformly at any sort. */
let mold_of = (f: FormId.t, sort: Sort.t): Mold.t =>
  switch (f) {
  | Compound(fam) =>
    switch (List.find_opt((def: def) => def.mold.out == sort, defs_of(fam))) {
    | Some(def) => def.mold
    | None => unmolded_mold(label_of_family(fam))
    }
  | Tok(t) =>
    switch (
      List.find_opt(
        ((_, m): (FormId.t, Mold.t)) => m.out == sort,
        atomic_candidates(t),
      )
    ) {
    | Some((_, m)) => m
    | None => unmolded_mold([t])
    }
  | TokInfix(_) => Mold.mk_bin(Precedence.concave_grout, sort, [])
  };

/* Classify a label at a sort: the (form, sort) pair to store on the
 * tile, chosen so that mold_of(form, sort) yields the classified
 * mold. First base candidate whose mold fits the sort (atomics
 * before compounds, priority order — TokInfix never wins here:
 * every InfixDelimiterPrefix token is var-shaped, and the var
 * classes precede it); no fit => the first compound (or Tok) with
 * stored sort Any. */
let classify_label = (sort: Sort.t, label: Label.t): (FormId.t, Sort.t) => {
  let fits = ((_, m): (FormId.t, Mold.t)): bool => m.out == sort;
  switch (List.find_opt(fits, base_candidates(label))) {
  | Some((id, _)) => (id, sort)
  | None =>
    switch (compound_defs(label)) {
    | [(fam, _), ..._] => (Compound(fam), Sort.Any)
    | [] =>
      switch (label) {
      | [t] => (Tok(t), Sort.Any)
      | [t, ..._] =>
        /* unregistered multi-token labels don't arise from the
         * grammar; approximate with the head token */
        (Tok(t), Sort.Any)
      | [] => (Tok(Token.empty), Sort.Any)
      }
    }
  };
};

/* The form wrapping a segment in parens at a given sort
 * (Segment.mk_duo/parenthesize): always the registered Paren form,
 * never the Ap form classify_label would find first at
 * Drv(Exp)/Drv(Pat). Sorts with no registered paren row (Rul, Mod,
 * Sig, MPat, Any, remaining Drv) fall back to classify_label:
 * (Parens, Any) with the op(Any) fallback mold. */
let mk_parens_id = (sort: Sort.t): (FormId.t, Sort.t) =>
  switch (sort) {
  | Exp
  | Pat
  | Typ
  | TPat
  | Drv(Prop)
  | Drv(Exp)
  | Drv(Pat)
  | Drv(Typ) => (Compound(Parens), sort)
  | _ => classify_label(sort, Token.tuple_lbl)
  };

let remold_candidates =
    (label: Label.t, sort: Sort.t): list((FormId.t, Sort.t)) =>
  base_candidates(label)
  |> List.filter(((_, m): (FormId.t, Mold.t)) => m.out == sort)
  |> List.map(((id, _)) => (id, sort));
