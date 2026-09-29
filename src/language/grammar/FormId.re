/* Enumerates Hazel's syntactic forms. `family` identifies each
 * compound form: one constructor per label x shape-role, spanning
 * every sort the form inhabits. `atomic_form` classifies free-text
 * tokens (variables, literals, holes, ...). `FormId.t` is the form
 * identity a tile stores. Labels — the delimiter spellings — are
 * defined here (label_of_family); molds and classification live
 * editor-side in Haz3lcore.Form, keyed by these ids (Form
 * `include`s this module). */

/* Classes of free-text tokens, recognized by predicate (see
 * Form.get_atomic_form). Declaration order is classification and
 * remolding priority for tokens matching more than one class. */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type atomic_form =
  | Var
  | DrvVar
  | ExplicitHole
  | ImplicitHoleMarker
  | LLMHole
  | Wild
  | String
  | QuotedLabel
  | IntLit
  | FloatLit
  | BoolLit
  | LivelitName
  | ProjectorInvoke
  | UndefinedLit
  | EmptyList
  | EmptyTuple
  | EmptyModule
  | Deferral
  | ExplicitNonlabel
  | TyVar
  | TyVarP
  | Ctr
  | MPatName
  | Type
  | InfixDelimiterPrefix;

/* Compound forms, up to sort. A family is an equivalence class of
 * form definitions sharing the same label AND the same outer-nib
 * shape-role (convex/concave pattern): sort variants (Cons at
 * Exp/Pat/Drv(Exp), parens at eight sorts, ...) collapse into one
 * family; same-label shape-splits stay distinct (bin `+` = Plus vs
 * prefix `+` = SumSingle). Within a family, (out sort -> mold) is a
 * function, so (family, sort) determines a mold; the family
 * invariants are machine-checked in test/Test_FormId.re. */
[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type family =
  | TypeArrow
  | CellJoin
  | Plus
  | Minus
  | Times
  | Power
  | FPower
  | Divide
  | Equals
  | StringConcat
  | Lt
  | Gt
  | NotEquals
  | Gte
  | Lte
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | FEquals
  | FLt
  | FGt
  | FNotEquals
  | FGte
  | FLte
  | LogicalAnd
  | LogicalOrLegacy
  | LogicalOr
  | ListConcat
  | Cons
  | TypeAsc
  | TupleLabeled
  | Dot
  | TupleExtension
  | Not
  | SumSingle
  | UnaryMinus
  | Comma
  | ListLit
  | Parens
  | ApEmpty
  | Ap
  | ApExpTyp
  | Case
  | Test
  | ProofOf
  | ProofObject
  | HintedTest
  | Fun
  | Fix
  | TypFun
  | Poly
  | Forall
  | Rec
  | Rule
  | Pipeline
  | FilterHide
  | FilterEval
  | FilterPause
  | FilterDebug
  | Use
  | OfProp
  | OfCtx
  | OfJdmt
  | OfAlfaExp
  | OfAlfaTyp
  | OfAlfaPat
  | OfAlfaTPat
  | Subst
  | Glb
  | Val
  | Eval
  | Entail
  | UnaryEntail
  | Consistent
  | MatchedArrow
  | MatchedProd
  | MatchedSum
  | Valid
  | Syn
  | And
  | Impl
  | If
  | Let
  | Theorem
  | TypeAlias
  | ModBody
  | ModLet
  | ModType
  | ModuleExp
  | ModuleMod
  | SigLet;

/* A form identity, sort-free: label is derived from the form alone;
 * the mold is derived from (form, sort) where sort is the tile's
 * stored local-sort guess (see Base.tile).
 * - Compound(family): a registered compound form up to sort; if the
 *   family has no row at the tile's sort, the mold is the
 *   Any-fallback;
 * - Tok(t): a single token, classified or not; mold = the token's
 *   registered atomic mold at the tile's sort, else the Any-fallback;
 * - TokInfix(t): the keyword-prefix backup-infix shape-role. Exists
 *   solely for the InfixDelimiterPrefix mechanism (see Form.re's
 *   infix_delimiter_ops_prefixes rationale); never produced by
 *   classification, only by remold shape-fitting. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Compound(family)
  | Tok(Token.t)
  | TokInfix(Token.t);

/* The label of each compound family: the single home of every
 * delimiter spelling (editor-side definition rows join their labels
 * from this table). Bracket labels reference the Token constants to
 * keep the open/close pairing single-homed there. */
let label_of_family: family => Label.t =
  fun
  | TypeArrow => ["->"]
  | CellJoin => [";"]
  | Plus => ["+"]
  | Minus => ["-"]
  | Times => ["*"]
  | Power => ["**"]
  | FPower => ["**."]
  | Divide => ["/"]
  | Equals => ["=="]
  | StringConcat => ["++"]
  | Lt => ["<"]
  | Gt => [">"]
  | NotEquals => ["!="]
  | Gte => [">="]
  | Lte => ["<="]
  | FPlus => ["+."]
  | FMinus => ["-."]
  | FTimes => ["*."]
  | FDivide => ["/."]
  | FEquals => ["==."]
  | FLt => ["<."]
  | FGt => [">."]
  | FNotEquals => ["!=."]
  | FGte => [">=."]
  | FLte => ["<=."]
  | LogicalAnd => ["&&"]
  | LogicalOrLegacy => ["\\/"]
  | LogicalOr => ["||"]
  | ListConcat => ["@"]
  | Cons => ["::"]
  | TypeAsc => [":"]
  | TupleLabeled => ["="]
  | Dot => ["."]
  | TupleExtension => ["..."]
  | Not => ["!"]
  | SumSingle => ["+"]
  | UnaryMinus => ["-"]
  | Comma => [","]
  | ListLit => Token.listlit_lbl
  | Parens => Token.tuple_lbl
  | ApEmpty => [Token.empty_tuple]
  | Ap => Token.tuple_lbl
  | ApExpTyp => ["@<", ">"]
  | Case => ["case", "end"]
  | Test => ["test", "end"]
  | ProofOf => ["proof_of", "end"]
  | ProofObject => ["proof_object", "end"]
  | HintedTest => ["hint", "test", "end"]
  | Fun => ["fun", "->"]
  | Fix => ["fix", "->"]
  | TypFun => ["typfun", "->"]
  | Poly => ["poly", "->"]
  | Forall => ["forall", "->"]
  | Rec => ["rec", "->"]
  | Rule => ["|", "=>"]
  | Pipeline => ["|>"]
  | FilterHide => ["hide", "in"]
  | FilterEval => ["eval", "in"]
  | FilterPause => ["pause", "in"]
  | FilterDebug => ["debug", "in"]
  | Use => ["use", "in"]
  | OfProp => ["of_prop", "end"]
  | OfCtx => ["of_ctx", "end"]
  | OfJdmt => ["of_jdmt", "end"]
  | OfAlfaExp => ["of_alfa_exp", "end"]
  | OfAlfaTyp => ["of_alfa_typ", "end"]
  | OfAlfaPat => ["of_alfa_pat", "end"]
  | OfAlfaTPat => ["of_alfa_tpat", "end"]
  | Subst => ["[", "/", "]"]
  | Glb => ["glb(", ",", ")"]
  | Val => ["val", "end"]
  | Eval => ["\\=/"]
  | Entail => ["|-"]
  | UnaryEntail => ["|-"]
  | Consistent => ["consistent", "~"]
  | MatchedArrow => ["matched_arrow", "with"]
  | MatchedProd => ["matched_prod", "with"]
  | MatchedSum => ["matched_sum", "with"]
  | Valid => ["valid", "end"]
  | Syn => ["=>"]
  | And => ["/\\"]
  | Impl => ["==>"]
  | If => ["if", "then", "else"]
  | Let => ["let", "=", "in"]
  | Theorem => ["theorem", "=", "in"]
  | TypeAlias => ["type", "=", "in"]
  | ModBody => Token.mod_lbl
  | ModLet => ["let", "="]
  | ModType => ["type", "="]
  | ModuleExp => ["module", "=", "in"]
  | ModuleMod => ["module", "="]
  | SigLet => ["let"];

let label_of: t => Label.t =
  fun
  | Compound(fam) => label_of_family(fam)
  | Tok(t)
  | TokInfix(t) => [t];

/* Does this form spell the same label as family fam?
 * Label-family check: labels are shared between families (e.g.
 * ["(",")"] is both Parens and Ap), and Tok ids can spell the same
 * tokens as a registered form. */
let has_label_of = (f: t, fam: family): bool =>
  label_of(f) == label_of_family(fam);

/* Every delimiter token of the grammar's compound forms. */
let delims: list(Token.t) =
  all_of_family
  |> List.concat_map(label_of_family)
  |> List.sort_uniq(compare);

/* The surface family of each binary operator; bin_op_to_string
 * below reads operator spellings off these maps and the label
 * table. The numeric classes (Int/SInt/Nat) share one surface
 * family per op; Float ops have their own (F*) families. These maps
 * live here because Operators sits below this module (the AST
 * references op_bin) and cannot see the label table. */
let int_op_family: Operators.op_bin_num => family =
  fun
  | Plus => Plus
  | Minus => Minus
  | Times => Times
  | Power => Power
  | Divide => Divide
  | LessThan => Lt
  | LessThanOrEqual => Lte
  | GreaterThan => Gt
  | GreaterThanOrEqual => Gte;

let float_op_family: Operators.op_bin_float => family =
  fun
  | Plus => FPlus
  | Minus => FMinus
  | Times => FTimes
  | Power => FPower
  | Divide => FDivide
  | LessThan => FLt
  | LessThanOrEqual => FLte
  | GreaterThan => FGt
  | GreaterThanOrEqual => FGte
  | Equals => FEquals
  | NotEquals => FNotEquals;

let bool_op_family: Operators.op_bin_bool => family =
  fun
  | And => LogicalAnd
  | Or => LogicalOr;

let string_op_family: Operators.op_bin_string => family =
  fun
  | Concat => StringConcat;

let poly_op_family: Operators.op_bin_poly => family =
  fun
  | Equals => Equals
  | NotEquals => NotEquals;

let bin_op_family: Operators.op_bin => family =
  fun
  | SInt(op)
  | Int(op)
  | Nat(op) => int_op_family(op)
  | Float(op) => float_op_family(op)
  | Bool(op) => bool_op_family(op)
  | String(op) => string_op_family(op)
  | Poly(op) => poly_op_family(op);

/* All these families are single-token, so hd is total. */
let bin_op_to_string = (op: Operators.op_bin): Token.t =>
  List.hd(label_of_family(bin_op_family(op)));
