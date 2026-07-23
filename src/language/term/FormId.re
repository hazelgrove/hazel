/* FormId is the grammar nucleus: it names every syntactic form and
 * owns each compound family's label — the single textual home of the
 * grammar's delimiter spellings. Everything here needs neither molds
 * nor precedence; the module is a language-side leaf that may depend
 * only on Token and Label. The definition table (molds, expansion)
 * and classification layer live editor-side in Haz3lcore.Form, which
 * re-exports this module via `include FormId`, so `Form.family`,
 * bare constructors, `all_of_family`, `Form.label_of`, ... keep
 * resolving. */

/* B. Operands:
   Order in this type determines relative remolding
   priority for forms with overlapping regexps */

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

/* C. Compound forms, up to sort. A family is an equivalence class of
 * form definitions sharing the same label AND the same outer-nib
 * shape-role (convex/concave pattern): sort variants (Cons at
 * Exp/Pat/Drv(Exp), parens at eight sorts, ...) collapse into one
 * family; same-label shape-splits stay distinct (bin `+` = Plus vs
 * prefix `+` = SumSingle). Within a family, (out sort -> mold) is a
 * function, so (family, sort) determines a mold. The definitions live
 * in Form.defs_of and their classification/remolding priority in
 * Form.priority; the family invariants are machine-checked in
 * test/Test_FormId.re. */
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
 *   classification, only by remold shape-fitting. Scheduled for
 *   demolition with virtual grout, which obsoletes IDP entirely
 *   (plans/completion-provenance.md). */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Compound(family)
  | Tok(Token.t)
  | TokInfix(Token.t);

/* The label of each compound family. Form.re's definition rows carry
 * no label text; they join it from this table at table build, so
 * each delimiter spelling below is stated exactly once. Bracket
 * labels reference the Token constants to keep the open/close
 * pairing single-homed there. */
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
