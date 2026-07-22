/* FormId names every syntactic form in the grammar. The type
 * declarations for atomic_form and family live here and are
 * re-exported by Form via `include FormId`, so `Form.family`, bare
 * constructors, and `all_of_family` keep resolving. This module is a
 * leaf: it may depend only on Sort, Token, and Util. The definition
 * table and classification layer (which need Mold) live in Form.re. */

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
