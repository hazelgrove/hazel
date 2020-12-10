(** Parsing of surface-level syntax.

    {b Note:} if there is a corresponding {!Post_parse} function to the
    function used for parsing here, it MUST be called afterward to ensure
    that the parsed value satisfies proper invariants. *)

open Lang

(** The possible parse errors. *)
type problem =
  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingLeftBracket
  | ExpectingRightBracket
  | ExpectingComma
  | ExpectingRightArrow
  | ExpectingLAngle
  | ExpectingRAngle
  | ExpectingSpace
  | ExpectingPound
  | ExpectingDot
  | ExpectingEquals
  | ExpectingDoubleEquals
  | ExpectingHole
  | ExpectingLambda
  | ExpectingPipe
  | ExpectingColon
  | ExpectingFuncSpec
  | ExpectingWildcard
  | ExpectingLineComment
  | ExpectingMultiCommentStart
  | ExpectingMultiCommentEnd
  | ExpectingExactly of int * int
  | ExpectingMoreIndent
  | ExpectingLet
  | ExpectingIn
  | ExpectingCase
  | ExpectingOf
  | ExpectingType
  | ExpectingAssert
  | ExpectingNat
  | ExpectingConstructorName
  | ExpectingVariableName
  | ExpectingHoleName
  | ExpectingFunctionArity
  | ExpectingTupleSize
  | ExpectingTupleIndex
  | ExpectingName of string * string
  | NegativeArity of int
  | ZeroArity
  | ExpectingEnd

(** The possible parse contexts. *)
type context =
  | CType
  | CTTuple
  | CTData
  | CTArr
  | CTForall
  | CTVar
  | CTypeParam
  | CTypeArg
  | CPat
  | CPTuple
  | CPVar
  | CPWildcard
  | CExp
  | CELet
  | CEVar
  | CECtor
  | CETuple
  | CEProj
  | CEApp
  | CEHole
  | CELambda
  | CECase
  | CEList
  | CENat
  | CStatement
  | CSDatatype
  | CSDatatypeCtors
  | CSDefinition
  | CSAssertion
  | CSFuncSpec
  | CSFuncSpecInput
  | CSFuncSpecOutput
  | CProgram

(** The type of a parser. *)
type 'a parser = (context, problem, 'a) Bark.parser

val exp : exp parser
(** Expression parser. *)

val typ : typ parser
(** Type parser. *)

val program : Desugar.program parser
(** Program parser.

    {b Warning:} parses expressions but does NOT call {!Post_parse.exp}
    (happens in {!Desugar.program}). *)
