//----------------------------------------------------------------------
//                                Types
//----------------------------------------------------------------------

// Variable and hole names
type identifier = int;
type hole_identifier = int;
type branches = Tools.pairlist(identifier, (pattern, exp)) //parser_generator.py: ignore

// Expressions in the language
//   Very small for now
and exp =
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Cons(exp, exp)
  | Nil
  | Function(identifier, identifier, type_, exp)
  | Application(exp, exp)
  | Hole(hole_identifier)
  | Unit
  | Var(identifier)
  | Pair(exp, exp)
  | Fst(exp)
  | Snd(exp)
  | Ctor(identifier, adt, exp)
  | Case(exp, branches)
  | Plus(exp, exp)
  | Minus(exp, exp)
  | Times(exp, exp)
  | Divide(exp, exp)
  | FPlus(exp, exp)
  | FMinus(exp, exp)
  | FTimes(exp, exp)
  | FDivide(exp, exp)
  | LessThan(exp, exp)
  | GreaterThan(exp, exp)
  | Equals(exp, exp)
  | FLessThan(exp, exp)
  | FGreaterThan(exp, exp)
  | FEquals(exp, exp)
  | And(exp, exp)
  | Or(exp, exp)

// Results in the language
//   Act as values that can have holes
and res =
  | Rint(int)
  | Rfloat(float)
  | Rbool(bool)
  | Rcons(res, res)
  | Rnil
  | Rfunc(identifier, identifier, type_, exp, environment)
  | Rapp(res, res) //Can we limit the type of result in the applicator position?
  | Rhole(hole_identifier, environment)
  | Runit
  | Rpair(res, res)
  | Rfst(res)
  | Rsnd(res)
  | Rctor(identifier, adt, res)
  | Rictor(identifier, adt, res)
  | Rcase(res, branches, environment)

// Types in the language
and type_ =
  | Int_t
  | Float_t
  | Bool_t
  | Cons_t(type_, type_)
  | Function_t(type_, type_)
  | Unit_t
  | Pair_t(type_, type_)
  | Any_t
  | Fail_t
  | D(adt)

and ann =
  | AnnNone
  | AnnArg
  | AnnRec
  | AnnFunc

and pattern =
  | V(identifier)
  | P(pattern, pattern)

// Map from variable names to results
and environment = Tools.pairlist(identifier, res) //parser_generator.py: ignore
// Map from variable names to types
and context = Tools.pairlist(identifier, (type_, ann)) //parser_generator.py: ignore

// Types all of the holes
// I think we should clarify this and the type which unevaluate returns.
and hole_context = Tools.pairlist(hole_identifier, (context, type_)) //parser_generator.py: ignore

// Abstract datatypes. Make sure to define constructors in context below.
// Also, I'm a hypocrite since I'm not defining them below.

/* Should list be parametarized? */
and adt =
  | List
  | Num
  | Boolean;

// Datatype context
let sigma: Tools.pairlist(adt, Tools.pairlist(identifier, type_)) = [
  (List, [(0, Unit_t), (1, Pair_t(D(Num), D(List)))]),
  (Num, [(0, Unit_t), (1, D(Num))]),
  (Boolean, [(0, Unit_t), (1, Unit_t)]),
];

// Examples
//   Needs to be filled out more
type example =
  | Top
  | Eunit
  | Eint(int)
  | Ebool(bool)
  | Epair(example, example)
  /* I don't understand this constructor.
     Is value the formal parameter and example the body?
     would that be an application expression? */
  // It takes the form of an input output pair, so v would
  // be the input value and example would be the output.
  | Efunc(value, example)
  | Ector(identifier, adt, example)

and value =
  | Vint(int)
  | Vbool(bool)
  | Vunit
  | Vpair(value, value)
  | Vctor(identifier, adt, value);

type hole_fillings = Tools.pairlist(hole_identifier, exp) //parser_generator.py: ignore
and unfilled_holes = Tools.pairlist(hole_identifier, excons) //parser_generator.py: ignore

and excons = Tools.pairlist(environment, example) //parser_generator.py: ignore
and unevalcons = (unfilled_holes, hole_fillings); //parser_generator.py: ignore

// Simple values
//   For now a single constant
//   plus pairs
type constraint_ = option(unevalcons); //parser_generator.py: ignore

type guess_output = list(exp); //parser_generator.py: ignore
type solver_output = (hole_fillings, hole_context); //parser_generator.py: ignore
type filler_output = (unevalcons, hole_context); //parser_generator.py: ignore
//type refiner_output = (exp, hole_context);//parser_generator.py: ignore

type debug_construct =
  | Exp(exp)
  | Environment(environment)
  | Res(res)
  | Type_(type_)
  | Example(example)
  | Constraint_(constraint_)
  | Context(context)
  | Hole_Context(hole_context)
  | DB_Int(int)
  | Guess_Output(guess_output)
  | Solver_Output(solver_output)
  | Filler_Output(filler_output)
  | Hole_Fillings(hole_fillings)
  | Unfilled_Holes(unfilled_holes)
  | Hole_Identifier(hole_identifier)
  | Excons(excons)
  | Unevalcons(unevalcons)
  | Branches(branches);

//marker for parser_generator.py

type goal = (context, hole_identifier, type_, excons);
type goals = list(goal);
