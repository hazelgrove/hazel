//----------------------------------------------------------------------
//                                Types
//----------------------------------------------------------------------

// Variable and hole names
type identifier = int
type hole_identifier = int

// Expressions in the language
//   Very small for now
type exp = 
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Cons(exp, exp)
  | Nil 
  | Variable(identifier)
  | Function(identifier, exp)
  | Application(exp, exp)
  | Hole(hole_identifier)
  | Unit 
  | Var(identifier)
  | Pair(exp, exp)
  | Fst(exp)
  | Snd(exp)

// Results in the language
//   Act as values that can have holes
and res =
    | Rint(int)
    | Rfloat(float)
    | Rbool(bool)
    | Rcons(res, res)
    | Rnil 
    | Rfunc(identifier, exp, environment)
    | Rapp(res, res)//Can we limit the type of result in the applicator position?
    | Rhole(hole_identifier, environment)
    | Runit 
    | Rpair(res, res)
    | Rfst(res)
    | Rsnd(res)

// Types in the language
//   Currently not in much use
and type_ =
  | Int_t 
  | Bool_t 
  | Cons_t(type_, type_)
  | Function_t(type_, type_)
  | Unit_t 
  | Pair_t(type_, type_)
  | Any_t 
  | Fail_t 

and debug_construct = 
    | Exp(exp)
    | Environment(environment)
    | Res(res)

//marker for parser_generator.py

// Map from variable names to results
and environment = Tools.pairlist(identifier, res)
// Map from variable names to types
and context = Tools.pairlist(identifier, type_)

// Types all of the holes
// I think we should clarify this and the type which unevaluate returns.
type hole_context = Tools.pairlist(hole_identifier, (context, type_));

// Examples
//   Needs to be filled out more
type example =
    | Top 
    | Eunit 
    | Epair(example, example)
/* I don't understand this constructor. 
Is value the formal parameter and example the body? 
would that be an application expression? */

// It takes the form of an input output pair, so v would
// be the input value and example would be the output.
    | Efunc(value, example)

and excons = Tools.pairlist(environment, example)

// Simple values
//   For now a single constant
//   plus pairs
and value =
    | Vunit 
    | Vpair(value, value);

type goal = (context, hole_identifier, type_, excons);
type goals = list(goal);

//----------------------------------------------------------------------
//                     Typecasting Functions
//----------------------------------------------------------------------

let rec valToExp (v:value) : exp = {
    switch (v) {
        | Vunit => Unit 
        | Vpair(v1, v2) => Pair(valToExp(v1), valToExp(v2))
        }
};

let rec valToRes (v: value) : res = {
    switch (v) {
        | Vunit => Runit 
        | Vpair(v1, v2) => Rpair(valToRes(v1), valToRes(v2))
        }
};

let rec exToExp (ex:example):option(exp) = {
    switch (ex) {
        | Epair(ex1, ex2) => 
            switch ((exToExp(ex1), exToExp(ex2))) {
                | (Some(x), Some(y)) => Some(Pair(x, y))
                | _ => None
                }
        | Eunit => Some(Unit)
        | _ => None
        }
};

let rec resToVal (res:res):option(value) = {
    switch (res) {
        | Runit => Some(Vunit)
        | Rpair(r1, r2) => 
            switch ((resToVal(r1), resToVal(r2))) {
                | (Some(x), Some(y)) => Some(Vpair(x, y))
                | _ => None
                }
        | _ => None
        }
};

// is res -> val possible?
let castable (res:res):bool = 
    switch (resToVal(res)) {
        | Some(_) => true
        | None => false
        }

