// this is the tool functions

open Extraction_declear;

// FIXME: A temporary solution to map Some(Hole) into None
let hole_to_none = (~uht: option(UHTyp.t)): option(UHTyp.t) =>
  switch (uht) {
  | None => None
  | Some(t) =>
    switch (t) {
    | OpSeq(_, S(Hole, E)) => None
    | _ => uht
    }
  };

let rec option_string_concat =
        (~strs: list(option(string))): option(string) =>
  switch (strs) {
  | [] => Some("")
  | [a, ...rest] =>
    switch (a, option_string_concat(~strs=rest)) {
    | (Some(s1), Some(s2)) => Some(s1 ++ s2)
    | _ => None
    }
  };

let rec option_string_concat_ignoreNone =
        (~strs: list(option(string))): option(string) =>
  switch (strs) {
  | [] => Some("")
  | [a, ...rest] =>
    switch (a, option_string_concat_ignoreNone(~strs=rest)) {
    | (Some(s1), Some(s2)) => Some(s1 ++ s2)
    | (Some(s1), None) => Some(s1)
    | (None, Some(s2)) => Some(s2)
    | _ => None
    }
  };

//insert indent_space just like previous work
let rec indent_space = (~level: int): string =>
  if (level > 0) {
    "  " ++ indent_space(~level=level - 1);
  } else {
    "";
  };

//=========================================
// PASS_T
//=========================================

// Check whether two types are EQUAL
// Logic: if Conflict appears in any subcases, pass Conflict
//        if CANNOT_INFER appears with some other type, use other type
//        FIXME: EMPTY case
//        FIXME: add Hole cases

// seems useless now
let rec pass_eq = (~type1: pass_t, ~type2: pass_t): bool =>
  switch (type1, type2) {
  | (CONFLICT, _) => false
  | (_, CONFLICT) => false
  | (UNK, _) => true
  | (_, UNK) => true
  | (EMPTY, EMPTY) => true
  | (_, EMPTY) => false
  | (EMPTY, _) => false
  | (Unit, Unit) => true
  | (Bool, Bool) => true
  | (Number, Number) => true
  | (List(a), List(b)) => pass_eq(~type1=a, ~type2=b)
  | (ARROW(a1, b1), ARROW(a2, b2)) =>
    pass_eq(~type1=a1, ~type2=a2) && pass_eq(~type1=b1, ~type2=b2)
  | (PROD(a1, b1), PROD(a2, b2)) =>
    pass_eq(~type1=a1, ~type2=a2) && pass_eq(~type1=b1, ~type2=b2)
  | (SUM(a1, b1), SUM(a2, b2)) =>
    pass_eq(~type1=a1, ~type2=a2) && pass_eq(~type1=b1, ~type2=b2)
  | _ => false
  };

// use whenever need to combine two type_t into one

let rec pass_check = (~type1: pass_t, ~type2: pass_t): pass_t =>
  switch (type1, type2) {
  | (HOLE, _) => HOLE
  | (_, HOLE) => HOLE
  | (EMPTY, _) => CONFLICT
  | (_, EMPTY) => CONFLICT
  | (CONFLICT, _) => CONFLICT
  | (_, CONFLICT) => CONFLICT
  | (UNK, t) => t
  | (t, UNK) => t //FIXME: ? if it's right for t but not CONFLICT
  //other cases
  | (Unit, Unit) => Unit
  | (Bool, Bool) => Bool
  | (Number, Number) => Number
  //EMTPY don't allow to appear in dependency type
  | (List(a), List(b)) =>
    switch (pass_check(~type1=a, ~type2=b)) {
    | CONFLICT => CONFLICT
    | EMPTY => CONFLICT //it can't be list(not_a_type)
    | HOLE => HOLE
    | _ => List(pass_check(~type1=a, ~type2=b))
    }
  | (ARROW(a1, b1), ARROW(a2, b2)) =>
    switch (
      pass_check(~type1=a1, ~type2=a2),
      pass_check(~type1=b1, ~type2=b2),
    ) {
    | (CONFLICT, _) => CONFLICT
    | (_, CONFLICT) => CONFLICT
    | (EMPTY, _) => CONFLICT
    | (_, EMPTY) => CONFLICT
    | (HOLE, _) => HOLE
    | (_, HOLE) => HOLE
    | _ =>
      ARROW(
        pass_check(~type1=a1, ~type2=a2),
        pass_check(~type1=b1, ~type2=b2),
      )
    }
  | (SUM(a1, b1), SUM(a2, b2)) =>
    switch (
      pass_check(~type1=a1, ~type2=a2),
      pass_check(~type1=b1, ~type2=b2),
    ) {
    | (CONFLICT, _) => CONFLICT
    | (_, CONFLICT) => CONFLICT
    | (EMPTY, _) => CONFLICT //I don't know how it can exists...
    | (_, EMPTY) => CONFLICT
    | (HOLE, _) => HOLE
    | (_, HOLE) => HOLE
    | _ =>
      SUM(
        pass_check(~type1=a1, ~type2=a2),
        pass_check(~type1=b1, ~type2=b2),
      )
    }
  | (PROD(a1, b1), PROD(a2, b2)) =>
    switch (
      pass_check(~type1=a1, ~type2=a2),
      pass_check(~type1=b1, ~type2=b2),
    ) {
    | (CONFLICT, _) => CONFLICT
    | (_, CONFLICT) => CONFLICT
    | (EMPTY, _) => CONFLICT
    | (_, EMPTY) => CONFLICT
    | (HOLE, _) => HOLE
    | (_, HOLE) => HOLE
    | _ =>
      PROD(
        pass_check(~type1=a1, ~type2=a2),
        pass_check(~type1=b1, ~type2=b2),
      )
    }
  | _ => CONFLICT
  };

// a sugar for pass check for multiconditions
let rec pass_concat = (~types: list(pass_t)): pass_t =>
  switch (types) {
  | [] => UNK
  | [a] => a
  | [h, ...t] => pass_check(~type1=h, ~type2=pass_concat(~types=t))
  };

//===================================
// Extract_t
//===================================

let extract_t_combine = (~ex1: extract_t, ~ex2: extract_t): extract_t =>
  switch (ex1, ex2) {
  | ((s1, p1), (s2, p2)) => (
      option_string_concat(~strs=[s1, s2]),
      pass_check(~type1=p1, ~type2=p2),
    )
  };

let rec extract_t_concat = (~le: list(extract_t)): extract_t =>
  switch (le) {
  | [] => (Some(""), UNK)
  | [h, ...t] => extract_t_combine(~ex1=h, ~ex2=extract_t_concat(~le=t))
  };

//===================================
// Vairable Set
//===================================

// assume won't duplicate
// find a variable in the set, if don't find, return EMPTY
let rec find_variable_set = (~var: string, ~set: variable_set_t): pass_t =>
  switch (set) {
  | [] => EMPTY //meaning None, or Not found
  | [a, ...l] =>
    if (var == fst(a)) {
      snd(a);
    } else {
      find_variable_set(~var, ~set=l);
    }
  };

// add a new variable to the current variable set
// if already exists, apply override
let rec add_variable = (~v: extract_t, ~env: variable_set_t): variable_set_t =>
  switch (fst(v)) {
  | None => env
  | Some(s) =>
    switch (env) {
    | [] => [(s, snd(v))]
    | [h, ...t] =>
      if (fst(h) == s) {
        [(s, snd(v)), ...t];
      } else {
        [h, ...add_variable(~v, ~env=t)];
      }
    }
  };
