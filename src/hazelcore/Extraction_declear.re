//===================
// Tools
//===================

// option_string_concat([s1, Some(constant)])
let rec option_string_concat = (~strs : list( option(string))) : option(string) =>
  switch (strs) {
    | [] => Some("")
    | [a, ...rest] => switch(a, option_string_concat(~strs=rest)) {
      | (Some(s1), Some(s2)) => Some(s1 ++ s2)
      | _ => None
    }
  };

let rec option_string_concat_ignoreNone = (~strs : list( option(string))) : option(string) =>
  switch (strs) {
    | [] => Some("")
    | [a, ...rest] => switch(a, option_string_concat_ignoreNone(~strs=rest)) {
      | (Some(s1), Some(s2)) => Some(s1 ++ s2)
      | (Some(s1), None) => Some(s1)
      | (None, Some(s2)) => Some(s2)
      | _ => None
    }
  };

//================================

// Add indent levels and pass into the handlers
// level starts from 0, +1 means double space
let rec indent_space = (~level: int): string =>
  if (level > 0) {
    "  " ++ indent_space(~level=level - 1);
  } else {
    "";
  };

//===============================
// Type Decleration
//===============================


//The type used to indicate the 
type pass_t =
  | HOLE          //it's a hole, don't know conflict or can infer
  | Bool
  | Number
  | Unit
  | List(pass_t)
  | APP(pass_t, pass_t) //pass_t -> pass_t
  | EMPTY           //means currently meaningless, or not something with a type
  // it seems EMPTY should be same as None, and never happens
  // can't appear in the dependency type
  | CANNOT_INFER  //it's dependency type like list(a)
  | CONFLICT;  //can't use in ocaml(include gradual typing) or detect error

type extract_t = (option(string), pass_t);

// Logic: if Conflict appears in any subcases, pass Conflict
//        if CANNOT_INFER appears with some other type, use other type
//        FIXME: EMPTY case, currently like CANNOT_INFER
//        FIXME: add Hole cases

let rec pass_eq = (~type1: pass_t, ~type2: pass_t) : bool =>
  switch (type1, type2){
    | (CONFLICT, _) => false
    | (_, CONFLICT) => false
    | (CANNOT_INFER, _) => true
    | (_, CANNOT_INFER) => true
    | (EMPTY, _) => true
    | (_, EMPTY) => true
    | (HOLE, HOLE) => true
    | (Unit, Unit) => true
    | (Bool, Bool) => true
    | (Number, Number) => true
    | (List(a), List(b)) => pass_eq(~type1=a, ~type2=b)
    | (APP(a1, b1), APP(a2, b2)) => pass_eq(~type1=a1, ~type2=a2) && pass_eq(~type1=b1, ~type2=b2)
    | _ => false
  };

//check conflict and pass on
let rec pass_check = (~type1: pass_t, ~type2: pass_t) : pass_t =>
  switch (type1, type2){
    | (EMPTY, a) => a
    | (a, EMPTY) => a 
    | (a, CANNOT_INFER) => a
    | (CANNOT_INFER, b) => b    
    | (CONFLICT, _) => CONFLICT
    | (_, CONFLICT) => CONFLICT
    //other cases
    | (Unit, Unit) => Unit
    | (Bool, Bool) => Bool
    | (Number, Number) => Number   
    //EMTPY don't allow to appear in dependency type 
    | (List(a), List(b)) => switch(pass_check(~type1=a, ~type2=b)){
      | CONFLICT => CONFLICT
      | EMPTY => CONFLICT
      | _ => List(pass_check(~type1=a, ~type2=b))
    }
    | (APP(a1, b1), APP(a2, b2)) => switch(pass_check(~type1=a1, ~type2=a2), pass_check(~type1=b1, ~type2=b2)){
      | (CONFLICT, _) => CONFLICT
      | (_, CONFLICT) => CONFLICT
      | (EMPTY, _) => CONFLICT
      | (_, EMPTY) => CONFLICT
      | _ => APP(pass_check(~type1=a1, ~type2=a2), pass_check(~type1=b1, ~type2=b2))
    }
    | _ => CONFLICT
  };

let rec pass_concat = (~types: list(pass_t)) : pass_t =>
    switch (types){
        | [] => EMPTY
        | [a] => a
        | [h,...t] => pass_check(~type1=h, ~type2=pass_concat(~types=t))
    };

//translate to string
let rec pass_trans = (~type1: pass_t) : option(string) =>
  switch (type1){
    | Bool => Some("bool")
    | Number => Some("int")
    | Unit => Some("()")
    | List(a) => option_string_concat(~strs=[pass_trans(~type1=a), Some(" list")])
    | APP(a, b) => option_string_concat(~strs=[pass_trans(~type1=a), Some(" -> "), pass_trans(~type1=b)])
    | EMPTY => None
    | _ => None
  };




// the variable set operation
type variable_set_t = list((string, pass_t));

// assume won't duplicate
let rec find_variable_set = (~var:string, ~set:variable_set_t) : pass_t=>
  switch(set){
    | [] => EMPTY
    | [a, ...l] => if (var == fst(a)) {snd(a);} else {find_variable_set(~var=var,~set=l);}
  };

// using option case of var to make future cases easy
let add_var_annotation = (~var:option(string), ~set:variable_set_t) : option(string) =>
  switch(var){
    | None => None
    | Some(s) => option_string_concat_ignoreNone(~strs=[var, option_string_concat(~strs=[Some(":"), pass_trans(~type1=find_variable_set(~var=s, ~set=set))])])
  };

