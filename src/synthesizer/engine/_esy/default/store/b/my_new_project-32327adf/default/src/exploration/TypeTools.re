open DatatypeDeclarations;

let type_intersection (t1:type_, t2:type_): type_ = 
  switch (t1, t2) {
  | (T_Any, x) | (x, T_Any) => x
  | (a,b) when a == b => a
  | _ => T_Fail
  }
let merge_examples = List.append
let ert_intersection (ert1:ert, ert2:ert):ert =
  switch (ert1, ert2) {
  | ((t1, es1), (t2, es2)) => (type_intersection(t1,t2), merge_examples(es1, es2))
  }

/* I don't know left from right in this context. The opperators commute, so I want the fast one. */
let type_intersection_list(ts:list(type_)):type_ = 
    List.fold_left(type_intersection, T_Any, ts)
let merge_examples_list(es) = 
    List.fold_left(merge_examples, [], es)
let ert_intersection_list (erts:list(ert)):ert =
    List.fold_left(ert_intersection, (T_Any, []), erts)


let type_of_value(value:value):type_ = 
  switch(value) {
    | V_Bool(_) => T_Bool
    | V_Function((result, _), _, ((argument, _), _)) => T_Function(argument, result)
    | V_Application(mutable_ert, _) => switch(MutablePair.fst(mutable_ert)) {
        | T_Function(argument, result) => result
        }
    | V_Hole(mutable_ert) => MutablePair.fst(mutable_ert)
    | V_Fail => T_Fail
  }

let subtype(t1:type_, t2:type_): bool = 
    type_intersection(t1, t2) == t1

let type_checks(value:value, type_:type_): bool = 
  subtype(type_of_value(value), type_)