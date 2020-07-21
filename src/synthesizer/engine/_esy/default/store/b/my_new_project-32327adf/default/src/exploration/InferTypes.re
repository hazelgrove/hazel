open DatatypeDeclarations;
open TypeTools;

let rec infer_types (ert_exp:ert_exp, env:environment):value = {
    let ((t, examples), exp) = ert_exp;
    /*
    1. Propigate erts down one step
    2. Recur
    3. Infer ert from the erts down one step
    4. Merge
    */
    switch(ert_exp) {
    | ((T_Bool | T_Any, examples), E_Bool(value)) when ListBasics.all_predicate(((_, result)) 
            =>  V_Bool(value) == result, examples)
        => V_Bool(value)
    | ((t, examples), E_Variable(id)) when ListBasics.all_predicate(((env, result)) 
            => Tools.lookup(id, env:Tools.pairlist(identifier, value)) == result && TypeTools.type_checks(result, t), examples)
        => Tools.lookup(id, env)
    | ((t, examples), E_Application(cand, cator)) => V_Fail
    | ((t, examples), E_Function(id, body)) => V_Fail
    | ((t, examples), E_Hole(mutable_ert)) => V_Fail
    | _ => V_Fail
    }

  }