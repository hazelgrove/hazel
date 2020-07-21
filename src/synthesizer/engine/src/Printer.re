open Types;

let rec string_of_debug_construct (c:debug_construct):string =
    switch(c) {
        | Exp(x) => string_of_exp(x)
        | Environment(x) => string_of_env(x)
        | Res(x) => string_of_res(x)
        | Type_(x) => string_of_type_(x)
        | Example(x) => string_of_example(x)
        | Constraint_(x) => string_of_constraint_(x)
        | Context(x) => string_of_context(x)
        | Hole_Context(x) => string_of_hole_context(x)
        | DB_Int(x) => string_of_int(x)
        | Guess_Output(x) => string_of_guess_output(x)
        | Solver_Output(x) => string_of_solver_output(x)
        | Filler_Output(x) => string_of_filler_output(x)
        | Hole_Fillings(x) => string_of_hole_fillings(x)
        | Unfilled_Holes(x) => string_of_unfilled_holes(x)
        | Hole_Identifier(x) => string_of_hole_identifier(x)
        | Excons(x) => string_of_excons(x)
        | Unevalcons(x) => string_of_unevalcons(x)
        | Branches(x) => string_of_branches(x)
    }
and string_of_exp(e:exp):string =
    switch(e) {
        | Int(int) => string_of_int(int)
        | Float(float) => Js.Float.toString(float)
        | Bool(bool) => string_of_bool(bool)
        | Cons(exp, exp2) => string_of_exp(exp) ++ "::" ++ string_of_exp(exp2)
        | Nil  => "Nil"
        | Var(identifier) => "var_" ++ string_of_identifier(identifier)
        | Function(n, identifier, typ, exp) => "("++string_of_identifier(n)++"): \\" ++ string_of_identifier(identifier) ++ ": " ++ string_of_type_(typ) ++ "." ++ string_of_exp(exp)
        | Application(exp, exp2) => string_of_exp(exp) ++ " " ++ string_of_exp(exp2)
        | Hole(hole_identifier) => "??_"++string_of_hole_identifier(hole_identifier)
        | Unit  => "()"
        | Pair(exp, exp2) => "(" ++ string_of_exp(exp) ++ ", " ++ string_of_exp(exp2) ++ ")"
        | Fst(exp) => "fst(" ++ string_of_exp(exp) ++ ")"
        | Snd(exp) => "snd(" ++ string_of_exp(exp) ++ ")"
        | Ctor(name, typ, exp) => "C" ++ string_of_int(name) ++ ": " ++string_of_adt(typ)++ " " ++ string_of_exp(exp) 
        | Case(exp, branches) =>  "case " ++ string_of_exp(exp)  ++ " of {" ++ string_of_branches(branches) ++ "}"
    }

and string_of_res(r:res):string =
    switch(r) {
        | Rint(int) => string_of_int(int)
        | Rfloat(float) => Js.Float.toString(float)
        | Rbool(bool) => string_of_bool(bool)
        | Rcons(res, res2) => string_of_res(res) ++ "::" ++ string_of_res(res2)
        | Rnil  => "Nil"
        | Rfunc(n, identifier, typ, exp, environment) => "["++string_of_env(environment)++"]("++string_of_identifier(n)++"): \\" ++ string_of_identifier(identifier) ++ ": " ++ string_of_type_(typ) ++ "." ++ string_of_exp(exp)
        | Rapp(res, res2) => string_of_res(res) ++ " " ++ string_of_res(res2)
        | Rhole(hole_identifier, environment) => "["++string_of_env(environment)++"]??_"++string_of_hole_identifier(hole_identifier)
        | Runit  => "()"
        | Rpair(res, res2) => "(" ++ string_of_res(res) ++ ", " ++ string_of_res(res2) ++ ")"
        | Rfst(res) => "fst(" ++ string_of_res(res) ++ ")"
        | Rsnd(res) => "snd(" ++ string_of_res(res) ++ ")"
        | Rctor(name, _, res) => "C" ++ string_of_int(name) ++ " " ++ string_of_res(res) 
        | Rictor(name, _, res) => "Ci" ++ string_of_int(name) ++ " " ++ string_of_res(res)
        | Rcase(res, branches, env) => "[" ++ string_of_env(env) ++"] case " string_of_res(res) ++ " of {" ++ string_of_branches(branches) ++ "}"
    }

and string_of_branches(b:branches):string =
    switch(b) {
        | [] => ""
        | [(name, (pat, exp)), ...xs] => "C" ++ string_of_int(name) ++ " " ++ string_of_pat(pat) ++ " -> " ++ string_of_exp(exp) ++ "; " ++ string_of_branches(xs)
        }

and string_of_pat(p:pattern):string =
    switch(p) {
        | V(x) => string_of_identifier(x);
        | P(p1, p2) => "(" ++ string_of_pat(p1)++", "++string_of_pat(p2)++")"
        }

and string_of_env(e:environment):string =
    switch(e) {
        | [] => "-"
        | [(identifier,res),...ms] => string_of_identifier(identifier) ++"->"++string_of_res(res)++"; "++string_of_env(ms)
    }
and string_of_identifier = string_of_int
and string_of_hole_identifier = string_of_int
and string_of_type_(t:type_):string =
    switch(t) {
        | Int_t => "Int"
        | Bool_t => "Bool"
        | Cons_t(a, b) => "Cons("++string_of_type_(a)++","++string_of_type_(b)++")"
        | Function_t(a, b) => "("++string_of_type_(a)++"->"++string_of_type_(b)++")"
        | Unit_t => "Unit"
        | Pair_t(a, b) => "("++string_of_type_(a)++", "++string_of_type_(b)++")"
        | D(adt) => string_of_adt(adt)
        | Any_t => "Any"
        | Fail_t => "Fail"
    }

and string_of_adt(d: adt):string = 
    switch (d) {
        | List => "List"
        | Num => "Num"
        | Bool => "Bool"
        }

and string_of_example(ex: example):string =
    switch(ex) {
        | Top => "T"
        | Eunit => "()"
        | Eint(x) => string_of_int(x)
        | Ebool(b) => string_of_bool(b)
        | Epair(ex1, ex2) => "("++string_of_example(ex1)++", "++string_of_example(ex2)++")"
        | Efunc(v, ex') => "{"++string_of_value(v)++" -> "++string_of_example(ex')++"}"
        | Ector(id, _, ex') => "C"++string_of_int(id)++" "++string_of_example(ex')
        }

and string_of_value(v):string =
    switch(v) {
        | Vint(i) => string_of_int(i)
        | Vbool(b) => string_of_bool(b)
        | Vunit => "()"
        | Vpair(v1, v2) => "("++string_of_value(v1)++", "++string_of_value(v2)++")"
        | Vctor(id, adt, v1) => "C" ++ string_of_int(id) ++ ": " ++string_of_adt(adt)++ " "++ string_of_value(v1)
        }
    
and string_of_one_constraint_(c):string =
    switch(c) {
        | [] => "-"
        | [(environment, (hole_identifier, example)),...cs] => 
            string_of_env(environment) ++": "++string_of_hole_identifier(hole_identifier)++"->"++string_of_example(example)++"; "++string_of_one_constraint_(cs)
    }
and string_of_constraint_(c:constraint_):string =
    switch(c) {
        | None => "None"
        | Some(c) => string_of_unevalcons(c)
    }
and string_of_context(e:context):string =
    switch(e) {
        | [] => "-"
        | [(identifier,(type_, _)),...ms] => string_of_identifier(identifier) ++"->"++string_of_type_(type_)++"; "++string_of_context(ms)
    }
and string_of_hole_context(c):string =
    switch(c) {
        | [] => "-"
        | [(hole_identifier, (context, type_)),...cs] => 
            string_of_context(context) ++": "++string_of_hole_identifier(hole_identifier)++"->"++string_of_type_(type_)++"; "++string_of_hole_context(cs)
    }
and string_of_guess_output(c):string =
    switch(c) {
        | [] => "-"
        | [(x),...cs] => 
            string_of_exp(x)++"; "++string_of_guess_output(cs)
    }
and string_of_solver_output(c):string = {
    let (hf,hc) = c;
    "("++string_of_hole_fillings(hf)++", "++string_of_hole_context(hc)++")"
}
and string_of_filler_output(c):string = {
    let (e,hc) = c;
    "("++string_of_unevalcons(e)++", "++string_of_hole_context(hc)++")"
}
and string_of_hole_fillings(c):string =
    switch(c) {
        | [] => "-"
        | [(h,x),...cs] => 
            string_of_int(h)++"->"++string_of_exp(x)++"; "++string_of_hole_fillings(cs)
    }
and string_of_unfilled_holes(c):string =
    switch(c) {
        | [] => "-"
        | [(h,x),...cs] => 
            string_of_int(h)++"->["++string_of_excons(x)++"]; "++string_of_unfilled_holes(cs)
    }
and string_of_excons(c):string =
    switch(c) {
        | [] => "-"
        | [(e,ex),...cs] => 
            "(["++string_of_env(e)++"], "++string_of_example(ex)++"); "++string_of_excons(cs)
    }
and string_of_unevalcons(c):string = {
        let (uf,hf) = c;
        "(["++string_of_unfilled_holes(uf)++"], ["++string_of_hole_fillings(hf)++"])"
    }
