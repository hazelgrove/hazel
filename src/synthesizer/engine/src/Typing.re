// File for typechecking.

// U -> hole_context
// hole_context, env (from U) -> context
// 

open Types;

let rec getType = (delta: hole_context, gamma: context, e: exp) : type_ =>
    switch(e) {
        | Int(_) => Int_t 
        | Float(_) => Any_t 
        | Bool(_) => Bool_t 
        | Cons(e1, e2) => Cons_t(getType(delta, gamma, e1), getType(delta, gamma, e2))
        | Nil => Any_t 
        | Var(x) => {
            let (t, _) = Tools.lookup(x, gamma);
            t
        }
        // I think this is where the problem is.
        | Function(_, _, typ, e') => Function_t(typ, getType(delta, gamma, e'))
        | Application(e1, e2) => switch (getType(delta, gamma, e1)) {
            | Function_t(t1, t2) when getType(delta, gamma, e2) == t1 => t2
            | x => {
                failwith("Application type error")
            }
        }
        | Hole(id) => {
            let (_, t) = Tools.lookup(id, delta);
            t
        }
        | Unit => Unit_t 
        | Pair(e1, e2) => Pair_t(getType(delta, gamma, e1), getType(delta, gamma, e2))
        | Fst(e') => switch (getType(delta, gamma, e')) {
            | Pair_t(t1, _) => t1
            | _ => failwith("Type error: Expected type pair for fst")
            }
        | Snd(e') => switch (getType(delta, gamma, e')) {
            | Pair_t(_, t2) => t2
            | _ => failwith("Type error: Expected type pair for snd")
            }
        | Ctor(id, d, e1) => {
            if (Tools.lookup(d, sigma) |> Tools.lookup(id) == getType(delta, gamma, e1)) {
                D(d)
            } else {
                failwith("Constructor did not typecheck")
            }
        }
        | Case(e1, branches) => switch (branches) {
            | [] => failwith("No branches supplied to case")
            | [(x, (v, e1)), ..._] => getType(delta, gamma, e1)
            }
        | _ => failwith("Not yet implemented")
    };

let rec getResType = (delta, r: res) => 
    switch(r) {
        | Rint(_) => Int_t 
        | Rfloat(_) => Any_t 
        | Rbool(_) => Bool_t 
        | Rfunc(n, id, typ, e, env) => getType(delta, generateContext(delta, env), Function(n, id, typ, e))
        | Rapp(r1, r2) => switch(getResType(delta, r1)) {
            | Function_t(t1, t) when t1 == getResType(delta, r2) => t
            | _ => failwith("Type error, failed application")
            }
        | Rhole(id, env) => {
            let (con, t) = Tools.lookup(id, delta);
            if (con == generateContext(delta, env)) {
                t
            } else {
                failwith("Type error: hole context doesn't match environment context")
            }
        }
        | Runit => Unit_t 
        | Rpair(r1, r2) => Pair_t(getResType(delta, r1), getResType(delta, r2))
        | Rfst(r') => switch (r') {
            | Rpair(r1, _) => getResType(delta, r1)
            | _ => failwith("Type error: Expected pair")
            }
        | Rsnd(r') => switch(r') { 
            | Rpair(_, r2) => getResType(delta, r2)
            | _ => failwith("Type error: Expected pair")
            }
        | Rctor(id, adt, r') => {
            if (Tools.lookup(adt, sigma) |> Tools.lookup(id) == getResType(delta, r')) {
                D(adt)
            } else {
                failwith("Type error: Result type doesn't match constructor type")
            }
        }
        | Rictor(_, _, r') => getResType(delta, r')
        | Rcase(r', bs, env) => {
            let D(d) = getResType(delta, r');
            let gamma = generateContext(delta, env);
            switch (bs) {
                | [] => Any_t
                | [(id, (_, e)), ...xs] => {
                    let t: type_ = Tools.lookup(d, sigma) |> Tools.lookup(id);
                    if (List.filter(
                            ((c, (_, e))) => getType(delta, gamma, e) == t,
                            xs) == xs) {
                        t
                    } else {
                        failwith("Not all branches have the same type")
                    }
                }
            }
        }
        | _ => failwith("Not yet implemented")
    }

and generateContext = (delta, env) => 
    switch (env) {
        | [] => []
        | [(x, r), ...env'] => 
            [(x, (getResType(delta, r), AnnNone)), ...generateContext(delta, env')]
    };

let rec getExType = (delta, ex) => {
   switch (ex) {
       | Top => Any_t 
       | Eunit => Unit_t 
       | Eint(_) => Int_t 
       | Ebool(_) => Bool_t 
       | Epair(ex1, ex2) => Pair_t(getExType(delta, ex1), getExType(delta, ex2))
       | Efunc(v, ex1) => Function_t(Typecasting.valToRes(v) |> getResType(delta), getExType(delta, ex1))
       | Ector(id, adt, ex1) => {
           let t: type_ = Tools.lookup(adt, sigma) |> Tools.lookup(id);
           if (t == getExType(delta, ex1)) {
               D(adt)  
           } else {
               failwith("Type error: Example doesn't have type required by example constructor")
           }
       }
       }
};

let getConstraintType = (delta, exs: excons) => {
    let contexts = List.map(
        ((env, ex)) => (generateContext(delta, env), getExType(delta, ex)),
        exs);
    switch (contexts) {
        | [] => ([], Any_t)
        | [x, ..._] => 
            switch (List.filter((y) => x != y, contexts)) {
                | [] => x
                | _ => failwith("Contexts are not consistent for set of example constraints")
                }
        }
};

let rec generateHoleContextU_h = (delta, us) => {
    switch (us) {
        | [] => delta
        | [(id, exs), ...xs] => {
            generateHoleContextU_h([(id, getConstraintType(delta, exs)), ...delta], xs)
        }
    }
};

let generateHoleContextU = (us) => generateHoleContextU_h([], List.rev(us));
        
let rec generateHoleContextF = (fs) => {
    switch (fs) {
        | [] => []
        | [(id, e), ...xs] => []
        }
};
