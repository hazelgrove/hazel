// File for typechecking.

open Types;

let rec getType = (delta: hole_context, gamma: context, e: exp) : type_ =>
    switch(e) {
        | Int(_) => Int_t 
        | Float(_) => Any_t 
        | Bool(_) => Bool_t 
        | Cons(e1, e2) => Cons_t(getType(delta, gamma, e1), getType(delta, gamma, e2))
        | Nil => Any_t 
        | Var(x) => Tools.lookup(x, gamma)
        | Function(id, e') => Function_t(Tools.lookup(id, gamma), getType(delta, gamma, e'))
        | Application(e1, e2) => switch (getType(delta, gamma, e1)) {
            | Function_t(t1, t2) when getType(delta, gamma, e2) == t1 => t2
            | _ => failwith("Application type error")
            }
        | Hole(id) => {
            let (_, t) = Tools.lookup(id, delta);
            t
        }
        | Unit => Unit_t 
        | Pair(e1, e2) => Pair_t(getType(delta, gamma, e1), getType(delta, gamma, e2))
        | Fst(e') => switch (e') {
            | Pair(e1, _) => getType(delta, gamma, e1)
            | _ => failwith("Type error, expected pair")
            }
        | Snd(e') => switch(e') {
            | Pair(_, e2) => getType(delta, gamma, e2)
            | _ => failwith("Type error, expected pair")
            }
        | _ => failwith("Not yet implemented")
    };

let rec getResType = (delta, r: res) => 
    switch(r) {
        | Rint(_) => Int_t 
        | Rfloat(_) => Any_t 
        | Rbool(_) => Bool_t 
        | Rfunc(id, e, env) => getType(delta, generateContext(delta, env), Function(id, e))
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
            | _ => failwith("Type error: Exppected pair")
            }
        | Rsnd(r') => switch(r') { 
            | Rpair(_, r2) => getResType(delta, r2)
            | _ => failwith("Type error: Exppected pair")
            }
        | _ => failwith("Not yet implemented")
    }

and generateContext = (delta, env) => 
    switch (env) {
        | [] => []
        | [(x, r), ...env'] => 
            [(x, getResType(delta, r)), ...generateContext(delta, env')]
    };

let rec getExType = (delta, ex) => {
   switch (ex) {
       | Top => Any_t 
       | Eunit => Unit_t 
       | Epair(ex1, ex2) => Pair_t(getExType(delta, ex1), getExType(delta, ex2))
       | Efunc(v, ex1) => Function_t(valToRes(v) |> getResType(delta), getExType(delta, ex1))
       }
};

let getConstraintType = (delta, exs) => {
    let contexts = List.map(
        ((env, ex)) => (generateContext(delta, env), getExType(delta, ex)),
        exs);
    switch (contexts) {
        | [] => ([], Any_t)
        | [x, _] => 
            switch (List.filter((y) => x != y, contexts)) {
                | [] => failwith("Inconsistent environment / example types")
                | _ => x
                }
        }
};

let rec generateHoleContextU = (us) => {
    switch (us) {
        | [] => []
        | [(id, exs), ...xs] => [(id, getConstraintType([], exs)), ...generateHoleContextU(xs)]
        }
};
        
let rec generateHoleContextF = (fs) => {
    switch (fs) {
        | [] => []
        | [(id, e), ...xs] => []
        }
};
