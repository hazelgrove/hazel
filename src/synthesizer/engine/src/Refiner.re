// Holds the refine function

open Types;

let outFunc = ref(true);

let refinable = (typ, exs) => 
    switch (typ) {
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => true
        | D(adt) => {
            switch (exs) {
                | [] => true
                | [(env, Ector(id, adt, _)), ...xs] => 
                    List.length(List.filter(((_, Ector(id', adt, _))) => id == id', xs)) == List.length(xs)
                | _ => false
            }
        }
        | _ => false
        };

let rec allUnit = (exs) => {
    switch (exs) {
        | [] => true
        | [(_, Eunit), ...xs] => allUnit(xs)
        | _ => false
    }
};

let rec allPairs = (exs) => {
    switch (exs) {
        | [] => true
        | [(_, Epair(_, _)), ...xs] => allPairs(xs)
        | _ => false
    }
};

let rec allFuncs = (exs) => {
    switch (exs) {
        | [] => true
        | [(_, Efunc(_, _)), ...xs] => allFuncs(xs)
        | _ => false
    }
};

let allConstructs = (exs):option(int) => {
    let c = switch (exs) {
        | [(_, Ector(id, _, ex)), ...xs] => Some(id)
        | _ => None
        };
    switch (c) {
        | None => None
        | Some(i) => {
            let haveIdC = List.filter(
                ((env, ex)) => {
                    switch(ex) {
                        | Ector(i, _, _) => true
                        | _ => false
                        }
                },
                exs);
            if (List.length(haveIdC) == List.length(exs)){
                c
            } else {
                None
            }
        }
    }
};

let firstExs = (exs) => List.map(
    ((env, Epair(ex1, _))) => (env, ex1),
    exs);

let sndExs = (exs) => List.map(
    ((env, Epair(_, ex2))) => (env, ex2),
    exs);

let prepFuncExs = (exs, e) => {
    let Function(n, x, t, e') = e;
    List.map(
    ((env, Efunc(v, ex))) => ([(n, Rfunc(n, x, t, e', env)), (x, Typecasting.valToRes(v)), ...env], ex),
     exs)
};

let prepConsExs = (exs) => List.map(
    ((env, Ector(id, _, ex))) => (env, ex),
    exs);

// Takes in hole context, context, goal type, and example constraints
// To Do:
//   - Need to generate hole and variable identifiers. (misc different file)
//   - Figure out how to suppress warnings
//   - Implement handling hole contexts (unclear in paper how this is handled)
// (Delta, Gamma, Type, X) -> e, G


// Testing::
//   goal type = (context, hole id, type, exs)
//   What it does: match against type.
//   type = ??1 : t1 -> t2 => (\x:t1  => ??2: t2)


let refine = (context, typ, exs) => {
    switch (typ) {
        | Unit_t when allUnit(exs) => Some((Unit, []))
        | Pair_t(t1, t2) when allPairs(exs) => {
            let x = IdGenerator.getId();
            let y = IdGenerator.getId();
            Some((Pair(Hole(x), Hole(y)), [(context, x, t1, firstExs(exs)), (context, y, t2, sndExs(exs))]))
        }
        | Function_t(t1, t2) when allFuncs(exs) => {
            let n = IdGenerator.getId();
            let x = IdGenerator.getId();
            let h = IdGenerator.getId();
            let e = Function(n, x, t1, Hole(h));
            if (outFunc^) {
                outFunc := false;
                Some((e, [([(n, (Function_t(t1, t2), AnnFunc)), (x, (t1, AnnArg)), ...context], h, t2, prepFuncExs(exs, e))]))
            } else {
                Some((e, [([(x, (t1, AnnArg)), ...context], h, t2, prepFuncExs(exs, e))]))
            }
        }
        | D(adt) => {
            let c = allConstructs(exs);
            switch (c) {
                | Some(i) => {
                    let h = IdGenerator.getId();
                    let t = Tools.lookup(adt, Types.sigma) |> Tools.lookup(i);
                    Some((Ctor(i, adt, Hole(h)), [(context, h, t, prepConsExs(exs))]))
                }
                | None => {
                    None
                }
            }
        }
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => {
            None
        }
        | _ => failwith("Not a refinement type")
        }
};

