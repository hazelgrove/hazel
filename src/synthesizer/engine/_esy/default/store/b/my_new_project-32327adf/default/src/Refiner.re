// Holds the refine function

open Types;

let refinable = (typ) => 
    switch (typ) {
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => true
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

let firstExs = (exs) => List.map(
    ((env, Epair(ex1, _))) => (env, ex1),
    exs);

let sndExs = (exs) => List.map(
    ((env, Epair(_, ex2))) => (env, ex2),
    exs);

let prepFuncExs = (exs, vid) => List.map(
    ((env, Efunc(v, ex))) => ([(vid, Types.valToRes(v)), ...env], ex),
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

let refine = (context, _, typ, exs) => {
    switch (typ) {
        | Unit_t when allUnit(exs) => (Unit, [])
        | Pair_t(t1, t2) when allPairs(exs) => {
            let x = IdGenerator.getId();
            let y = IdGenerator.getId();
            (Pair(Hole(x), Hole(y)), [(context, x, t1, firstExs(exs)), (context, y, t2, sndExs(exs))])
        }
        | Function_t(t1, t2) when allFuncs(exs) => {
            let x = IdGenerator.getId();
            let h = IdGenerator.getId();
            (Function(x, Hole(h)), [([(x, t1), ...context], h, t2, prepFuncExs(exs, x))])
        }
        | Unit_t 
        | Pair_t(_, _)
        | Function_t(_, _) => failwith("Goal type inconsistent with examples")
        | _ => failwith("Not a refinement type")
        }
};

