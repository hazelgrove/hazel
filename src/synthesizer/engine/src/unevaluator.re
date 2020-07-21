/* Class to house the unevaluator.
    The main function unevaluate
    takes a result and example and
    generates a set of constraints.
*/

open Types;

// constraints = (U, F)
// U = list((id, (env, ex)))
// result r, example ex => (U, F)

// call uneval on different results and examples.
// r = ([E] ??1, ()), ex = () => ([(1, (E, ()))], -) 

// IMPORTANT NEXT STEP SAM
// Make sure all of the constraints are merged so that there is only one
// set of constraints per hole, rather than multiple.

// Generates constraints from a result and example
// No constructors in language yet, will probably add later
// But that means there's a bunch less cases
// I believe this is all cases without adding in constructors
let rec unevaluate = (delta, f, res:res, ex:example) : option(unevalcons) => {
    let res = Evaluator.fillRes(res, f);
    switch ((ex, res)) {
        // Top adds no constraints
        | (Top, _) => Some(([], []))
        // Matching constants adds no constraints
        | (Eunit, Runit) => {
            Some(([], []))
        }
        | (Eint(x), Rint(y)) when x == y => Some(([], []))
        | (Ebool(x), Rbool(y)) when x == y => Some(([], []))
        // A pair of examples adds the constraints of both examples
        // on their respective results
        | (Epair(ex1, ex2), Rpair(r1, r2)) => {
            switch (unevaluate(delta, f, r1, ex1), unevaluate(delta, f, r2, ex2)) {
                | (Some(k1), Some(k2)) => Some(merge(k1, k2))
                | _ => None
                }
        }
        // A hole adds its environment and example to the list of 
        // unfilled holes.
        | (_, Rhole(id, env)) => {
            Some(([(id, [(env, ex)])], []))
        }
        | (_, Rfst(r)) => unevaluate(delta, f, r, Epair(ex, Top))
        | (_, Rsnd(r)) => unevaluate(delta, f, r, Epair(Top, ex))
        // Attempts to cast r2 to a value, and then continues
        // unevaluation on r1 with a new example.
        | (_, Rapp(r1, r2)) =>
            if (Typecasting.castable(r2)) {
                let Some(v) = Typecasting.resToVal(r2);
                unevaluate(delta, f, r1, Efunc(v, ex))
            } else {
                None // fail
            }
        // Takes and input-output example and a lambda result,
        // and binds the variable of the lambda to the value,
        // and then performs bidirectional evaluation.
        | (Efunc(v, ex'), Rfunc(name, id, _, exp, env)) => {
            let env' = [(name, res), (id, Typecasting.valToRes(v)), ...env];
            let exs = [(env', ex')];
            constrainExp(delta, f, exp, exs)
        }
        | (Ector(id1, _, ex'), Rctor(id2, _, r')) when id1 == id2 => {
            unevaluate(delta, f, r', ex')
        }
        | (_, Rictor(id, adt, r')) => unevaluate(delta, f, r', Ector(id, adt, ex))
        | (_, Rcase(r', branches, env)) => {
            let cons = List.map(
                ((ctor_id, (p, e1))) => {
                    let D(t) = Typing.getResType(delta, r');
                    let k1 = unevaluate(delta, f, r', Ector(ctor_id, t, Top));
                    let patBinds = List.map(
                        (x) => getPatRes(x, p, Rictor(ctor_id, t, r')),
                        getPatIds(p));
                    let k2 = constrainExp(delta, f, e1, [(patBinds @ env, ex)]);
                    switch (mergeCons(k1, k2)) {
                        | None => None
                        | x => x
                        }
                },
                branches) |> List.filter(optionPred);
            switch (cons) {
                | [] => None
                | [k, ...xs] => k
                }
        }
        // When none of the inference rules apply
        | _ => None // fail
    }
}

and unevalInit = (delta, r, ex) => unevaluate(delta, [], r, ex)

and getPatIds = (p) => 
    switch (p) {
        | V(x) => [x]
        | P(p1, p2) => getPatIds(p1) @ getPatIds(p2)
        }

and getPatRes = (id, p, r) => 
    switch (getPatRes_h(id, p, r)) {
        | Some(x) => (id, x)
        | None => failwith("Id wasn't found in pattern")
        }

and getPatRes_h = (id, p, r) => 
    switch (p) {
        | V(x) when x == id => Some(r)
        | V(_) => None
        | P(p1, p2) => switch (getPatRes_h(id, p1, r), getPatRes_h(id, p2, r)) {
            | (Some(r'), None) => Some(Rfst(r'))
            | (None, Some(r')) => Some(Rsnd(r'))
            | (None, None) => failwith("Id not found in pattern")
            | (Some(r1), Some(r2)) => failwith("The same variable id is bound in two places in the same pattern")
        }
    }

// exs = list((env, ex))
// e -> r using env
// unevaluate(r, ex)

and constrainExp = (delta, f, exp, exs) => {
    switch (exs) {
        | [] => Some(([], []))
        | [(env, ex), ...xs] => {
            switch (constrainExp(delta, f, exp, xs), unevaluate(delta, f, Evaluator.evalAndFill(env, exp, f), ex)) {
                | (None, _) => None
                | (_, None) => None
                | (Some(k1), Some(k2)) => Some(merge(k1, k2))
                }
        }
    }
}

// Assuming k2 has no repeats this works
and merge = (k1: unevalcons, k2: unevalcons) => {
    let (u1, f1) = k1;
    let (u2, f2) = k2;
    (merge_h(u1, u2), f1 @ f2)
}

and merge_h = (u1: unfilled_holes, u2: unfilled_holes) => {
    switch (u1) {
        | [] => u2
        | [h, ...xs] => merge_h(xs, Tools.add(h, u2))
        }
}

and mergeCons = (k1, k2) => {
    switch (k1, k2) {
        | (None, _) => None
        | (_, None) => None
        | (Some(k1'), Some(k2')) => 
            Some(merge(k1', k2'))
    }
}
    
and optionPred = (x) =>
    switch(x) {
        | Some(_) => true
        | None => false
        };



