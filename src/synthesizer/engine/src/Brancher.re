
open Types;

let simplifyConstructor = (res) =>
    switch (res) {
        | Rictor(id1, _, Rctor(id2, _, r)) when id1 == id2 => r
        | _ => res
        };

let rec branch = (delta:hole_context, gamma:context, f, typ:type_, exs:excons) => {
    let datatypes = List.map(
        ((id, (t, _))) => t,
        gamma)
        |> List.filter(
            (t) => switch (t) {
                | D(_) => true
                | _ => false
                },
        )
        |> List.map(
            (t) => switch (t) {
                | D(adt) => adt
                | _ => failwith("Error in branch")
                },
        )
        |> List.sort_uniq(
            (t1, t2) => 0,
        );

    List.map(
        (d) => branch_indiv(delta, gamma, f, typ, exs, d),
        datatypes)
    |> List.concat
}

and branch_indiv = (delta, gamma, f, typ, exs, datatype) => {
    let es = Guesser.guess(delta, gamma, D(datatype), 1);
    List.map(
        (e) => {
            let constructors = Tools.lookup(datatype, sigma);
            let distributedExs = distribute(delta, f, exs, datatype, e, constructors);
            let unevalCons: option(unevalcons) = List.map(
                (exs) => Unevaluator.constrainExp(delta, f, e, exs),
                distributedExs)
                |> List.fold_left(Unevaluator.mergeCons, Some(([], [])));
            let branches = List.map(
                ((id, t)) => {
                    let h = IdGenerator.getId();
                    switch (t) {
                        | Pair_t(t1, t2) => {
                            let x1 = IdGenerator.getId();
                            let x2 = IdGenerator.getId();
                            (id, (P(V(x1), V(x2)), Hole(h)))
                        }
                        | _ => {
                            let x = IdGenerator.getId();
                            (id, (V(x), Hole(h)))
                        }
                    }
                },
                constructors);
            let exp = Case(e, branches);
            let newExCons = List.map2(
                    (dExs, (id, (p, _))) => List.map(
                        ((env, ex)) => {
                            let r = simplifyConstructor(Rictor(id, datatype, Evaluator.eval(env, e)));
                            let patBinds = List.map(
                                (x) => Unevaluator.getPatRes(x, p, r),
                                Unevaluator.getPatIds(p));
                            (patBinds @ env, ex)
                        },
                        dExs),
                    distributedExs, branches);

            let goals = List.mapi(
                    (i, (id, (pat, Hole(h)))) => {
                        let (_, ti) = List.nth(constructors, i);
                        let xs = List.nth(newExCons, i);
                        let newGamma = switch (e) {
                            | Var(x) => List.filter(((id, _)) => x != id, gamma)
                            | _ => gamma
                            };
                        switch (pat) {
                            | V(var) => ([(var, (ti, AnnRec)), ...newGamma], h, typ, xs)
                            | P(V(x1), V(x2)) => {
                                let Pair_t(t1, t2) = ti;
                                ([(x1, (t1, AnnRec)), (x2, (t2, AnnRec)), ...newGamma], h, typ, xs)
                            }
                            | _ => failwith("Sam took a shortcut and this isn't implemented yet. Blame him")
                            }
                    },
                    branches);
                (exp, goals, unevalCons)
        }, es);
}

and distribute = (delta, f, exs, adt, scrut, ctors) => {
    List.map(
        ((id, t)) => {
            List.filter(
                ((env, ex)) => {
                    let r = Evaluator.eval(env, scrut);
                    Unevaluator.unevaluate(delta, f, r, Ector(id, adt, Top))
                        |> Unevaluator.optionPred
                }, exs)
        }, ctors)
};



