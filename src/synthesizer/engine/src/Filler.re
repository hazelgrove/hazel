// Class to house the filler, which calls both refinement and guessing,
// as well as deferal.

// Entirely nondeterministic. Probably need to create a bunch of tools to aid with
// this.

// gs : [(context, hole, type, ex constraints)]

open Types;

let rec updateHoleContext_h = (delta: Types.hole_context, gs: Types.goals) => {
    switch (gs) {
        | [] => delta
        | [(context, hole, typ, _), ...gs'] => {
            let xs = updateHoleContext_h(delta, gs');
            [(hole, (context, typ)), ...xs]
        }
    }
};

let updateHoleContext = (delta, h, gs) => {
    List.filter(
        ((h', _)) => h != h',
        updateHoleContext_h(delta, gs));
}

let rec updateUnfilledHoles = (gs) => 
    switch (gs) {
        | [] => []
        | [(_, h, _, exs), ...gs'] =>
            [(h, exs), ...updateUnfilledHoles(gs')]
    };

let optionPred = (x) => 
    switch (x) {
        | Some(_) => true
        | None => false
        };

let rec guessAndCheck_h = (delta, gamma, f, typ, exs, i) => {
    if (i > 8) {
        None
    } else {
        let es: list(Types.exp) = Guesser.guess(delta, gamma, typ, i);
        let checked = List.filter(
            (e) => {
                Unevaluator.constrainExp(delta, f, e, exs) -> optionPred
            },
            es);
        switch (checked) {
            | [] => guessAndCheck_h(delta, gamma, f, typ, exs, i + 1)
            | [e, ..._] => Some(e)
            }
    }
};

let guessAndCheck = (delta, gamma, f, typ, exs) => {
    Guesser.resetMemo();
    guessAndCheck_h(delta, gamma, f, typ, exs, 1)
};

let rec allBranchesFound = (xs) => {
    switch (xs) {
        | [] => true
        | [None, ..._] => false
        | [Some(_), ...xs] => allBranchesFound(xs)
        }
};


// In returns:
//  - K = (U, F)
//  - U = the new holes added
//  - F = The existing hole fillings + 1 new filled hole
//  - delta = the existing minus the whole just filled, plus any new holes

// Note from Sam:
//
// Branching is pretty basic right now.
// All it does is guess a scrutinee of each possible type, and
// then guesses a filling for the hole in each branch. This
// has obvious drawbacks in that no refinement will happen for these
// fillings and since we don't really guess refinement types it's a bit
// rough. 

// Fill should return list, of form [(k, delta)].  Update that now.

let rec fill = (delta, holeFillings, gamma, h, typ, exs, depth) => {
    switch (fill_h(delta, holeFillings, gamma, h, typ, exs, depth)) {
        | Some((depth, x)) => Some((depth, x))
        | None => None 
        }
}

and fill_h = (delta, holeFillings, gamma, h, typ, exs, depth) => {
    if (Refiner.refinable(typ, exs)) {
        switch(Refiner.refine(gamma, typ, exs)) {
            | Some((e, gs)) => {
                let f = [(h, e), ...holeFillings];
                let delta' = updateHoleContext(delta, h, gs);
                let u = updateUnfilledHoles(gs);
                let k = (u, f);
                Some((depth, [(k, delta')]))
            }
            | None => guessAndOrBranch(delta, holeFillings, gamma, h, typ, exs, depth)
        }
    } else {
        guessAndOrBranch(delta, holeFillings, gamma, h, typ, exs, depth)
    }
}

and guessAndOrBranch = (delta, holeFillings, gamma, h, typ, exs, depth) => {
    let g = guessAndCheck(delta, gamma, holeFillings, typ, exs);
    switch (g) {
        | None when depth <= 3 => {
            // Branch

            let bs = Brancher.branch(delta, gamma, holeFillings, typ, exs)
                |> List.map(
                    ((exp, goals, unevalCons)) => {
                        let f = [(h, exp), ...holeFillings];
                        let u = List.map(
                            ((gamma, h, typ, xs)) => (h, xs),
                            goals);
                        let delta' = List.filter(
                            ((h', _)) => h != h',
                            delta);
                        let delta' = List.map(
                            ((gamma, h, typ, _)) => (h, (gamma, typ)),
                            goals) @ delta';
                        ((u, f), delta')
                    });
            Some((depth+1, bs));
        }
        | Some(e') => {
            let f = [(h, e'), ...holeFillings];
            let delta' = List.filter(
                ((h', _)) => h != h',
                delta);
            let k = ([], f);
            Some((depth, [(k, delta')]))
        }
        | None => None
        }
};
