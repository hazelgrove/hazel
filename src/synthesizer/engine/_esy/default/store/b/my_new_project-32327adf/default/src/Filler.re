// Class to house the filler, which calls both refinement and guessing,
// as well as deferal.

// Entirely nondeterministic. Probably need to create a bunch of tools to aid with
// this.

// gs : [(context, hole, type, ex constraints)]

let rec updateHoleContext_h = (delta: Types.hole_context, gs: Types.goals) => {
    switch (gs) {
        | [] => delta
        | [(context, hole, typ, exs), ...gs'] => {
            let xs = updateHoleContext_h(delta, gs');
            [(hole, (context, typ)), ...xs]
        }
    }
};

let updateHoleContext = (delta, h, gs) => 
    List.filter(
        ((h', _)) => h != h',
        updateHoleContext_h(delta, gs));

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

let rec guessAndCheck_h = (delta, gamma, typ, exs, i) => {
    if (i > 5) {
        failwith("Guessing timed out")
    } else {
        let es = Guesser.guess(delta, gamma, typ, i);
        let checked = List.filter(
            (e) => optionPred (Unevaluator.constrainExp(e, exs)),
            es);
        switch (checked) {
            | [] => guessAndCheck_h(delta, gamma, typ, exs, i + 1)
            | [e, _] => e
            }
    }
};

let guessAndCheck = (delta, gamma, typ, exs) => guessAndCheck_h(delta, gamma, typ, exs, 1);

let rec fill = (delta, holeFillings, gamma, h, typ, exs) => {
    if (Refiner.refinable(typ)) {
        let (e, gs) = Refiner.refine(gamma, h, typ, exs);
        let f = [(h, e), ...holeFillings];
        let delta' = updateHoleContext(delta, h, gs);
        let u = updateUnfilledHoles(gs);
        let k = (u, f);
        (k, delta')
    } else {
        let e = guessAndCheck(delta, gamma, typ, exs);
        let f = [(h, e), ...holeFillings];
        let delta' = List.filter(
            ((h', _)) => h != h',
            delta);
        let k = ([], f);
        (k, delta')
    }
};
