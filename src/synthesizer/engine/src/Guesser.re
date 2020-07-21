// File to hold the main guessing function

// Takes in context and hole type and returns an expression.
// (context, goal type) -> e (all of the goal type)

// Gamma [(variable name, type)]
// guess(context, t) ::> 

open Types;

let memo = Array.make(10, []);

let resetMemo = () => Array.mapi((i, x) => memo[i] = [], memo);

let rec partition_h = (n, m, i) => {
    if (m == i) {
        []
    } else {
        [(n, m), ...partition_h(n - 1, m + 1, i)]
    }
};

let partition = (n) => partition_h(n - 1, 1, n);

let guessFst = (delta, gamma, typ, i) => {
    let candidates = List.filter(
        (e) => switch (Typing.getType(delta, gamma, e)) {
            | Pair_t(typ, _) => {
                true
            }
            | t => {
                false
            }
            },
            memo[i-2]);
    let ret = List.map((e) => Fst(e), candidates);
    List.map((e) => {
        e}, 
        ret);
    ret
}

let guessSnd = (delta, gamma, typ, i) => {
    let candidates = List.filter(
        (e) => switch (Typing.getType(delta, gamma, e)) {
            | Pair_t(_, typ) => true
            | _ => false
            },
            memo[i-2]);
    List.map((e) => Snd(e), candidates)
}

let guessPairs = (delta, gamma, t1, t2, i, j) => {
    let es = List.filter(
        (e) => Typing.getType(delta, gamma, e) == t1,
        memo[i-1])
        |> List.map(
            (e1) => List.filter(
                (e2) => Typing.getType(delta, gamma, e2) == t2, memo[j - 1])
                |> List.map(
                    (e2) => Pair(e1, e2)))
        |> List.concat;
    es
}

let guessCtors = (delta, gamma, typ, i) => {
    switch (typ) {
        | D(adt) => {
            let constructors = Tools.lookup(adt, sigma);
            List.map(
                ((c, t)) => 
                switch (t) {
                    | Pair_t(t1, t2) => {
                        let pairs = partition(i);
                        List.map(
                            ((i, j)) => guessPairs(delta, gamma, t1, t2, i, j),
                            pairs)
                            |> List.concat
                            |> List.map(
                                (e) => Ctor(c, adt, e))
                    }
                    | _ => {
                        List.filter(
                            (e) => Typing.getType(delta, gamma, e) == t,
                            memo[i-2])
                            |> List.map(
                                (e) => Ctor(c, adt, e))
                    }
                },
                constructors)
            |> List.concat
        }
        | _ => []
    }
};

let guessApp = (delta, gamma: context, typ: type_, i: int, j: int): list(exp) => {
    let funcs = List.filter(
        (e) => switch(Typing.getType(delta, gamma, e)) {
            | Function_t(_, _) => true
            | _ => false
            },
        memo[i - 1]);
    let args = List.map(
        (e) => {
            let candidates = List.filter(
                (x) => {
                    let Function_t(t1, t2) = Typing.getType(delta, gamma, e);
                    let t = Typing.getType(delta, gamma, x);
                    switch (e) {
                        | Function(n, _, _, _)
                        | Var(n) => {
                            let (_, ann1) = Tools.lookup(n, gamma);
                            if (ann1 == AnnFunc) {
                                switch (x) {
                                    | Var(id) => {
                                        let (_, ann2) = Tools.lookup(id, gamma);
                                        ann2 == AnnRec && t == t1
                                    }
                                    | _ => false
                                    }
                            } else {
                                t == t1
                            }
                        }
                        | _ => t == t1
                        }
                },
                memo[j - 1]);
            List.map(
                (x) => Application(e, x),
                candidates)
        },
        funcs);
    List.concat(args)
};

let guess = (delta: hole_context, gamma: context, typ: type_, i: int): list(exp) => {
    if (i == 1) {
        let terms = List.filter(((_, (t, _))) => t == typ, gamma);
        memo[0] = [Unit] @ List.map(
            ((x, _)) => Var(x), 
            gamma);
        List.map(((x, _)) => Var(x), terms)
    } else {
        // Guess first
        let firsts = guessFst(delta, gamma, typ, i);
        // Guess second
        let seconds = guessSnd(delta, gamma, typ, i);
        // Guess constructors
        let ctors = guessCtors(delta, gamma, typ, i);
        // Guess Applications
        let pairs = partition(i);
        let apps = List.map(((n, m)) => guessApp(delta, gamma, typ, n, m), pairs) |> List.concat;
        memo[i - 1] = firsts @ seconds @ ctors @ apps;
        List.filter(
            (e) => Typing.getType(delta, gamma, e) == typ,
            memo[i - 1])
    }
};


