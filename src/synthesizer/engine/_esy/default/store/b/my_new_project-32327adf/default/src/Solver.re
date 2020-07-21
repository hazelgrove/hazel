// Houses the main recursive function which iterates through
// the list of all unsolved holes and fills them.

let rec solve = (hContext, k) => {
    let (u, f) = k;
    switch (u) {
        | [] => (f, hContext)
        | [(h, x), ...us] => {
            let (context, t) = Tools.lookup(h, hContext);
            let (k', hContext') = Filler.fill(hContext, f, context, h, t, x);
            // merge((us, k), k') = k'';
            // solve(hContext', k'');
            ([], [])
        }
    }
};
