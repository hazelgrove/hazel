type pairlist('k, 'v) = list(('k, 'v));

let rec lookup = (key, plst) => {
    switch (plst) {
        | [] => failwith("Key not in list")
        | [(id, value), _] when id == key => value
        | [_, ...xs] => lookup(key, xs)
    }
};
