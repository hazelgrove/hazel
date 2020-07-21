type pairlist('k, 'v) = list(('k, 'v));

let rec lookup = (key, plst) => {
    switch (plst) {
        | [] => {
            raise(Not_found)
        }
        | [(id, value), ..._] when id == key => value
        | [_, ...xs] => lookup(key, xs)
    }
};

let rec add = ((k, v: pairlist('a, 'b)), plst) => {
    switch(plst) {
        | [] => [(k, v)]
        | [(k, v'), ...xs] => [(k, List.concat([v, v'])), ...xs]
        | [p, ...xs] => [p, ...add((k, v), xs)]
        }
};

