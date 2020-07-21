let rec all(x:list(bool)):bool =
switch(x) {
| [] => true
| [false, ...xs] => false
| [true, ...xs] => all(xs)
}
let all_predicate(f:'a=>bool, l:list('a)):bool =
    all(List.map(f, l))