type t('x) = list(('x, Assoc.t));

let map = f => List.map(((x, a)) => (f(x), a));
