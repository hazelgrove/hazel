include IntMap;

let mergefn = (_, a, b) =>
  switch (a, b) {
  | (Some(_), Some(y)) => Some(y)
  | (Some(x), _) => Some(x)
  | (_, Some(y)) => Some(y)
  | _ => None
  };

let shadowed_merge = (a, b) => merge(mergefn, a, b);
