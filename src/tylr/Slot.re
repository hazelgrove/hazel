[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Meld.slot('a);

let empty = None;
let full = m => Some(m);

module Profile = {
  type t = option(Meld.Profile.t);

  let mk = (~has_tokens=false, sort: Material.sorted) =>
    Some(Meld.Profile.{sort, has_tokens});

  let has_tokens: t => bool =
    fun
    | None => false
    | Some(p) => p.has_tokens;

  let merge = (l: t, r: t) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), None) => l
    | (None, Some(_)) => r
    | (Some(l), Some(r)) =>
      mk(~has_tokens=l.has_tokens || r.has_tokens, Grout())
    };
};
