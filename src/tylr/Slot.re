[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Meld.slot('a);

let empty = None;
let full = m => Some(m);

module Profile = {
  type t = option(Meld.Profile.t);

  let mk = (~has_tokens=false, sort) =>
    Some(Meld.Profile.{sort, has_tokens});

  let has_tokens =
    fun
    | None => false
    | Some(p) => p.has_tokens;

  let merge = (l, r) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), None) => l
    | (None, Some(_)) => r
    | (Some(l), Some(r)) =>
      let p =
        Meld.Profile.{sort: Grout, has_tokens: l.has_tokens || r.has_tokens};
      Some(p);
    };
};
