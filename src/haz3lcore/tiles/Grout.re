open Util_web;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type shape =
  | Convex
  | Concave;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  shape,
};
let equal = (a: t, b: t) => a.shape == b.shape;
let id = g => g.id;

let shapes = g =>
  switch (g.shape) {
  | Convex => Nib.Shape.(Convex, Convex)
  | Concave => Nib.Shape.(Concave(Precedence.min), Concave(Precedence.min))
  };

let grout_cache: ref(option(Id.t)) = ref(None);

let cache_id = (id: option(Id.t)) => {
  grout_cache := id;
};

let get_cached_id = () =>
  switch (grout_cache.contents) {
  | Some(id) =>
    grout_cache := None;
    id;
  | None =>
    grout_cache := None;
    Id.mk();
  };

let mk_fits_shape = (s: Nib.Shape.t): t => {
  id: get_cached_id(),
  shape:
    switch (s) {
    | Convex => Concave
    | Concave(_) => Convex
    },
};

let fits_shape = (g: t, s: Nib.Shape.t) =>
  switch (g.shape, s) {
  | (Convex, Convex)
  | (Concave, Concave(_)) => false
  | (Convex, Concave(_))
  | (Concave, Convex) => true
  };

let fits = (g: t, g': t) =>
  switch (g.shape, g'.shape) {
  | (Convex, Convex)
  | (Concave, Concave) => false
  | (Convex, Concave)
  | (Concave, Convex) => true
  };

/* Tracks a grout inserted in lieu of a user-typed space.
 * When consumed, a space should be emitted in its place. */
let suppressed_space: ref(option(Id.t)) = ref(None);

let mark_space_owed = (id: Id.t): unit => suppressed_space := Some(id);

/* Check if grout ID owes a space. Clears ref on match. */
let redeem_space = (id: Id.t): option(Secondary.t) =>
  switch (suppressed_space^) {
  | Some(owed_id) when owed_id == id =>
    suppressed_space := None;
    Some(Secondary.mk_space(Id.mk()));
  | _ => None
  };

/* Check a list of grout for any that owe a space. */
let redeem_space_from = (gs: list(t)): option(Secondary.t) =>
  switch (suppressed_space^) {
  | None => None
  | Some(owed_id) =>
    if (List.exists((g: t) => g.id == owed_id, gs)) {
      suppressed_space := None;
      Some(Secondary.mk_space(Id.mk()));
    } else {
      None;
    }
  };

let merge = (gs: list(t)): option(t) =>
  switch (gs) {
  | [] => None
  | [hd, ...tl] =>
    switch (ListUtil.split_last_opt(tl)) {
    | None => Some(hd)
    | Some((_, ft)) => hd.shape == ft.shape ? Some(hd) : None
    }
  };
