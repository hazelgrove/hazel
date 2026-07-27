open Util.OptUtil.Syntax;
open Util;

/* Secondary runs type - duplicated here from IdTagged to avoid dependency cycle.
   Stores (before, after) pairs for round-tripping whitespace/comments. */
[@deriving (show({with_path: false}), sexp, yojson)]
type secondary_runs = (list(Secondary.t), list(Secondary.t));

let empty_secondary: secondary_runs = ([], []);

/* Annotation for variants - stores ids and secondary for round-tripping. */
[@deriving (show({with_path: false}), sexp, yojson)]
type variant_ann = {
  ids: list(Id.t),
  secondary: secondary_runs,
};

let empty_variant_ann: variant_ann = {
  ids: [],
  secondary: empty_secondary,
};
let mk_variant_ann = (~ids, ~secondary=empty_secondary, ()): variant_ann => {
  ids,
  secondary,
};

/* Variant now stores full annotation to preserve secondary for round-tripping.
   Previously stored only list(Id.t), losing whitespace information. */
[@deriving (show({with_path: false}), sexp, yojson)]
type variant('a) =
  | Variant(Constructor.t, variant_ann, option('a))
  | BadEntry('a);

/* Helper to extract ids from annotation for backwards compatibility */
let variant_ids =
  fun
  | Variant(_, ann, _) => ann.ids
  | BadEntry(_) => [];

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = list(variant('a));

let equal_constructor =
    (eq: ('a, 'a) => bool, x: variant('a), y: variant('a)): bool =>
  switch (x, y) {
  | (Variant(ctr1, _, Some(x1)), Variant(ctr2, _, Some(y1))) =>
    Constructor.equal(ctr1, ctr2) && eq(x1, y1)
  | (Variant(ctr1, _, None), Variant(ctr2, _, None)) =>
    Constructor.equal(ctr1, ctr2)
  | (BadEntry(x), BadEntry(y)) => eq(x, y)
  | (Variant(_), Variant(_))
  | (BadEntry(_), Variant(_))
  | (Variant(_), BadEntry(_)) => false
  };

let is_empty = (x: t('a)): bool =>
  List.for_all(
    fun
    | Variant(_, _, _) => false
    | BadEntry(_) => true,
    x,
  );

let same_constructor =
    (eq: ('a, 'a) => bool, x: variant('a), y: variant('a)): bool =>
  switch (x, y) {
  | (Variant(ctr1, _, _), Variant(ctr2, _, _)) =>
    Constructor.equal(ctr1, ctr2)
  | (BadEntry(x), BadEntry(y)) => eq(x, y)
  | (BadEntry(_), Variant(_))
  | (Variant(_), BadEntry(_)) => false
  };

let has_bad_entry = (x: t('a)): bool =>
  List.exists(
    fun
    | BadEntry(_) => true
    | Variant(_) => false,
    x,
  );

let has_good_entry = (x: t('a)): bool =>
  List.exists(
    fun
    | BadEntry(_) => false
    | Variant(_) => true,
    x,
  );

let free_variables = (f, m) =>
  m
  |> List.map(
       fun
       | Variant(_, _, Some(value)) => f(value)
       | _ => [],
     )
  |> List.flatten;

let is_ground = is_hole =>
  fun
  | [BadEntry(x)] when is_hole(x) => true
  | _ => false;

/* Extract constructor name from a variant, if it has one */
let constructor_key =
  fun
  | Variant(ctr, _, _) => Some(ctr)
  | BadEntry(_) => None;

/* computes all three regions of a venn diagram of two sets represented as lists.

   Variants are matched by constructor name through an index (so this stays
   linear in the size of the maps); BadEntries fall back to a linear scan with
   `f`. Sums may contain repeated constructor names, so the pairing order is
   load-bearing: we pair the FIRST xs entry with the FIRST ys entry of a given
   name and drop the later duplicates. Using Hashtbl.add/find_opt/remove as a
   multi-binding stack instead pairs duplicates up in reverse, which makes a
   sum compare unequal to a structural copy of itself. */
let venn_regions =
    (f: ('a, 'a) => bool, xs: list('a), ys: list('a))
    : (list(('a, 'a)), list('a), list('a)) => {
  /* First ys entry per constructor name, plus the positions of ys BadEntries */
  let ys_first: Hashtbl.t(string, 'a) = Hashtbl.create(List.length(ys));
  let ys_bad = ref([]);
  List.iteri(
    (i, y) =>
      switch (constructor_key(y)) {
      | Some(key) =>
        if (!Hashtbl.mem(ys_first, key)) {
          Hashtbl.add(ys_first, key, y);
        }
      | None => ys_bad := [(i, y), ...ys_bad^]
      },
    ys,
  );
  let ys_bad = List.rev(ys_bad^);
  /* Constructor names consumed by some x. A match consumes *every* ys entry
     sharing that name, so those never reach `right`. */
  let matched_keys: Hashtbl.t(string, unit) =
    Hashtbl.create(List.length(ys));
  /* Positions (in ys) of BadEntries consumed by some x */
  let bad_consumed: Hashtbl.t(int, unit) =
    Hashtbl.create(List.length(ys_bad));
  /* Constructor names of xs already processed; later duplicates are dropped */
  let seen_keys: Hashtbl.t(string, unit) = Hashtbl.create(List.length(xs));
  /* BadEntry xs already processed, for the same dedup on payloads */
  let seen_bad = ref([]);
  let acc = ref([]);
  let left = ref([]);
  List.iter(
    x =>
      switch (constructor_key(x)) {
      | Some(key) =>
        if (!Hashtbl.mem(seen_keys, key)) {
          Hashtbl.replace(seen_keys, key, ());
          switch (Hashtbl.find_opt(ys_first, key)) {
          | Some(y) =>
            Hashtbl.replace(matched_keys, key, ());
            acc := [(x, y), ...acc^];
          | None => left := [x, ...left^]
          };
        }
      | None =>
        let first_match = ref(None);
        List.iter(
          ((i, y)) =>
            if (!Hashtbl.mem(bad_consumed, i) && f(x, y)) {
              Hashtbl.replace(bad_consumed, i, ());
              if (Option.is_none(first_match^)) {
                first_match := Some(y);
              };
            },
          ys_bad,
        );
        switch (first_match^) {
        | Some(y) => acc := [(x, y), ...acc^]
        | None =>
          if (!List.exists(f(x, _), seen_bad^)) {
            left := [x, ...left^];
          }
        };
        seen_bad := [x, ...seen_bad^];
      },
    xs,
  );
  /* Whatever of ys nothing in xs consumed, in the original order */
  let right =
    List.filteri(
      (i, y) =>
        switch (constructor_key(y)) {
        | Some(key) => !Hashtbl.mem(matched_keys, key)
        | None => !Hashtbl.mem(bad_consumed, i)
        },
      ys,
    );
  (acc^ |> List.rev, left^ |> List.rev, right);
};

let meet_entry =
    (meet: ('a, 'a) => option('a), (x: variant('a), y: variant('a)))
    : option(variant('a)) =>
  switch (x, y) {
  | (Variant(ctr1, ids1, Some(value1)), Variant(ctr2, _, Some(value2)))
      when Constructor.equal(ctr1, ctr2) =>
    let+ value = meet(value1, value2);
    Variant(ctr1, ids1, Some(value));
  | (Variant(ctr1, ids1, None), Variant(ctr2, _, None))
      when Constructor.equal(ctr1, ctr2) =>
    Some(Variant(ctr1, ids1, None))
  | (BadEntry(x), BadEntry(_)) => Some(BadEntry(x))
  | _ => None
  };

let meet =
    (
      eq: ('a, 'a) => bool,
      meet: ('a, 'a) => option('a),
      m1: t('a),
      m2: t('a),
    )
    : option(t('a)) =>
  /* Short-circuit: physical equality - meet of identical maps is that map */
  if (m1 === m2) {
    Some(m1);
  } else {
    let (inter, left, right) = venn_regions(same_constructor(eq), m1, m2);
    let meet_entries = List.filter_map(meet_entry(meet), inter);
    if (List.length(meet_entries) == List.length(inter)) {
      switch (
        has_good_entry(left),
        has_bad_entry(m1),
        has_good_entry(right),
        has_bad_entry(m2),
      ) {
      | (_, true, _, true) => Some(meet_entries @ left @ right)
      | (false, true, _, _) => Some(meet_entries @ right)
      | (_, _, false, true) => Some(meet_entries @ left)
      | _ when left == [] && right == [] => Some(meet_entries)
      | _ => None
      };
    } else {
      None;
    };
  };

let match_synswitch =
    (
      match_synswitch: ('a, 'a) => 'a,
      eq: ('a, 'a) => bool,
      m1: t('a),
      m2: t('a),
    )
    : t('a) =>
  /* Short-circuit: physical equality */
  if (m1 === m2) {
    m1;
  } else {
    let (inter, left, _) = venn_regions(same_constructor(eq), m1, m2);
    let inter' =
      List.map(
        fun
        | (Variant(ctr, ids, Some(value1)), Variant(_, _, Some(value2))) =>
          Variant(ctr, ids, Some(match_synswitch(value1, value2)))
        | (v, _) => v,
        inter,
      );
    inter' @ left;
  };

let equal = (eq: ('a, 'a) => bool, m1: t('a), m2: t('a)) =>
  /* Short-circuit: physical equality */
  if (m1 === m2) {
    true;
  } else if (List.length(m1) != List.length(m2)) {
    false;
         /* Short-circuit: length mismatch means not equal */
  } else {
    switch (venn_regions(same_constructor(eq), m1, m2)) {
    | (inter, [], []) =>
      List.for_all(
        ((x, y)) =>
          switch (x, y) {
          | (Variant(_, _, Some(value1)), Variant(_, _, Some(value2))) =>
            eq(value1, value2)
          | (Variant(_, _, None), Variant(_, _, None)) => true
          | (BadEntry(x), BadEntry(y)) => eq(x, y)
          | _ => false
          },
        inter,
      )
    | _ => false
    };
  };

let map = (type a, f: option(a) => option(a), m: t(a)): t(a) => {
  let changed = ref(false);
  let result =
    List.map(
      variant => {
        switch (variant) {
        | Variant(ctr, args, value) =>
          let value' = f(value);
          if (value' !== value) {
            changed := true;
          };
          Variant(ctr, args, value');
        | BadEntry(value) => BadEntry(value)
        }
      },
      m,
    );
  if (changed^) {
    result;
  } else {
    m;
  };
};

let map_preserving = (type a, type b, f: a => b, m: t(a)): t(b) => {
  List.map(
    fun
    | Variant(ctr, args, Some(value)) =>
      Variant(ctr, args, Some(f(value)))
    | Variant(ctr, args, None) => Variant(ctr, args, None)
    | BadEntry(value) => BadEntry(f(value)),
    m,
  );
};

// TODO: maybe define a variant here instead of double option
let get_entry = (ctr, m) =>
  List.find_map(
    fun
    | Variant(ctr', _, value) when Constructor.equal(ctr, ctr') =>
      Some(value)
    | Variant(_)
    | BadEntry(_) => None,
    m,
  );

let has_constructor_no_args = ctr =>
  List.exists(
    fun
    | Variant(ctr', _, None) when Constructor.equal(ctr, ctr') => true
    | Variant(_) => false
    | BadEntry(_) => false,
  );

let get_constructors =
  List.filter_map(
    fun
    | Variant(ctr, _, _) => Some(ctr)
    | BadEntry(_) => None,
    _,
  );

let nth = (map: t('a), ctr: Constructor.t): option(int) => {
  // TODO: use List.find_index instead, which is available for OCaml 5.1
  let ctrs_sorted = map |> get_constructors |> List.sort(String.compare);
  List.find_opt(
    nth => List.nth(ctrs_sorted, nth) == ctr,
    List.init(List.length(ctrs_sorted), Fun.id),
  );
};
