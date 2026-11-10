/* a one-entry memo: the last key and its value. [get] returns the value
   while [same(last key, key)] holds, else computes and keeps a new one */
type t('k, 'v) = ref(option(('k, 'v)));

let mk = (): t('k, 'v) => ref(None);

let get =
    (
      ~same: ('k, 'k) => bool=(===),
      ~on_miss: (unit => 'v) => 'v=?,
      slot: t('k, 'v),
      key: 'k,
      f: unit => 'v,
    )
    : 'v =>
  switch (slot^) {
  | Some((k, v)) when same(k, key) => v
  | _ =>
    /* [on_miss], when given, runs [f]: to time it, say */
    let v =
      switch (on_miss) {
      | Some(m) => m(f)
      | None => f()
      };
    slot := Some((key, v));
    v;
  };
