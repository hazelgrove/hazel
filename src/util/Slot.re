/* a one-entry memo: the last key and its value. [get] returns the value
   while [same(last key, key)] holds, else computes and keeps a new one */
type t('k, 'v) = ref(option(('k, 'v)));

let mk = (): t('k, 'v) => ref(None);

let get =
    (~same: ('k, 'k) => bool=(===), slot: t('k, 'v), key: 'k, f: unit => 'v)
    : 'v =>
  switch (slot^) {
  | Some((k, v)) when same(k, key) => v
  | _ =>
    let v = f();
    slot := Some((key, v));
    v;
  };
