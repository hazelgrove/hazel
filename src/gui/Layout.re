open Sexplib.Std;

/* Variable: `layout` */
[@deriving sexp]
type t('tag) =
  | VZero /* identity for VCat */
  | VCat(t('tag), t('tag))
  | HCat(t('tag), t('tag))
  | String(string)
  | Align(t('tag))
  | Tagged('tag, t('tag));

let strings_of_layout: t('tag) => list(string) = {
  let rec go: t('tag) => (bool, list(string)) =
    fun
    | VZero => (false, [])
    | VCat(l1, l2) => {
        let (a1, s1) = go(l1);
        let (_a2, s2) = go(l2);
        (a1, s1 @ s2);
      }
    | HCat(l1, l2) => {
        let (a1, s1) = go(l1);
        let (a2, s2) = go(l2);
        switch (GeneralUtil.split_last(s1), s2) {
        | (None, []) => failwith("unimplemented")
        | (None, [_head, ..._tail]) => failwith("unimplemented")
        | (Some((_init, _last)), []) => failwith("unimplemented")
        | (Some((init, last)), [head, ...tail]) =>
          let tail' =
            if (a2) {
              let indent = String.make(String.length(last), ' ');
              List.map(s => indent ++ s, tail);
            } else {
              tail;
            };
          (a1, init @ [last ++ head, ...tail']);
        };
      }
    | String(string) => (false, [string])
    | Align(l) => {
        let (_a, s) = go(l);
        (true, s);
      }
    | Tagged(_tag, l) => go(l);
  l => snd(go(l));
};
