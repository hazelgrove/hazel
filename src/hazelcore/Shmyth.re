module Option = {
  /* Note that references to `Option` in this module refer to ocaml's option */
  module Let_syntax = {
    module Let_syntax = {
      /* let return = (o: 'a): option('a) => Some(o); */

      let map = (~f: 'a => 'b, o: option('a)): option('b) =>
        Option.map(f, o);

      let bind = (o: option('a), ~f: 'a => option('b)): option('b) =>
        Option.bind(o, f);
    };
  };
};

let htyp_to_styp: HTyp.t => Smyth.Lang.typ = (
  failwith(__LOC__): HTyp.t => Smyth.Lang.typ
);

let rec styp_to_htyp: Smyth.Lang.typ => option(HTyp.t) =
  fun
  | TArr(t1, t2) => {
      let%bind.Option t1' = styp_to_htyp(t1);
      let%map.Option t2' = styp_to_htyp(t2);
      HTyp.Arrow(t1', t2');
    };
