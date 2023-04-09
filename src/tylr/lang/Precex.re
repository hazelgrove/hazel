type t = PTable.t(Regex.t);

let enter_eq =
    (~from: Dir.t, ~bound=?, p: t): list(list(Regex.Zipper.t(Label.t))) =>
  p
  |> List.mapi((prec, (r, a: Assoc.t)) => {
       open Regex;
       let entered = Zipper.enter(~from, r, Unzipped.empty);
       // assuming same opseq form within each precedence level
       let entered_toks = List.exists(((a, _)) => Atom.is_tok(a), entered);
       let bounded =
         switch (bound) {
         | None => true
         | Some(bound) =>
           switch (a) {
           | Some(R) => prec >= bound
           | Some(L)
           | None => prec > bound
           }
         };
       if (entered_toks) {
         entered;
       } else if (bounded) {
         entered |> List.concat_map(Zipper.move_to_tok(Dir.toggle(from)));
       } else {
         [];
       };
     });

let end_toks = (side: Dir.t) => PTable.map(Regex.end_toks(side));
