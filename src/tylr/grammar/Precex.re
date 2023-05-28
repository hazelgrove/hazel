type t('s) = list((Regex.t('s), Assoc.t));

/**
 * `enter_eq(~from, ~bound, p)` returns, for every precedence level in `p`,
 * a list of zippers unzipped to a `from`-most position across all choices
 * of the regex at that prec level.
 */;
// let enter_eq =
//     (~from: Util.Dir.t, ~bound=?, p: t(_))
//     : list(list(Regex.Zipper.t(Label.t, _))) =>
//   p
//   |> List.mapi((prec, (r, a: Assoc.t)) => {
//        let entered = Regex.enter(~from, r, Regex.Unzipped.empty);
//        // assuming same opseq form within each precedence level,
//        // thus safe to check for single existence
//        let entered_toks =
//          List.exists(((a, _)) => Option.is_some(Regex.Atom.is_tok(a)), entered);
//        let bounded =
//          switch (bound) {
//          | None => true
//          | Some(bound) =>
//            switch (a) {
//            | Some(R) => prec >= bound
//            | Some(L)
//            | None => prec > bound
//            }
//          };
//        if (entered_toks) {
//          entered;
//        } else if (bounded) {
//          entered
//          |> List.concat_map(Regex.Zipper.move_to_tok(Dir.toggle(from)));
//        } else {
//          [];
//        };
//      });

// let end_toks = (side: Dir.t) => PTable.map(Regex.end_toks(side));
