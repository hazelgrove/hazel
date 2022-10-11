open Util;

[@deriving show({with_path: false})]
type t = Aba.t(list(Whitespace.t), Hole.t);

let empty = Aba.mk([[]], []);

let rev: t => t = Aba.rev(List.rev, Fun.id);

let length = trim =>
  trim |> Aba.join(List.length, _ => 1) |> List.fold_left((+), 0);

let append: (t, t) => t = Aba.append((@));

let cons_w = (w: Whitespace.t, (wss, gs)) => {
  // safe bc Aba always has at least one A element
  let (ws, wss) = ListUtil.split_first(wss);
  Aba.mk([[w, ...ws], ...wss], gs);
};
let cons_g = (g: Hole.t, (wss, gs)) => Aba.mk([[], ...wss], [g, ...gs]);

// TODO clean up l_pad bool in return type
let repad = (l_pad, trim: t, r_pad): IdGen.t((bool, t)) =>
  IdGen.Syntax.(
    switch (trim) {
    | ([ws], []) =>
      (l_pad || r_pad) && ws == []
        ? {
          let+ space = Whitespace.mk_space;
          (true, ([[space]], []));
        }
        : return((false, trim))
    | _ =>
      let* (l_padded, trim) =
        l_pad && Aba.first_a(trim) == []
          ? {
            let+ space = Whitespace.mk_space;
            (true, append(([[space]], []), trim));
          }
          : return((false, trim));
      let+ trim =
        r_pad && Aba.last_a(trim) == []
          ? {
            let+ space = Whitespace.mk_space;
            append(trim, ([[space]], []));
          }
          : return(trim);
      (l_padded, trim);
    }
  );

let regrout =
    (~lint=true, ~caret: option(int)=?, (l, r): Nibs.t, trim: t, s: Sort.t)
    : IdGen.t((int, t)) => {
  // index each element of the original trim to determine change
  // in caret index after regrouting
  let (_, itrim) =
    trim
    |> Aba.fold_left_map(
         ws => (List.length(ws), List.mapi((i, w) => (i, w), ws)),
         (i, g, ws) => {
           let n = List.length(ws);
           (i + 1 + n, (i, g), List.mapi((j, w) => (i + 1 + j, w), ws));
         },
       );
  open IdGen.Syntax;
  let* new_gs = Hole.mk((l, r), s);
  let (remaining_gs, new_itrim) =
    itrim
    |> Aba.map_a(
         List.filter(((_, w)) => !lint || Whitespace.is_linebreak(w)),
       )
    |> Aba.fold_left_map(
         ws => (new_gs, ws),
         (new_gs, (i, _), ws) =>
           switch (new_gs) {
           | [] => ([], None, ws)
           | [hd, ...tl] => (tl, Some((i, hd)), ws)
           },
       );
  let new_itrim: Aba.t(list((int, Whitespace.t)), (int, Hole.t)) =
    new_itrim
    |> Aba.fold_right(
         (ws, g, trim) =>
           switch (g, trim) {
           | (None, ([hd, ...tl], gs)) => Aba.mk([ws @ hd, ...tl], gs)
           | (Some(g), _) => Aba.cons(ws, g, trim)
           | _ => raise(Aba.Invalid)
           },
         ws => Aba.mk([ws], []),
       );

  let lt_caret = i =>
    switch (caret) {
    | None => false
    | Some(j) => i < j
    };

  let new_itrim_with_extra_gs:
    Aba.t(list((int, Whitespace.t)), (int, Hole.t)) = {
    let cons_remaining_gs = (trim: Aba.t(_)) =>
      List.fold_right(
        // HACK(d) -1 index safe because this will only happen after caret
        (g, trim) => Aba.cons([], ((-1), g), trim),
        remaining_gs,
        trim,
      );

    let go_iws = (iws, (consed, trim: Aba.t(_))) =>
      List.fold_right(
        ((i, _) as iw, (consed, trim)) => {
          let (consed, trim) =
            lt_caret(i) && !consed
              ? (true, cons_remaining_gs(trim)) : (consed, trim);
          (consed, Aba.append((@), ([[iw]], []), trim));
        },
        iws,
        (consed, trim),
      );

    let (consed, new_itrim) =
      new_itrim
      |> Aba.fold_right(
           (iws, ig, (consed, trim: Aba.t(_))) => {
             let trim = consed ? trim : cons_remaining_gs(trim);
             let trim = Aba.cons([], ig, trim);
             go_iws(iws, (true, trim));
           },
           iws => go_iws(iws, (false, ([[]], []))),
         );
    consed ? new_itrim : cons_remaining_gs(new_itrim);
  };

  let caret =
    new_itrim_with_extra_gs
    |> Aba.map_a(List.map(((i, _)) => lt_caret(i) ? 1 : 0))
    // HACK(d): i >= 0 to account for negative index hack above
    |> Aba.map_b(((i, _)) => i >= 0 && lt_caret(i) ? 1 : 0)
    |> Aba.join(List.fold_left((+), 0), Fun.id)
    |> List.fold_left((+), 0);

  let new_trim =
    new_itrim_with_extra_gs |> Aba.map_a(List.map(snd)) |> Aba.map_b(snd);

  // let new_trim_with_extra_gs =
  //   remaining_gs
  //   |> List.fold_left((trim, g) => Aba.snoc(trim, g, []), new_trim);

  let+ (l_padded, padded_trim) =
    lint
      ? repad(Nib.is_padded(l), new_trim, Nib.is_padded(r))
      : return((false, new_trim));

  (caret + (l_padded ? 1 : 0), padded_trim);
};

let is_linted = (nibs, trim, s) => {
  let ((_, trim'), _) = regrout(nibs, trim, s, 0);
  switch (Aba.zip_opt(trim, trim')) {
  | None => false
  | Some((wswss, _)) =>
    wswss
    |> List.for_all(((ws, ws')) => List.length(ws) == List.length(ws'))
  };
};
