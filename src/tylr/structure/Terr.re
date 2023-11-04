open Util;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) = {
    wald: Wald.t('a),
    slot: Slot.t('a),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type m = t(Material.Molded.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type p = t(Piece.t);
};
include Base;

let mk = (~slot=None, wald) => {slot, wald};

module L = {
  // left to right: wald slot
  include Base;

  let combine = (ps: list(_), slots: list(Slot.t(_))): option(t(_)) =>
    ListUtil.split_last_opt(slots)
    |> Option.map(((slots, slot)) => mk(Wald.combine(ps, slots), ~slot));

  let rec s_of_slot =
    fun
    | None => []
    | Some(Meld.M(l, w, r)) => s_of_slot(l) @ [mk(w, ~slot=r)];

  let split_face = (t: p): (Piece.t, Chain.t(Slot.p, Piece.t)) => {
    let (p, (slots, ps)) = Wald.split_fst(t.wald);
    (p, Chain.mk(slots @ [t.slot], ps));
  };
  let face = t => fst(split_face(t));

  let pull = (~char=false, t: p): (Piece.t, list(p)) => {
    let (face, rest) = split_face(t);
    switch (Piece.unzip(1, face)) {
    | Ok((c, rest_face)) when char =>
      let (slots, ps) = Chain.(loops(rest), links(rest));
      (c, Option.to_list(combine([rest_face, ...ps], slots)));
    | _ =>
      let (slot, top) =
        switch (Chain.unlink(rest)) {
        | Error(slot) => (slot, [])
        | Ok((slot, p, tl)) =>
          let (slots, ps) = Chain.(loops(tl), links(tl));
          (slot, Option.to_list(combine([p, ...ps], slots)));
        };
      (face, s_of_slot(slot) @ top);
    };
  };
};

module R = {
  // left to right: slot wald
  include Base;

  let combine = (slots, ps) =>
    ListUtil.split_first_opt(slots)
    |> Option.map(((slot, slots)) => mk(~slot, Wald.combine(ps, slots)));

  let rec s_of_slot =
    fun
    | None => []
    | Some(Meld.M(l, w, r)) => s_of_slot(r) @ [mk(~slot=l, w)];

  let split_face = (t: p): (Chain.t(Slot.p, Piece.t), Piece.t) => {
    let ((ps, slots), p) = Wald.split_lst(t.wald);
    (Chain.mk([t.slot, ...slots], ps), p);
  };
  let face = t => snd(split_face(t));

  let pull = (~char=false, t: p): (list(p), Piece.t) => {
    let (rest, face) = split_face(t);
    let n = Piece.token_length(face);
    switch (Piece.unzip(n - 1, face)) {
    | Ok((rest_face, c)) when char =>
      let (slots, ps) = Chain.(loops(rest), links(rest));
      (Option.to_list(combine(slots, ps @ [rest_face])), c);
    | _ =>
      let (top, slot) =
        switch (Chain.unknil(rest)) {
        | Error(slot) => ([], slot)
        | Ok((tl, p, slot)) =>
          let (slots, ps) = Chain.(loops(tl), links(tl));
          (Option.to_list(combine(slots, ps @ [p])), slot);
        };
      (s_of_slot(slot) @ top, face);
    };
  };
};
