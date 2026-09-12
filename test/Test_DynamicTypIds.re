/* Colouring the runtime-derived parts of a type in the type probe's Dynamic
   mode. Code.re colours a tile when its id is in the set, so the ids have to
   line up with the tiles the renderer actually emits. Two ways that can fail,
   neither caught by checking the padding alone: an id in the set but never
   emitted colours nothing, and a tile emitted for a runtime-derived part but
   left out stays the static colour. Parens are where both bite -- preparing
   for rendering inserts them as real nodes that the renderer emits. */

open Alcotest;
open Haz3lcore;
open Language;

/* The settings the projector renders with -- ProjectorInfo.utility, not a
   hand-written record. Testing any other configuration would test something
   the projector never runs. */
let settings: ExpToSegment.Settings.t = {
  ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
  show_unknown_as_hole: false,
  hole_tiles: false,
  fold_fn_bodies: `NoFold,
  project_tables: false,
};

/* The dynamic ids, and the one render they describe, reported two ways.
   The id sets are deliberately different: `emitted` includes Grout and
   Secondary, because a runtime-derived node can legitimately render as Grout
   -- an Unknown does, with show_unknown_as_hole off -- and including it is
   harmless. `tiles` is what Code.re actually colours, so it is the right set
   to require full coverage of. */
type rendered = {
  dynamic_ids: Id.Set.t,
  emitted: Id.Set.t,
  tiles: Id.Set.t,
};

let dynamic_ids_and_rendered =
    (~static_typ: Typ.t, ~dynamic_typ: Typ.t): rendered => {
  let (seg, dynamic_ids) =
    TypToSegment.typ_to_segment_with_diff_ids(
      ~settings,
      ~against=static_typ,
      dynamic_typ,
    );
  {
    dynamic_ids,
    emitted: Segment.ids(seg) |> Id.Set.of_list,
    tiles: Segment.tile_ids(seg) |> Id.Set.of_list,
  };
};

/* SOUNDNESS. Every id in the set must appear somewhere in the render. An id that
   appears nowhere describes nothing, and means the dynamic_ids and the segment were
   computed from different types. */
let qcheck_dynamic_ids_are_emitted =
  QCheck.Test.make(
    ~name="every dynamic id appears in the rendered segment",
    ~count=300,
    QCheck.pair(
      QCheck_Util.arb_typ(~minimal_idents=false, 12),
      QCheck_Util.arb_typ(~minimal_idents=false, 12),
    ),
    ((static_typ, dynamic_typ)) => {
      let {dynamic_ids, emitted, _} =
        dynamic_ids_and_rendered(~static_typ, ~dynamic_typ);
      Id.Set.subset(dynamic_ids, emitted);
    },
  );

/* COMPLETENESS, in the case that needs no oracle: if statics knew nothing
   then the whole type came from runtime, so every tile must be green. */
let qcheck_fully_dynamic_colours_everything =
  QCheck.Test.make(
    ~name="a wholly runtime-derived type has every tile green",
    ~count=300,
    QCheck_Util.arb_typ(~minimal_idents=false, 12),
    dynamic_typ => {
      /* Only meaningful when runtime refined something: if the dynamic type
         is itself unknown, nothing was learned and nothing should be green. */
      QCheck.assume(
        switch (Typ.term_of(dynamic_typ)) {
        | Unknown(_) => false
        | _ => true
        },
      );
      let {dynamic_ids, tiles, _} =
        dynamic_ids_and_rendered(
          ~static_typ=Typ.fresh(Unknown(Internal)),
          ~dynamic_typ,
        );
      Id.Set.subset(tiles, dynamic_ids);
    },
  );

/* A type runtime merely confirmed has nothing to colour. */
let qcheck_identical_colours_nothing =
  QCheck.Test.make(
    ~name="a type identical to the static one dynamic_ids nothing",
    ~count=300,
    QCheck_Util.arb_typ(~minimal_idents=false, 12),
    typ => {
      let {dynamic_ids, _} =
        dynamic_ids_and_rendered(~static_typ=typ, ~dynamic_typ=typ);
      Id.Set.is_empty(dynamic_ids);
    },
  );

/* The invariant the whole scheme rests on: a prepared type already carries
   every id its rendering consumes, so the renderer never mints one. An id
   minted during rendering is in the DOM but in no type, so nothing can name
   it and the token it labels can never be coloured. */
let qcheck_prepared_ids_are_sufficient =
  QCheck.Test.make(
    ~name="a prepared type carries every id its rendering consumes",
    ~count=500,
    QCheck_Util.arb_typ(~minimal_idents=false, 20),
    typ =>
    TypToSegment.ids_sufficient(~settings, typ)
  );

/* Unit pins for the id counts typ_to_pretty pads from. */
let count_tests =
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "necessary_ids matches what each form consumes",
      `Quick,
      () => {
        let check_count = (name, expected, typ) =>
          check(Alcotest.int, name, expected, PadIds.necessary_ids(typ));
        check_count("unit renders from rep_id", 1, Prod([]) |> Typ.temp);
        check_count(
          "a pair needs one separator",
          1,
          Prod([int(), bool()]) |> Typ.temp,
        );
        check_count(
          "a triple needs two separators",
          2,
          Prod([int(), bool(), string()]) |> Typ.temp,
        );
        check_count("Void renders from rep_id", 1, Sum([]) |> Typ.temp);
        check_count("Int renders from rep_id", 1, int());
        check_count(
          "an empty sig renders from rep_id",
          1,
          Sig([]) |> Typ.temp,
        );
        let sig_item = (): Sig.t => Sig.temp(EmptyHole);
        check_count(
          "a one-item sig renders from rep_id",
          1,
          Sig([sig_item()]) |> Typ.temp,
        );
        check_count(
          "a three-item sig needs two separators plus rep_id",
          3,
          Sig([sig_item(), sig_item(), sig_item()]) |> Typ.temp,
        );
      },
    ),
  ];
let tests = [
  (
    "DynamicTypIds",
    count_tests
    @ [
      QCheck_alcotest.to_alcotest(qcheck_dynamic_ids_are_emitted),
      QCheck_alcotest.to_alcotest(qcheck_fully_dynamic_colours_everything),
      QCheck_alcotest.to_alcotest(qcheck_identical_colours_nothing),
      QCheck_alcotest.to_alcotest(qcheck_prepared_ids_are_sufficient),
    ],
  ),
];
