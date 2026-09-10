/* Colouring the runtime-derived parts of a type in the type probe's Dynamic
   mode. Code.re colours a tile when its id is in the set,
   so the dynamic_ids have to line up with the tiles the renderer actually emits.
   Two ways that can fail, and neither is caught by checking the padding
   alone: an id in the set but never emitted colours nothing, and a tile emitted
   for a runtime-derived part but left out stays the static colour.

   Both used to fail. Parens are the reason: normalization inserts them as
   real nodes and the renderer emits them as tiles, so a comparison of the
   un-normalized types could not name them. */

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

let normalize = ExpToSegment.normalize_typ(~settings);
let render_normalized = ExpToSegment.normalized_typ_to_segment(~settings);

/* The dynamic_ids, and the one render they describe, reported two ways. Mirrors
   DynamicTypInfer.displayed_segment_and_dynamic_ids: normalize both, diff the
   normalized forms, render the normalized dynamic type once.

   The two id sets are deliberately different. `all` includes Grout and
   Secondary, because a runtime-derived node can legitimately render as Grout --
   an Unknown does, with show_unknown_as_hole off -- and including it is harmless.
   `tiles` is what Code.re actually colours, so it is the right set to
   require full coverage of. */
let dynamic_ids_and_rendered =
    (~static_typ: Typ.t, ~dynamic_typ: Typ.t): (Id.Set.t, Id.Set.t, Id.Set.t) => {
  let static_n = normalize(static_typ);
  let dynamic_n = normalize(dynamic_typ);
  let dynamic_ids = Typ.diff(static_n, dynamic_n) |> Id.Set.of_list;
  let seg = render_normalized(dynamic_n);
  (
    dynamic_ids,
    Segment.ids(seg) |> Id.Set.of_list,
    Segment.tile_ids(seg) |> Id.Set.of_list,
  );
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
      let (dynamic_ids, all, _tiles) =
        dynamic_ids_and_rendered(~static_typ, ~dynamic_typ);
      Id.Set.subset(dynamic_ids, all);
    },
  );

/* COMPLETENESS, in the case that needs no oracle: if statics knew nothing
   then the whole type came from runtime, so every tile must be green.
   The parens were what failed here. */
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
      let (dynamic_ids, _all, tiles) =
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
      let (dynamic_ids, _, _) =
        dynamic_ids_and_rendered(~static_typ=typ, ~dynamic_typ=typ);
      Id.Set.is_empty(dynamic_ids);
    },
  );

/* The invariant the whole scheme rests on: a normalized type already carries
   every id its rendering consumes, so the renderer never mints one. An id
   minted during rendering is in the DOM but in no type, so nothing can name
   it and the token it labels can never be coloured.

   This is what the old `raise_if_padding` settings field was for. As a
   property over generated types it covers far more than that flag did --
   it only fired on whatever input a test happened to render, and its single
   `true` lived in a test while eight production modules carried a `false`. */
let qcheck_normalized_ids_are_sufficient =
  QCheck.Test.make(
    ~name="a normalized type carries every id its rendering consumes",
    ~count=500,
    QCheck_Util.arb_typ(~minimal_idents=false, 20),
    typ =>
    ExpToSegment.typ_ids_sufficient(
      ExpToSegment.normalize_typ(~settings, typ),
    )
  );

/* Unit pins for the id counts, which mirror the pad_ids calls in
   typ_to_pretty. Both of these were wrong: Sum padded one id too many, and
   MultiHole was treated as needing one when it needs one per gap. */
let count_tests =
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "necessary_ids matches what each form consumes",
      `Quick,
      () => {
        let check_count = (name, expected, typ) =>
          check(
            Alcotest.int,
            name,
            expected,
            ExpToSegment.necessary_ids(typ),
          );
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
      QCheck_alcotest.to_alcotest(qcheck_normalized_ids_are_sufficient),
    ],
  ),
];
