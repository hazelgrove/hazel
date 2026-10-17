open Alcotest;
open Haz3lcore;
open Language;

/* The statics at a livelit projector's id describe the Projector node,
   and a text-loaded use adds a Parens layer; the model lookup has to see
   through both, or the livelit renders "No livelit found". The placeholder
   width comes from the livelit's own size only when the lookup succeeds. */
let placeholder_width = (source: string): int => {
  let z =
    PersistentZipper.parse_text(~source="livelit test", ~root=Exp, source)
    |> Option.get;
  let statics =
    CachedStatics.init(
      ~settings=CoreSettings.on,
      ~is_dynamic_term=false,
      ~stitch=Fun.id,
      ~root=Exp,
      z,
    );
  let parsed = MakeTerm.from_zip_for_sem(z, ~root=Exp);
  let p = Id.Map.find(List.hd(parsed.projector_list), parsed.projectors);
  let info =
    ProjectorInfo.mk_info(
      p,
      ~sample_focus=Sample.Focus.init,
      ~statics=statics.info_map,
      ~dynamics=Dynamics.Map.empty,
      ~elaborated=None,
    );
  let (module P) = ProjectorInit.to_module(p.kind);
  P.placeholder(p.model, info).horizontal;
};

let tests = (
  "LivelitProj",
  [
    test_case("js livelit finds its definition", `Quick, () =>
      check(
        int,
        "placeholder width",
        40,
        placeholder_width({|^^livelit(^js("1 + 1", ""))|}),
      )
    ),
    test_case("slider livelit finds its definition", `Quick, () =>
      check(
        int,
        "placeholder width",
        20,
        placeholder_width("^^livelit(^slider(50))"),
      )
    ),
  ],
);
