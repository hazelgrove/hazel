open Language;

/* The Flags config slide: switches for the editor, each a Bool shown as a
   checkbox. `hud` shows the frame-rate HUD (the script in index.html). */
let hud_label = "hud";

let checkbox = (b: bool): Exp.t =>
  IdTagged.FreshGrammar.Exp.(
    projector(
      {
        kind: ProjectorKind.Checkbox,
        model: "()",
      },
      bool(b),
    )
  );

let source = {
  open Haz3lcore;
  let exp =
    IdTagged.FreshGrammar.(
      Exp.(
        let_(
          Pat.var("flags"),
          tuple([tup_label(label(hud_label), checkbox(false))]),
          var("flags"),
        )
      )
    );
  ExpToSegment.exp_to_segment(
    ~settings=ExpToSegment.Settings.editable(~inline=false),
    exp,
  )
  |> PrettySegment.prettify
  |> Zipper.unzip(~direction=Left)
  |> PersistentZipper.persist;
};

let expected_type =
  IdTagged.FreshGrammar.Typ.(prod([tup_label(label(hud_label), bool())]));

/* The evaluated slide's switches, by label. */
let rec flags_of_value = (v: Exp.t): list((string, bool)) =>
  switch (v.term) {
  | Parens(e) => flags_of_value(e)
  | Tuple(es) => List.concat_map(flags_of_value, es)
  | TupLabel({term: Label(name), _}, {term: Atom(Bool(on)), _}) => [
      (name, on),
    ]
  | _ => []
  };

let hud_of_value = (v: Exp.t): bool =>
  List.assoc_opt(hud_label, flags_of_value(v))
  |> Option.value(~default=false);

/* Parse, analyze, evaluate and read back a Flags slide, for startup. Any
   slide that fails leaves the HUD off. */
let hud_of_source = (slide: Haz3lcore.PersistentZipper.t): bool =>
  try({
    let zipper = Haz3lcore.PersistentZipper.unpersist(slide, ~root=Exp);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    let (_, elaborated) =
      Statics.mk(
        ~ana=expected_type,
        CoreSettings.on,
        Builtins.ctx_init(Some(Int)),
        term,
      );
    let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, elaborated);
    hud_of_value(result);
  }) {
  | _ => false
  };

/* Show or hide the HUD through the hook index.html's script defines. */
let set_hud = (on: bool): unit =>
  ignore(
    Js_of_ocaml.Js.Unsafe.eval_string(
      "typeof window !== 'undefined' && window.hazelHud && window.hazelHud("
      ++ (on ? "true" : "false")
      ++ ")",
    ),
  );
