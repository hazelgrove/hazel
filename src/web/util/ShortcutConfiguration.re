open Language;
open Language.Unboxing;

type shortcut = {
  action_name: string,
  hotkey: string,
};

module DefaultConfiguration = {
  /* Default shortcut configuration - extracted from Shortcuts.ml */
  let shortcuts = [
    {
      action_name: "Undo",
      hotkey: "ctrl+z",
    },
    {
      action_name: "Redo",
      hotkey: "ctrl+shift+z",
    },
    {
      action_name: "Go to Definition",
      hotkey: "F12",
    },
    {
      action_name: "Go to Previous Hole",
      hotkey: "shift+tab",
    },
    {
      action_name: "Go To Next Hole",
      hotkey: "?",
    },
    {
      action_name: "Select current term",
      hotkey: "ctrl+d",
    },
    {
      action_name: "Select All",
      hotkey: "ctrl+a",
    },
    {
      action_name: "Toggle Selection Focus",
      hotkey: "?",
    },
    {
      action_name: "Set Selection Focus Left",
      hotkey: "ctrl+alt+shift+left",
    },
    {
      action_name: "Set Selection Focus Right",
      hotkey: "ctrl+alt+shift+right",
    },
    {
      action_name: "Fold",
      hotkey: "alt + f",
    },
    {
      action_name: "Probe",
      hotkey: "alt+v",
    },
    {
      action_name: "Type",
      hotkey: "alt+t",
    },
    {
      action_name: "Livelit",
      hotkey: "alt+l",
    },
    {
      action_name: "Toggle Statics",
      hotkey: "?",
    },
    {
      action_name: "Toggle Completion",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Whitespace",
      hotkey: "?",
    },
    {
      action_name: "Toggle Print Benchmarks",
      hotkey: "?",
    },
    {
      action_name: "Toggle Toggle Dynamics",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Elaboration",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Function Bodies",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Case Clauses",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show fixpoints",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Ascription Steps",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Lookup Steps",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Stepper Filters",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Hidden Steps",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Sidebar",
      hotkey: "?",
    },
    {
      action_name: "Toggle Show Docs Feedback",
      hotkey: "?",
    },
    {
      action_name: "TyDi Assistant",
      hotkey: "ctrl+/",
    },
    {
      action_name: "Export Scratch Slide",
      hotkey: "?",
    },
    {
      action_name: "Export For Init",
      hotkey: "?",
    },
    {
      action_name: "Export Submission",
      hotkey: "?",
    },
    {
      action_name: "Reparse Current Editor",
      hotkey: "?",
    },
    {
      action_name: "Run Benchmark",
      hotkey: "F7",
    },
    {
      action_name: "Introduce",
      hotkey: "ctrl+i",
    },
    {
      action_name: "Add New Buffer",
      hotkey: "?",
    },
    {
      action_name: "Rename Current Buffer",
      hotkey: "?",
    },
    {
      action_name: "Delete Current Buffer",
      hotkey: "?",
    },
    {
      action_name: "Export Exercise Module",
      hotkey: "?",
    },
    {
      action_name: "Export Transitionary Exercise Module",
      hotkey: "?",
    },
    {
      action_name: "Export Grading Exercise Module",
      hotkey: "?",
    },
  ];
};

let shortcut_theme = (shortcuts: list(shortcut)): Language.Exp.t => {
  open Language;
  open IdTagged.FreshGrammar.Exp;
  let lits =
    List.map(
      ({action_name, hotkey}) =>
        tuple([string(action_name), string(hotkey)]),
      shortcuts,
    );
  list_lit(lits);
};

let segment = {
  open Language;
  open Haz3lcore;
  let exp =
    IdTagged.FreshGrammar.(
      Exp.(
        let_(
          Pat.var("shortcuts"),
          shortcut_theme(DefaultConfiguration.shortcuts),
          var("shortcuts"),
        )
      )
    );

  ExpToSegment.exp_to_segment(
    ~settings=
      ExpToSegment.Settings.editable(~inline=false, ~multiline_lists=true),
    exp,
  )
  |> PersistentSegment.persist;
};

let perform_shortcut_side_effect = (value: Language.Exp.t): unit => {
  switch (value.term) {
  | ListLit(lits) =>
    let shortcuts =
      List.concat_map(
        x => {
          switch (Unboxing.unbox(Tuple(2), x)) {
          | Matches([x, y]) =>
            switch (
              Unboxing.unbox(Atom(String), x),
              Unboxing.unbox(Atom(String), y),
            ) {
            | (Matches(action_name), Matches(hotkey)) => [
                (action_name, hotkey),
              ]
            | _ => []
            }
          | _ => []
          }
        },
        lits,
      );
    List.iter(
      ((action_name, hotkey)) => {
        // Update the hotkey for this action via NinjaKeys
        NinjaKeys.update_shortcut_hotkey(
          action_name,
          hotkey,
        )
      },
      shortcuts,
    );
  | _ => ()
  };
};
