open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let put: string => Piece.t = Piece.mk_mono(Exp);

let get_opt = (piece: Piece.t): option(string) =>
  piece |> Piece.of_mono |> Option.map(Form.parse_livelit);

let get = (piece: Piece.t): string =>
  switch (get_opt(piece)) {
  | None => failwith("ERROR: Slider: not integer literal")
  | Some(s) => s
  };

let getApArgs = (term: UExp.term): list(UExp.t) =>
  switch (term) {
  | UExp.Parens(args) =>
    switch (args.term) {
    | Tuple(lst) => lst
    | _ => [args]
    }
  | _ => failwith("Not an Ap term")
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = p => true;
  let can_focus = false;
  let placeholder = (_, _) => Inline(10);
  let update = (model, _) => model;
  let view =
      (_, ~info, ~local as _, ~parent: external_action => Ui_effect.t(unit)) => {
    let args: list(UExp.t) =
      switch (info.ci) {
      | Some(InfoExp(exp)) =>
        print_endline("Livelit term: " ++ UExp.show(exp.term));
        let (term, _) = UExp.unwrap(exp.term);
        switch (term) {
        | Parens(args) =>
          switch (args.term) {
          | Ap(_dir, _f, args) =>
            let (term, _) = UExp.unwrap(args);
            switch (term) {
            | Tuple(lst) => lst
            | _ => [args]
            };
          | _ => failwith("Not an Ap term")
          }
        | _ => failwith("Not an Ap term")
        };
      | _ => failwith("Not an Ap term")
      };

    print_endline(
      "Livelit args: "
      ++ (args |> List.map(UExp.show) |> String.concat(", ")),
    );

    // print_endline("Livelit syntax: " ++ debug);
    switch (info.ci) {
    | _ =>
      Node.div(
        ~attrs=[Attr.class_("livelit")],
        [Node.text("I am a livelit")],
      )
    };
  };
  let focus = _ => ();
};
