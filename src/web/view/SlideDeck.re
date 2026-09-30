open Util;
open Haz3lcore;

/* A mode's slides: which one is current, and managing them. */

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Persist = ScratchPersist;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SwitchSlide(int)
    /* deferred slide hydration: SwitchSlide shows a loading frame first */
    | HydrateCurrent
    | ResetCurrent
    | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImportScratchpad(option(string))
    | Export
    | Encode
    | AddSlide
    | AddDrvSlide
    | RenameSlide
    | DeleteSlide;
};

open Action;

let integrate_share =
    (~settings: Language.CoreSettings.t, model: Model.t): Model.t => {
  let share_name =
    switch (JsUtil.QueryParams.get_param("name")) {
    | None => "Unknown Share"
    | Some(name) => name
    };
  switch (JsUtil.QueryParams.get_param("share")) {
  | None => model
  | Some(data) =>
    let shared_text = data |> StringUtil.decompress;
    /* zipper: "" = the intentional text path (share links carry only
       text); a non-empty sentinel would take the sexp arm and print the
       stale-serialization warning on every share-link load */
    let shared: PersistentZipper.t = {
      zipper: "",
      backup_text: shared_text,
    };
    let shared: CellEditor.Model.persistent = {
      editor: {
        root: Exp,
        zipper: shared,
      },
      result: EvalResult.Model.init |> EvalResult.Model.persist,
    };
    let new_sp =
      Scratchpad.mk_code(
        ~name=share_name,
        ~editor=CellEditor.Model.unpersist(~settings, shared),
        (),
      );
    Model.{
      current: List.length(model.scratchpads),
      scratchpads: model.scratchpads @ [new_sp],
    };
  };
};

let export_scratch_slide = (model: Model.t): unit => {
  let scratchpad = List.nth(model.scratchpads, model.current);
  switch (scratchpad.kind) {
  | Code({program, _}) =>
    let persistent = CellEditor.Model.persist(Program.whole(program));
    let data =
      persistent
      |> CellEditor.Model.sexp_of_persistent
      |> Sexplib.Sexp.to_string;
    let current_name = scratchpad.name;
    let filename = current_name |> StringUtil.sanitize_filename;
    JsUtil.download_string_file(
      ~filename,
      ~content_type="text/plain",
      ~contents=data,
    );
  | Drv(_) => ()
  };
};

let encode_scratch_slide = (model: Model.t): unit => {
  let scratchpad = List.nth(model.scratchpads, model.current);
  JsUtil.QueryParams.set_param("name", scratchpad.name);
  switch (scratchpad.kind) {
  | Code({program, _}) =>
    let c = Program.whole(program) |> CellEditor.Model.to_string;
    JsUtil.QueryParams.set_param("share", StringUtil.compress(c));
  | Drv(_) => ()
  };
};
let rec prompt_slide_name =
        (
          ~error: option(string)=?,
          ~existing_scratchpads: Seq.t(string),
          default: string,
        )
        : Option.t(string) => {
  let new_name =
    JsUtil.prompt(
      (
        switch (error) {
        | Some(e) => e ++ "\n"
        | None => ""
        }
      )
      ++ "Enter new slide name:",
      default,
    );

  if (existing_scratchpads |> Seq.exists(name => Some(name) == new_name)) {
    prompt_slide_name(
      ~error="Slide name already exists. Please choose a different name.",
      ~existing_scratchpads,
      Option.value(~default, new_name),
    );
  } else {
    new_name;
  };
};

/* Kind of scratchpad to create. Code is the default ("Scratchpad N");
   Drv creates a blank derivation slide with the same auto-naming scheme. */
[@deriving (show({with_path: false}), sexp, yojson)]
type new_slide_kind =
  | NewCode
  | NewDrv;

let add_new_slide =
    (
      ~kind: new_slide_kind,
      ~settings: Language.CoreSettings.t,
      model: Model.t,
      is_documentation: bool,
    )
    : Model.t => {
  let blank = name =>
    switch (kind) {
    | NewCode => Scratchpad.blank_code(name)
    | NewDrv => Scratchpad.blank_drv(~settings, name)
    };
  let add_empty_slide = (name): Model.t => {
    current: List.length(model.scratchpads),
    scratchpads: model.scratchpads @ [blank(name)],
  };
  switch (is_documentation) {
  | false =>
    let prefix =
      switch (kind) {
      | NewCode => "Scratchpad"
      | NewDrv => "Derivation"
      };
    let used_numbers =
      model.scratchpads
      |> List.filter_map((s: Scratchpad.t) => {
           switch (String.split_on_char(' ', s.name)) {
           | [p, num] when p == prefix => int_of_string_opt(num)
           | _ => None
           }
         });
    let unused_ids =
      Seq.filter(i => !List.mem(i, used_numbers), Seq.ints(1));
    let new_number =
      Seq.uncons(unused_ids)
      |> Option.get  // This is safe because unused_ids is infinite
      |> fst;

    add_empty_slide(prefix ++ " " ++ string_of_int(new_number));
  | true =>
    let new_name =
      prompt_slide_name(
        ~existing_scratchpads=
          model.scratchpads
          |> List.to_seq
          |> Seq.map((s: Scratchpad.t) => s.name),
        "New Slide Name",
      );
    switch (new_name) {
    | None => model // Prompt cancelled so no new scratchpad created
    | Some(name) => add_empty_slide(name)
    };
  };
};

let update =
    (
      ~settings: Settings.t,
      ~schedule_action: Action.t => unit,
      ~is_documentation: bool,
      action: Action.t,
      model: Model.t,
    )
    : Updated.t(Model.t) => {
  Updated.(
    switch (action) {
    | SwitchSlide(i) =>
      WorkerClient.cancel();
      /* hydration is slow on large slides, so paint a loading frame
         first; schedule_action drains before the next render, so defer
         via a real timer */
      ignore(
        Js_of_ocaml.Dom_html.window##setTimeout(
          Js_of_ocaml.Js.wrap_callback(() => schedule_action(HydrateCurrent)),
          30.,
        ),
      );
      {
        ...model,
        current: i,
      }
      |> Updated.return(~historic=false);
    | HydrateCurrent =>
      let model =
        Persist.hydrate_current(
          ~settings=settings.core,
          is_documentation ? "doc" : "scratch",
          model,
        );
      model |> Updated.return(~historic=false);
    | AddSlide =>
      WorkerClient.cancel();
      Updated.return(
        add_new_slide(
          ~kind=NewCode,
          ~settings=settings.core,
          model,
          is_documentation,
        ),
      );
    | AddDrvSlide =>
      WorkerClient.cancel();
      Updated.return(
        add_new_slide(
          ~kind=NewDrv,
          ~settings=settings.core,
          model,
          is_documentation,
        ),
      );
    | RenameSlide =>
      let current = List.nth(model.scratchpads, model.current);
      let new_name =
        prompt_slide_name(
          ~existing_scratchpads=
            model.scratchpads
            |> List.to_seq
            |> Seq.zip(Seq.ints(0))
            |> Seq.filter(((idx, _)) => idx != model.current)
            |> Seq.map(snd)
            |> Seq.map((s: Scratchpad.t) => s.name),
          current.name,
        );

      switch (new_name) {
      | None => model |> return_quiet
      | Some(new_name) =>
        Persist.rename_slide(
          is_documentation ? "doc" : "scratch",
          current.name,
          new_name,
        );
        let new_sp =
          ListUtil.put_nth(
            model.current,
            {
              ...current,
              name: new_name,
            },
            model.scratchpads,
          );
        Updated.return({
          ...model,
          scratchpads: new_sp,
        });
      };
    | DeleteSlide =>
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to delete this slide? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        WorkerClient.cancel();
        Persist.forget_slide(
          is_documentation ? "doc" : "scratch",
          List.nth(model.scratchpads, model.current).name,
        );
        let new_sp =
          ListUtil.remove_nth(model.current, model.scratchpads)
          |> Option.value(~default=model.scratchpads);

        let m: Model.t =
          List.is_empty(new_sp)
            ? add_new_slide(
                ~kind=NewCode,
                ~settings=settings.core,
                {
                  ...model,
                  scratchpads: [],
                },
                is_documentation,
              )
            : Persist.hydrate_current(
                ~settings=settings.core,
                is_documentation ? "doc" : "scratch",
                {
                  scratchpads: new_sp,
                  current: max(model.current - 1, 0),
                },
              );
        Updated.return(m);
      } else {
        model |> return_quiet;
      };

    | ResetCurrent =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({agent, _}) =>
        let source =
          switch (is_documentation) {
          | false =>
            CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp))
            |> CellEditor.Model.persist
          | true => Init.default_documentation_slide_name(scratchpad.name)
          };
        let* data = source |> CellEditor.Model.unpersist |> Updated.return;
        {
          ...model,
          scratchpads:
            ListUtil.put_nth(
              model.current,
              {
                ...scratchpad,
                kind:
                  Code({
                    program: Whole(data),
                    view: SlideView.init,
                    agent,
                  }),
              },
              model.scratchpads,
            ),
        };
      | Drv(_) =>
        let new_sp =
          Scratchpad.blank_drv(~settings=settings.core, scratchpad.name);
        {
          ...model,
          scratchpads:
            ListUtil.put_nth(model.current, new_sp, model.scratchpads),
        }
        |> Updated.return;
      };
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      model |> return_quiet;
    | FinishImportScratchpad(data) =>
      // reset file input so same file can be re-imported if desired
      JsUtil.reset_file_input("import-scratchpad");
      switch (data) {
      | None => model |> return_quiet
      | Some(data) =>
        let scratchpad = List.nth(model.scratchpads, model.current);
        switch (scratchpad.kind) {
        | Code({agent, _}) =>
          let new_data =
            data
            |> Sexplib.Sexp.of_string
            |> CellEditor.Model.persistent_of_sexp
            |> CellEditor.Model.unpersist(~settings=settings.core);

          let scratchpads =
            ListUtil.put_nth(
              model.current,
              {
                ...scratchpad,
                kind:
                  Code({
                    program: Whole(new_data),
                    view: SlideView.init,
                    agent,
                  }),
              },
              model.scratchpads,
            );
          {
            ...model,
            scratchpads,
          }
          |> Updated.return;
        | Drv(_) => model |> return_quiet
        };
      };
    | Export =>
      export_scratch_slide(model);
      model |> Updated.return_quiet;
    | Encode =>
      encode_scratch_slide(model);
      model |> Updated.return_quiet;
    }
  );
};
