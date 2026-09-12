/* The adapton event list of the Fumola instance the cursor is in.

   A `fumola $graphical as <instance> in … end` runs against a VM instance
   that keeps an adapton store, and the store records what it did: nodes and
   edges added, edges realigned or removed, and the Begin and End of every
   force. That list is the thing worth watching while editing an incremental
   program, and it is invisible from the program text.

   The events are fetched by running `prim "adaptonPeekHistory" ()` in the
   instance through the shim that is already there, rather than by adding an
   export to the wasm module. The boundary stays source text, as it does
   everywhere else here, and a change to what this panel shows costs a Hazel
   rebuild rather than a Rust one -- ~22 s against ~110 s, measured in
   docs/fumola-tiles-design.md.

   This is a main-thread view of an instance, which is why the Fumola runtime
   stays on the main thread; see src/language/fumola/README.md. */

open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Language;

/* Every `fumola <instance> … end` in a term, with the ids of its subtree.

   Collected with map_term rather than a hand-written walk, so a form added
   later is not silently missed. The ids are gathered the same way, since
   what the panel needs is "does this program contain the cursor". */
let instances = (e: Exp.t): list((string, list(Id.t))) => {
  let found = ref([]);
  let subtree_ids = (e: Exp.t): list(Id.t) => {
    let ids = ref([]);
    let _ =
      Exp.map_term(
        ~f_exp=
          (cont, e) => {
            ids := IdTagged.ids(e) @ ids^;
            cont(e);
          },
        e,
      );
    ids^;
  };
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (e.term) {
          | FumolaQuote(name, _, _) =>
            switch (Annotated.term_of(name)) {
            | Var(x) => found := [(x, subtree_ids(e)), ...found^]
            | _ => ()
            }
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  found^;
};

/* The instance to show.

   The cursor decides when it is inside one, innermost first: a Fumola program
   can embed Hazel through `hazel … end`, and that Hazel can hold another
   Fumola program.

   When the cursor says nothing -- which happens whenever it indicates
   whitespace, since secondary pieces are not in the term -- a single instance
   in the editor is still the one meant. Without that the panel emptied itself
   every time the caret crossed a space, which is most of the time while
   typing. With more than one it stays quiet rather than guessing. */
let instance_to_show = (~cursor_id: option(Id.t), e: Exp.t): option(string) => {
  let all = instances(e);
  let containing =
    switch (cursor_id) {
    | None => []
    | Some(id) => all |> List.filter(((_, ids)) => List.mem(id, ids))
    };
  switch (containing) {
  | [_, ..._] =>
    containing
    |> List.sort(((_, a), (_, b)) =>
         compare(List.length(a), List.length(b))
       )
    |> (
      fun
      | [(name, _), ..._] => Some(name)
      | [] => None
    )
  | [] =>
    switch (all) {
    | [(name, _)] => Some(name)
    | _ => None
    }
  };
};

/* One event, as the panel shows it. */
type event = {
  meta_time: string,
  kind: string,
  subject: string,
};

/* Fumola's event names, in the terms this project uses for them: a force has
   a Begin and an End, and an edge is realigned rather than "updated". See
   the repair terminology in the Fumola repo. */
let kind_of = (name: string): string =>
  switch (name) {
  | "addNode" => "node added"
  | "addEdge" => "edge added"
  | "updateEdge" => "edge realigned"
  | "removeEdge" => "edge removed"
  | "forceBegin" => "force begins"
  | "forceEnd" => "force ends"
  | other => other
  };

let view =
    (~globals as _: Globals.t, ~cursor: Cursor.cursor('update)): Node.t => {
  let section = (cls, header, body) =>
    div(
      ~attrs=[clss(["fumola-section", cls])],
      [div(~attrs=[clss(["fumola-section-header"])], [text(header)])]
      @ body,
    );

  /* Nothing in focus: say what a Fumola instance is and how to make one,
     rather than showing an empty table. */
  let how_to_make_one =
    section(
      "fumola-empty",
      "No Fumola instance in focus",
      [
        div(
          ~attrs=[clss(["fumola-blurb"])],
          [
            text(
              "A Fumola program runs against a VM instance, which keeps an "
              ++ "adapton store. Put the cursor inside one to watch what its "
              ++ "store does.",
            ),
          ],
        ),
        div(
          ~attrs=[clss(["fumola-example"])],
          [text("fumola $graphical as store in 0 := 41 end")],
        ),
        div(
          ~attrs=[clss(["fumola-blurb"])],
          [
            text(
              "The instance is named in the program, so its store survives "
              ++ "every edit that leaves the name alone. Only a $graphical "
              ++ "instance records events; a $simple one keeps no graph.",
            ),
          ],
        ),
      ],
    );

  let events_view = (instance: string, events: list(event)) =>
    section(
      "fumola-events",
      "Events of " ++ instance,
      switch (events) {
      | [] => [
          div(
            ~attrs=[clss(["fumola-blurb"])],
            [
              text(
                "This instance has recorded nothing yet. A $simple instance "
                ++ "never will; a $graphical one records as it forces.",
              ),
            ],
          ),
        ]
      | events => [
          div(
            ~attrs=[clss(["fumola-event-table"])],
            List.map(
              ev =>
                div(
                  ~attrs=[clss(["fumola-event"])],
                  [
                    div(
                      ~attrs=[clss(["fumola-event-time"])],
                      [text(ev.meta_time)],
                    ),
                    div(
                      ~attrs=[clss(["fumola-event-kind"])],
                      [text(ev.kind)],
                    ),
                    div(
                      ~attrs=[clss(["fumola-event-subject"])],
                      [text(ev.subject)],
                    ),
                  ],
                ),
              events,
            ),
          ),
        ]
      },
    );

  let body =
    switch (cursor.editor) {
    | Some(editor) =>
      let term =
        Haz3lcore.MakeTerm.from_zip_for_sem(
          editor.state.zipper,
          ~root=editor.root,
        ).
          term;
      let cursor_id = Option.map(Info.id_of, cursor.info);
      switch (instance_to_show(~cursor_id, term)) {
      | None => how_to_make_one
      | Some(instance) =>
        switch (FumolaEvents.of_instance(instance)) {
        | Error(message) =>
          section(
            "fumola-unavailable",
            "Events of " ++ instance,
            [div(~attrs=[clss(["fumola-blurb"])], [text(message)])],
          )
        | Ok(events) =>
          events_view(
            instance,
            List.map(
              ((meta_time, name, subject)) =>
                {
                  meta_time,
                  kind: kind_of(name),
                  subject,
                },
              events,
            ),
          )
        }
      };
    | None => how_to_make_one
    };

  div(~attrs=[clss(["sidebar-panel", "fumola-panel"])], [body]);
};
