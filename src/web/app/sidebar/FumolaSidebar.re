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
    /* The program between `in` and `end` is a Fumola term, not a Hazel one,
       so map_term never reaches its nodes. Without this the panel only
       recognised the cursor when it sat on the quote itself, and emptied
       itself the moment the caret moved onto anything inside -- which is
       what you do to point at a cell. The annotation the Fumola grammar
       carries is the same IdTag a Hazel term carries, so the ids are there
       to be had. */
    let fumola_ids = (body: FumolaTermBase.t) => {
      let _ =
        FumolaGrammar.map_annotation(
          (
            Fun.id,
            (ann: IdTagged.IdTag.t) => {
              ids := ann.ids @ ids^;
              ann;
            },
          ),
          body,
        );
      ();
    };
    let _ =
      Exp.map_term(
        ~f_exp=
          (cont, e) => {
            ids := IdTagged.ids(e) @ ids^;
            switch (e.term) {
            | FumolaQuote(_, _, body) => fumola_ids(body)
            | _ => ()
            };
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
    /* By instance name, not by block: a slide that runs five programs
       against `look` names one instance, and that is the one meant. */
    switch (List.sort_uniq(compare, List.map(fst, all))) {
    | [name] => Some(name)
    | _ => None
    }
  };
};

/* One event, as the panel shows it. */
type event = {
  meta_time: string,
  kind: string,
  subject: list(FumolaEvents.span),
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

let view = (~globals: Globals.t, ~cursor: Cursor.cursor('update)): Node.t => {
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

  /* Spelled once, because it is spelled in two places -- the events and the
     "runtime is not loaded" branch -- and a rename that reached only one of
     them would leave the error state naming the panel something the rest of
     the app no longer calls it. */
  let panel_title = (instance: string) => "Fumola VM instance " ++ instance;

  /* A revision is named by the space it belongs to and the moment it was
     born at, which is the pair the panel opens and closes. */
  let revision_key = (space: string, meta_time: string) =>
    space ++ "@" ++ meta_time;

  let is_open = (key: string) => List.mem(key, globals.fumola_open);

  let number = (s: string) =>
    switch (int_of_string_opt(s)) {
    | Some(n) => n
    | None => (-1)
    };

  /* The revision of [space] in force at [at]: the latest one born at or
     before that moment. Following a pointer out of the event list has to
     land on THAT node and not the same node at some other moment, and a
     pointer mentioned at moment 7 may well have been born at 3. */
  let revision_at =
      (~space: string, ~at: string, nodes: list(FumolaHistory.node_row))
      : option(string) => {
    let at = number(at);
    let candidates =
      List.filter(
        (row: FumolaHistory.node_row) =>
          row.space == space && number(row.meta_time) <= at,
        nodes,
      );
    let best =
      List.fold_left(
        (best, row: FumolaHistory.node_row) =>
          switch (best) {
          | Some(b: FumolaHistory.node_row)
              when number(b.meta_time) >= number(row.meta_time) =>
            Some(b)
          | _ => Some(row)
          },
        None,
        candidates,
      );
    switch (best) {
    | Some(row) => Some(revision_key(row.space, row.meta_time))
    /* Mentioned before it was made: nothing to land on. */
    | None => None
    };
  };

  /* Following a pointer means two things at once: open that revision alone,
     and show the view it lives in. */
  let follow = (key: string) =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(FumolaFocus(key)),
      globals.inject_global(
        Set(Sidebar(SwitchFumolaTab(SidebarModel.Settings.Nodes))),
      ),
    ]);

  /* The rows of the Events view. The section around it is the panel's, shared
     with the other two views, so this returns its contents rather than a
     section of its own. */
  let events_body =
      (~nodes: list(FumolaHistory.node_row), events: list(event)) =>
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
                    List.map(
                      fun
                      /* The symbol is the part that tells two events
                         apart, so it is the part that is set apart. */
                      | FumolaEvents.Sym(s) =>
                        /* The symbol is the pointer. Where the revision it
                           names can be found, it is also the way to it. */
                        switch (
                          revision_at(~space=s, ~at=ev.meta_time, nodes)
                        ) {
                        | Some(key) =>
                          span(
                            ~attrs=[
                              clss(["fumola-event-symbol", "fumola-pointer"]),
                              Attr.title(
                                "Show this node as it was at " ++ ev.meta_time,
                              ),
                              Attr.on_click(_ => follow(key)),
                            ],
                            [text(s)],
                          )
                        | None =>
                          span(
                            ~attrs=[clss(["fumola-event-symbol"])],
                            [text(s)],
                          )
                        }
                      | FumolaEvents.Plain(s) => text(s),
                      ev.subject,
                    ),
                  ),
                ],
              ),
            events,
          ),
        ),
      ]
    };

  /* The three views are three indices on one history, so the strip is a view
     choice and not three panels. Markup and CSS are the problems panel's
     `.toggle-option`, which is unscoped and already reads as a tab strip. */
  let tab_strip = (current: SidebarModel.Settings.fumola_tab) => {
    let tab = (tab, label) =>
      span(
        ~attrs=[
          clss(["toggle-option"] @ (current == tab ? ["active"] : [])),
          Attr.on_click(_ =>
            current == tab
              ? Virtual_dom.Vdom.Effect.Ignore
              : globals.inject_global(Set(Sidebar(SwitchFumolaTab(tab))))
          ),
        ],
        [text(label)],
      );
    div(
      ~attrs=[clss(["problem-view-toggle", "fumola-tabs"])],
      [
        tab(SidebarModel.Settings.Events, "Events"),
        tab(SidebarModel.Settings.Nodes, "Nodes"),
        tab(SidebarModel.Settings.Edges, "Edges"),
      ],
    );
  };

  /* A row's value is a Hazel value of a declared Hazel type, so it is shown
     the way a probe shows one: the probe's own pretty printer, and the code
     view the explanation and debug panels already put code in the sidebar
     with. Nothing here formats anything.

     `pretty_seg_of_value` rather than ExpToSegment directly, so the panel and
     the probes on the Node info slide are the same renderer on the same kind
     of value and not two things that resemble each other. The utility it
     takes is a free top-level value, not something a projector owns, which is
     what lets a sidebar call it at all.

     The code keeps its own font size: Code.view sizes an empty-hole
     decoration from globals.font_metrics, which is the editor's, so shrinking
     the text here would mis-size any hole a value contains. The row scrolls
     instead. */
  let value_view = (value: Exp.t) =>
    div(
      ~attrs=[clss(["fumola-row-value"])],
      [
        CodeViewable.view_segment(
          ~globals,
          Haz3lcore.ProbeProj.pretty_seg_of_value(
            Haz3lcore.ProjectorInfo.utility,
            ~width=50,
            value,
          ),
        ),
      ],
    );

  let rows_view = (~name: string, ~empty: string, rows: list(Node.t)) =>
    switch (rows) {
    | [] => [div(~attrs=[clss(["fumola-blurb"])], [text(empty)])]
    | rows => [
        div(~attrs=[clss(["fumola-rows", "fumola-" ++ name])], rows),
      ]
    };

  let nodes_view = (nodes: list(FumolaHistory.node_row)) =>
    rows_view(
      ~name="nodes",
      ~empty="This instance has made no nodes yet.",
      List.map(
        (row: FumolaHistory.node_row) => {
          let key = revision_key(row.space, row.meta_time);
          let open_ = is_open(key);
          div(
            ~attrs=[clss(["fumola-row"] @ (open_ ? ["open"] : []))],
            [
              /* Closed, a row is its name and its moment and nothing else,
                 which is what makes a list of revisions readable. Open, the
                 value follows. */
              div(
                ~attrs=[
                  clss(["fumola-row-key"]),
                  Attr.title(
                    open_ ? "Collapse this revision" : "Show this revision",
                  ),
                  Attr.on_click(_ =>
                    globals.inject_global(FumolaToggleOpen(key))
                  ),
                ],
                [
                  span(
                    ~attrs=[clss(["fumola-caret"])],
                    [text(open_ ? "\xE2\x8C\x84" : "\xE2\x80\xBA")],
                  ),
                  span(
                    ~attrs=[clss(["fumola-event-symbol"])],
                    [text(row.space)],
                  ),
                  text(" at " ++ row.meta_time),
                ],
              ),
              ...open_ ? [value_view(row.value)] : [],
            ],
          );
        },
        nodes,
      ),
    );

  let edges_view = (edges: list(FumolaHistory.edge_row)) =>
    rows_view(
      ~name="edges",
      ~empty="This instance has made no edges yet.",
      List.map(
        (row: FumolaHistory.edge_row) => {
          let (from_, to_) = row.meta_times;
          div(
            ~attrs=[clss(["fumola-row"])],
            [
              div(
                ~attrs=[clss(["fumola-row-key"])],
                [
                  text(row.edge_id ++ ": "),
                  span(
                    ~attrs=[clss(["fumola-event-symbol"])],
                    [text(row.source)],
                  ),
                  text(" to "),
                  span(
                    ~attrs=[clss(["fumola-event-symbol"])],
                    [text(row.target)],
                  ),
                  text(" spanning " ++ from_ ++ "-" ++ to_),
                ],
              ),
              value_view(row.value),
            ],
          );
        },
        edges,
      ),
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
        switch (FumolaHistory.of_instance(instance)) {
        | Error(message) =>
          section(
            "fumola-unavailable",
            panel_title(instance),
            [div(~attrs=[clss(["fumola-blurb"])], [text(message)])],
          )
        | Ok(history) =>
          let tab = globals.settings.sidebar.fumola_tab;
          section(
            "fumola-events",
            panel_title(instance),
            [tab_strip(tab)]
            @ (
              switch (tab) {
              | Nodes => nodes_view(history.nodes)
              | Edges => edges_view(history.edges)
              | Events =>
                events_body(
                  ~nodes=history.nodes,
                  List.map(
                    ((meta_time, name, subject)) =>
                      {
                        meta_time,
                        kind: kind_of(name),
                        subject,
                      },
                    history.events,
                  ),
                )
              }
            ),
          );
        }
      };
    | None => how_to_make_one
    };

  div(~attrs=[clss(["sidebar-panel", "fumola-panel"])], [body]);
};
