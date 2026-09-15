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
            /* Name and mode as well as the body. All three are Fumola
               terms and all three are inside the form a reader is pointing
               at -- and the name is the likeliest thing to point at, since
               it is what the panel is titled after. Collecting only the
               body meant that clicking the very word the panel names
               emptied it. */
            | FumolaQuote(name, mode, body) =>
              fumola_ids(name);
              fumola_ids(mode);
              fumola_ids(body);
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
  /* The editor's own doing rather than the program's: true when the edge
     this event names is sourced in the editor. An event that names no edge
     and no node is neither, and is never dimmed. */
  editor: bool,
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
      [div(~attrs=[clss(["fumola-section-header"])], header)] @ body,
    );

  /* Nothing in focus: say what a Fumola instance is and how to make one,
     rather than showing an empty table. */
  let how_to_make_one =
    section(
      "fumola-empty",
      [text("No Fumola instance in focus")],
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
  let panel_name = "Fumola VM instance ";

  /* The reset strip offers G and S, and the header decodes the one the
     instance is actually in. Both spelled out would be a legend, which a
     reader needs once; one spelled out is a statement about this instance,
     which is the half that changes and the half worth the room. */
  let mode_key = (mode: Language.FumolaRun.mode) =>
    switch (mode) {
    | Language.FumolaRun.Simple => "S = Simple"
    | Language.FumolaRun.Graphical => "G = Graphical"
    };

  /* The mode is an option rather than a mode because there are runtimes that
     cannot be asked: Hazel pins no version, and `adaptonMode` arrived in
     Adapton/fumola#134. A header that guessed would be worse than one that
     says nothing, since the guess would be wrong in exactly the case a reader
     is looking -- just after a reset into the other mode. */
  let panel_title =
      (~mode: option(Language.FumolaRun.mode)=?, instance: string) =>
    [
      /* The label is what every instance has in common, the name is what
         tells one from another -- so the weight goes on the name. The header
         is bold as a whole, and this turns the constant half back down. */
      span(~attrs=[clss(["fumola-panel-label"])], [text(panel_name)]),
      span(~attrs=[clss(["fumola-panel-name"])], [text(instance)]),
    ]
    @ (
      switch (mode) {
      | None => []
      | Some(mode) => [
          span(
            ~attrs=[clss(["fumola-mode-key"])],
            [text(" (" ++ mode_key(mode) ++ ")")],
          ),
        ]
      }
    );

  /* A revision is named by the space it belongs to and the moment it was
     born at, which is the pair the panel opens and closes. */
  let revision_key = (space: string, meta_time: string) =>
    "n:" ++ space ++ "@" ++ meta_time;

  /* Edges are opened by the same set, so the two namespaces must not meet:
     an edge id and a space name are both just digits often enough. */
  let edge_key = (edge_id: string) => "e:" ++ edge_id;

  let is_open = (key: string) => List.mem(key, globals.fumola_open);

  /* The row a pointer was just followed to gets scrolled to. Following a
     pointer switches views as well as opening a row, so without this the
     reader arrives at the top of a list they did not ask to be at the top
     of, with what they clicked somewhere below the fold. */
  let scroll_to = (key: string): list(Attr.t) =>
    globals.fumola_focused == Some(key)
      ? [ScrollIntoView.attr(~name="scroll-followed-fumola-row", key)] : [];

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
  /* An edge named inside a node's value: show it where edges live, alone. */
  let follow_edge = (edge_id: string) =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(FumolaFocus(edge_key(edge_id))),
      globals.inject_global(
        Set(Sidebar(SwitchFumolaTab(SidebarModel.Settings.Edges))),
      ),
    ]);

  let follow = (key: string) =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(FumolaFocus(key)),
      globals.inject_global(
        Set(Sidebar(SwitchFumolaTab(SidebarModel.Settings.Nodes))),
      ),
    ]);

  /* A moment, as a way to what happened at it.

     The third entity the panel names, after pointers and edge ids, and the
     one that had no way on: a node says which moment it was born at and an
     edge says which pair it spans, and neither led anywhere. What a moment
     leads to is the events at it, since the Events view is already the list
     ordered by moment.

     An event's OWN moment stays plain. Following it would scroll to the row
     you clicked, which is the same reason a node does not link to itself. */
  let moment_key = (at: string) => "m:" ++ at;

  let follow_moment = (at: string) =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(FumolaFocus(moment_key(at))),
      globals.inject_global(
        Set(Sidebar(SwitchFumolaTab(SidebarModel.Settings.Events))),
      ),
    ]);

  /* `at` reads as a moment rather than as a number, so it is worth saying
     which it is: the title is what tells a reader this leads to the events
     and not to another row of the list they are already in. */
  let moment_link = (~stop: bool, at: string) =>
    span(
      ~attrs=[
        clss(["fumola-moment", "fumola-pointer"]),
        Attr.title("Show what happened at " ++ at),
        Attr.on_click(_ =>
          stop
            ? Virtual_dom.Vdom.Effect.Many([
                Virtual_dom.Vdom.Effect.Stop_propagation,
                follow_moment(at),
              ])
            : follow_moment(at)
        ),
      ],
      [text(at)],
    );

  /* A node id, in place, as a way to the node it names.

     The Events view has had this since it had symbols; the Edges view showed
     the same two node ids and offered no way on, so the link went one way
     only. `at` is the moment to read the node AS OF -- an edge names a node
     whose own revision may be older than the edge, and revision_at answers
     with the newest revision at or before it, which is the one this edge saw.

     A click here must not also work the row's own toggle, which is the div
     this span sits inside. Stop_propagation is what keeps following a pointer
     from collapsing the record you were reading it out of.

     The editor's root names no node, so it stays plain rather than pretending
     to lead somewhere. */
  let node_link =
      (
        ~nodes: list(FumolaHistory.node_row),
        ~space: string,
        ~at: string,
        label: string,
      ) =>
    switch (revision_at(~space, ~at, nodes)) {
    | Some(key) =>
      span(
        ~attrs=[
          clss(["fumola-event-symbol", "fumola-pointer"]),
          Attr.title("Show this node as it was at " ++ at),
          Attr.on_click(_ =>
            Virtual_dom.Vdom.Effect.Many([
              Virtual_dom.Vdom.Effect.Stop_propagation,
              follow(key),
            ])
          ),
        ],
        [text(label)],
      )
    | None => span(~attrs=[clss(["fumola-event-symbol"])], [text(label)])
    };

  /* Show, Dim or Hide, applied to one row. Hiding drops it; dimming keeps it
     and says so, which is the point of having three settings rather than a
     checkbox: the editor's own traffic is noise most of the time and the
     thing you are looking for some of the time. */
  let editor_mode = globals.settings.sidebar.fumola_editor;

  let with_editor = (~editor: bool, row: unit => Node.t): list(Node.t) =>
    switch (editor ? editor_mode : Show) {
    | Hide => []
    | Show
    | Dim => [row()]
    };

  let dim_class = (~editor: bool) =>
    editor && editor_mode == SidebarModel.Settings.Dim ? ["fumola-dim"] : [];

  /* Whether a row NAMES something, which is what the bold in it means.

     A symbol is set in bold because it is the part that tells two rows apart:
     it says which cell the row was about. A row carrying only a verb and an
     edge id -- `edge added edgeId 1002` -- is the editor going about its
     business, and is what the Show / Dim / Hide control is for.

     So the editor's own traffic is judged twice over. A row of the editor's
     that names nothing is traffic, and Dim quietens it and Hide drops it. A
     row of the editor's that names a cell is how that cell came to exist --
     `node added Symbol myThunk` is the thunk being made -- and that is
     content whoever caused it. Dim leaves it at full strength and Hide keeps
     it, which is the point of Hide: what remains is the story of what the
     store holds, with the bookkeeping taken out and the naming left in.

     An edge id is a link rather than a name, so it does not count. */
  let names_a_cell = (subject: list(FumolaEvents.span)) =>
    List.exists(
      fun
      | FumolaEvents.Sym(_) => true
      | FumolaEvents.Edge(_)
      | FumolaEvents.Plain(_) => false,
      subject,
    );

  /* The editor's, for the purpose of hiding and dimming. */
  let is_traffic = (ev: event) => ev.editor && !names_a_cell(ev.subject);

  /* Hiding the editor empties a list that is not empty, and the empty blurb
     then said this instance had made none of whatever it was -- false, and
     false in the direction that reads as a broken panel rather than a
     working filter. So a list with rows in it says where they went. */
  let all_editor = (~total: int, ~one: string, ~many: string) =>
    total == 1
      ? "The editor is hidden, and the one " ++ one ++ " here is its own."
      : "The editor is hidden, and all "
        ++ string_of_int(total)
        ++ " "
        ++ many
        ++ " here are its own.";

  /* Hazel's passes, in words rather than in the symbol the marker carries.
     The symbols come from FumolaRun.pass_symbol; an unknown one is shown as
     itself, since a pass this panel has not heard of is still worth seeing. */
  let pass_label = (name: string): string =>
    switch (name) {
    | "eval" => "evaluation"
    | "step" => "a stepper step"
    | "decompose" => "the stepper, finding the redex"
    | "valueCheck" => "the value check"
    | other => other
    };

  /* Rows, with a header wherever the pass changes.

     A header is emitted only above a row that is actually shown, so hiding
     the editor cannot leave a heading with nothing under it. The pass of a
     hidden row is still read, so a boundary that falls inside a hidden run
     is not lost -- the next visible row carries it.

     Rows before the first marker get no header. That is every row an
     instance recorded before this build of Hazel, and every row a program
     put there itself; none of them belong to a pass this panel can name. */
  let by_pass:
    'a.
    (
      ~passes: list((int, string)),
      ~at: 'a => string,
      'a => list(Node.t),
      list('a)
    ) =>
    list(Node.t)
   =
    (~passes, ~at, row, items) => {
      let (out, _) =
        List.fold_left(
          ((acc, shown), item) => {
            let rendered = row(item);
            let pass = FumolaHistory.pass_at(passes, at(item));
            switch (rendered, pass) {
            | ([], _) => (acc, shown)
            | (_, Some(p)) when Some(p) != shown => (
                acc
                @ [
                  div(
                    ~attrs=[clss(["fumola-pass"])],
                    [text(pass_label(p))],
                  ),
                ]
                @ rendered,
                Some(p),
              )
            | _ => (acc @ rendered, shown)
            };
          },
          ([], None),
          items,
        );
      out;
    };

  /* The rows of the Events view. The section around it is the panel's, shared
     with the other two views, so this returns its contents rather than a
     section of its own. */
  let events_body =
      (
        ~passes: list((int, string)),
        ~nodes: list(FumolaHistory.node_row),
        events: list(event),
      ) =>
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
    | events =>
      /* Several events share a moment, so one of them has to be what a link
         to that moment lands on. The first, since the list is in order --
         and only that one carries the scroll, or they would all answer the
         same focus and fight over it. */
      let numbered = List.mapi((i, ev) => (i, ev), events);
      let first_at: Hashtbl.t(string, int) = Hashtbl.create(64);
      List.iter(
        ((i, ev: event)) =>
          if (!Hashtbl.mem(first_at, ev.meta_time)) {
            Hashtbl.add(first_at, ev.meta_time, i);
          },
        numbered,
      );
      let anchors_moment = ((i, ev: event)) =>
        Hashtbl.find_opt(first_at, ev.meta_time) == Some(i);
      let rows =
        by_pass(
          ~passes,
          ~at=((_, ev): (int, event)) => ev.meta_time,
          ((_, ev) as item) =>
            with_editor(~editor=is_traffic(ev), () =>
              div(
                ~attrs=[
                  clss(
                    ["fumola-event"] @ dim_class(~editor=is_traffic(ev)),
                  ),
                  ...anchors_moment(item)
                       ? scroll_to(moment_key(ev.meta_time)) : [],
                ],
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
                      /* An edge id names a row in the Edges view, so it
                         leads there, the same way a symbol leads to a node. */
                      | FumolaEvents.Edge(id) =>
                        span(
                          ~attrs=[
                            clss(["fumola-pointer"]),
                            Attr.title("Show " ++ id),
                            Attr.on_click(_ => follow_edge(id)),
                          ],
                          [text(id)],
                        )
                      | FumolaEvents.Plain(s) => text(s),
                      ev.subject,
                    ),
                  ),
                ],
              )
            ),
          numbered,
        );
      /* An empty table renders as nothing at all, which said even less than
         the wrong blurb did on the other two views. */
      switch (rows) {
      | [] => [
          div(
            ~attrs=[clss(["fumola-blurb"])],
            [
              text(
                all_editor(
                  ~total=List.length(events),
                  ~one="event",
                  ~many="events",
                ),
              ),
            ],
          ),
        ]
      | rows => [div(~attrs=[clss(["fumola-event-table"])], rows)]
      };
    };

  /* The three views are three indices on one history, so the strip is a view
     choice and not three panels. Markup and CSS are the problems panel's
     `.toggle-option`, which is unscoped and already reads as a tab strip. */
  /* Empty the store and build it again from the program. Beside the views
     rather than in them: it is about the instance, not about what is being
     looked at. */
  /* One letter each, because this sits on the same line as the tab strip and
     the two words did not leave it room. What the letters stand for is in the
     header, which names the mode the instance is in and so decodes one of
     them against the thing it is about; the tooltip is left to say what
     pressing one would do, which is the part no label can carry. */
  /* The one that keeps the instance where it is comes in bold. Both buttons
     empty the store; only one of them leaves the mode alone, and which one
     that is depends on the instance rather than on the strip. Nothing is bold
     when the mode could not be asked, which is the same silence the header
     keeps. */
  let reset_button =
      (~mode: option(Language.FumolaRun.mode)=?, instance: string) => {
    let into = (into_mode, label, what) =>
      span(
        ~attrs=[
          clss(
            ["fumola-reset"]
            @ (mode == Some(into_mode) ? ["fumola-reset-current"] : []),
          ),
          Attr.title(
            "Empty this instance and run the program again, "
            ++ what
            ++ ". Bindings from other cells that share the instance do not "
            ++ "come back. The mode stays as you asked until the program's "
            ++ "own mode is edited.",
          ),
          Attr.on_click(_ =>
            globals.inject_global(FumolaReset(instance, into_mode))
          ),
        ],
        [text(label)],
      );
    div(
      ~attrs=[clss(["fumola-resets"])],
      [
        span(~attrs=[clss(["fumola-strip-label"])], [text("reset:")]),
        /* Graphical first: it is Fumola's default and the mode that records,
           so it is the one a reader of this panel is usually coming back to.
           Simple is the narrowing. */
        into(Language.FumolaRun.Graphical, "G", "recording as it forces"),
        into(Language.FumolaRun.Simple, "S", "keeping no graph"),
      ],
    );
  };

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

  /* The editor's own edges, nodes and events. On all three views: an edge is
     the editor's when its source is, and a node when its own space is
     Here. */
  let editor_strip = () => {
    let option = (mode, label, title) =>
      span(
        ~attrs=[
          clss(["toggle-option"] @ (editor_mode == mode ? ["active"] : [])),
          Attr.title(title),
          Attr.on_click(_ =>
            editor_mode == mode
              ? Virtual_dom.Vdom.Effect.Ignore
              : globals.inject_global(
                  Set(Sidebar(SwitchFumolaEditor(mode))),
                )
          ),
        ],
        [text(label)],
      );
    div(
      ~attrs=[clss(["fumola-editor-strip"])],
      [
        span(~attrs=[clss(["fumola-strip-label"])], [text("editor")]),
        div(
          ~attrs=[clss(["problem-view-toggle"])],
          [
            option(
              SidebarModel.Settings.Show,
              "Show",
              "Show the editor's own nodes, edges and events",
            ),
            option(SidebarModel.Settings.Dim, "Dim", "Keep them, faintly"),
            option(SidebarModel.Settings.Hide, "Hide", "Leave them out"),
          ],
        ),
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

  /* What the store holds and the panel could not show.

     A row that will not translate is dropped, which is right -- one bad row
     should not cost the list -- but dropping it in silence is not. A shorter
     list reads as a shorter history, and the reader has no way to tell the
     difference. That is how a compound name went missing from the library's
     panel without anyone noticing.

     Reasons are deduplicated: fifty rows failing for one reason is one thing
     to say, said once. */
  let missed_note = (~one: string, ~many: string, missed: list(string)) =>
    switch (missed) {
    | [] => []
    | missed =>
      let n = List.length(missed);
      [
        div(
          ~attrs=[clss(["fumola-missed"])],
          [
            text(
              string_of_int(n)
              ++ " "
              ++ (n == 1 ? one : many)
              ++ " in the store could not be shown here: "
              ++ String.concat("; ", List.sort_uniq(compare, missed)),
            ),
          ],
        ),
      ];
    };

  let rows_view =
      (
        ~name: string,
        ~empty: string,
        ~total: int,
        ~one: string,
        ~many: string,
        ~missed: list(string),
        rows: list(Node.t),
      ) =>
    (
      switch (rows) {
      /* Nothing shown and something missed is not an empty instance, and
         saying so would be the same lie in a louder voice. */
      | [] when missed != [] => []
      | [] => [
          div(
            ~attrs=[clss(["fumola-blurb"])],
            [text(total == 0 ? empty : all_editor(~total, ~one, ~many))],
          ),
        ]
      | rows => [
          div(~attrs=[clss(["fumola-rows", "fumola-" ++ name])], rows),
        ]
      }
    )
    @ missed_note(~one, ~many, missed);

  let nodes_view =
      (
        ~passes: list((int, string)),
        ~missed: list(string),
        nodes: list(FumolaHistory.node_row),
      ) =>
    rows_view(
      ~missed,
      ~name="nodes",
      ~empty="This instance has made no nodes yet.",
      ~total=List.length(nodes),
      ~one="node",
      ~many="nodes",
      by_pass(
        ~passes,
        ~at=(row: FumolaHistory.node_row) => row.meta_time,
        (row: FumolaHistory.node_row) => {
          let key = revision_key(row.space, row.meta_time);
          let open_ = is_open(key);
          with_editor(~editor=row.editor, () =>
            div(
              ~attrs=[
                clss(
                  ["fumola-row"]
                  @ (open_ ? ["open"] : [])
                  @ (row.editor ? ["fumola-editor"] : [])
                  @ dim_class(~editor=row.editor),
                ),
                ...scroll_to(key),
              ],
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
                    text(" at "),
                    moment_link(~stop=true, row.meta_time),
                  ],
                ),
              ]
              @ (
                open_
                  ? [value_view(row.value)]
                    @ (
                      row.trace == []
                        ? []
                        : [
                          /* The trace is inside the value too, but a rendered
                             value is code and code is not clickable. These are
                             the same edges, offered as ways on. */
                          div(
                            ~attrs=[clss(["fumola-row-links"])],
                            [text("edges: ")]
                            @ (
                              row.trace
                              |> List.map(id =>
                                   span(
                                     ~attrs=[
                                       clss(["fumola-pointer"]),
                                       Attr.title("Show " ++ id),
                                       Attr.on_click(_ => follow_edge(id)),
                                     ],
                                     [text(id)],
                                   )
                                 )
                              /* A literal separator rather than a flex gap:
                                 the chips must not run together even where
                                 this row's stylesheet has not arrived. */
                              |> List.mapi((i, node) =>
                                   i == 0 ? [node] : [text(", "), node]
                                 )
                              |> List.flatten
                            ),
                          ),
                        ]
                    )
                  : []
              ),
            )
          );
        },
        nodes,
      ),
    );

  let edges_view =
      (
        ~passes: list((int, string)),
        ~nodes: list(FumolaHistory.node_row),
        ~missed: list(string),
        edges: list(FumolaHistory.edge_row),
      ) =>
    rows_view(
      ~missed,
      ~name="edges",
      ~empty="This instance has made no edges yet.",
      ~total=List.length(edges),
      ~one="edge",
      ~many="edges",
      by_pass(
        ~passes,
        ~at=(row: FumolaHistory.edge_row) => fst(row.meta_times),
        (row: FumolaHistory.edge_row) => {
          let (from_, to_) = row.meta_times;
          let key = edge_key(row.edge_id);
          let open_ = is_open(key);
          with_editor(~editor=row.editor, () =>
            div(
              ~attrs=[
                clss(
                  ["fumola-row"]
                  @ (open_ ? ["open"] : [])
                  /* Marked whatever the editor setting is: the point of the
                     colour is to say whose edge this is before anyone reads
                     the record inside it. */
                  @ (row.editor ? ["fumola-editor"] : [])
                  @ dim_class(~editor=row.editor),
                ),
                ...scroll_to(key),
              ],
              [
                div(
                  ~attrs=[
                    clss(["fumola-row-key"]),
                    Attr.title(
                      open_ ? "Collapse this edge" : "Show this edge",
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
                    text(row.edge_id ++ ": "),
                    node_link(
                      ~nodes,
                      ~space=row.source,
                      ~at=from_,
                      row.source,
                    ),
                    text(" to "),
                    node_link(~nodes, ~space=row.target, ~at=to_, row.target),
                    text(" spanning "),
                    moment_link(~stop=true, from_),
                    text("-"),
                    moment_link(~stop=true, to_),
                  ],
                ),
                ...open_ ? [value_view(row.value)] : [],
              ],
            )
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
            /* No mode beside the name here: the branch a reader reaches
               when the runtime could not be asked anything is not the place
               to claim to know what it answered. */
            panel_title(instance),
            [div(~attrs=[clss(["fumola-blurb"])], [text(message)])],
          )
        | Ok(history) =>
          let tab = globals.settings.sidebar.fumola_tab;
          /* Asked once and read twice -- the header spells it out, the strip
             marks the button that would keep it -- so that the two cannot
             disagree about the same instance in the same render. */
          let mode = Language.FumolaRun.mode_of_instance(instance);
          section(
            "fumola-events",
            panel_title(~mode?, instance),
            [
              div(
                ~attrs=[clss(["fumola-controls"])],
                [tab_strip(tab), reset_button(~mode?, instance)],
              ),
            ]
            @ [editor_strip()]
            @ (
              switch (tab) {
              | Nodes =>
                nodes_view(
                  ~passes=history.passes,
                  ~missed=history.nodes_missed,
                  history.nodes,
                )
              | Edges =>
                edges_view(
                  ~passes=history.passes,
                  ~nodes=history.nodes,
                  ~missed=history.edges_missed,
                  history.edges,
                )
              | Events =>
                events_body(
                  ~passes=history.passes,
                  ~nodes=history.nodes,
                  {
                    /* Which edges are the editor's, by id, and which nodes
                       the editor made, by space. Built once per render
                       rather than searched per event.

                       An event naming an edge is judged by that edge. An
                       event naming a node -- added, signaling, repaired --
                       is the editor's on either of two counts: the node is
                       the editor's own, or something the editor did points
                       AT it. The second is what a cell needs: it signals
                       because someone put into it, so a cell the editor put
                       into signals on the editor's account. Without it the
                       Hide setting left a list of signalling about nodes
                       whose every edge it had just hidden. */
                    let editor_edges = Hashtbl.create(64);
                    let editor_nodes = Hashtbl.create(64);
                    List.iter(
                      (row: FumolaHistory.node_row) =>
                        if (row.editor) {
                          Hashtbl.replace(editor_nodes, row.space, true);
                        },
                      history.nodes,
                    );
                    List.iter(
                      (row: FumolaHistory.edge_row) => {
                        Hashtbl.replace(
                          editor_edges,
                          row.edge_id,
                          row.editor,
                        );
                        if (row.editor) {
                          Hashtbl.replace(editor_nodes, row.target, true);
                        };
                      },
                      history.edges,
                    );
                    let known = (table, key) =>
                      switch (Hashtbl.find_opt(table, key)) {
                      | Some(p) => p
                      | None => false
                      };
                    List.map(
                      ((meta_time, name, subject, edge, node)) =>
                        {
                          meta_time,
                          kind: kind_of(name),
                          subject,
                          editor:
                            switch (edge, node) {
                            | (Some(id), _) => known(editor_edges, id)
                            | (None, Some(space)) =>
                              known(editor_nodes, space)
                            | (None, None) => false
                            },
                        },
                      history.events,
                    );
                  },
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
