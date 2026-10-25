open Haz3lcore;
open Util;

/* View as agent: the main editor shows a request's view, read-only.
   Stacked: that request's program, its map (AgentLensMap) above exactly
   the agent's open entries as cells, the outline folded to the map.
   Sent: the view's text as sent. Trajectory: everything sent over the API
   up to it, or (Window) exactly what it carried. The user's program,
   view and folds stay as they are; leaving drops this. Live, it follows
   each new request. */

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Stacked
  | Sent
  | Trajectory
  /* Trajectory's exact-window toggle: only what the request carried */
  | Window;

/* the mode last chosen, for the session */
let last_mode = ref(Stacked);
/* Stacked's map panel shown, for the session */
let map_shown = ref(true);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  chat: Id.t,
  seq: int,
  live: bool,
  /* the requests the chat had, for the banner */
  count: int,
  program: Program.t,
  view: SlideView.t,
  folds: OutlineTree.folds,
  mode,
  /* the request's agentEditorView, as sent */
  sent: string,
  /* its map's summary: errors, tests */
  summary: string,
  /* in Trajectory mode */
  trajectory: option(Trajectory.t),
  /* Stacked: the map, above the cells */
  map: AgentLensMap.t,
  map_shown: bool,
};

/* the summary on a view's first line, after the map's name */
let summary_of_view = (view: string): string => {
  let first =
    switch (String.index_opt(view, '\n')) {
    | Some(i) => String.sub(view, 0, i)
    | None => view
    };
  let opening = CompositionView.marker ++ " ";
  let inner =
    String.starts_with(~prefix=opening, first)
    && String.ends_with(~suffix=" #", first)
      ? String.sub(
          first,
          String.length(opening),
          String.length(first) - String.length(opening) - 2,
        )
      : "";
  switch (StringUtil.plain_split(inner, {js| · |js})) {
  | [_name, ...parts] => String.concat({js| · |js}, parts)
  | [] => ""
  };
};

/* the outline folded as the map shows the program: modules open
   [module_depth] levels down, every other row with rows below closed */
let map_folds =
    (~module_depth: int, outline: OutlineTree.t): OutlineTree.folds => {
  let rec go = (~depth, prefix, ns: list(OutlineTree.node)) =>
    List.concat_map(
      ((n: OutlineTree.node, seg)) => {
        let path = prefix @ [seg];
        switch (n.o_children) {
        | [] => []
        | kids =>
          let open_ = n.o_kind == KModule && depth < module_depth;
          [(path, !open_), ...go(~depth=depth + 1, path, kids)];
        };
      },
      OutlineTree.segs(ns),
    );
  go(~depth=0, [], OutlineTree.rows(outline));
};

/* the record [seq] of [chat] (None: its latest) */
let record = (chat: Chat.Model.t, seq: option(int)): option(SentView.t) =>
  switch (seq) {
  | Some(k) => List.find_opt((r: SentView.t) => r.seq == k, chat.sent)
  | None => ListUtil.last_opt(chat.sent)
  };

/* [l] in [mode], its trajectory built when shown */
let with_mode = (chat: Chat.Model.t, mode: mode, l: t): t => {
  ...l,
  mode,
  trajectory:
    switch (mode, l.trajectory) {
    | (Trajectory | Window, Some(_)) when mode == l.mode => l.trajectory
    | (Trajectory, _) => Trajectory.build(chat, l.seq)
    | (Window, _) => Trajectory.window(chat, l.seq)
    | (Stacked | Sent, _) => None
    },
};

let build =
    (
      ~settings: Language.CoreSettings.t,
      ~view as view_settings: CompositionView.settings,
      ~live: bool,
      chat: Chat.Model.t,
      seq: option(int),
    )
    : option(t) =>
  Option.bind(record(chat, seq), (r: SentView.t) =>
    Option.map(
      text => {
        let root = r.root;
        let z = PersistentZipper.from_backup_text(text, ~root);
        let statics =
          CachedStatics.init(
            ~settings,
            ~is_dynamic_term=false,
            ~stitch=x => x,
            ~root,
            z,
          );
        let editor = CellEditor.Model.mk(Editor.Model.mk(z, ~root));
        let editor = {
          ...editor,
          editor: {
            ...editor.editor,
            statics,
          },
        };
        /* the open entries by their paths then: this text's ids are new */
        let outline = Program.outline(Whole(editor));
        let opened =
          List.filter_map(
            (e: AgentContext.Model.entry) =>
              switch (Outline.resolve(outline, e.path)) {
              | Ok(x) => Some(x.id)
              | Error(_) => None
              },
            r.opened,
          );
        let view =
          List.fold_left(
            (v, id) => SlideView.pin(~outline, id, v),
            SlideView.init,
            opened,
          );
        let (view, program) =
          SlideView.realize(
            ~info_map=statics.info_map,
            ~outline,
            view,
            Whole(editor),
          );
        let sent = SentView.editor_view(r.view);
        with_mode(
          chat,
          last_mode^,
          {
            chat: chat.id,
            seq: r.seq,
            live,
            count: List.length(chat.sent),
            program,
            view,
            folds:
              map_folds(~module_depth=view_settings.module_depth, outline),
            mode: Stacked,
            sent,
            summary: summary_of_view(sent),
            trajectory: None,
            map:
              AgentLensMap.build(
                ~view=view_settings,
                ~root,
                ~statics,
                ~opened,
                z,
              ),
            map_shown: map_shown^,
          },
        );
      },
      Chat.Utils.sent_text(chat, r),
    )
  );

/* live: the chat's latest request, when there is a newer one */
let follow = (~settings, ~view, chat: Chat.Model.t, lens: t): option(t) =>
  switch (ListUtil.last_opt(chat.sent)) {
  | Some(r) when lens.live && r.seq != lens.seq =>
    build(~settings, ~view, ~live=true, chat, None)
  | _ => None
  };
