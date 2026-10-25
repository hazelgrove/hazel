open Haz3lcore;
open Util;

/* View as agent: the main editor shows a request's view, read-only: that
   request's program, exactly the agent's open entries as cells, the
   outline folded to its map. The user's program, view and folds stay as
   they are; leaving drops this. Live, it follows each new request. */

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
  /* the request's map, shown when it had nothing open */
  map: string,
};

/* the map in a context as sent: its view up to the open entries */
let map_of_view = (view: string): string => {
  let after = (s, mark) =>
    switch (StringUtil.plain_search(mark, s, 0)) {
    | i when i >= 0 =>
      let j = i + String.length(mark);
      String.sub(s, j, String.length(s) - j);
    | _ => s
    };
  let before = (s, mark) =>
    switch (StringUtil.plain_search(mark, s, 0)) {
    | i when i >= 0 => String.sub(s, 0, i)
    | _ => s
    };
  before(
    before(after(view, "<agentEditorView>\n```\n"), "\n```"),
    "\n\n" ++ CompositionView.marker ++ " open: ",
  );
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

let build =
    (
      ~settings: Language.CoreSettings.t,
      ~module_depth: int,
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
        let view =
          List.fold_left(
            (v, e: AgentContext.Model.entry) =>
              switch (Outline.resolve(outline, e.path)) {
              | Ok(x) => SlideView.pin(~outline, x.id, v)
              | Error(_) => v
              },
            SlideView.init,
            r.opened,
          );
        let (view, program) =
          SlideView.realize(
            ~info_map=statics.info_map,
            ~outline,
            view,
            Whole(editor),
          );
        {
          chat: chat.id,
          seq: r.seq,
          live,
          count: List.length(chat.sent),
          program,
          view,
          folds: map_folds(~module_depth, outline),
          map: map_of_view(r.view),
        };
      },
      Chat.Utils.sent_text(chat, r),
    )
  );

/* live: the chat's latest request, when there is a newer one */
let follow =
    (~settings, ~module_depth, chat: Chat.Model.t, lens: t): option(t) =>
  switch (ListUtil.last_opt(chat.sent)) {
  | Some(r) when lens.live && r.seq != lens.seq =>
    build(~settings, ~module_depth, ~live=true, chat, None)
  | _ => None
  };
