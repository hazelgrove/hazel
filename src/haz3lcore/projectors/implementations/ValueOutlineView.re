open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;
open ValueOutline;

/* Draws a ValueOutline layout. A head is the part of a row a click acts
   on: a summary opens its group, an opening or closing delimiter (with its
   label) folds it, a count shows more items. The caret beside a head shows
   only while that head is hovered (proj-probe.css). */

let closest = (el: Js.t(Dom_html.element), sel: string) =>
  Js.Opt.to_option(
    Js.Unsafe.meth_call(
      el,
      "closest",
      [|Js.Unsafe.inject(Js.string(sel))|],
    ),
  );

/* Once an open has re-rendered, scroll the drawer so the opened group
   shows, without scrolling its head out of view. */
let reveal_later = (head: Js.t(Dom_html.element), key: string): unit =>
  switch (closest(head, ".value"), closest(head, ".below-wrapper")) {
  | (Some(chip), Some(wrapper)) =>
    let reveal = () =>
      switch (
        Js.Opt.to_option(
          chip##querySelector(
            Js.string(Printf.sprintf("[data-fold=\"%s\"]", key)),
          ),
        )
      ) {
      | Some(el) =>
        let r = el##getBoundingClientRect;
        let c = wrapper##getBoundingClientRect;
        let below = r##.bottom -. c##.bottom;
        if (below > 0.) {
          Util.JsUtil.adjust_scroll(
            wrapper,
            min(below, max(0., r##.top -. c##.top)),
          );
        };
      | None => ()
      };
    /* the open renders on the next frame; measure on the one after */
    let frame = f =>
      Dom_html.window##requestAnimationFrame(Js.wrap_callback(_ => f()))
      |> ignore;
    frame(() => frame(reveal));
  | _ => ()
  };

let tok = (s: string): Node.t =>
  span(~attrs=[Attr.classes(["token", "Exp"])], [text(s)]);

let head =
    (~kind: string, ~caret: option(string), ~on_click, parts: list(Node.t)) =>
  span(
    ~attrs=[
      Attr.classes(["fold-head", kind]),
      Attr.on_click(e => on_click(e)),
      /* a quick second click must not reach the sample's dbl-click
         (which toggles One/Many), nor select a word */
      Attr.on_double_click(_ => Effect.Stop_propagation),
      Attr.on_mousedown(evt => {
        let detail: int = Js.Unsafe.get(evt, "detail");
        detail > 1 ? Effect.Prevent_default : Effect.Ignore;
      }),
    ],
    (
      switch (caret) {
      | Some(c) => [
          span(~attrs=[Attr.classes(["fold-caret"])], [text(c)]),
        ]
      | None => []
      }
    )
    @ parts,
  );

let row = (nodes: list(Node.t)) =>
  div(~attrs=[Attr.classes(["fold-row"])], nodes);

let rec view =
        (
          ~on_fold: (string, bool) => Effect.t(unit),
          ~on_show: (string, int) => Effect.t(unit),
          ~view_seg: Segment.t => Node.t,
          o: out,
        )
        : Node.t => {
  let recur = view(~on_fold, ~on_show, ~view_seg);
  let label_tok = l =>
    Option.to_list(Option.map(l => tok(prefix(Some(l))), l));
  let suffix_tok = s => s == "" ? [] : [tok(s)];
  let more = (key, hidden, next) =>
    head(
      ~kind="more",
      ~caret=None,
      ~on_click=_ => on_show(key, next),
      [tok(count_note(hidden))],
    );
  let open_ = (key, e: Js.t(Dom_html.mouseEvent)) => {
    /* read now: currentTarget is unset once dispatch ends */
    let head = Js.Opt.to_option(e##.currentTarget);
    Effect.Many([
      on_fold(key, true),
      Effect.of_sync_fun(
        () => Option.iter(h => reveal_later(h, key), head),
        (),
      ),
    ]);
  };
  switch (o) {
  | Row(label, Text(t), suffix) =>
    row(label_tok(label) @ [tok(Lazy.force(t))] @ suffix_tok(suffix))
  | Row(label, Summary(key, t), suffix) =>
    row(
      [
        head(
          ~kind="summary",
          ~caret=Some({js|▸|js}),
          ~on_click=open_(key),
          label_tok(label) @ [tok(Lazy.force(t))],
        ),
      ]
      @ suffix_tok(suffix),
    )
  | Row(label, Pretty(key, seg, _), suffix) =>
    /* an unlabeled root leaf is what today's drawer shows: nothing to fold */
    let fold =
      label == None
        ? []
        : [
          head(
            ~kind="opener",
            ~caret=Some({js|▾|js}),
            ~on_click=_ => on_fold(key, false),
            label_tok(label),
          ),
        ];
    div(
      ~attrs=[Attr.classes(["fold-row"]), Attr.create("data-fold", key)],
      fold
      @ [div(~attrs=[Attr.classes(["fold-pretty"])], [view_seg(seg)])]
      @ suffix_tok(suffix),
    );
  | Row(_, More(key, hidden, next), _) => row([more(key, hidden, next)])
  | Block(b) =>
    let fold = _ => on_fold(b.key, false);
    div(
      ~attrs=[
        Attr.classes(["fold-block"]),
        Attr.create("data-fold", b.key),
      ],
      [
        row([
          head(
            ~kind="opener",
            ~caret=Some({js|▾|js}),
            ~on_click=fold,
            label_tok(b.label) @ [tok(b.opener)],
          ),
        ]),
        div(~attrs=[Attr.classes(["fold-kids"])], List.map(recur, b.kids)),
        row(
          [
            head(
              ~kind="closer",
              ~caret=None,
              ~on_click=fold,
              [tok(b.closer)],
            ),
          ]
          @ suffix_tok(b.suffix),
        ),
      ],
    );
  | Packed(rows) =>
    div(
      ~attrs=[Attr.classes(["fold-packed"])],
      List.map(
        toks =>
          row(
            List.mapi(
              (i, t) =>
                (i > 0 ? [tok(" ")] : [])
                @ [
                  switch (t) {
                  | Item(s) => tok(s)
                  | PackedMore(key, hidden, next) => more(key, hidden, next)
                  },
                ],
              toks,
            )
            |> List.concat,
          ),
        rows,
      ),
    )
  };
};

/* The root has class `code` so ProbeProj.pos_rel_to_target measures
   shift-drags from it, as it does for printed samples. */
let mk = (~on_fold, ~on_show, ~view_seg, o: out): Node.t =>
  div(
    ~attrs=[Attr.classes(["code", "fold-outline"])],
    [
      div(
        ~attrs=[Attr.classes(["code-text"])],
        [view(~on_fold, ~on_show, ~view_seg, o)],
      ),
    ],
  );
