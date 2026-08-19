open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* HtmlRenderer - renders a probe sample as the thing its value describes:
 * static HTML as DOM, or an (init, update, view, subs) 4-tuple as a running,
 * clickable app.
 *
 * This is the value-side counterpart to HTMLProj, and it mirrors that
 * projector's two modes. HTMLProj renders the projector's *syntax*, which is
 * what lets it commit edits back: in syntax-commit mode the expression is the
 * model. That only works when the expression is itself HTML or a literal
 * 4-tuple. An expression that *computes* one — `chart(sales)`, or a function
 * returning an app — is an Ap, with nothing renderable in its syntax at all.
 *
 * A probe sample is exactly the missing piece: it carries the evaluated value.
 * So the result of a computation can be an interactive app, played with
 * through the probe's sample interface.
 *
 * The two modes differ only in where a dispatched msg is committed:
 *
 * - Static: nowhere. There is no syntax to splice a transformed value back
 *   into, so handlers are inert (HazelDOM.Value).
 * - App: the web-side AppStore, keyed by the probe's id, exactly as an inline
 *   app is keyed by its projector's id. */

[@deriving (show({with_path: false}), sexp, yojson)]
type v =
  | Static(Exp.t)
  | App(Exp.t);

/* No UI state: the view is a pure function of the sampled value. */
[@deriving (show({with_path: false}), sexp, yojson)]
type m = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

/* An app is checked for first: a 4-tuple is never HTML, but the check is the
   more specific one and reads better in that order. `is_html` derives its
   constructor set from BuiltinsADT.HTML, so it can't drift from the ADT, and
   both helpers strip the Asc/Closure/Parens wrappers evaluated values arrive
   in. */
let parse = (_sort: Sort.t, exp: Exp.t): option(value) =>
  if (MvuShape.looks_like_mvu_app(exp)) {
    Some(App(exp));
  } else if (MvuShape.is_html(exp)) {
    Some(Static(exp));
  } else {
    None;
  };

let empty = ();
let init = (_: value) => empty;
let update = (model: model, _: action) => model;

let html_icon =
  SvgUtil.simple_icon(
    ~view="0 0 8 8",
    [
      /* an angle-bracket pair: < > */
      "m 2.72 2.02 -1.86 1.72 a 0.265 0.265 0 0 0 0 0.39 l 1.86 1.72 0.36 -0.39 -1.65 -1.53 1.65 -1.52 z",
      "m 5.28 2.02 -0.36 0.39 1.65 1.52 -1.65 1.53 0.36 0.39 1.86 -1.72 a 0.265 0.265 0 0 0 0 -0.39 z",
    ],
  );

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["html-badge"]),
      Attr.title("Click to view as rendered HTML"),
    ],
    [html_icon],
  );

/* What to splice in front of the probed html when a handler fires.
 *
 * A msg arrives as a VALUE — the evaluated handler — so splicing it inlines
 * the whole function body at the use site, and does so again on every click.
 * The evaluator records the binding name on a `Fun`, so `let bump = fun ...`
 * gives back a `Fun` tagged "bump" and we can splice the NAME instead. That
 * keeps the edit short, and keeps it bound to the definition: changing
 * `bump` later changes what every committed call does.
 *
 * The name is only used when the probe site actually binds it. A handler
 * defined in an inner scope and returned outward carries a name that means
 * nothing here, and splicing it would write an unbound variable into the
 * program; inlining the value is correct in that case, just verbose. */
let handler_name = (msg: Exp.t): option(string) =>
  switch (MvuShape.strip_wrappers(msg).term) {
  | Fun(_, _, _, name) => name
  | _ => None
  };

let handler_ref = (info: info, msg: Exp.t): Exp.t => {
  let bound = name =>
    switch (info.statics) {
    | Some(i) => Ctx.lookup_var(Info.ctx_of(i), name) != None
    | None => false
    };
  switch (handler_name(msg)) {
  | Some(name) when bound(name) => IdTagged.FreshGrammar.Exp.var(name)
  | _ => msg
  };
};

/* Commit a handler by REWRITING THE SOURCE, never by evaluating.
 *
 * The probed expression `H` becomes `f(H)`, and the program's own pipeline
 * evaluates it — with statics, elaboration, and the scope `H` already sits
 * in. That is what makes this work where evaluating the transform here did
 * not: a probe has no elaborated form of its syntax and no environment to
 * resolve `f` in, so the application went through unreduced and a stuck term
 * got spliced over the program. Handing the application back as syntax puts
 * the evaluation somewhere that has all three.
 *
 * It also means the rewrite lands on the DEFINITION. If the html sits inside
 * a function, every call renders the transformed version, not just the
 * invocation that was clicked. */
let commit_syntax = (info: info, msg: Exp.t, src: Exp.t): Base.segment =>
  Exp(IdTagged.FreshGrammar.Exp.ap(Forward, handler_ref(info, msg), src))
  |> info.utility.term_to_seg(~inline=true);

let message = (text: string): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["html-probe-message"])],
    [Node.text(text)],
  );

let render =
    (
      ~info: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model as _: model,
      ~local as _: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort as _: Sort.t,
      _: unit,
    )
    : Node.t => {
  /* Subterms HazelDOM can't interpret fall back to a read-only code view,
     so a malformed node shows what it actually is instead of vanishing. */
  let view_term = (term: Exp.t) =>
    Exp(term)
    |> info.utility.term_to_seg(~inline=true)
    |> view_seg(Sort.Exp);
  let content =
    switch (value) {
    /* Handlers rewrite the source: the msg is applied to the probed syntax
       and the application is spliced back, so the program re-evaluates it
       in its own scope. Read-only only when there is no syntax to rewrite. */
    | Static(html) =>
      let seed: HazelDOM.t = {
        inject: msg =>
          switch (info.syntax |> info.utility.seg_to_term) {
          | Some(Exp(src)) =>
            parent(SetSyntax(commit_syntax(info, msg, src)))
          | _ => Effect.Ignore
          },
        view_term,
        commit: HazelDOM.Syntax,
      };
      HazelDOM.go(seed, html);
    /* The store owns the model; hand it the evaluated app and render whatever
       html it currently holds. `ensure_app` is a no-op once the entry is
       bound to this value, which is what keeps this render-time call cheap.
       Keying on the probe's id means one live app per probe: focusing a
       sample whose value is a different app rebinds the entry, so the app on
       screen is always the one for the sample in focus.

       No checkpoint is passed. A projector can write one back quietly
       (local_quiet), but a rich probe has no quiet channel, and spending an
       undo entry every two seconds to let an app survive reload is the worse
       trade. A probed app restarts from `init` on reload. */
    | App(app) =>
      AppBridge.ensure_app^(info.id, app, None);
      switch (AppBridge.current_html^(info.id)) {
      | None => message("starting app…")
      | Some(html) =>
        let seed: HazelDOM.t = {
          inject: msg => AppBridge.dispatch^(info.id, msg),
          view_term,
          commit: HazelDOM.State,
        };
        HazelDOM.go(seed, html);
      };
    };
  Node.div(~attrs=[Attr.classes(["html-probe"])], [content]);
};
