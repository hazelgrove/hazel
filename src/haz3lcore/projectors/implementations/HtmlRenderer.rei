/* HtmlRenderer - renders a probe sample whose value is HTML, or an MVU app,
 * as the thing it describes rather than as abbreviated syntax. */
[@deriving (show({with_path: false}), sexp, yojson)]
type m = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = unit;
/* What the sample turned out to be: static HTML, or a running app. */
[@deriving (show({with_path: false}), sexp, yojson)]
type v =
  | Static(Language.Exp.t)
  | App(Language.Exp.t);

/* The binding name the evaluator recorded on a handler value, when it has
   one. `let bump = fun ...` yields a Fun tagged "bump"; an inline lambda
   yields None. */
let handler_name: Language.Exp.t => option(string);

/* The syntax spliced in front of the probed html when a handler fires:
   `f(html)`, using `handler_name` when the probe site binds it and the
   handler value itself otherwise. */
let handler_ref: (ProjectorBase.info, Language.Exp.t) => Language.Exp.t;

/* The pure core of the above: what to commit for a handler, given a test for
   which names are in scope. Exposed so the commit shape can be tested without
   an editor. */
let handler_syntax: (~bound: string => bool, Language.Exp.t) => Language.Exp.t;

/* `base |> handler`, the shape a press commits. */
let spliced: (~handler: Language.Exp.t, Language.Exp.t) => Language.Exp.t;

/* The segment a press commits: `base |> handler`, lifted through
   info.utility.lift_syntax. None when the lift fails. */
let commit_syntax:
  (ProjectorBase.info, Language.Exp.t) => option(Base.segment);

include
  RichProbe.RichProbe with
    type model = m and type action = a and type value = v;
