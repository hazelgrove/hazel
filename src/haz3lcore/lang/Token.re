include Language.Token;

/* Placement-aware projector invocation syntax. Language.Token knows nothing
   about Placement (it lives in ProjectorCore, above the language library),
   so the placement-carrying half of the invoke vocabulary is defined here
   and shadows the placement-free versions included above. */

/* A docked projector's invoke token carries its placement, so that a
   projector survives a round-trip through text (a slide stores both a
   zipper and its backup_text; if placement were only in the zipper the two
   would disagree). `_` is safe as a separator: no kind name contains one. */
let projector_invoke_sidebar = "_sidebar";

/* Split a trailing placement suffix off the invoke body. Placement is a
   SUFFIX and the option split below reads the FIRST `_`, so placement must
   come off first: "probe_table_sidebar" ==> ("probe_table", Sidebar). */
let split_invoke_placement = (body: t): (t, ProjectorCore.Placement.t) =>
  String.ends_with(~suffix=projector_invoke_sidebar, body)
    ? (
      String.sub(
        body,
        0,
        String.length(body) - String.length(projector_invoke_sidebar),
      ),
      ProjectorCore.Placement.Sidebar,
    )
    : (body, ProjectorCore.Placement.Inline);

/* Invoke body and placement, split apart. The body keeps any option suffix.
   "^^slider_sidebar" ==> Some(("slider", Sidebar))
   "^^probe_table"    ==> Some(("probe_table", Inline)) */
let of_projector_invoke_parts =
    (input: t): option((t, ProjectorCore.Placement.t)) =>
  Option.map(split_invoke_placement, of_projector_invoke(input));

/* The option a trigger token selects, if it carries one.
   "^^probe_table" ==> Some("table")
   "^^probe"       ==> None
   "let"           ==> None   (not a trigger at all) */
let of_projector_invoke_opt = (input: t): option(t) =>
  Option.bind(of_projector_invoke_parts(input), ((body, _)) =>
    snd(split_invoke_opt(body))
  );

/* The kind name a trigger token names, with option and placement stripped.
   "^^probe_table" ==> Some("probe")
   "^^probe"       ==> Some("probe")
   "let"           ==> None   (not a trigger at all) */
let of_projector_invoke_base = (input: t): option(t) =>
  Option.map(
    ((body, _)) => fst(split_invoke_opt(body)),
    of_projector_invoke_parts(input),
  );

/* Does this token name a known projector kind? Checks the whole body modulo
   placement, so a trigger carrying an option fails here even though its base
   names a kind — Triggers.is_refractor_trigger is the option-aware
   counterpart, which is why Triggers.expand_projector tries the refractor
   arm first.
     "^^probe" / "^^slider" / "^^slider_sidebar" ==> true
     "^^probe_table"                             ==> false  (no such kind)
     "^^p" / "let" / "^^"                        ==> false */
let is_projector_invoke = (str: t): bool =>
  switch (of_projector_invoke_parts(str)) {
  | Some((name, _)) => ProjectorCore.Kind.is_name(name)
  | None => false
  };

/* The trigger token naming a kind, plus any option and placement suffixes.
   Suffix ORDER is load-bearing: the option is read from the first `_` of the
   placement-stripped body, so placement goes last.
     Probe                                 ==> "^^probe"
     Probe ~opt="table"                    ==> "^^probe_table"
     Slider ~placement=Sidebar             ==> "^^slider_sidebar" */
let mk_projector_invoke =
    (
      ~opt: option(t)=?,
      ~placement=ProjectorCore.Placement.Inline,
      kind: ProjectorCore.Kind.t,
    )
    : string =>
  append(projector_invoke_prefix, ProjectorCore.Kind.name(kind))
  ++ (
    switch (opt) {
    | Some(opt) => "_" ++ opt
    | None => ""
    }
  )
  ++ (
    switch (placement) {
    | Inline => ""
    | Sidebar => projector_invoke_sidebar
    }
  );
