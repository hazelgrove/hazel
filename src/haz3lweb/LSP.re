open Js_of_ocaml;
open Util;
open Haz3lcore;

let unk = Typ.Unknown(Internal);

[@deriving (show({with_path: false}), sexp, yojson)]
type constrain =
  | Grammar
  | Context
  | Types;

[@deriving (show({with_path: false}), sexp, yojson)]
type arguments = {
  debug: bool,
  constrain,
  new_token: option(string),
  program: string,
  ctx: Ctx.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type completability =
  | Completeable(Id.t, string)
  | Inert(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type left_shape =
  | LeftConvex
  | LeftConcave;

[@deriving (show({with_path: false}), sexp, yojson)]
type maybe_padded =
  | Just
  | SpacePadded;

[@deriving (show({with_path: false}), sexp, yojson)]
type left_neighbor_info = option((maybe_padded, left_shape, completability));

[@deriving (show({with_path: false}), sexp, yojson)]
type right_neighbor_info =
  | Nothing
  | ConvexHole(Id.t)
  | StringLit(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type incompleteness =
  | Grammatical
  | Contextual
  | Fine;

[@deriving (show({with_path: false}), sexp, yojson)]
type infodump = {
  ci: Info.t,
  bidi_ci: Info.t,
  bidi_ctx_cls: Term.Cls.t,
  bidi_ctx_expected_ty: Typ.t,
  bidi_parent_ci: option(Info.t),
  bidi_parent_ctx_cls: option(Term.Cls.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type generation_options =
  | OnlyCompletion(infodump, string)
  | OnlyCompletionString(string)
  | NewRightConvex(infodump)
  | CompletionOrNewRightConvex(infodump, string, infodump) //TODO: betterize
  | NewRightConcave(infodump)
  | CompletionOrNewRightConcave(infodump, string);

[@deriving (show({with_path: false}), sexp, yojson)]
type pre_grammar = {
  completions: list(string),
  new_tokens: list(string),
};

let default_settings = {
  debug: false,
  constrain: Types,
  program: "",
  ctx: [],
  new_token: None,
};

let show_settings = (s: arguments): string =>
  Printf.sprintf(
    "Debug: %b, Constrain: %s",
    s.debug,
    switch (s.constrain) {
    | Grammar => "Grammar"
    | Context => "Context"
    | Types => "Types"
    },
  );

let get_info_map = (~init_ctx=[], z: Zipper.t) =>
  z
  |> MakeTerm.from_zip_for_sem
  |> fst
  |> Interface.Statics.mk_map_ctx(CoreSettings.on, init_ctx);

let _is_file = (path: string): bool => {
  Js.Unsafe.(
    [|inject(Js.string(path))|]
    |> fun_call(get(js_expr("require('fs')"), "existsSync"))
    |> Js.to_bool
  );
};

let string_of_file = (~encoding: string="utf8", path: string): string => {
  Js.Unsafe.(
    Array.map(inject, [|Js.string(path), Js.string(encoding)|])
    |> fun_call(get(js_expr("require('fs')"), "readFileSync"))
    |> Js.to_string
  );
};

let get_args = (): list(string) => {
  Js.Unsafe.(
    switch (
      get(js_expr("require('process')"), "argv")
      |> Js.to_array
      |> Array.map(Js.to_string)
      |> Array.to_list
    ) {
    | [_, _, ...args] => args
    | _ => failwith("LSP: EXN: Args malformed")
    }
  );
};

let hash_of_string = (str: string): string =>
  str |> Digest.string |> Digest.to_hex;

let tempDir = "tmp"; // Name of the temporary directory

/* Ensure the temporary directory exists */
let ensureTempDir = () =>
  if (!Sys.file_exists(tempDir)) {
    Sys.mkdir(
      tempDir,
      0o755 // Uses Unix permissions, adjust as necessary
    );
  };

/* Construct a path within the temporary directory for a given filename */
let tempFilePath = (filename: string): string => {
  ensureTempDir();
  tempDir ++ "/" ++ filename; // Simple path concatenation
};

let serialize_ctx = (ctx: Ctx.t, filename: string) => {
  let tempFilename = tempFilePath(filename);
  let channel = open_out_bin(tempFilename);
  Marshal.to_channel(channel, ctx, []);
  close_out(channel);
};

let deserialize_ctx = (filename: string): Ctx.t => {
  let tempFilename = tempFilePath(filename);
  let channel = open_in_bin(tempFilename);
  let ctx: Ctx.t = Marshal.from_channel(channel);
  close_in(channel);
  ctx;
};

let serialize_zipper = (z: Zipper.t, filename: string) => {
  let tempFilename = tempFilePath(filename);
  let channel = open_out_bin(tempFilename);
  Marshal.to_channel(channel, z, []);
  close_out(channel);
};

let deserialize_zipper = (filename: string): Zipper.t => {
  let tempFilename = tempFilePath(filename);
  let channel = open_in_bin(tempFilename);
  let z: Zipper.t = Marshal.from_channel(channel);
  close_in(channel);
  z;
};

let serialized_filename_z = program_str =>
  hash_of_string(program_str) ++ ".zipper.serialized";

let process_zipper = (~db=ignore, program_str: string): Zipper.t => {
  let serialized_filename = serialized_filename_z(program_str);
  if (_is_file(serialized_filename)) {
    db("LSP: Get Zipper: Found serialized zipper, deserializing");
    deserialize_zipper(serialized_filename);
  } else {
    db("LSP: Get Zipper: No serialized zipper, processing string");
    let z =
      OptUtil.get_or_fail(
        "LSP: EXN: Couldn't parse string",
        Printer.zipper_of_string(program_str),
      );
    serialize_zipper(z, serialized_filename);
    z;
  };
};

let get_zipper = (~db, settings) => {
  let program_str = settings.program;
  switch (settings.new_token) {
  | None =>
    db("LSP: Recieved string: " ++ program_str);
    let z = process_zipper(~db, program_str);
    db("LSP: String parsed successfully to zipper");
    z;
  | Some(new_token) =>
    db("LSP: New token mode: " ++ program_str ++ new_token);
    let base_z = process_zipper(~db, program_str);
    let new_z =
      OptUtil.get_or_fail(
        "LSP: EXN: New token mode: Couldn't paste into zipper",
        Printer.paste_into_zip(base_z, new_token),
      );
    serialize_zipper(new_z, serialized_filename_z(program_str ++ new_token));
    new_z;
  };
};

let pp_inner = (~db=ignore, ~init_ctx, str: string): Ctx.t => {
  let sym = 666;
  let str = str ++ "\n" ++ string_of_int(sym);
  let ctx_if_sym = (_, info: Info.t, acc) =>
    switch (info) {
    | InfoExp({ctx, term: {term: Int(n), _}, _}) when n == sym => Some(ctx)
    | _ => acc
    };
  let get_ctx_thing = (map: Statics.Map.t): option(Ctx.t) =>
    Id.Map.fold(ctx_if_sym, map, None);
  db("LSP: Prelude: Recieved string: " ++ str);
  let z =
    OptUtil.get_or_fail(
      "LSP: Prelude: EXN: Couldn't parse string",
      Printer.zipper_of_string(str),
    );
  db("LSP: Prelude: String parsed successfully");
  let info_map = get_info_map(~init_ctx, z);
  switch (get_ctx_thing(info_map)) {
  | Some(ctx) => ctx
  | None =>
    failwith(
      "LSP: Prelude: EXN: Couldn't find sym to extract ctx:"
      ++ string_of_int(sym),
    )
  };
};

let process_prelude = (~db=ignore, ~init_ctx, str: string): Ctx.t => {
  let hashed_str = hash_of_string(str);
  let serialized_filename = hashed_str ++ ".ctx.seralized";
  if (_is_file(serialized_filename)) {
    db("LSP: Prelude: Found serialized file, deserializing");
    deserialize_ctx(serialized_filename);
  } else {
    db("LSP: Prelude: No serialized file, processing string");
    let ctx = pp_inner(~db, ~init_ctx, str);
    serialize_ctx(ctx, serialized_filename);
    ctx;
  };
};

let usage_debug = "[--debug <true|false>]";
let usage_constrain = "[--constrain <grammar|context|types>]";
let usage_ctx = "[--ctx <empty|init>]";
let usage_prelude = "[--prelude <path>]";
let usage_new_token = "[--new-token <new-token-to-append>]";
let usage_str =
  String.concat(
    " ",
    [
      "lsp",
      usage_debug,
      usage_constrain,
      usage_ctx,
      usage_prelude,
      "<program>",
    ],
  );

let rec parse_args = (args, currentSettings) =>
  switch (args) {
  | [] => failwith("LSP: EXN: No program specified. Usage: " ++ usage_str)
  | ["--debug", "false", ...rest] =>
    parse_args(rest, {...currentSettings, debug: false})
  | ["--debug", "true", ...rest] =>
    parse_args(rest, {...currentSettings, debug: true})
  | ["--debug", ..._] => failwith("LSP: EXN: Usage: " ++ usage_debug)
  | ["--constrain", "grammar", ...rest] =>
    parse_args(rest, {...currentSettings, constrain: Grammar})
  | ["--constrain", "context", ...rest] =>
    parse_args(rest, {...currentSettings, constrain: Context})
  | ["--constrain", "types", ...rest] =>
    parse_args(rest, {...currentSettings, constrain: Types})
  | ["--constrain", ..._] => failwith("LSP: EXN: Usage: " ++ usage_constrain)
  | ["--ctx", "empty", ...rest] =>
    parse_args(rest, {...currentSettings, ctx: []})
  | ["--ctx", "init", ...rest] =>
    parse_args(rest, {...currentSettings, ctx: Builtins.ctx_init})
  | ["--ctx", ..._] => failwith("LSP: EXN: Usage: " ++ usage_ctx)
  | ["--prelude", maybe_path, ...rest] =>
    switch (string_of_file(maybe_path)) {
    | exception _ =>
      failwith("LSP: EXN: Could not load prelude from path: " ++ maybe_path)
    | str =>
      let ctx = process_prelude(str, ~init_ctx=currentSettings.ctx);
      parse_args(rest, {...currentSettings, ctx});
    }
  | ["--prelude", ..._] => failwith("LSP: EXN: Usage: " ++ usage_prelude)
  | ["--new-token", new_token, ...rest] =>
    parse_args(rest, {...currentSettings, new_token: Some(new_token)})
  | ["--new-token", ..._] => failwith("LSP: EXN: Usage: " ++ usage_new_token)
  | [arg, ..._] when String.starts_with(~prefix="--", arg) =>
    failwith("LSP: EXN: Unrecognized argument: " ++ arg)
  | [program] => {...currentSettings, program}
  | [_, ..._] =>
    failwith("LSP: EXN: Multiple unnamed arguments. Usage: " ++ usage_str)
  };

/* Assume for now left-to-right entry, so the present shards are
   a prefix of the complete tile. this means that regardless of
   completeness, we can just use the left nibs */

let left_nib_dir = (t: Tile.t): Direction.t =>
  fst(t.mold.nibs).shape == Convex ? Left : Right;

let right_nib_dir = (t: Tile.t): Direction.t =>
  Tile.is_complete(t)
    ? snd(t.mold.nibs).shape == Convex ? Right : Left : Left;

let right_nib_str = (t: Tile.t): string =>
  right_nib_dir(t) == Left ? "<" : ">";

let left_nib_str = (t: Tile.t): string =>
  left_nib_dir(t) == Left ? "<" : ">";

let tile_str = (t: Tile.t): string => {
  let label_str = t.label |> String.concat(" ");
  left_nib_str(t) ++ label_str ++ right_nib_str(t);
};

let thing_to_right = (~db, z: Zipper.t): right_neighbor_info =>
  /* If we're doing pure left to right entry, there should be nothing to
     the right except for maybe a convex hole inserted by the grouter.
     If there is a such a hole, its CI should be used to inform new
     token insertions (but NOT completions of the token to the left) */
  switch (z.relatives.siblings |> snd) {
  | [] =>
    db("  LSP: Syntax: No rightwards piece");
    Nothing;
  | [Grout({id, shape: Convex, _})] =>
    /* If the leftward neighbor has a rightwards concave nib,
       a convex grout will be inserted to the right of the caret.
       We record its ID as its CI can be used to inform
       new token insertions */
    db("  LSP: Syntax: Rightwards piece is Convex Grout");
    ConvexHole(id);
  | [Tile({label: [str], _})] when Form.is_string(str) =>
    /* Special case: When we insert a quote, another quote is inserted
       to the right of the caret. This allow string literals, which are
       implemented as a single token, to 'emulate' the ghost mechanics,
       even though quotes are not proper delimiters. This case is the
       only known situation where the caret position may be Inner during
       pure left-to-right entry */
    StringLit(str)
  | [_, ..._] as rhs =>
    db("  LSP: Syntax: Rightwards segment is: " ++ Segment.show(rhs));
    failwith(
      "  LSP: Syntax: EXN: Nonempty Rightwards segment not single Convex Grout or String literal",
    );
  };

let is_completable = (t: Tile.t) =>
  switch (t.shards |> List.map(List.nth(t.label))) {
  | _ when List.length(t.label) == 1 =>
    /* Monotiles are completable */
    Completeable(t.id, List.hd(t.label))
  | [("(" | "[") as tok_to_left] =>
    /* SPECIAL CASE: Instant-expanding leading polytiles
       that also have a monotile completion */
    Completeable(t.id, tok_to_left)
  | _ =>
    /* Other polytiles are not completable */
    Inert(t.id)
  };

let piece_to_left = (~db, z: Zipper.t): left_neighbor_info =>
  /*
   Returning Left means we're looking for either a new thing
    that starts with a left-facing chevron, or a completion of the
    thing to the left, which has a left-side right-facing chevron.
    */
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [] => None
  | [lht, ..._] as seg =>
    if (Piece.is_secondary(lht)) {
      db("  LSP: Syntax: Leftward is Secondary: trimming");
    };
    switch (Segment.trim_secondary(Left, seg)) {
    | [] => failwith("  LSP: Syntax: EXN: Rightwards seg empty after trim")
    | [lht', ..._] =>
      switch (lht') {
      | Tile(t) =>
        db(
          "  LSP: Syntax: Leftward is "
          ++ (Tile.is_complete(t) ? "Complete" : "Incomplete")
          ++ " Tile: "
          ++ tile_str(t),
        );
        let c = is_completable(t);
        let d = right_nib_dir(t) == Right ? LeftConvex : LeftConcave;
        let p = Piece.is_secondary(lht) ? SpacePadded : Just;
        Some((p, d, c));
      | Grout({id: _, shape, _}) =>
        failwith(
          "  LSP: Syntax: EXN: Leftward Grout " ++ Grout.show_shape(shape),
        )
      | Secondary(_) =>
        failwith("  LSP: Syntax: EXN: Secondary after trimming secondaries")
      }
    };
  };

let of_sugs: list(Suggestion.t) => string =
  s => s |> List.map((s: Suggestion.t) => s.content) |> String.concat(" ");

let error_str = ci =>
  switch (Info.error_of(ci)) {
  | Some(err) => Info.show_error(err)
  | None => "None"
  };

let self_ty = (ci: Info.t): Typ.t =>
  switch (ci) {
  | InfoExp({ctx, self, _}) =>
    switch (Self.typ_of_exp(ctx, self)) {
    | Some(ty) => ty
    | None => unk
    }
  | InfoPat({ctx, self, _}) =>
    switch (Self.typ_of_pat(ctx, self)) {
    | Some(ty) => ty
    | None => unk
    }
  | _ => unk
  };

let expected_ty = (ci: Info.t): Typ.t =>
  switch (ci) {
  | InfoExp({mode, _})
  | InfoPat({mode, _}) => Mode.ty_of(mode)
  | _ => unk
  };

let is_incomplete = (ci: Info.t): incompleteness =>
  /* Is the term (which should be a monotile) something that
     is invalid on its own, but might become valid by extension?
     NOTE: keyword expansion isn't explictly taken into account
     here, since keywords are suggested with trailing spaces,
     so they'll always have a completion suggestion even when
     the keyword is fully written. If this is changed, this will
     need to change as well. */
  switch (ci) {
  | InfoExp({status: InHole(Common(NoType(BadToken(_) | MultiError))), _})
  | InfoPat({status: InHole(Common(NoType(BadToken(_) | MultiError))), _})
  /* Note: an example of the multierror case is "1:" in an expression context */
  | InfoTyp({status: InHole(BadToken(_)), _}) => Grammatical
  | InfoExp({status: InHole(FreeVariable(_)), _})
  | InfoExp({status: InHole(Common(NoType(FreeConstructor(_)))), _})
  | InfoPat({status: InHole(Common(NoType(FreeConstructor(_)))), _})
  | InfoTyp({status: InHole(FreeTypeVariable(_)), _}) => Contextual
  | InfoTyp({status: InHole(DuplicateConstructor(_)), _})
  | InfoTPat({status: InHole(ShadowsType(_)), _}) =>
    /* Kind of an abuse but whatever */
    Contextual
  | _ => Fine
  };

let show_info =
    (db, info_map, ci: Info.t, bidi_ci, bidi_parent_ci, z: Zipper.t) => {
  let sort = Info.sort_of(ci);
  let cls = Info.cls_of(ci);
  let ctx = Info.ctx_of(ci);
  let expected_ty = AssistantForms.Typ.expected(ci);
  let backpack_tokens = AssistantBackpack.to_token_list(z.backpack);
  let errors = Haz3lcore.ErrorPrint.collect_static(info_map);
  db("  LSP: Info: Cls: " ++ Term.Cls.show(cls));
  db("  LSP: Info: Sort: " ++ Sort.to_string(sort));
  db("  LSP: Info: Expected type: " ++ Typ.to_string(expected_ty));
  db("  LSP: Info: Seft type: " ++ Typ.to_string(self_ty(ci)));
  db("  LSP: Info: Error Status: " ++ error_str(ci));
  db("  LSP: Info: Typing Context: " ++ Ctx.to_string(ctx));
  db("  LSP: Info: Backpack stack: " ++ String.concat(" ", backpack_tokens));
  if (errors != []) {
    db("  LSP: Info: ALL errors:\n" ++ String.concat("  \n", errors));
  };
  let expected_ty = AssistantForms.Typ.expected(bidi_ci);
  db("  LSP: Info: BidiCtx: Cls: " ++ Term.Cls.show(Info.cls_of(bidi_ci)));
  db("  LSP: Info: BidiCtx: Expected type: " ++ Typ.to_string(expected_ty));
  switch (bidi_parent_ci) {
  | Some(ci) =>
    db("  LSP: Info: Bidi Parent Cls: " ++ Term.Cls.show(Info.cls_of(ci)))
  | None => db("  LSP: Info: Bidi Parent: Root")
  };
};

let print_gen_option = (~db, gen_options: generation_options): unit =>
  switch (gen_options) {
  | NewRightConvex(_id) => db("  LSP: Syntax: Can insert left-convex")
  | NewRightConcave(_id) => db("  LSP: Syntax: Can insert left-concave")
  | CompletionOrNewRightConvex(_id_l, tok_to_left, _id_new) =>
    db("  LSP: Syntax: Can insert left-convex or complete: " ++ tok_to_left)
  | CompletionOrNewRightConcave(_id_l, tok_to_left) =>
    db("  LSP: Syntax: Can insert left-concave or complete: " ++ tok_to_left)
  | OnlyCompletionString(stringlit) =>
    db("LSP: Must extend/complete stringlit: " ++ stringlit)
  | OnlyCompletion(_id, tok_to_left) =>
    db("  LSP: Syntax: Must complete: " ++ tok_to_left)
  };

let get_bidi_id = (z: Zipper.t, indicated_id: Id.t) => {
  let orig_segment =
    Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
  let map = Measured.path_map(orig_segment);
  let old_path = Id.Map.find(indicated_id, map); //TODO: catch or opt
  let new_z = Zipper.zip_to_path(orig_segment, old_path, Outer);
  let seg: Segment.t = new_z.relatives.siblings |> Siblings.zip;
  seg
  |> Segment.skel
  |> Skel.root
  |> Aba.map_a(List.nth(seg))
  |> Aba.first_a
  |> Piece.id;
};

let collate_info = (~settings, ~db, z: Zipper.t, id: Id.t): infodump => {
  let init_ctx = settings.ctx;
  let info_map = get_info_map(~init_ctx, z);
  let get_info = id => Id.Map.find_opt(id, info_map);
  let ci =
    OptUtil.get_or_fail(
      "LSP: Gen: EXN: Couldn't find CI for id " ++ Id.to_string(id),
      get_info(id),
    );
  let bidi_ci =
    OptUtil.get_or_fail(
      "LSP: Gen: EXN: Couldn't find Bidi CI for id " ++ Id.to_string(id),
      get_info(get_bidi_id(z, id)),
    );
  let bidi_parent_ci =
    bidi_ci
    |> Info.ancestors_of
    |> ListUtil.hd_opt
    |> OptUtil.and_then(get_info);
  /* TODO: Disabled any_errors at the moment as I think it's too restrictive.
      For example, given "let x:Bool = (1" we want to suggest ")".
     Closer is gating on errors in bidelimited ctx, but even that
     doesn't work in above, as error is on the 1. Parens may be
     special case here.

     Must be no errors to drop:
     "let = in" (inner types independent of expectation)
     "type = in" (inner things independent)
     "fun ->" (inner type independent)
     "| =>" (inner type independent of expectation)
     "test end" (since inner type independent of expected type)

     Errors might be ok:
     "()" (parens) e.g. given "let a:Bool = (1" could do ") < 1"
     "case end" e.g. given "let a:Bool = case 1 | 1 => 1" could do "end < 1"

     UNSURE:
     "<???>(<???>)" (fn ap; might be okay since binds v tight)
     "[<???>]"
     "if <fine> then <???> else"

     Logic is maybe something like:
     Can drop, if resultant operand after dropping only has errors
     due to expectation for which there is a resolvant type path.

     Or maybe: it shouldnt be about checking for errors per se, but
     instead analyzing the contents of the bidelimited context against
     a type with a typepath to /its/ bidelimited ctx

     */
  show_info(db, info_map, ci, bidi_ci, bidi_parent_ci, z);
  {
    ci,
    bidi_ci,
    bidi_ctx_cls: Info.cls_of(bidi_ci),
    bidi_ctx_expected_ty: expected_ty(bidi_ci),
    bidi_parent_ci,
    bidi_parent_ctx_cls: Option.map(Info.cls_of, bidi_parent_ci),
  };
};

let generation_options = (~db, ~settings, z: Zipper.t) => {
  let get_info = collate_info(~db, ~settings, z);
  switch (piece_to_left(~db, z), thing_to_right(~db, z)) {
  | (None, Nothing) => failwith("LSP: EXN: Nothing to left or right")
  | (Some((to_left, shape, compl)), Nothing) =>
    switch (shape) {
    | LeftConcave =>
      failwith("LSP: EXN: Concave to left and nothing to right")
    | LeftConvex =>
      switch (to_left, compl) {
      | (Just, Inert(id_l))
      | (SpacePadded, Completeable(id_l, _) | Inert(id_l)) =>
        NewRightConcave(get_info(id_l))
      | (Just, Completeable(id_l, left_token)) =>
        let left_info = get_info(id_l);
        switch (is_incomplete(left_info.ci)) {
        | Grammatical =>
          db("  LSP: Syntax: Bad token; only completion");
          OnlyCompletion(left_info, left_token);
        | Contextual
            when settings.constrain == Types || settings.constrain == Context =>
          db("  LSP: Syntax: Free token; only completion");
          OnlyCompletion(left_info, left_token);
        | _ => CompletionOrNewRightConcave(left_info, left_token)
        };
      }
    }
  | (None, ConvexHole(id)) => NewRightConvex(get_info(id))
  | (Some((to_left, shape, compl)), ConvexHole(id_r)) =>
    switch (shape) {
    | LeftConvex => failwith("LSP: EXN: Convex to left and right")
    | LeftConcave =>
      switch (to_left, compl) {
      | (Just, Inert(_))
      | (SpacePadded, _) => NewRightConvex(get_info(id_r))
      | (Just, Completeable(id_l, left_tok)) =>
        let left_info = get_info(id_l);
        switch (is_incomplete(get_info(id_l).ci)) {
        | Grammatical =>
          db("  LSP: Syntax: Bad token; only completion");
          OnlyCompletion(left_info, left_tok);
        | Contextual
            when settings.constrain == Types || settings.constrain == Context =>
          db("  LSP: Syntax: Free token; only completion");
          OnlyCompletion(left_info, left_tok);
        | _ =>
          CompletionOrNewRightConvex(left_info, left_tok, get_info(id_r))
        };
      }
    }
  | (_, StringLit(id)) => OnlyCompletionString(id)
  };
};

let get_backpack_sugs = (~convex, z: Zipper.t): Suggestion.s =>
  List.filter_map(
    convex ? AssistantBackpack.is_convex : AssistantBackpack.is_concave,
    AssistantBackpack.suggest(z),
  );

let rec is_strict_prefix_up_to_consistency = (ctx, p_syn, p_ana) =>
  switch (p_syn, p_ana) {
  | (_, []) => false
  | ([], _) => true
  | ([ty_syn, ...p_syn], [ty_ana, ...p_ana]) =>
    Typ.is_consistent(ctx, ty_syn, ty_ana)
    && is_strict_prefix_up_to_consistency(ctx, p_syn, p_ana)
  };
let suggest_comma = (bidi_ctx_ci: Info.t) =>
  switch (bidi_ctx_ci) {
  | InfoExp({mode: Syn | Ana(Unknown(_)), _})
  | InfoPat({mode: Syn | Ana(Unknown(_)), _}) => true
  | InfoExp({mode: Ana(Prod(p_ana)), self: Common(Just(syn)), ctx, _})
  | InfoPat({mode: Ana(Prod(p_ana)), self: Common(Just(syn)), ctx, _}) =>
    /*  For reference: Orginially based on their being an inconsistency error,
        but updated to deal with cases without errors e.g.:
        PAT: "let find: (Bool, Int) -> Int = fun b"
        EXP: "let a:  =  in let _: (Bool, Int) = a"  */
    print_endline(
      "LSP: commas: p_ana is prod: "
      ++ String.concat(" ", List.map(Typ.show, p_ana)),
    );
    print_endline("LSP: commas: self syn is " ++ Typ.show(syn));
    switch (p_ana, syn) {
    | (_, Unknown(_)) => true // technically redundant?
    | (_, Prod(p_syn)) =>
      is_strict_prefix_up_to_consistency(ctx, p_syn, p_ana)
    | ([t1_ana, ..._], _) => Typ.is_consistent(ctx, t1_ana, syn)
    | _ => false
    };
  | InfoExp({mode: SynFun | Ana(_), _})
  | InfoPat({mode: SynFun | Ana(_), _}) => false
  | InfoTyp(_) => true
  | InfoTPat(_) => false
  };

let n_ary_sugs = (~settings, ~db as _, bidi_ci): Suggestion.s => {
  let comma_sug = Suggestion.mk(",");
  switch (settings.constrain) {
  | Types => suggest_comma(bidi_ci) ? [comma_sug] : []
  | Context
  | Grammar => [comma_sug]
  };
};

let dedup = List.sort_uniq(compare);

let suggest_exp = (~fns, ctx: Ctx.t, ty): Suggestion.s =>
  AssistantCtx.suggest_bound_exp(~fns, ty, ctx)
  @ AssistantForms.suggest_all_ty_convex(Exp, ctx, ty);

let suggest_pat = (~fns, ctx: Ctx.t, co_ctx, ty): Suggestion.s =>
  AssistantCtx.suggest_free_var(ty, ctx, co_ctx)
  @ AssistantCtx.suggest_bound_pat(~fns, ty, ctx)
  @ AssistantForms.suggest_all_ty_convex(Pat, ctx, ty);

let suggest_typ = (ctx: Ctx.t): Suggestion.s =>
  AssistantCtx.suggest_bound_typ(ctx)
  @ AssistantForms.suggest_all_ty_convex(Typ, ctx, unk);

let suggest_tpat = (ctx: Ctx.t): Suggestion.s =>
  AssistantForms.suggest_all_ty_convex(TPat, ctx, unk);

let convex_sugs = (~settings, ci: Info.t) =>
  switch (settings.constrain) {
  | Types =>
    switch (ci) {
    | InfoExp({mode, ctx, _}) =>
      suggest_exp(~fns=false, ctx, Mode.ty_of(mode))
    | InfoPat({mode, ctx, co_ctx, _}) =>
      suggest_pat(~fns=false, ctx, co_ctx, Mode.ty_of(mode))
    | InfoTyp({ctx, _}) => suggest_typ(ctx)
    | InfoTPat({ctx, _}) => suggest_tpat(ctx)
    }
  | Context =>
    switch (ci) {
    | InfoExp({ctx, _}) => suggest_exp(~fns=false, ctx, unk)
    | InfoPat({ctx, co_ctx, _}) => suggest_pat(~fns=false, ctx, co_ctx, unk)
    | InfoTyp({ctx, _}) => suggest_typ(ctx)
    | InfoTPat({ctx, _}) => suggest_tpat(ctx)
    }
  | Grammar =>
    switch (ci) {
    | InfoExp(_) =>
      [Suggestion.mk("~PATVAR~")]
      @ [Suggestion.mk("~CONSTRUCTOR~")]
      @ suggest_exp(~fns=false, [], unk)
    | InfoPat(_) =>
      [Suggestion.mk("~CONSTRUCTOR~")]
      @ suggest_pat(~fns=false, [], [], unk)
    | InfoTyp(_) => [Suggestion.mk("~TYPVAR~")] @ suggest_typ([])
    | InfoTPat(_) => suggest_tpat([])
    }
  };

let convex_lookahead_sugs = (~settings, ~db, ci: Info.t) => {
  switch (settings.constrain) {
  | Context
  | Grammar => []
  | Types =>
    switch (ci) {
    | InfoExp({mode, ctx, _}) =>
      let ty = Mode.ty_of(mode);
      let ty_paths = AssistantCtx.get_lookahead_tys_exp(ty);
      db(
        "  LSP: Convex: Ty paths:\n " ++ AssistantCtx.show_type_path(ty_paths),
      );
      let tys =
        List.map(Util.ListUtil.last, ty_paths) |> List.sort_uniq(compare);
      // Filter out the current type
      //let tys = List.filter((!=)(ty), tys);
      db(
        "  LSP: Convex: Target types: "
        ++ (List.map(Typ.to_string, tys) |> String.concat(", ")),
      );
      suggest_exp(~fns=true, ctx, ty)
      @ (List.map(suggest_exp(~fns=true, ctx), tys) |> List.flatten);
    | InfoPat({mode, ctx, co_ctx, _}) =>
      let ty = Mode.ty_of(mode);
      let tys = AssistantCtx.get_lookahead_tys_pat(ty);
      suggest_pat(~fns=true, ctx, co_ctx, ty)
      @ (List.map(suggest_pat(~fns=true, ctx, co_ctx), tys) |> List.flatten);
    | InfoTyp(_) => []
    | InfoTPat(_) => []
    }
  };
};

let postfix_sugs =
    (
      ~settings,
      ~db as _,
      ci: Info.t,
      bidi_ctx_cls: Term.Cls.t,
      bidi_parent_ctx_cls: option(Term.Cls.t),
    )
    : Suggestion.s => {
  /* NOTE: We have to check both the bidictx and the parent here
     because the way case is implemented, rule tiles get the ci for
     the whole case, so if we're on a rule tile, the bidictx will be
     the whole case. (TODO: This is confusing, clarify) */
  let case_rule_sug =
    bidi_parent_ctx_cls == Some(Exp(Match)) || bidi_ctx_cls == Exp(Match)
      ? [Suggestion.mk("|")] : [];
  let postfix_ap_sug = (ctx, self_ty) =>
    /* Could alternatively make this more restrictive and require
       that actually arrow type not merely consistent. This would
       enforce use with appropriate constructor in patterns,
       but would screw up current impl of grammar/context-only generation */
    if (Typ.is_consistent(ctx, self_ty, Arrow(unk, unk))) {
      [Suggestion.mk("(")];
    } else {
      [];
    };
  switch (settings.constrain) {
  | Grammar =>
    switch (ci) {
    | InfoExp(_) => postfix_ap_sug([], unk) @ case_rule_sug
    | InfoPat(_) => postfix_ap_sug([], unk)
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | InfoTPat(_) => []
    }
  | Context =>
    let ctx = Info.ctx_of(ci);
    switch (ci) {
    | InfoExp(_) => postfix_ap_sug(ctx, unk) @ case_rule_sug
    | InfoPat(_) => postfix_ap_sug(ctx, unk)
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | InfoTPat(_) => []
    };
  | Types =>
    switch (ci) {
    | InfoExp({ctx, _}) => postfix_ap_sug(ctx, self_ty(ci)) @ case_rule_sug
    | InfoPat({ctx, _}) => postfix_ap_sug(ctx, self_ty(ci))
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | InfoTPat(_) => []
    }
  };
};

let of_ops = (ctx, expected_ty: Typ.t, child_ty: Typ.t, ty1, ty2, ops) =>
  if (Typ.is_consistent(ctx, expected_ty, ty1)
      && Typ.is_consistent(ctx, child_ty, ty2)) {
    List.map(Suggestion.mk, ops);
  } else {
    [];
  };

let sug_exp_infix = (ctx: Ctx.t, l_child_ty: Typ.t, expected_ty: Typ.t) => {
  let of_ops = of_ops(ctx, expected_ty, l_child_ty);
  let bb = of_ops(Bool, Bool, ["&&", "\\/"]);
  let bs = of_ops(Bool, String, ["$=="]);
  let bi = of_ops(Bool, Int, ["==", "!=", "<=", ">=", "<", ">"]);
  let bf = of_ops(Bool, Float, ["==.", "!=.", "<=.", ">=.", "<.", ">."]);
  let i = of_ops(Int, Int, ["+", "-", "*", "/", "**"]);
  let f = of_ops(Float, Float, ["+.", "-.", "*.", "/.", "**."]);
  let s = of_ops(String, String, ["++"]);
  let l1 = of_ops(List(unk), Typ.matched_list(ctx, expected_ty), ["::"]);
  /*Note: Using List(matched) in 2nd arg below because don't want this
    check to pass if expected is unknown but l_child is e.g. Int */
  let l2 =
    of_ops(List(unk), List(Typ.matched_list(ctx, expected_ty)), ["@"]);
  bb @ bs @ bi @ bf @ i @ f @ s @ l1 @ l2;
};

let infix_sugs =
    (
      ~completion: bool,
      ~settings,
      ~db,
      ci: Info.t,
      bidi_ctx_expected_ty: Typ.t,
    )
    : Suggestion.s => {
  let infix_mono_sugs = AssistantForms.suggest_infix_mono(ci);
  switch (settings.constrain) {
  | Grammar =>
    switch (ci) {
    | InfoExp(_) => sug_exp_infix([], unk, unk)
    | InfoPat(_)
    | InfoTyp(_)
    | InfoTPat(_) => infix_mono_sugs
    }
  | Context =>
    switch (ci) {
    | InfoExp({ctx, _}) => sug_exp_infix(ctx, unk, unk)
    | InfoPat(_)
    | InfoTyp(_)
    | InfoTPat(_) => infix_mono_sugs
    }
  | Types =>
    switch (ci) {
    | InfoExp({ctx, mode, self, _}) =>
      /* 1. Calc skel of lseg and thread it here.
            2. recurse on skel, looking for infix operators
           3. if infix op lower precedence, rec on its left child.
             if left child is not lower precdence, cur will be parent of new op,
             and left child will be child of new op.
         4. in other words, want find furthest-down infix with lower precedence.
         5. then can generated expected_tys for type paths to the parent expected type
         6. and child_ty from child
         7. so basic fn will take precdence and seg, and return ids of prospective parent, child
          */
      //TODO: lookahead expected_tys should take precedence into account as well
      let expected_ty = Mode.ty_of(mode);
      let self_ty =
        switch (Self.typ_of_exp(ctx, self)) {
        | _ when completion =>
          /* TODO: this is a hack that gives too-liberal recommendations
             really need to figure out, for each operator completion, what
             the left child would actually become, and get that type. */
          unk
        | Some(ty) => ty
        | None => unk
        };
      db(
        "  LSP: Concave: Infix: Left child Self type: "
        ++ Typ.to_string(self_ty),
      );
      let base = sug_exp_infix(ctx, self_ty, expected_ty);
      db("  LSP: Concave: Infix: Base: " ++ of_sugs(base));
      let ty_paths = AssistantCtx.get_lookahead_tys_exp(bidi_ctx_expected_ty);
      let tys =
        List.map(Util.ListUtil.last, ty_paths) |> List.sort_uniq(compare);
      db(
        "  LSP: Concave: Infix: Lookahead types: "
        ++ (List.map(Typ.to_string, tys) |> String.concat(", ")),
      );
      let lookahead =
        List.map(sug_exp_infix(ctx, self_ty), tys) |> List.flatten;
      db(
        "  LSP: Concave: Infix: Lookahead: " ++ of_sugs(lookahead |> dedup),
      );
      base @ lookahead;
    //TODO: get self_ty of actual prospective child, on a per-operator basis
    | InfoPat(_)
    | InfoTyp(_)
    | InfoTPat(_) => infix_mono_sugs
    }
  };
};

let completion_filter =
    (caret: Zipper.Caret.t, tok_to_left: string, sug: Suggestion.t) => {
  let sug = Suggestion.content_of(sug);
  /*  For fixed tokens, a suggestion is a valid completion if
      the token to the left of the caret is a proper prefix of the
      suggestion. If the suggestion is a regexp form like atomic
      literals or binding variable names, if the token to the left
      is a prefix of something matching the regexp, we want to
      suggest a regexp which represents possible ways of extending
      the token such that it remains a prefix of something matching
      the regexp.

       I assume this can be done generally for at least basic
      regular expressions, but for now each form is special-cased */
  let is_prefix_of_intlit = Form.is_int;
  let is_prefix_of_floatlit = t => Form.is_float(t) || Form.is_int(t);
  let is_prefix_if_stringlit = Form.is_string;
  let is_prefix_of_var = Form.is_var;
  let is_prefix_of_typvar = Form.is_typ_var;
  let is_prefix_of_ctr = Form.is_ctr;
  if (String.starts_with(~prefix=tok_to_left, sug)) {
    TyDi.suffix_of(sug, tok_to_left);
  } else {
    switch (sug) {
    | "~INTLIT~" when is_prefix_of_intlit(tok_to_left) =>
      /* TODO: limit length? */
      Some("~EXTEND-INTLIT~")
    | "~FLOATLIT~" when is_prefix_of_floatlit(tok_to_left) =>
      /* TODO: figure out general case incl NaN, length limit, etc. */
      Some("~EXTEND-FLOATLIT~")
    | "~STRINGLIT~" when is_prefix_if_stringlit(tok_to_left) =>
      /* HACK: This only actually covers the case of a finished stringlit
         (caret is Outer). The in-progress stringlit case is captured above
         (the StringLit case for thing_to_right. However we retain the
         currently-unreachable Some case below defenfsively. */
      caret == Outer ? None : Some("~EXTEND-STRINGLIT~")
    | "~PATVAR~" when is_prefix_of_var(tok_to_left) =>
      Some("~EXTEND-PATVAR~")
    | "~TYPVAR~" when is_prefix_of_typvar(tok_to_left) =>
      Some("~EXTEND-TYPVAR~")
    | "~CONSTRUCTOR~" when is_prefix_of_ctr(tok_to_left) =>
      Some("~EXTEND-CONSTRUCTOR~")
    | _ => None
    };
  };
};

let left_convex_sugs = (~settings, ~info_dump, ~db, z) => {
  let {ci, _} = info_dump;
  let left_convex_backpack_sugs = get_backpack_sugs(~convex=true, z);
  let convex_sugs = convex_sugs(~settings, ci);
  let convex_lookahead_sugs = convex_lookahead_sugs(~settings, ~db, ci);
  //TODO: should prefix be factored out here somewhere?
  db("  LSP: Convex: Backpack: " ++ of_sugs(left_convex_backpack_sugs));
  db("  LSP: Convex: Base: " ++ of_sugs(convex_sugs));
  db(
    "  LSP: Convex: Lookahead: " ++ of_sugs(convex_lookahead_sugs |> dedup),
  );
  left_convex_backpack_sugs
  @ convex_sugs
  @ convex_lookahead_sugs
  |> List.sort_uniq(Suggestion.compare);
};

let left_concave_sugs = (~info_dump, ~completion, ~settings, ~db, z) => {
  let {
    ci,
    bidi_ci,
    bidi_parent_ctx_cls,
    bidi_ctx_cls,
    bidi_ctx_expected_ty,
    _,
  } = info_dump;
  let left_concave_backpack_sugs = get_backpack_sugs(~convex=false, z);
  let infix_sugs =
    infix_sugs(~completion, ~settings, ~db, ci, bidi_ctx_expected_ty)
    |> List.sort_uniq(Suggestion.compare);
  let postfix_sugs =
    postfix_sugs(~settings, ~db, ci, bidi_ctx_cls, bidi_parent_ctx_cls);
  let n_ary_sugs = n_ary_sugs(~settings, ~db, bidi_ci);
  db("  LSP: Concave: Backpack: " ++ of_sugs(left_concave_backpack_sugs));
  db("  LSP: Concave: N-ary: " ++ of_sugs(n_ary_sugs));
  db("  LSP: Concave: Infix: " ++ of_sugs(infix_sugs));
  db("  LSP: Concave: Postfix: " ++ of_sugs(postfix_sugs));
  left_concave_backpack_sugs
  @ n_ary_sugs
  @ infix_sugs
  @ postfix_sugs
  |> List.sort_uniq(Suggestion.compare);
};

let mk_completions =
    (~settings: arguments, ~db, ~tok_to_left: string, z: Zipper.t, info_dump)
    : list(string) => {
  /* Note that for completion suggestions, we ignore shape expectations,
     as the left tile might get keyword-completed or remolded */
  db("LSP: Generating Completions for prefix: " ++ tok_to_left);
  left_convex_sugs(~settings, ~db, ~info_dump, z)
  @ left_concave_sugs(~completion=true, ~settings, ~info_dump, ~db, z)
  |> List.filter_map(completion_filter(z.caret, tok_to_left))
  |> List.sort_uniq(String.compare);
};

let mk_new_left_convex =
    (~settings: arguments, ~db, z: Zipper.t, info_dump): list(string) => {
  db("LSP: Generating new left convex tokens");
  left_convex_sugs(~settings, ~info_dump, ~db, z)
  |> List.map(Suggestion.content_of)
  |> List.sort_uniq(String.compare);
};

let mk_new_left_concave =
    (~settings: arguments, ~db, z: Zipper.t, info_dump): list(string) => {
  db("LSP: Generating new left concave tokens");
  left_concave_sugs(~completion=false, ~settings, ~db, ~info_dump, z)
  |> List.map(Suggestion.content_of)
  |> List.sort_uniq(String.compare);
};

let dispatch_generation =
    (~settings: arguments, ~db, z: Zipper.t): pre_grammar => {
  let seg_before = z.relatives.siblings |> fst |> List.rev;
  let seg_after = z.relatives.siblings |> snd;
  if (seg_before == [] && seg_after == []) {
    failwith("LSP: EXN: Empty segment");
  };
  let gen_options = generation_options(~settings, ~db, z);
  print_gen_option(~db, gen_options);
  let mk_completions = mk_completions(~settings, ~db, z);
  let mk_left_convex = mk_new_left_convex(~settings, ~db, z);
  let mk_left_concave = mk_new_left_concave(~settings, ~db, z);
  switch (gen_options) {
  | NewRightConvex(info_dump) => {
      completions: [],
      new_tokens: mk_left_convex(info_dump),
    }
  | NewRightConcave(info_dump) => {
      completions: [],
      new_tokens: mk_left_concave(info_dump),
    }
  | CompletionOrNewRightConvex(info_dump_l, tok_to_left, info_dump_new) => {
      completions: mk_completions(~tok_to_left, info_dump_l),
      new_tokens: mk_left_convex(info_dump_new),
    }
  | CompletionOrNewRightConcave(info_dump, tok_to_left) => {
      completions: mk_completions(~tok_to_left, info_dump),
      new_tokens: mk_left_concave(info_dump),
    }
  | OnlyCompletion(info_dump, tok_to_left) => {
      completions: mk_completions(~tok_to_left, info_dump),
      new_tokens: [],
    }
  | OnlyCompletionString(_) => {
      completions: ["~EXTEND-STRINGLIT~"],
      new_tokens: [],
    }
  };
};

let normalize_token = tok =>
  switch (tok) {
  | "~INTLIT~" => "intlit"
  | "~EXTEND-INTLIT~" => "extend-intlit"
  | "~FLOATLIT~" => "floatlit"
  | "~EXTEND-FLOATLIT~" => "extend-floatlit"
  | "~STRINGLIT~" => "stringlit"
  | "~EXTEND-STRINGLIT~" => "extend-stringlit"
  | "~PATVAR~" => "patvar"
  | "~EXTEND-PATVAR~" => "extend-patvar"
  | "~TYPVAR~" => "typvar"
  | "~EXTEND-TYPVAR~" => "extend-typvar"
  | "~CONSTRUCTOR~" => "constructor"
  | "~EXTEND-CONSTRUCTOR~" => "extend-constructor"
  | "~WHITESPACE~" => "whitespace"
  | "\\/" => {|"\\/"|}
  | tok => "\"" ++ tok ++ "\""
  };

let base_sorts = {|whitespace ::= [ \n]+
intlit ::= [0-9]+
extend-intlit ::= [0-9]+
floatlit ::= [0-9]+ "." [0-9]+
extend-floatlit ::= [0-9]* "." [0-9]+
stringlit ::= "\"" [^"]* "\""
extend-stringlit ::= [^"]* "\""
patvar ::= [a-z][a-zA-Z0-9_]*
extend-patvar ::= [a-zA-Z0-9_]*
typvar ::= [A-Z][a-zA-Z0-9_]*
extend-typvar ::= [a-zA-Z0-9_]*
constructor ::= [A-Z][a-zA-Z0-9_]*
extend-constructor ::= [a-zA-Z0-9_]*
|};

let mk_grammar = (pre_grammar: pre_grammar): string => {
  let sort_completions = {
    let completions =
      pre_grammar.completions
      |> List.map(normalize_token)
      |> String.concat(" | ");
    pre_grammar.completions == [] ? "" : "\ncompletions ::= " ++ completions;
  };
  // Note addition of whitespace token
  let sort_new_tokens = {
    let new_tokens =
      ["~WHITESPACE~", ...pre_grammar.new_tokens]
      |> List.map(normalize_token)
      |> String.concat(" | ");
    pre_grammar.new_tokens == [] ? "" : "\nnew-tokens ::= " ++ new_tokens;
  };
  let sort_root =
    switch (pre_grammar.completions, pre_grammar.new_tokens) {
    | ([], []) => failwith("LSP: EXN: No completions or new tokens")
    | ([], _) => "new-tokens"
    | (_, []) => "completions"
    | _ => "completions | new-tokens"
    };
  base_sorts
  ++ sort_completions
  ++ sort_new_tokens
  ++ "\nroot ::= "
  ++ sort_root;
};

let main = args => {
  let settings = parse_args(args, default_settings);
  let db = s => settings.debug ? print_endline(s) : ();
  db(show_settings(settings));
  let z = get_zipper(~db, settings);
  let grammar = z |> dispatch_generation(~settings, ~db) |> mk_grammar;
  db("LSP: Grammar:");
  print_endline(grammar);
};

main(get_args());

//MAYBE: consider reducing generation of duplicate lookahead suggestions
//MAYBE: abstract out default forms of any type eg case if let etc.
//MAYBE: Serialize prelude for speed
//MAYBE: make sure stopping completions doesn't prohibit completing unit (+ unit ap)

//TODO: Make sure synthetic mode is handled appropriately
//TODO: Type defintions are basically untested
//TODO: Type annotations and patterns are only lightly tested

//TODO(andrew): find cases where operator types go wrong
//BUG: "let _:String =\"yo\"+" only looks for ints so doesn't sug completion to "++"
//BUG: "\"yo\"+" only looks for ints so doesn't sug completion to "+."
//(above are monkeypatched to be too liberal for now)

//BUG: too general type in interior tuple elems (FIX: maybe actually insert commas at end)
// e.g. "let _:(Int, String, Float) =1," suggests everything

/* TODO: Restict new tokens and whitespace logic.
   1. (DONE)
   2. GATE backpack suggestion ON bidictx term ci has no errors
   3. GATE comma suggestion ON bidictx term ci has no _internal_ errors AND
      the bidictx analyzes against prefix of expected tuple */

/* TODO: More sophisticated logic for operand/operator insertion
      1. Operand Insertion
         0. Note that inserting an operand can't fix any errors, and
            neither can it bury ie render unfixable any existing errors (i think)
         a. If the operand analyzes against the type its parent expects for
            its right child, then we can insert it.
         b. Otherwise, we want to know if there exists an operator which could become its parent.
         c. That is, is there an infix or postfix operator, which binds tighter than the operator
            to the left of the operand (if any; if none, any infix or postfix operator will do),
            whose right child slot accept's the operand's type, and that itself will be consistent
            with the expectation of some possible chain of ancestors (infix and postfix operators)
            which can be inserted 'between' the operand and its parent.
         d. Operationally we approximate that last condition of c by asking getting the type chains
            for the current parent's right child, and seeing if any of them lead to the operand's type.
         e. If we extend the type chains with precdence information, then I think we can make this
            subsume (a) as well. We get the typechains (including the trivial one) for the operand's
           parent's right child type, see if any of them lead to the operand's type, and if so,
           then <INSERT SOME COMPLICATED PRECEDENCE LOGIC HERE>.
         f. Actually modulo the precedence logic, this is effectively the current logic i think.
     2. Operator Insertion:
        1. This can cause up to 2 errors, and fix up to 1 error. It can also cause errors to be
           buried, i.e. become unfixable.
        2. Assume for the moment that there are no errors to start.
        3. The for each prospective _infix_ operator, we can use its precedence to query the skel,
           and get its prospective parent and prospective child. the prospective left child
           must agree exactly as it cannot be re-parented. we then want their to be
           a typechain from prospective parent's right child slot to the new operator.
           <INSERT MAYBE MORE COMPLICATED PRECEDENCE LOGIC HERE>.
       4. (An Attempt): Something like... the prospectice parent is the nearest leftwards
          infix operator whose precdedence is looser than the new operator. for another operator
          to come between the two in the skel, it must have a looser precence that the new operator,
          but tighter than the prospective parent. so if we're considering inserting a sequence of
          operators to make the new operator agree with the parent, they all have to have prededences
          between the two (if they're infix, posffix are always fine i think?), and they have to be able
          to form a typechain. but i'm not sure if the typechain order has the same order so as to
          reduce this to predence monotonicty.
   */
