(* Migrate doc/slide .ml files to the current segment sexp format.
 *
 * The branch changed `Base.projector` from `ProjectorCore.t(piece)` to
 * `ProjectorCore.t(segment)`, so every projector's stored `syntax` is now
 * a list of pieces rather than a single piece. Old .ml files have
 * `(syntax (Tile ...))` in their persisted segment sexps; they need to
 * become `(syntax (<unparenthesized children>))` (or `(syntax ((Tile ...)))`
 * for non-paren tiles).
 *
 * This tool reads each .ml file, locates the `segment = "..."` field, sexp-
 * parses its decoded contents, walks the tree replacing every
 * `(Projector ((... (syntax X) ...)))` with the segment-based equivalent,
 * and writes back the file with the new sexp content. Everything else in
 * the .ml file (backup_text, refractors, name) is left untouched.
 *
 * Run: `dune exec src/regen/regen_docs.exe`. *)

let ml_files =
  [
    "src/web/init/docs/BasicReference.ml";
    "src/web/init/docs/Projectors.ml";
    "src/web/init/docs/ADTs.ml";
    "src/web/init/docs/Tuples.ml";
    "src/web/init/docs/Modules.ml";
    "src/web/init/docs/Tables.ml";
    "src/web/init/docs/Polymorphism.ml";
    "src/web/init/docs/Cards.ml";
    "src/web/init/docs/Probes.ml";
    "src/web/init/docs/Livelits.ml";
    "src/b2t2/slides/B2T2ExampleTables.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsemptyTable.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsaddRows.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsaddColumn.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsbuildColumn.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsvcat.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorshcat.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsvalues.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorscrossJoin.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsleftJoin.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIProperties.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIAccessSubcomponents.ml";
    "src/b2t2/slides/table_api/B2T2TableAPISubtable.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIOrdering.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIAggregate.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIMissingValues.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIDataCleaning.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesFlatten.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiestransformColumn.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesrenameColumns.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesfind.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesgroupByRetentive.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesgroupBySubtractive.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesupdate.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesselect.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesselectMany.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesgroupJoin.ml";
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesjoin.ml";
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsDotProduct.ml";
    "src/b2t2/slides/example_programs/B2T2ExampleProgramspHackingHomogeneous.ml";
    "src/b2t2/slides/example_programs/B2T2ExampleProgramspHackingHeterogeneous.ml";
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsquizScoreFilter.ml";
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsquizScoreSelect.ml";
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsgroupByRetentive.ml";
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsgroupBySubtractive.ml";
    "src/b2t2/slides/errors/B2T2ErrorsMalformedTables.ml";
    "src/b2t2/slides/errors/B2T2ErrorsUsingTablesPart1.ml";
    "src/b2t2/slides/errors/B2T2ErrorsUsingTablesPart2.ml";
    "src/b2t2/slides/errors/B2T2ErrorsUsingTablesPart3.ml";
  ]

(* OCaml string-literal decoding: turn the textual contents of a quoted
 * string into the actual string value. Handles the escapes that ocamlformat
 * and ppx_deriving.show actually emit on these files: backslash-n, -t, -r,
 * -backslash, -quote, -apostrophe, decimal triples, hex escapes, and the
 * line-continuation form (backslash followed by newline + whitespace). *)
let decode_ocaml_string (raw : string) : string =
  let buf = Buffer.create (String.length raw) in
  let len = String.length raw in
  let i = ref 0 in
  while !i < len do
    let c = raw.[!i] in
    if c = '\\' && !i + 1 < len then begin
      let n = raw.[!i + 1] in
      match n with
      | 'n' ->
          Buffer.add_char buf '\n';
          i := !i + 2
      | 't' ->
          Buffer.add_char buf '\t';
          i := !i + 2
      | 'r' ->
          Buffer.add_char buf '\r';
          i := !i + 2
      | 'b' ->
          Buffer.add_char buf '\b';
          i := !i + 2
      | '\\' ->
          Buffer.add_char buf '\\';
          i := !i + 2
      | '"' ->
          Buffer.add_char buf '"';
          i := !i + 2
      | '\'' ->
          Buffer.add_char buf '\'';
          i := !i + 2
      | ' ' ->
          Buffer.add_char buf ' ';
          i := !i + 2
      | '\n' ->
          i := !i + 2;
          while !i < len && (raw.[!i] = ' ' || raw.[!i] = '\t') do
            incr i
          done
      | '0' .. '9' when !i + 3 < len ->
          let code = int_of_string (String.sub raw (!i + 1) 3) in
          Buffer.add_char buf (Char.chr code);
          i := !i + 4
      | 'x' when !i + 3 < len ->
          let code = int_of_string ("0x" ^ String.sub raw (!i + 2) 2) in
          Buffer.add_char buf (Char.chr code);
          i := !i + 4
      | 'o' when !i + 4 < len ->
          let code = int_of_string ("0o" ^ String.sub raw (!i + 2) 3) in
          Buffer.add_char buf (Char.chr code);
          i := !i + 5
      | _ ->
          (* Unknown escape; keep verbatim. *)
          Buffer.add_char buf c;
          incr i
    end
    else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

(* Encode a string as an OCaml string literal body (without surrounding
 * quotes). Uses String.escaped which matches what ocamlformat / [%show]
 * would emit, then we let ocamlformat handle line-wrapping later. *)
let encode_ocaml_string = String.escaped

(* Locate `<field-name> =` then its following "..." string literal in
 * `text` (starting search at `from`). Returns the (string-content-start,
 * string-content-end-exclusive, opening-quote-index, closing-quote-index)
 * tuple, or raises if not found. *)
let find_string_field text ~field ~from =
  let re = Str.regexp ("\\b" ^ Str.quote field ^ "[ \t\n]*=[ \t\n]*\"") in
  let _ = Str.search_forward re text from in
  let opening = Str.match_end () - 1 in
  let rec find_close i =
    if i >= String.length text then
      failwith ("Unterminated string for field " ^ field)
    else
      let c = text.[i] in
      if c = '"' then i
      else if c = '\\' && i + 1 < String.length text then find_close (i + 2)
      else find_close (i + 1)
  in
  let closing = find_close (opening + 1) in
  (opening + 1, closing, opening, closing)

(* Walks a sexp tree and rewrites every (Projector (... (syntax X) ...))
 * so that X is converted from a single piece to a segment via
 * Piece.unparenthesize semantics:
 *   - if X is (Tile ((... (label ("(" ")")) (mold (... (nibs ((shape Convex ...) (shape Convex ...))))) ... (children ((<seg>))))))
 *     then the new value is <seg>;
 *   - otherwise the new value is (X) (a singleton segment containing X). *)
let rec patch_sexp (s : Sexplib.Sexp.t) : Sexplib.Sexp.t =
  match s with
  | Atom _ -> s
  | List items -> (
      let items = List.map patch_sexp items in
      match items with
      | [ Atom "Projector"; List record ] ->
          let record =
            List.map
              (fun field ->
                match field with
                | Sexplib.Sexp.List [ Atom "syntax"; value ] ->
                    Sexplib.Sexp.List [ Atom "syntax"; piece_to_segment value ]
                | other -> other)
              record
          in
          List [ Atom "Projector"; List record ]
      | _ -> List items)

(* Detect whether `value` is already a segment (the post-migration shape)
 * vs. still a single piece (the pre-migration shape). A piece always has
 * the form `(<tag> <record>)` where <tag> is one of the variant names,
 * so its first element is an Atom; a segment is a list of pieces, so its
 * first element (if any) is itself a List. *)
and is_segment (value : Sexplib.Sexp.t) : bool =
  match value with List [] -> true | List (List _ :: _) -> true | _ -> false

(* Mirror Piece.unparenthesize at the sexp level. *)
and piece_to_segment (value : Sexplib.Sexp.t) : Sexplib.Sexp.t =
  if is_segment value then value
  else
    match value with
    | List [ Atom "Tile"; List tile_record ] when is_paren_tile tile_record -> (
        (* Pull the (children ((<seg>))) → <seg>. *)
        match tile_children tile_record with
        | Some [ child_seg ] -> child_seg
        | _ -> Sexplib.Sexp.List [ value ])
    | _ -> Sexplib.Sexp.List [ value ]

and is_paren_tile (record : Sexplib.Sexp.t list) : bool =
  let label = field record "label" in
  let mold = field record "mold" in
  match (label, mold) with
  | Some (List [ Atom "("; Atom ")" ]), Some mold_value ->
      (* Confirm Convex/Convex outer shape (matches Piece.unparenthesize). *)
      is_convex_convex_mold mold_value
  | _ -> false

and is_convex_convex_mold (mold : Sexplib.Sexp.t) : bool =
  match mold with
  | List record -> (
      match field record "nibs" with
      | Some (List [ List left; List right ]) ->
          has_convex_shape left && has_convex_shape right
      | _ -> false)
  | _ -> false

and has_convex_shape (nib : Sexplib.Sexp.t list) : bool =
  match field nib "shape" with Some (Atom "Convex") -> true | _ -> false

and tile_children (record : Sexplib.Sexp.t list) : Sexplib.Sexp.t list option =
  match field record "children" with
  | Some (List children) -> Some children
  | _ -> None

and field (record : Sexplib.Sexp.t list) (name : string) : Sexplib.Sexp.t option
    =
  let rec go = function
    | [] -> None
    | Sexplib.Sexp.List (Atom n :: [ v ]) :: _ when n = name -> Some v
    | _ :: rest -> go rest
  in
  go record

let migrate_segment_sexp (segment_text : string) : string =
  let sexp = Sexplib.Sexp.of_string segment_text in
  let patched = patch_sexp sexp in
  Sexplib.Sexp.to_string patched

let migrate_file (path : string) : unit =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let text = Bytes.to_string buf in
  let start, stop, _, _ = find_string_field text ~field:"segment" ~from:0 in
  let raw_segment_str = String.sub text start (stop - start) in
  let decoded = decode_ocaml_string raw_segment_str in
  let migrated = migrate_segment_sexp decoded in
  if migrated = decoded then begin
    Printf.printf "  unchanged: %s\n" path
  end
  else begin
    let encoded = encode_ocaml_string migrated in
    let new_text =
      String.sub text 0 start ^ encoded
      ^ String.sub text stop (String.length text - stop)
    in
    let oc = open_out path in
    output_string oc new_text;
    close_out oc;
    Printf.printf "  migrated:  %s\n" path
  end

let () =
  let files =
    if Array.length Sys.argv > 1 then List.tl (Array.to_list Sys.argv)
    else ml_files
  in
  Printf.printf "Migrating %d files...\n" (List.length files);
  List.iter
    (fun f ->
      try migrate_file f
      with exn ->
        Printf.eprintf "  FAILED %s: %s\n" f (Printexc.to_string exn))
    files;
  Printf.printf "Done.\n"
