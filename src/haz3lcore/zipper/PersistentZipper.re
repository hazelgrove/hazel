open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  zipper: string,
  backup_text: string,
};

let to_string = Printer.of_zipper(~holes="", ~indent="");

let persist = (zipper: Zipper.t) => {
  {
    zipper: Zipper.sexp_of_t(zipper) |> Sexplib.Sexp.to_string,
    backup_text: to_string(zipper),
  };
};

/* A text-backed slide: the program text (a committed .hz file) is the
   only source. The empty zipper string marks the text path as
   intentional, so unpersist takes it without the stale-serialization
   warning. */
let of_text = (text: string): t => {
  zipper: "",
  backup_text: text,
};

/* Persisted programs are complete, so the linear Menhir zip usually
   takes this; the simulated-typing parser is quadratic in program
   size and can hang startup on big stale-sexp slides. */
/* Caret starts at the TOP: unzip's default direction (Right) leaves the
   caret after the whole program, and the editor scrolls the caret into
   view on display — a freshly loaded slide would open at the bottom. */
let apply_collected_refractors = (z: Zipper.t): Zipper.t =>
  List.fold_left(
    (z, (id, trigger)) =>
      switch (Triggers.refractor_of_invoke_token(trigger)) {
      | Some((kind, model)) => ZipperBase.add_manual(~model?, id, kind, z)
      | None => z
      },
    z,
    FastParse.collected_refractors^,
  );

let from_backup_text = (backup_text: string, ~root): Zipper.t =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root,
      String.trim(backup_text),
    )
  ) {
  | Some(segment) =>
    Zipper.unzip(~direction=Left, segment) |> apply_collected_refractors
  | None =>
    /* MarkerParse subsumes the plain typing parse and also destructs
       `¿` markers back into Grout (concave grout and other fast-path
       bails land here). */
    switch (MarkerParse.of_text(~root, backup_text)) {
    | None => Zipper.init()
    | Some(z) =>
      /* reposition the caret to the start WITHOUT dropping refractors:
         unselect_and_zip yields a bare segment, and unzip would mint a
         fresh (empty) refractor state — losing pins built from trigger
         text during the parse */
      let refractors = z.refractors;
      Zipper.unzip(~direction=Left, Zipper.unselect_and_zip(z))
      |> ZipperBase.update_refractors(_, _ => refractors);
    }
  };

let unpersist = (persisted: t, ~root) =>
  if (persisted.zipper == "") {
    from_backup_text(persisted.backup_text, ~root);
  } else {
    try(Sexplib.Sexp.of_string(persisted.zipper) |> Zipper.t_of_sexp) {
    | _ =>
      print_endline(
        "Warning: using backup text! Serialization may be for an older version of Hazel.",
      );
      from_backup_text(persisted.backup_text, ~root);
    };
  };
