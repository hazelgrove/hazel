open Util;
open Language;
open InfoMessage;
/* Print static errors to a string */

[@deriving (show({with_path: false}), yojson, sexp)]
type t =
  | ParseError(string)
  | StaticErrors(list(string))
  | NoErrors;

let remove_projectors = (segment: Segment.t) =>
  //TODO: Remove this when splices is merged
  ZipperBase.MapPiece.of_segment(
    fun
    | Projector(pr) => [pr.syntax]
    | x => [x],
    segment,
  );

module Print = {
  let seg = (~holes, segment: Segment.t): string => {
    let segment = remove_projectors(segment);
    Printer.of_segment(
      ~holes,
      ~measured=Measured.of_segment(segment, Id.Map.empty),
      ~caret=None,
      ~indent=" ",
      segment,
    );
  };

  let term = (term: Any.t): string => {
    let settings =
      ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off);
    term |> ExpToSegment.any_to_pretty(~settings) |> seg(~holes="");
  };

  let typ = (ty: Typ.t): string => term(Typ(ty));
};

let render_string = (fragments: list(InfoMessage.fragment)): string =>
  List.map(
    fun
    | Text(s) => s
    | Code(s) => "\"" ++ s ++ "\""
    | Type(ty) => Print.typ(ty)
    | Term(term) => Print.term(term)
    | Label(s) => Token.quote_label_when_necessary(s),
    fragments,
  )
  |> String.concat("");

let string_of: Info.t => string =
  info => render_string(InfoMessage.build_message(info).fragments);

let term_string_of: Info.t => string =
  fun
  | InfoExp({term, _}) => Print.term(Exp(term))
  | InfoPat({term, _}) => Print.term(Pat(term))
  | InfoTyp({term, _}) => Print.term(Typ(term))
  | InfoTPat({term, _}) => Print.term(TPat(term))
  | Secondary(_) => failwith("ChatLSP: term_string_of: Secondary");

let all = (info_map: Statics.Map.t): list(string) => {
  Id.Map.fold(
    (_id: Id.t, info: Info.t, acc) =>
      switch (Info.error_of(info)) {
      | None => acc
      | Some(_) => [info] @ acc
      },
    info_map,
    [],
  )
  |> List.sort_uniq(compare)
  |> List.filter_map(info =>
       switch (Info.error_of(info)) {
       | None => None
       | Some(_) =>
         let term = term_string_of(info);
         Some(
           Printf.sprintf(
             "Error in term:\n  %s\nNature of error: %s",
             term,
             string_of(info),
           ),
         );
       }
     );
};
