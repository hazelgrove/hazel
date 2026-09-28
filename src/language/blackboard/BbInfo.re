open Util;

/* Per-node static information for Blackboard terms, mirroring DrvInfo.
   Errors come from BbCheck, which works on kernel terms; they are attached
   here to the node whose subterm produced them.  Blackboard keeps its own
   status rather than joining Mark.t, whose declaration order is
   load-bearing for the other sorts. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ancestors = list(Id.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error =
  /* the term could not be read as a Blackboard document at all */
  | Malformed(string)
  /* an error the checker reported, already rendered by BbError */
  | Check(BbError.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status =
  | NotInHole
  | InHole(error);

/* Which kind of block a node sits in, when it sits in one. Used by the
   editor to tint a block by its modality: what an `assume` block declares
   is postulated, what a `construct` block declares is a conservative
   extension that still owes a witness. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type modality =
  | Assume
  | Construct;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Bb.Term.t,
  cls: Cls.t,
  ancestors,
  modality: option(modality),
  status,
};

/* The display sort: which block this node sits in, so the code view can
   tint it. See BbSort. */
let sort_of: t => BbSort.t =
  fun
  | {modality: None, _} => BbSort.Term
  | {modality: Some(Assume), _} => BbSort.Assumed
  | {modality: Some(Construct), _} => BbSort.Constructed;
let cls_of: t => Cls.t = ({cls, _}) => cls;
let ancestors_of: t => ancestors = ({ancestors, _}) => ancestors;
let id_of: t => Id.t = ({term, _}) => Bb.Term.rep_id(term);
let modality_of: t => option(modality) = ({modality, _}) => modality;

let error_of: t => option(error) =
  fun
  | {status: NotInHole, _} => None
  | {status: InHole(err), _} => Some(err);

let is_error: t => bool = t => error_of(t) != None;

let derived = (term: Bb.Term.t, ~ancestors, ~modality, ~status): t => {
  term,
  cls: Cls.Bb(Bb.Term.cls_of(term)),
  ancestors,
  modality,
  status,
};

let message: error => string =
  fun
  | Malformed(why) => why
  | Check(err) => BbError.to_string(err);
