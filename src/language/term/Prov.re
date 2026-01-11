include TermBase.Prov;

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | CycleHole
  | MultiHole
  | SynSwitch
  | Internal
  | LArrow
  | RArrow
  | NProduct
  | MList
  | RForall
  | TupLabelProv
  | TupLabelArg
  | Meet
  | TypeSubstitution;

let cls_of_term: Grammar.type_provenance('a) => cls =
  fun
  | Hole(Invalid(_)) => Invalid
  | Hole(EmptyHole) => EmptyHole
  | Hole(CycleHole) => CycleHole
  | Hole(MultiHole(_)) => MultiHole
  | SynSwitch => SynSwitch
  | Internal => Internal
  | LArrow(_) => LArrow
  | RArrow(_) => RArrow
  | NProduct(_) => NProduct
  | MList(_) => MList
  | RForall(_) => RForall
  | TupLabel(_) => TupLabelProv
  | TupLabelArg(_) => TupLabelArg
  | Meet(_) => Meet
  | TypeSubstitution(_) => TypeSubstitution;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type"
  | MultiHole => "Broken type"
  | EmptyHole => "Type hole"
  | CycleHole => "Cycle type hole"
  | SynSwitch => "Synthetic type"
  | Internal => "Internal type"
  | LArrow => "Left arrow prov type"
  | RArrow => "Right arrow prov type"
  | NProduct => "Tuple prov type"
  | MList => "List prov type"
  | RForall => "Right Forall prov type"
  | TupLabelProv => "Tuple label prov"
  | TupLabelArg => "Tuple arg prov"
  | Meet => "Join prov"
  | TypeSubstitution => "Type substitution";

let fresh: term => t = IdTagged.fresh;
/* fresh assigns a random id, whereas temp assigns Id.invalid, which
   is a lot faster, and since we so often make types and throw them away
   shortly after, it makes sense to use it. */
let anonymous: term => t = IdTagged.temp;

let term_of: t => term = IdTagged.term_of;

let is_identified = (p: t) => IdTagged.rep_id(p) != Id.invalid;
