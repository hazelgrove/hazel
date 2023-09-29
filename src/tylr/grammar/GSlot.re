include Slot;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Slot.t(GMaterial.Sorted.t);
