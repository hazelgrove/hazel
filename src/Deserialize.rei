open SemanticsCore;
let deserialize: in_channel => (UHExp.t, HTyp.t, MetaVar.t);
let uhexp_of_string: string => UHExp.t;
