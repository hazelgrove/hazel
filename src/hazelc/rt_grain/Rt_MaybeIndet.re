/**
  "hazel/rt/maybe_indet.gr", handwritten.
 */
open Rt_.WithoutImpl({
       let name = "MaybeIndet";
       let path = "maybe_indet";
     });

/**
  Import statement.
 */
let import = import;

/**
  Implementation module.
 */
let impl_md = impl_md;

let value = e => ctor1("Value", e);
let indet = e => ctor1("Indet", e);

let value_pat = p => pctor1("Value", p);
let indet_pat = p => pctor1("Indet", p);
