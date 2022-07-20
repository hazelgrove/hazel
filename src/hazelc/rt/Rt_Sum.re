/**
  "hazel/rt/sum.gr", handwritten.
 */
open Rt_.WithoutImpl({
       let name = "Sum";
       let path = "sum";
     });
/**
  Import statement.
 */
let import = import;

/**
  Implementation module.
 */
let impl_md = impl_md;

let inj_l = e => ctor1("L", e);
let inj_r = e => ctor1("R", e);

let inj_l_pat = p => pctor1("L", p);
let inj_r_pat = p => pctor1("R", p);
