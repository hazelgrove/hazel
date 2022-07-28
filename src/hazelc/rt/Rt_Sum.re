/**
  "hazel/rt/sum.gr", handwritten.
 */
open Grain;
open Grain.Ident;
open Rt_.Stub({
       let path = "sum" |> Path.v;
     });

/**
  Implementation module.
 */
let impl = impl;

module Use = (I: Make.I) => {
  include Use(I);

  let imp = imp;

  let inj_l = e => ctor1(v("L"), e);
  let inj_r = e => ctor1(v("R"), e);

  let inj_l_pat = p => pctor1(v("L"), p);
  let inj_r_pat = p => pctor1(v("R"), p);
};
