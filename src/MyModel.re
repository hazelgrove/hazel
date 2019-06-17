type t = {
  put_zblock: ZExp.zblock => unit,
  get_zblock: unit => ZExp.zblock,
  get_typ: unit => HTyp.t,
  get_u_gen: MetaVarGen.t,
  get_cursor_info: CursorInfo.t,
  get_result: Dynamics.(DHExp.t, DHExp.HoleInstanceInfo.t, Evaluator.result),
};

let _put_zblock = (zblock: ZExp.zblock, typ: ref(HTyp.t), u_gen: ref(MetaVarGen.t), cursor_info: )

let init = (): t => {
  let (u0, u_gen1) = MetaVarGen.next(MetaVarGen.init);
  let zblock = ref(ZExp.wrap_in_block(ZExp.place_before_exp(EmptyHole(u0))));
  let typ = ref(HTyp.Hole);
  let u_gen = ref(u_gen1);

}