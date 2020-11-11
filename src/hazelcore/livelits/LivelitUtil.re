type llctordata =
  | FreeLivelitData(MetaVar.t, LivelitName.t)
  | ApLivelitData(
      MetaVar.t,
      LivelitName.t,
      LivelitName.t,
      SerializedModel.t,
      UHExp.splice_info,
    );

let get_livelit_name_from_data =
  fun
  | FreeLivelitData(_, lln)
  | ApLivelitData(_, _, lln, _, _) => lln;

let rec check_livelit_skel =
        (
          ~permit_free_livelit=false,
          ~permit_insufficient_params_hole=false,
          seq,
          skel,
        ) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (seq |> Seq.nth_operand(n)) {
    | UHExp.ApLivelit(llu, NotInHole, base_lln, lln, model, splice_info) =>
      Some((ApLivelitData(llu, base_lln, lln, model, splice_info), []))
    | UHExp.ApLivelit(
        llu,
        InHole(TypeInconsistent(Some(InsufficientParams)), _),
        base_lln,
        lln,
        model,
        splice_info,
      )
        when permit_insufficient_params_hole =>
      Some((ApLivelitData(llu, base_lln, lln, model, splice_info), []))
    | UHExp.FreeLivelit(llu, lln) when permit_free_livelit =>
      Some((FreeLivelitData(llu, lln), []))
    | _ => None
    }
  | Skel.BinOp(NotInHole, Operators_Exp.Space, skel1, skel2) =>
    check_livelit_skel(
      ~permit_free_livelit,
      ~permit_insufficient_params_hole,
      seq,
      skel1,
    )
    |> Option.map(((data, rev_args)) => (data, [skel2, ...rev_args]))
  | _ => None
  };

/*
 Digs into the left side of skel, to see if it's of the form (NE-hole free)
 ApLivelit Space param1 ... Space paramN, where N is the number of params accepted
 by the livelit. If so, it returns the ApLivelit info, livelit_defn, param_tys, and
 args. If there's a non-space operator, the lefmost operand isn't an ApLivelit, the
 ApLivelit or one of the space BinOps is in a hole, ctx lookup fails, or the number of
 params is wrong, then None is returned. These various None cases happen for diverse
 reasons, but the ultimate conclusion of all such cases is that we needn't do anything
 special with the current BinOp as a whole, rather the calling code can continue digging
 in until it either hits the right number of args, or until it hits the ApLivelit with
 no args (which, if it takes any params, indicates that the ApLivelit itself should be in
 a hole). Note that type compatibility of params and args is not checked, the caller must
 do this.
 If ~permit_free_livelit is true, then it also returns data for a FreeLivelit.
 If ~permit_insufficient_params_hole is true, then it's ok for the ApLivelit operand is
    in a TypeInconsistent(Some(InsufficientParams)) hole.
 If ~permit_insufficient_args is true, then it returns Some(data) even if there are too
    few args
 */
let check_livelit =
    (
      ~permit_free_livelit=false,
      ~permit_insufficient_params_hole=false,
      ~permit_insufficient_args=false,
      ctx,
      seq,
      skel,
    ) =>
  check_livelit_skel(
    ~permit_free_livelit,
    ~permit_insufficient_params_hole,
    seq,
    skel,
  )
  |> OptUtil.and_then(((data, rev_args)) =>
       LivelitCtx.lookup(
         Contexts.livelit_ctx(ctx),
         get_livelit_name_from_data(data),
       )
       |> OptUtil.and_then(
            ((livelit_defn: LivelitDefinition.t, closed_args)) => {
            let args = List.rev(rev_args);
            let param_tys =
              livelit_defn.param_tys
              |> ListUtil.drop(List.length(closed_args));
            let num_params = List.length(param_tys);
            let num_args = List.length(args);
            if (permit_insufficient_args
                && num_args < num_params
                || num_args == num_params) {
              Some((data, livelit_defn, closed_args, param_tys, args));
            } else {
              None;
            };
          })
     );
