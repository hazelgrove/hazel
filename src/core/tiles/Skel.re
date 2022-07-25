open Util;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Op(int)
  | Pre(int, t)
  | Post(t, int)
  | Bin(t, int, t);

let rec size =
  fun
  | Op(_) => 1
  | Pre(_, r) => 1 + size(r)
  | Post(l, _) => size(l) + 1
  | Bin(l, _, r) => size(l) + 1 + size(r);

let root_index =
  fun
  | Op(n)
  | Pre(n, _)
  | Post(_, n)
  | Bin(_, n, _) => n;

let children =
  fun
  | Op(_) => []
  | Pre(_, skel) => [(Direction.Right, skel)]
  | Post(skel, _) => [(Left, skel)]
  | Bin(l, _, r) => [(Left, l), (Right, r)];

// returns inclusive lower bound, exclusive upper bound
let rec range =
  fun
  | Op(n) => (n, n + 1)
  | Pre(n, r) => (n, snd(range(r)))
  | Post(l, n) => (fst(range(l)), n + 1)
  | Bin(l, _, r) => (fst(range(l)), snd(range(r)));

let rec skel_at = (n, skel) =>
  switch (skel) {
  | Op(m) => n == m ? skel : raise(Invalid_argument("Skel.skel_at"))
  | Pre(m, r) => n == m ? skel : skel_at(n, r)
  | Post(l, m) => n == m ? skel : skel_at(n, l)
  | Bin(l, m, r) =>
    if (n < m) {
      skel_at(n, l);
    } else if (n > m) {
      skel_at(n, r);
    } else {
      skel;
    }
  };

exception Nonconvex_segment;

type iss = (int, Nibs.shapes);
let mk = (seg: list(iss)): t => {
  let push_output = ((i, ss): iss, output_stack: list(t)): list(t) =>
    switch (ss) {
    | (Convex, Convex) => [Op(i), ...output_stack]
    | (Convex, Concave(_)) =>
      switch (output_stack) {
      | [] => failwith("impossible: pre encountered empty stack")
      | [skel, ...skels] => [Pre(i, skel), ...skels]
      }
    | (Concave(_), Convex) =>
      switch (output_stack) {
      | [] => failwith("impossible: post encountered empty stack")
      | [skel, ...skels] => [Post(skel, i), ...skels]
      }
    | (Concave(_), Concave(_)) =>
      switch (output_stack) {
      | []
      | [_] =>
        failwith("impossible: bin encountered empty or singleton stack")
      | [skel1, skel2, ...skels] => [Bin(skel2, i, skel1), ...skels]
      }
    };

  let process_op = (~output_stack, ~shunted_stack, iop) => (
    output_stack,
    [iop, ...shunted_stack],
  );

  let rec process_pre =
          (~output_stack: list(t), ~shunted_stack: list(iss), ipre: iss) => {
    switch (shunted_stack) {
    | [] => (output_stack, [ipre, ...shunted_stack])
    | [(_, ss) as iss, ...ips] =>
      switch (ss) {
      | (_, Concave(_)) => (output_stack, [ipre, ...shunted_stack])
      | (_, Convex) =>
        process_pre(
          ~output_stack=push_output(iss, output_stack),
          ~shunted_stack=ips,
          ipre,
        )
      }
    };
  };
  // assumes postops lose ties with preops and binops
  let rec process_post =
          (
            ~output_stack: list(t),
            ~shunted_stack: list(iss),
            ~prec: Precedence.t,
            ipost: iss,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [ipost, ...shunted_stack])
    | [(_, ss) as iss, ...ips] =>
      switch (ss) {
      | (_, Convex) =>
        process_post(
          ~output_stack=push_output(iss, output_stack),
          ~shunted_stack=ips,
          ~prec,
          ipost,
        )
      | (_, Concave(prec_p)) =>
        prec_p < prec
        || prec_p == prec
        && Precedence.associativity(prec_p) == Some(Left)
          ? process_post(
              ~output_stack=push_output(iss, output_stack),
              ~shunted_stack=ips,
              ~prec,
              ipost,
            )
          : (output_stack, [ipost, ...shunted_stack])
      }
    };
  // currently assumes binops lose ties with preops
  let rec process_bin =
          (
            ~output_stack: list(t),
            ~shunted_stack: list(iss),
            ~prec: Precedence.t,
            ibin: iss,
          ) =>
    switch (shunted_stack) {
    | [] => (output_stack, [ibin, ...shunted_stack])
    | [(_, ss) as iss, ...ips] =>
      switch (ss) {
      | (_, Convex) =>
        process_bin(
          ~output_stack=push_output(iss, output_stack),
          ~shunted_stack=ips,
          ~prec,
          ibin,
        )
      | (_, Concave(prec_p)) =>
        prec_p < prec
        || prec_p == prec
        && Precedence.associativity(prec_p) == Some(Left)
          ? process_bin(
              ~output_stack=push_output(iss, output_stack),
              ~shunted_stack=ips,
              ~prec,
              ibin,
            )
          : (output_stack, [ibin, ...shunted_stack])
      }
    };
  let rec go =
          (
            ~output_stack: list(t)=[],
            ~shunted_stack: list(iss)=[],
            ips: list(iss),
          )
          : list(t) => {
    switch (ips) {
    | [] =>
      shunted_stack
      |> List.fold_left(
           (output_stack, t) => push_output(t, output_stack),
           output_stack,
         )
    | [(_, ss) as iss, ...ips] =>
      let process =
        switch (ss) {
        | (Convex, Convex) => process_op
        | (Convex, Concave(_)) => process_pre
        | (Concave(prec), Convex) => process_post(~prec)
        | (Concave(prec), Concave(_)) => process_bin(~prec)
        };
      let (output_stack, shunted_stack) =
        process(~output_stack, ~shunted_stack, iss);
      go(~output_stack, ~shunted_stack, ips);
    };
  };

  ListUtil.hd_opt(go(seg)) |> OptUtil.get_or_raise(Nonconvex_segment);
};
