open Util

type nat = int

type t = nat list * ZExp.cursor_side

let cons' (step : nat) (r : t) : t = 
  let (steps, side) = r in 
  (step :: steps, side) 

let rec of_ztyp (zty : ZTyp.t) : t = 
  begin match zty with 
  | ZTyp.CursorT(cursor_side, _) -> ([], cursor_side)
  | ZTyp.ParenthesizedZ zty1 -> cons' 0 (of_ztyp zty1)
  | ZTyp.ListZ zty1 -> cons' 0 (of_ztyp zty1)
  | ZTyp.OpSeqZ(_, zty1, surround) -> 
    let n = OperatorSeq.surround_prefix_length surround in 
    cons' n (of_ztyp zty1)
  end

let rec of_zpat (zp : ZPat.t) : t = 
  begin match zp with 
  | ZPat.CursorP(cursor_side, _) -> ([], cursor_side)
  | ZPat.Deeper(_, zp') -> of_zpat' zp'
  | ZPat.ParenthesizedZ zp1 -> cons' 0 (of_zpat zp1)
  end
and of_zpat' (zp' : ZPat.t') : t = 
  begin match zp' with 
  | ZPat.InjZ(_, zp1) -> cons' 0 (of_zpat zp1)
  (* | ZPat.ListLitZ zps -> 
    let prefix_length = ZList.prefix_length zps in 
    let zp0 = ZList.prj_z zps in 
    cons' prefix_length (of_zpat zp0) *)
  | ZPat.OpSeqZ(_, zp1, surround) -> 
    let n = OperatorSeq.surround_prefix_length surround in 
    cons' n (of_zpat zp1)
  end 

let rec of_zexp (ze : ZExp.t) : t = 
  begin match ze with 
  | ZExp.CursorE(cursor_side, _) -> ([], cursor_side)
  | ZExp.Deeper(_, ze') -> of_zexp' ze'
  | ZExp.ParenthesizedZ ze1 -> cons' 0 (of_zexp ze1)
  end
and of_zexp' (ze : ZExp.t') : t = 
  begin match ze with 
  | ZExp.AscZ1(ze', _) -> cons' 0 (of_zexp ze')
  | ZExp.AscZ2(_, zty) -> cons' 1 (of_ztyp zty)
  | ZExp.LetZP(zp, _, _, _) -> cons' 0 (of_zpat zp) 
  | ZExp.LetZA(_, zann, _, _) -> cons' 1 (of_ztyp zann)
  | ZExp.LetZE1(_, _, ze1, _) -> cons' 2 (of_zexp ze1) 
  | ZExp.LetZE2(_, _, _, ze2) -> cons' 3 (of_zexp ze2)
  | ZExp.LamZP(zp, _, _) -> cons' 0 (of_zpat zp)
  | ZExp.LamZA(_, zann, _) -> cons' 1 (of_ztyp zann)
  | ZExp.LamZE(_, ann, ze') -> cons' 2 (of_zexp ze')
  | ZExp.InjZ(_, ze') -> cons' 0 (of_zexp ze')
  (* | ZExp.ListLitZ zes -> 
    let prefix_length = ZList.prefix_length zes in 
    let ze0 = ZList.prj_z zes in 
    cons' prefix_length (of_zexp ze0) *)
  | ZExp.CaseZE(ze1, _) -> cons' 0 (of_zexp ze1)
  | ZExp.CaseZR(_, zrules) -> 
    let prefix_len = List.length (ZList.prj_prefix zrules) in 
    let zrule = ZList.prj_z zrules in 
    cons' ( prefix_len) (of_zrule zrule) 
  | ZExp.OpSeqZ(_, ze', surround) -> 
    let n = OperatorSeq.surround_prefix_length surround in 
    cons' n (of_zexp ze')
  | ZExp.ApPaletteZ(_, _, zholedata) -> 
    let (_, zholemap) = zholedata in 
    let (_, tz) = zholemap in 
    let (n, tz') = tz in
    let (_, ze') = tz' in 
    cons' n (of_zexp ze')
  end
and of_zrule (zrule : ZExp.zrule) : t = 
  begin match zrule with 
  | ZExp.RuleZP(zp, _) -> cons' 0 (of_zpat zp)
  | ZExp.RuleZE(_, ze) -> cons' 1 (of_zexp ze)
  end

let of_OpSeqZ (ze : ZExp.t) (surround : ZExp.opseq_surround) = 
  let n = OperatorSeq.surround_prefix_length surround in 
  cons' n (of_zexp ze)

let of_OpSeqZ_pat (zp : ZPat.t) (surround : ZPat.opseq_surround) = 
  let n = OperatorSeq.surround_prefix_length surround in 
  cons' n (of_zpat zp)

let rec follow_ty (path : t) (uty : UHTyp.t) : ZTyp.t option = 
  begin match path with
  | ([], cursor_side) -> Some (ZTyp.CursorT(cursor_side, uty))
  | (x :: xs, cursor_side) -> 
    begin match uty with 
    | UHTyp.Hole
    | UHTyp.Unit
    | UHTyp.Num
    | UHTyp.Bool -> None
    | UHTyp.Parenthesized uty1 -> 
      begin match x with 
      | 0 -> 
        begin match follow_ty (xs, cursor_side) uty1 with 
        | Some zty -> Some (ZTyp.ParenthesizedZ zty)
        | None -> None
        end
      | _ -> None
      end
    | UHTyp.List uty1 -> 
      begin match x with 
      | 0 -> 
        begin match follow_ty (xs, cursor_side) uty1 with 
        | None -> None
        | Some zty -> Some (ZTyp.ListZ zty)
        end
      | _ -> None
      end
    | UHTyp.OpSeq(skel, seq) -> 
      begin match OperatorSeq.split x seq with 
      | Some (uty_n, surround) -> 
        begin match follow_ty (xs, cursor_side) uty_n with 
        | Some zty_n -> 
          Some (ZTyp.OpSeqZ(skel, zty_n, surround))
        | None -> None
        end
      | None -> None
      end
    end
  end

let rec follow_pat (path : t) (p : UHPat.t) : ZPat.t option = 
  begin match path with 
  | ([], cursor_side) -> Some (ZPat.CursorP(cursor_side, p))
  | (x :: xs, cursor_side) -> 
    begin match p with 
    | UHPat.Parenthesized p1 -> 
      begin match x with 
      | 0 -> 
        begin match follow_pat (xs, cursor_side) p1 with 
        | None -> None
        | Some zp1 -> Some (ZPat.ParenthesizedZ zp1)
        end
      | _ -> None
      end
    | UHPat.Pat(err_status, p') -> 
      begin match (x, p') with 
      | (_, UHPat.EmptyHole _)
      | (_, UHPat.Wild)
      | (_, UHPat.Var _)
      | (_, UHPat.NumLit _)
      | (_, UHPat.BoolLit _)
      | (_, UHPat.ListNil) -> None
      (* | (n, UHPat.ListLit ps) -> 
        begin match ZList.split_at n ps with 
        | None -> None
        | Some psz -> 
          begin match ZList.optmap_z (follow_pat (xs, cursor_side)) psz with 
          | None -> None
          | Some zps -> 
            Some (ZPat.Deeper err_status (ZPat.ListLitZ zps))
          end
        end*)
      | (0, UHPat.Inj(side, p1)) -> 
        begin match follow_pat (xs, cursor_side) p1 with 
        | None -> None
        | Some zp1 -> Some (ZPat.Deeper(err_status, (ZPat.InjZ(side, zp1))))
        end
      | (_, UHPat.Inj(_, _)) -> None
      | (n, UHPat.OpSeq(skel, seq)) -> 
        begin match OperatorSeq.split n seq with 
        | None -> None
        | Some (p, surround) -> 
            begin match follow_pat (xs, cursor_side) p with 
            | Some zp -> 
                Some (ZPat.Deeper(err_status, (ZPat.OpSeqZ(skel, zp, surround))))
            | None -> None
            end
        end
      end
    end
  end

let rec follow_e (path : t) (e : UHExp.t) : ZExp.t option = 
  begin match path with 
  | ([], cursor_side) -> Some (ZExp.CursorE(cursor_side, e))
  | (x :: xs, cursor_side) -> 
    begin match e with 
    | UHExp.Parenthesized e1 -> 
      begin match x with 
      | 0 -> 
        begin match follow_e (xs, cursor_side) e1 with 
        | Some ze1 -> Some (ZExp.ParenthesizedZ ze1)
        | None -> None
        end
      | _ -> None
      end
    | UHExp.Tm(err_status, e) -> 
      begin match (x, e) with 
      | (_, UHExp.EmptyHole _) -> None
      | (0, UHExp.Asc(e1, ty)) -> 
        begin match follow_e (xs, cursor_side) e1 with 
        | Some ze -> Some (ZExp.Deeper(err_status, (ZExp.AscZ1(ze, ty))))
        | None -> None
        end
      | (1, UHExp.Asc(e1, ty)) -> 
        begin match follow_ty (xs, cursor_side) ty with 
        | Some ztau -> Some (ZExp.Deeper(err_status, (ZExp.AscZ2(e1, ztau))))
        | None -> None
        end
      | (_, UHExp.Asc(_, _)) -> None
      | (_, UHExp.Var(_, _)) -> None
      | (0, UHExp.Let(p, ann, e1,  e2)) -> 
        begin match follow_pat (xs, cursor_side) p with 
        | None -> None
        | Some zp -> 
          Some (ZExp.Deeper(err_status, (ZExp.LetZP(zp, ann, e1, e2))))
        end
      | (1, UHExp.Let(p, ann, e1, e2)) -> 
        begin match ann with 
        | None -> None
        | Some ann_ty -> 
          begin match follow_ty (xs, cursor_side) ann_ty with 
          | None -> None
          | Some zann -> Some (ZExp.Deeper(err_status, (ZExp.LetZA(p, zann, e1, e2)))) 
          end
        end
      | (2, UHExp.Let(p, ann, e1, e2)) -> 
        begin match follow_e (xs, cursor_side) e1 with 
        | Some ze1 -> Some (ZExp.Deeper(err_status, (ZExp.LetZE1(p, ann, ze1, e2))))
        | None -> None
        end
      | (3, UHExp.Let(p, ann, e1, e2)) -> 
        begin match follow_e (xs, cursor_side) e2 with 
        | Some ze2 -> Some (ZExp.Deeper(err_status, (ZExp.LetZE2(p, ann, e1, ze2))))
        | None -> None
        end
      | (_, UHExp.Let(_, _, _, _)) -> None
      | (0, UHExp.Lam(p, ann, e1)) -> 
        begin match follow_pat (xs, cursor_side) p with 
        | None -> None
        | Some zp -> 
          Some (ZExp.Deeper(err_status, (ZExp.LamZP(zp, ann, e1))))
        end
      | (1, UHExp.Lam(p, ann, e1)) -> 
        begin match ann with 
        | None -> None
        | Some ann_ty -> 
          begin match follow_ty (xs, cursor_side) ann_ty with 
          | None -> None
          | Some zann -> 
            Some (ZExp.Deeper(err_status, (ZExp.LamZA(p, zann, e1))))
          end
        end
      | (2, UHExp.Lam(p, ann, e1)) -> 
        begin match follow_e (xs, cursor_side) e1 with 
        | None -> None
        | Some ze -> Some (ZExp.Deeper(err_status, (ZExp.LamZE(p, ann, ze))))
        end
      | (_, UHExp.Lam(_, _, _)) -> None
      | (_, UHExp.NumLit _) -> None
      | (_, UHExp.BoolLit _) -> None
      | (0, UHExp.Inj(side, e1)) -> 
        begin match follow_e (xs, cursor_side) e1 with 
        | Some ze -> Some (ZExp.Deeper(err_status, (ZExp.InjZ(side, ze))))
        | None -> None
        end
      | (_, UHExp.Inj(_, _)) -> None
      | (_, UHExp.ListNil) -> None
      (* | (n, UHExp.ListLit es) -> 
        begin match ZList.split_at n es with 
        | None -> None
        | Some esz -> 
          begin match ZList.optmap_z (follow_e (xs, cursor_side)) esz with 
          | None -> None
          | Some zes -> 
            Some (ZExp.Deeper err_status (ZExp.ListLitZ zes))
          end
        end*)
      | (0, UHExp.Case(e1, rules)) -> 
        begin match follow_e (xs, cursor_side) e1 with 
        | Some ze -> Some (ZExp.Deeper(err_status, (ZExp.CaseZE(ze, rules))))
        | None -> None
        end
      | (x, UHExp.Case(e1, rules)) -> 
        begin match ZList.split_at x rules with 
        | None -> None
        | Some split_rules -> 
          begin match ZList.optmap_z (follow_rule (xs, cursor_side)) split_rules with 
          | None -> None
          | Some zrules -> 
            Some (ZExp.Deeper(err_status, (ZExp.CaseZR(e1, zrules))))
          end
        end
      | (n, UHExp.OpSeq(skel, seq)) -> 
        begin match OperatorSeq.split n seq with 
        | Some (e, surround) -> 
            begin match follow_e (xs, cursor_side) e with 
            | Some ze -> 
                Some (ZExp.Deeper(err_status, (ZExp.OpSeqZ(skel, ze, surround))))
            | None -> None
            end
        | None -> None
        end
      | (hole_ref, UHExp.ApPalette(name, serialized_model, hole_data)) -> 
        let (next_hole_ref, holemap) = hole_data in 
        begin match NatMap.drop holemap hole_ref with
        | None -> None
        | Some (holemap', te) ->
          let (ty, e') = te in 
          begin match follow_e (xs, cursor_side) e' with 
          | None -> None
          | Some ze -> 
            let zholemap = (holemap', (hole_ref, (ty, ze))) in 
            let zholedata = (next_hole_ref, zholemap) in 
            Some (ZExp.Deeper(NotInHole, (ZExp.ApPaletteZ(name, serialized_model, zholedata))))
          end
        end
      end
    end
  end
and follow_rule (path : t) (rule : UHExp.rule) : ZExp.zrule option = 
  begin match rule with 
  | UHExp.Rule(p, e) -> 
    begin match path with 
    | ([], _) -> None
    | (0 :: xs, cursor_side) -> 
      begin match follow_pat (xs, cursor_side) p with 
      | None -> None
      | Some zp -> Some (ZExp.RuleZP(zp, e))
      end
    | (1 :: xs, cursor_side) -> 
      begin match follow_e (xs, cursor_side) e with 
      | None -> None
      | Some ze -> Some (ZExp.RuleZE(p, ze))
      end
    | (_ :: _, _) -> None
    end
  end

let cons_opt (n : nat) (x : nat list option) : nat list option = 
  begin match x with 
  | None -> None
  | Some xs -> Some (n :: xs)
  end

let cons_opt2 
  (n1 : nat) (x1 : nat list option)
  (n2 : nat) (x2 : unit -> nat list option)
  : nat list option = 
    begin match x1 with 
    | Some xs -> Some (n1 :: xs)
    | None -> 
      begin match x2 () with 
      | Some xs -> Some (n2 :: xs)
      | None -> None
      end
    end

let cons_opt3 
  (n1 : nat) (x1 : nat list option)
  (n2 : nat) (x2 : unit -> nat list option)
  (n3 : nat) (x3 : unit -> nat list option)
  : nat list option = 
    begin match x1 with 
    | Some xs -> Some (n1 :: xs)
    | None -> 
      begin match x2 () with 
      | Some xs -> Some (n2 :: xs)
      | None -> 
        begin match x3 () with 
        | Some xs -> Some (n3 :: xs)
        | None -> None
        end
      end
    end

let rec steps_to_hole_pat (p : UHPat.t) (u : MetaVar.t) : nat list option = 
  begin match p with 
  | UHPat.Pat(_, (UHPat.EmptyHole u')) -> 
    if MetaVar.eq u u' then 
      Some []
    else None
  | UHPat.Parenthesized p1 -> 
    cons_opt 0 (steps_to_hole_pat p1 u)
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, (UHPat.Var _)) 
  | UHPat.Pat(_, (UHPat.NumLit _)) 
  | UHPat.Pat(_, (UHPat.BoolLit _))
  | UHPat.Pat(_, UHPat.ListNil) -> None
  (* | UHPat.Pat _ (UHPat.ListLit ps) -> 
    Util.findmapi ps (fun i p -> 
      begin match steps_to_hole_pat p u with 
      | None -> None
      | Some ns -> Some (i :: ns)
      end *)
  | UHPat.Pat(_, (UHPat.Inj(_, p1))) -> 
    cons_opt 0 (steps_to_hole_pat p1 u)
  | UHPat.Pat(_, (UHPat.OpSeq(skel, seq))) -> 
    steps_to_hole_seq_pat seq u
  end
and steps_to_hole_seq_pat (seq : UHPat.opseq) (u : MetaVar.t) : nat list option =  
  begin match seq with 
  | OperatorSeq.ExpOpExp(p1, _, p2) -> 
    cons_opt2
      0 (steps_to_hole_pat p1 u)
      1 (fun _ -> steps_to_hole_pat p2 u)
  | OperatorSeq.SeqOpExp(seq1, op, p1) -> 
    begin match steps_to_hole_seq_pat seq1 u with 
    | (Some steps) as path -> path
    | None -> cons_opt (OperatorSeq.seq_length seq1) (steps_to_hole_pat p1 u)
    end
  end

let rec steps_to_hole (e : UHExp.t) (u : MetaVar.t) : nat list option = 
  begin match e with 
  | UHExp.Tm(_, (UHExp.EmptyHole u')) -> 
    if MetaVar.eq u u' then 
      Some []
    else None
  | UHExp.Parenthesized e1 ->   
    cons_opt 0 (steps_to_hole e1 u)
  | UHExp.Tm(_, (UHExp.Var(_, _)))
  | UHExp.Tm(_, (UHExp.NumLit _))
  | UHExp.Tm(_, (UHExp.BoolLit _)) -> None
  | UHExp.Tm(_, (UHExp.Asc(e1, _)))  
  | UHExp.Tm(_, (UHExp.Inj(_, e1))) -> 
    cons_opt 0 (steps_to_hole e1 u)
  | UHExp.Tm(_, UHExp.ListNil) -> None
  (* | UHExp.Tm _ (UHExp.ListLit es) -> 
    Util.findmapi es (fun i e -> 
      begin match steps_to_hole e u with 
      | None -> None
      | Some ns -> Some (i :: ns)
      end *)
  | UHExp.Tm(_, (UHExp.Lam(p, _, e1))) -> 
    cons_opt2 
      0 (steps_to_hole_pat p u)
      2 (fun _ -> steps_to_hole e1 u)
  | UHExp.Tm(_, (UHExp.Let(p, ann, e1, e2))) -> 
    cons_opt3 
      0 (steps_to_hole_pat p u)
      2 (fun _ -> steps_to_hole e1 u)
      3 (fun _ -> steps_to_hole e2 u)
  | UHExp.Tm(_, (UHExp.Case(e1, rules))) -> 
    begin match steps_to_hole e1 u with 
    | Some steps -> Some (0 :: steps) 
    | None -> 
      Util.findmapi rules (
        fun i rule -> 
          begin match rule with 
          | UHExp.Rule(p, e) -> 
            begin match steps_to_hole_pat p u with 
            | Some steps -> Some ((i + 1) :: (0 :: steps))
            | None -> 
              begin match steps_to_hole e u with 
              | Some steps -> Some ((i + 1) :: (1 :: steps))
              | None -> None
              end
            end
          end) 
    end
  | UHExp.Tm(_, (UHExp.OpSeq(skel, seq))) -> 
    steps_to_hole_seq seq u  
  | UHExp.Tm(_, (UHExp.ApPalette(_, _, holedata))) -> 
    let (_, holemap) = holedata in 
    NatMap.fold holemap (fun c v -> 
      begin match c with 
      | Some _ -> c
      | None -> 
        let (_, te) = v in
        let (_, e) = te in 
        steps_to_hole e u 
      end
    ) None
  end
and steps_to_hole_seq (seq : UHExp.opseq) (u : MetaVar.t) : nat list option =  
  begin match seq with 
  | OperatorSeq.ExpOpExp(e1, _, e2) -> 
    cons_opt2
      0 (steps_to_hole e1 u)
      1 (fun _ -> steps_to_hole e2 u)
  | OperatorSeq.SeqOpExp(seq1, op, e1) -> 
    begin match steps_to_hole_seq seq1 u with 
    | (Some steps) as path -> path
    | None -> cons_opt (OperatorSeq.seq_length seq1) (steps_to_hole e1 u)
    end
  end

let path_to_hole (e : UHExp.t) (u : MetaVar.t) : t option = 
  begin match steps_to_hole e u with 
  | Some steps -> Some (steps, Before)
  | None -> None
  end

let rec first_hole_steps_ty (uty : UHTyp.t) : nat list option =
  begin match uty with
  | UHTyp.Parenthesized uty' -> cons_opt 0 (first_hole_steps_ty uty')
  | UHTyp.Unit
  | UHTyp.Num 
  | UHTyp.Bool -> None
  | UHTyp.Hole -> Some []
  | UHTyp.List uty1 -> cons_opt 0 (first_hole_steps_ty uty1)
  | UHTyp.OpSeq(_, opseq) -> first_hole_steps_ty_opseq opseq 0
  end
(* return an optional path of the first hole in opseq starting and the nth term *)
and first_hole_steps_ty_opseq (opseq : UHTyp.opseq) (n : nat) : nat list option =
    if (OperatorSeq.seq_length opseq) <= n
    then None
    else
      begin match OperatorSeq.seq_nth n opseq with
      | None -> None (* degenerate case *)
      | Some uty' ->
        begin match first_hole_steps_ty uty' with
        | Some ns -> Some (n :: ns)
        | None -> first_hole_steps_ty_opseq opseq (n+1)
        end
      end

let rec first_hole_steps_pat (p : UHPat.t) : nat list option = 
  begin match p with 
  | UHPat.Parenthesized p1 -> cons_opt 0 (first_hole_steps_pat p1)
  | UHPat.Pat(_, (UHPat.EmptyHole _)) -> Some []
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, (UHPat.Var _))
  | UHPat.Pat(_, (UHPat.NumLit _)) 
  | UHPat.Pat(_, (UHPat.BoolLit _)) -> None
  | UHPat.Pat(_, (UHPat.Inj(_, p1))) -> cons_opt 0 (first_hole_steps_pat p1)
  | UHPat.Pat(_, UHPat.ListNil) -> None
  (* | UHPat.Pat _ (UHPat.ListLit ps) -> 
    Util.findmapi ps (fun i p -> 
      begin match first_hole_steps_pat p with 
      | None -> None
      | Some ns -> Some (i :: ns)
      end *)
  | UHPat.Pat(_, (UHPat.OpSeq(_, seq))) -> first_hole_steps_opseq_pat seq 0
  end
and first_hole_steps_opseq_pat (opseq : UHPat.opseq) (n : nat) : nat list option =
    if (OperatorSeq.seq_length opseq) <= n
    then None
    else
      begin match OperatorSeq.seq_nth n opseq with
      | None -> None
      | Some ue ->
        begin match first_hole_steps_pat ue with
        | Some ns -> Some (n :: ns)
        | None -> first_hole_steps_opseq_pat opseq (n+1)
        end
      end

let rec first_hole_steps (ue : UHExp.t) : nat list option =
  begin match ue with
  | UHExp.Parenthesized ue1 -> cons_opt 0 (first_hole_steps ue1)
  | UHExp.Tm(_, ue') ->
    begin match ue' with
    | UHExp.EmptyHole _ -> Some []
    | UHExp.Asc(ue1, uty) ->
      cons_opt2 
        0 (first_hole_steps ue1)
        1 (fun _ -> first_hole_steps_ty uty)
    | UHExp.Var(_, _) -> None
    | UHExp.Let(p, ann, ue1, ue2) ->
      begin match first_hole_steps_pat p with 
      | Some ns -> Some (0 :: ns)
      | None -> 
        begin match ann with 
        | Some ann_ty -> 
          cons_opt3 
            1 (first_hole_steps_ty ann_ty)
            2 (fun _ -> first_hole_steps ue1)
            3 (fun _ -> first_hole_steps ue2)
        | None -> 
          cons_opt2
            2 (first_hole_steps ue1)
            3 (fun _ -> first_hole_steps ue2)
        end
      end
    | UHExp.Lam(p, ann, e1) -> 
      begin match first_hole_steps_pat p with 
      | Some ns -> Some (0 :: ns)
      | None -> 
        begin match ann with 
        | Some uty -> 
          cons_opt2 
            1 (first_hole_steps_ty uty)
            2 (fun _ -> first_hole_steps e1)
        | None -> cons_opt 2 (first_hole_steps e1)
        end
      end
    | UHExp.NumLit _ -> None
    | UHExp.BoolLit _ -> None
    | UHExp.ListNil -> None
    (* | UHExp.ListLit es -> 
      Util.findmapi es (fun i e -> 
        begin match first_hole_steps e with 
        | None -> None
        | Some ns -> Some (i :: ns)
        end *)
    | UHExp.Inj(_, e1) -> cons_opt 0 (first_hole_steps e1)
    | UHExp.Case(e1, rules) ->
      begin match first_hole_steps e1 with
      | Some ns -> Some (0 :: ns)
      | None -> first_hole_steps_rules rules
      end
    | UHExp.OpSeq(_, opseq) -> first_hole_steps_opseq opseq 0
    | UHExp.ApPalette(_, _, _) -> None (* TODO figure out tab order protocol *)
    end
  end
and first_hole_steps_rules (rules : UHExp.rules) : nat list option = 
    Util.findmapi rules (
      fun i rule -> 
        begin match rule with 
        | UHExp.Rule(p, e) -> 
          begin match first_hole_steps_pat p with 
          | Some ns -> Some ((i + 1)::0::ns)
          | None -> 
            begin match first_hole_steps e with 
            | Some ns -> Some ((i + 1)::1::ns)
            | None -> None
            end
          end
        end)
(* return an optional path of the first hole in opseq starting and the nth term )*)
and first_hole_steps_opseq (opseq : UHExp.opseq) (n : nat) : nat list option =
    if (OperatorSeq.seq_length opseq) < n
    then None
    else
      begin match OperatorSeq.seq_nth n opseq with
      | None -> None
      | Some ue ->
        begin match first_hole_steps ue with
        | Some ns -> Some (n :: ns)
        | None -> first_hole_steps_opseq opseq (n+1)
        end
      end

let rec next_hole_steps_ty (zty : ZTyp.t) : nat list option =
  begin match zty with
  | ZTyp.CursorT(cursor_side, uty) ->
    begin match cursor_side, uty with
    | _, UHTyp.Hole -> None
    | Before, _ -> first_hole_steps_ty uty
    | After, _ -> None
    | In _, _ -> None
    end
  | ZTyp.ParenthesizedZ zty' -> cons_opt 0 (next_hole_steps_ty zty')
  | ZTyp.ListZ zty1 -> cons_opt 0 (next_hole_steps_ty zty1) 
  | ZTyp.OpSeqZ(_, zty', surround) ->
    let n = OperatorSeq.surround_prefix_length surround in
    begin match next_hole_steps_ty zty' with
    | Some ns -> Some (n :: ns)
    | None ->
      let uty' = ZTyp.erase zty' in
      let opseq = OperatorSeq.opseq_of_exp_and_surround uty' surround in
      first_hole_steps_ty_opseq opseq (n+1)
    end
  end

let rec next_hole_path_ty (zty : ZTyp.t) : t option =
  begin match next_hole_steps_ty zty with
  | None -> None
  | Some path -> Some (path, Before)
  end

let rec next_hole_steps_pat (zp : ZPat.t) : nat list option = 
  begin match zp with 
  | ZPat.ParenthesizedZ zp1 -> cons_opt 0 (next_hole_steps_pat zp1)
  | ZPat.CursorP(cursor_side, p) -> 
    begin match cursor_side, p with 
    | _, (UHPat.Pat(_, (UHPat.EmptyHole _))) -> None
    | After, _ -> None
    | Before, _ -> first_hole_steps_pat p
    | In k, _ -> 
      begin match p with 
      | UHPat.Parenthesized _ -> None
      | UHPat.Pat(err, p') -> 
        begin match p' with 
        | UHPat.Wild
        | UHPat.Var _
        | UHPat.NumLit _
        | UHPat.BoolLit _ 
        | UHPat.ListNil 
        (* | UHPat.ListLit _ *)
        | UHPat.OpSeq(_, _) -> None
        | UHPat.Inj(_, p1) -> first_hole_steps_pat p1 
        | UHPat.EmptyHole _ -> None
        end
      end
    end
  | ZPat.Deeper(_, (ZPat.InjZ(_, zp1))) -> cons_opt 0 (next_hole_steps_pat zp1)
  (* | ZPat.Deeper _ (ZPat.ListLitZ zps) -> 
    let prefix_length = ZList.prefix_length zps in 
    let zp0 = ZList.prj_z zps in 
    begin match next_hole_steps_pat zp0 with 
    | Some ns -> 
      Some (prefix_length :: ns)
    | None ->
      let suffix = ZList.prj_suffix zps in 
      Util.findmapi suffix (fun i p -> 
        begin match first_hole_steps_pat p with 
        | None -> None
        | Some ns -> Some (cons (prefix_length + i + 1) ns)
        end
    end*)
  | ZPat.Deeper(_, (ZPat.OpSeqZ(_, zp1, surround))) ->
    let n = OperatorSeq.surround_prefix_length surround in
    begin match next_hole_steps_pat zp1 with
    | Some ns -> Some (n :: ns)
    | None ->
      let p = ZPat.erase zp1 in
      let opseq = OperatorSeq.opseq_of_exp_and_surround p surround in
      first_hole_steps_opseq_pat opseq (n+1)
    end
  end

let rec next_hole_path_pat (zp : ZPat.t) : t option =
  begin match next_hole_steps_pat zp with
  | None -> None
  | Some path -> Some (path, Before)
  end

let rec next_hole_steps (ze : ZExp.t) : nat list option =
  begin match ze with
  | ZExp.CursorE(cursor_side, ue) ->
    begin match cursor_side, ue with
    | _, (UHExp.Tm(_, (UHExp.EmptyHole _))) -> None
    | After, _ -> None
    | Before, _ -> first_hole_steps ue
    | In k, _ ->
      begin match ue with
      | UHExp.Parenthesized _ -> None
      | UHExp.Tm(err, ue') ->
        begin match ue' with
        | UHExp.Asc(_, uty) -> cons_opt 1 (first_hole_steps_ty uty)
        | UHExp.Var(_, _) -> None
        | UHExp.Let(p, ann, ue1, ue2) -> 
          first_hole_steps ue 
        | UHExp.Lam(_, _, _) -> 
          first_hole_steps ue
        | UHExp.NumLit _
        | UHExp.BoolLit _
        | UHExp.ListNil 
        (* | UHExp.ListLit _ *) -> None
        | UHExp.Inj(_, ue'') -> 
          first_hole_steps ue 
        | UHExp.Case(e1, rules) -> 
          begin match k with 
          | 0 -> first_hole_steps ue
          | 1 -> None
          | _ -> None
          end
        | UHExp.EmptyHole _ -> None
        | UHExp.OpSeq(_, _) -> None
        | UHExp.ApPalette(_, _, _) -> None (* TODO(move, into, palette, holes) *)
        end
      end
    end
  | ZExp.Deeper(_, ze') ->
    begin match ze' with
    | ZExp.AscZ1(ze'', uty) ->
      cons_opt2
        0 (next_hole_steps ze'')
        1 (fun _ -> first_hole_steps_ty uty)
    | ZExp.AscZ2(_, zty) -> cons_opt 1 (next_hole_steps_ty zty)
    | ZExp.LetZP(zp, ann, ue1, ue2) -> 
      begin match next_hole_steps_pat zp with 
      | Some ns -> Some (0 :: ns)
      | None -> 
        begin match ann with 
        | Some ann_ty -> 
          cons_opt3 
            1 (first_hole_steps_ty ann_ty) 
            2 (fun _ -> first_hole_steps ue1) 
            3 (fun _ -> first_hole_steps ue2)
        | None -> 
          cons_opt2 
            2 (first_hole_steps ue1)
            3 (fun _ -> first_hole_steps ue2)
        end
      end
    | ZExp.LetZA(_, zann, e1, e2) -> 
      cons_opt3 
        1 (next_hole_steps_ty zann)
        2 (fun _ -> first_hole_steps e1) 
        3 (fun _ -> first_hole_steps e2)
    | ZExp.LetZE1(_, _, ze1, e2) ->
      cons_opt2
        2 (next_hole_steps ze1)
        3 (fun _ -> first_hole_steps e2)
    | ZExp.LetZE2(_, _, _, ze2) -> 
      cons_opt 
        3 (next_hole_steps ze2)
    | ZExp.LamZP(zp, ann, e1) -> 
      begin match next_hole_steps_pat zp with 
      | Some ns -> Some (0 :: ns)
      | None -> 
        begin match ann with 
        | Some uty -> 
          cons_opt2 
            1 (first_hole_steps_ty uty)
            2 (fun _ -> first_hole_steps e1)
        | None -> 
           cons_opt 
             2 (first_hole_steps e1)
        end
      end
    | ZExp.LamZA(_, zann, e1) -> 
      cons_opt2
        1 (next_hole_steps_ty zann)
        2 (fun _ -> first_hole_steps e1)
    | ZExp.LamZE(_, _, ze1) -> 
      cons_opt 
        2 (next_hole_steps ze1)
    | ZExp.InjZ(_, ze'') -> 
      cons_opt 
        0 (next_hole_steps ze'')
    (* | ZExp.ListLitZ zes -> 
      let prefix_length = ZList.prefix_length zes in 
      let ze0 = ZList.prj_z zes in 
      begin match next_hole_steps ze0 with 
      | Some ns -> 
        Some (prefix_length :: ns)
      | None ->
        let suffix = ZList.prj_suffix zes in 
        Util.findmapi suffix (fun i e -> 
          begin match first_hole_steps e with 
          | None -> None
          | Some ns -> Some (cons (prefix_length + i + 1) ns)
          end
      end*)
    | ZExp.CaseZE(ze1, rules) ->
      begin match next_hole_steps ze1 with
      | Some ns -> Some (0 :: ns)
      | None -> first_hole_steps_rules rules
      end
    | ZExp.CaseZR(_, zrules) -> 
      let zr = ZList.prj_z zrules in 
      let prefix_len = List.length (ZList.prj_prefix zrules) in 
      begin match zr with 
      | ZExp.RuleZP(zp, e) -> 
        begin match next_hole_steps_pat zp with 
        | Some ns -> Some ((prefix_len + 1)::0::ns)
        | None -> 
          begin match first_hole_steps e with 
          | Some ns -> Some ((prefix_len + 1)::1 :: ns)
          | None -> 
            let suffix = ZList.prj_suffix zrules in 
            begin match first_hole_steps_rules suffix with 
            | Some (offset :: ns) -> Some ((prefix_len + offset + 1)::ns)
            | Some [] -> None (* should never happen *)
            | None -> None
            end
          end
        end
      | ZExp.RuleZE(_, ze) -> 
        begin match next_hole_steps ze with 
        | Some ns -> Some ((prefix_len + 1)::1::ns)
        | None -> 
          let suffix = ZList.prj_suffix zrules in 
          begin match first_hole_steps_rules suffix with 
          | Some (offset :: ns) -> Some ((prefix_len + offset + 1)::ns)
          | Some [] -> None (* should never happen *)
          | None -> None
          end
        end
      end
    | ZExp.OpSeqZ(_, ze'', surround) ->
      let n = OperatorSeq.surround_prefix_length surround in
      begin match next_hole_steps ze'' with
      | Some ns -> Some (n :: ns)
      | None ->
        let ue'' = ZExp.erase ze'' in
        let opseq = OperatorSeq.opseq_of_exp_and_surround ue'' surround in
        first_hole_steps_opseq opseq (n+1)
      end
    | ZExp.ApPaletteZ(_, _, _) -> None (* TODO(figure, out, tab, order) protocol *)
    end
  | ZExp.ParenthesizedZ ze' -> cons_opt 0 (next_hole_steps ze')
  end

let rec next_hole_path (ze : ZExp.t) : t option =
  begin match next_hole_steps ze with
  | None -> None
  | Some path -> Some (path, Before)
  end

let rec last_hole_steps_ty (uty : UHTyp.t) : nat list option =
  begin match uty with
  | UHTyp.Hole -> Some []
  | UHTyp.Parenthesized uty' -> cons_opt 0 (last_hole_steps_ty uty')
  | UHTyp.Unit
  | UHTyp.Num 
  | UHTyp.Bool -> None
  | UHTyp.List uty1 -> cons_opt 0 (last_hole_steps_ty uty1)
  | UHTyp.OpSeq(_, opseq) -> last_hole_steps_ty_opseq opseq 0
  end
(* return an optional path of the last hole in opseq starting and the mth term from the end
   (e.g., the 0th and 1st term from the endof `1 + 2 + 3` are 3 and 2 respectively) *)
and last_hole_steps_ty_opseq (opseq : UHTyp.opseq) (m : nat) : nat list option =
    let l = OperatorSeq.seq_length opseq in
    if l < m
    then None
    else
      let n = l-m-1 in
      begin match OperatorSeq.seq_nth n opseq with
      | None -> None (* degenerate case *)
      | Some uty' ->
        begin match last_hole_steps_ty uty' with
        | Some ns -> Some (n :: ns)
        | None -> last_hole_steps_ty_opseq opseq (m+1)
        end
      end

let rec last_hole_steps_pat (p : UHPat.t) : nat list option = 
  begin match p with 
  | UHPat.Parenthesized p1 -> cons_opt 0 (last_hole_steps_pat p1)
  | UHPat.Pat(_, (UHPat.EmptyHole _)) -> Some []
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, (UHPat.Var _))
  | UHPat.Pat(_, (UHPat.NumLit _)) 
  | UHPat.Pat(_, (UHPat.BoolLit _)) -> None
  | UHPat.Pat(_, (UHPat.Inj(_, p1))) -> cons_opt 0 (last_hole_steps_pat p1)
  | UHPat.Pat(_, UHPat.ListNil) -> None
  (* | UHPat.Pat _ (UHPat.ListLit ps) -> 
    let num_elts = List.length ps in 
    Util.findmapi ps (fun i p -> 
      begin match last_hole_steps_pat p with 
      | None -> None
      | Some ns -> Some (cons (num_elts - i - 1) ns)
      end *)
  | UHPat.Pat(_, (UHPat.OpSeq(_, opseq))) -> last_hole_steps_opseq_pat opseq 0
  end
and last_hole_steps_opseq_pat (opseq : UHPat.opseq) (m : nat) : nat list option =
    let l = OperatorSeq.seq_length opseq in
    if l < m
    then None
    else
      let n = l-m-1 in
      begin match OperatorSeq.seq_nth n opseq with
      | None -> None
      | Some ue ->
        begin match last_hole_steps_pat ue with
        | Some ns -> Some (n :: ns)
        | None -> last_hole_steps_opseq_pat opseq (m+1)
        end
      end

let rec last_hole_steps (ue : UHExp.t) : nat list option =
  begin match ue with
  | UHExp.Parenthesized ue' -> cons_opt 0 (last_hole_steps ue')
  | UHExp.Tm(_, ue') ->
    begin match ue' with
    | UHExp.EmptyHole _ -> Some []
    | UHExp.Asc(ue0, uty1) ->
      cons_opt2 
        1 (last_hole_steps_ty uty1)
        0 (fun _ -> last_hole_steps ue0)
    | UHExp.Var(_, _) -> None
    | UHExp.Let(p, ann, e1, e2) ->
      begin match last_hole_steps e2 with
      | Some ns -> Some (3 :: ns)
      | None -> 
        begin match last_hole_steps e1 with 
        | Some ns -> Some (2 :: ns) 
        | None -> 
          begin match ann with 
          | Some ann_ty -> 
            cons_opt2
              1 (last_hole_steps_ty ann_ty)
              0 (fun _ -> last_hole_steps_pat p)
          | None -> 
            cons_opt 0 (last_hole_steps_pat p)
          end
        end
      end
    | UHExp.Lam(p, ann, e1) -> 
      begin match last_hole_steps e1 with
      | Some ns -> Some (2 :: ns)
      | None -> 
        begin match ann with 
        | Some uty1 -> 
          cons_opt2
            1 (last_hole_steps_ty uty1)
            0 (fun _ -> last_hole_steps_pat p)
        | None -> 
          cons_opt 0 (last_hole_steps_pat p)
        end
      end
    | UHExp.NumLit _ 
    | UHExp.BoolLit _ -> None
    | UHExp.Inj(_, ue0) -> cons_opt 0 (last_hole_steps ue0)
    | UHExp.ListNil -> None
    (* | UHExp.ListLit es -> 
      let num_elts = List.length es in 
      Util.findmapi es (fun i e -> 
        begin match last_hole_steps e with 
        | None -> None
        | Some ns -> Some (cons (num_elts - i - 1) ns)
        end *)
    | UHExp.Case(e1, rules) ->
      begin match last_hole_steps_rules rules with 
      | (Some ns) as result -> result
      | None -> cons_opt 0 (last_hole_steps e1)
      end
    | UHExp.OpSeq(_, opseq) -> last_hole_steps_opseq opseq 0
    | UHExp.ApPalette(_, _, _) -> None (* TODO(figure, out, tab, order) protocol *)
    end
  end
and last_hole_steps_rules 
  (rules : UHExp.rules)
  : nat list option =  
      let n_rules = List.length rules in 
      Util.findmapi (List.rev rules) (
        fun i rule -> 
          begin match rule with 
          | UHExp.Rule(p, e) -> 
            begin match last_hole_steps e with 
            | Some ns -> Some ((n_rules - i)::1 :: ns)
            | None -> 
              begin match last_hole_steps_pat p with 
              | Some ns -> Some ((n_rules - i)::0 :: ns)
              | None -> None
              end
            end
          end)
(* return an optional path of the last hole in opseq starting and the mth term from the end
   (e.g., the 0th and 1st term from the endof `1 + 2 + 3` are 3 and 2 respectively) *)
and last_hole_steps_opseq (opseq : UHExp.opseq) (m : nat) : nat list option =
    let l = OperatorSeq.seq_length opseq in
    if l < m
    then None
    else
      let n = l-m-1 in
      begin match OperatorSeq.seq_nth n opseq with
      | None -> None
      | Some ue ->
        begin match last_hole_steps ue with
        | Some ns -> Some (n :: ns)
        | None -> last_hole_steps_opseq opseq (m+1)
        end
      end

let rec prev_hole_steps_ty (zty : ZTyp.t) : nat list option =
  begin match zty with
  | ZTyp.CursorT(cursor_side, uty) ->
    begin match cursor_side, uty with
    | _, UHTyp.Hole -> None
    | Before, _ -> None
    | After, _ -> last_hole_steps_ty uty
    | In _, _ -> None
    end
  | ZTyp.ParenthesizedZ zty' -> cons_opt 0 (prev_hole_steps_ty zty')
  | ZTyp.ListZ zty1 -> cons_opt 0 (prev_hole_steps_ty zty1)
  | ZTyp.OpSeqZ(_, zty', surround) ->
    let n = OperatorSeq.surround_prefix_length surround in
    begin match prev_hole_steps_ty zty' with
    | Some ns -> Some (n :: ns)
    | None ->
      let uty' = ZTyp.erase zty' in
      let opseq = OperatorSeq.opseq_of_exp_and_surround uty' surround in
      let m = OperatorSeq.surround_suffix_length surround in
      last_hole_steps_ty_opseq opseq (m+1)
    end
  end

let rec prev_hole_path_ty (zty : ZTyp.t) : t option =
  begin match prev_hole_steps_ty zty with
  | None -> None
  | Some path -> Some (path, Before)
  end

let rec prev_hole_steps_pat (zp : ZPat.t) : nat list option = 
  begin match zp with 
  | ZPat.ParenthesizedZ zp1 -> cons_opt 0 (prev_hole_steps_pat zp1)
  | ZPat.CursorP(cursor_side, p) -> 
    begin match cursor_side, p with 
    | _, (UHPat.Pat(_, (UHPat.EmptyHole _))) -> None
    | Before, _ -> None
    | After, _ -> last_hole_steps_pat p
    | In k, _ -> 
      begin match p with 
      | UHPat.Parenthesized _ -> None
      | UHPat.Pat(err, p') -> 
        begin match p' with 
        | UHPat.EmptyHole _ -> None
        | UHPat.Wild
        | UHPat.Var _
        | UHPat.NumLit _
        | UHPat.BoolLit _
        | UHPat.ListNil 
        (* | UHPat.ListLit _ *) -> None
        | UHPat.Inj(_, p1) -> None
        | UHPat.OpSeq(_, _) -> None
        end
      end
    end
  | ZPat.Deeper(_, (ZPat.InjZ(_, zp1))) -> cons_opt 0 (prev_hole_steps_pat zp1)
  (* | ZPat.Deeper _ (ZPat.ListLitZ ((prefix, zp0), _)) -> 
    let prefix_length = List.length prefix in 
    begin match prev_hole_steps_pat zp0 with 
    | Some ns -> Some (prefix_length :: ns)
    | None -> last_hole_steps_pat (UHPat.Pat NotInHole (UHPat.ListLit prefix))
    end*)
  | ZPat.Deeper(_, (ZPat.OpSeqZ(_, zp1, surround))) -> 
    let n = OperatorSeq.surround_prefix_length surround in
    begin match prev_hole_steps_pat zp1 with
    | Some ns -> Some (n :: ns)
    | None ->
      let ue_n = ZPat.erase zp1 in
      let opseq = OperatorSeq.opseq_of_exp_and_surround ue_n surround in
      let m = OperatorSeq.surround_suffix_length surround in
      last_hole_steps_opseq_pat opseq (m+1)
    end
  end

let rec prev_hole_path_pat (zp : ZPat.t) : t option =
  begin match prev_hole_steps_pat zp with
  | None -> None
  | Some path -> Some (path, Before)
  end

let rec prev_hole_steps (ze : ZExp.t) : nat list option =
  begin match ze with
  | ZExp.CursorE(cursor_side, ue) ->
    begin match cursor_side, ue with
    | _, (UHExp.Tm(_, (UHExp.EmptyHole _))) -> None
    | After, _ -> last_hole_steps ue
    | Before, _ -> None
    | In k, _ ->
      begin match ue with
      | UHExp.Parenthesized _ -> None (* cannot be In(Parenthesized, term) *)
      | UHExp.Tm(err, ue') ->
        begin match ue' with
        | UHExp.Asc(ue'', _) -> cons_opt 0 (last_hole_steps ue'')
        | UHExp.Var(_, _) -> None
        | UHExp.Let(_, _, _, _) -> None
        | UHExp.Lam(_, _, _) -> None
        | UHExp.NumLit _
        | UHExp.BoolLit _
        | UHExp.ListNil 
        (* | UHExp.ListLit _ *) -> None
        | UHExp.Inj(_, _) -> None
        | UHExp.Case(_, _) -> 
          begin match k with 
          | 0 -> None
          | 1 -> last_hole_steps ue
          | _ -> None
          end
        | UHExp.EmptyHole _ -> None
        | UHExp.OpSeq(_, _) -> None
        | UHExp.ApPalette(_, _, _) -> None (* TODO *)
        end
      end
    end
  | ZExp.Deeper(_, ze') ->
    begin match ze' with
    | ZExp.AscZ1(ze0, _) -> 
      cons_opt 0 (prev_hole_steps ze0)
    | ZExp.AscZ2(ue0, zty1) ->
      cons_opt2
        1 (prev_hole_steps_ty zty1)
        0 (fun _ -> last_hole_steps ue0)
    | ZExp.LetZP(zp, _, _, _) -> 
      cons_opt 0 (prev_hole_steps_pat zp) 
    | ZExp.LetZA(p, zann, _, _) -> 
      cons_opt2
        1 (prev_hole_steps_ty zann)
        0 (fun _ -> last_hole_steps_pat p)
    | ZExp.LetZE1(p, ann, ze1, _) -> 
      begin match prev_hole_steps ze1 with
      | Some ns -> Some (2 :: ns)
      | None -> 
        begin match ann with 
        | Some ann_ty -> 
          cons_opt2 
            1 (last_hole_steps_ty ann_ty)
            0 (fun _ -> last_hole_steps_pat p)
        | None -> 
          cons_opt 0 (last_hole_steps_pat p)
        end
      end
    | ZExp.LetZE2(p, ann, e1, ze2) -> 
      begin match prev_hole_steps ze2 with
      | Some ns -> Some (3 :: ns)
      | None -> 
        begin match last_hole_steps e1 with 
        | Some ns -> Some (2 :: ns)
        | None -> 
          begin match ann with 
          | Some ann_ty -> 
            cons_opt2
              1 (last_hole_steps_ty ann_ty)
              0 (fun _ -> last_hole_steps_pat p)
          | None -> None
          end
        end
      end
    | ZExp.LamZP(zp, _, _) -> prev_hole_steps_pat zp 
    | ZExp.LamZA(p, zann, _) -> 
      cons_opt2 
        1 (prev_hole_steps_ty zann)
        0 (fun _ -> last_hole_steps_pat p)
    | ZExp.LamZE(p, ann, ze1) -> 
      begin match prev_hole_steps ze1 with 
      | Some ns -> Some (2 :: ns)
      | None -> 
        begin match ann with 
        | Some uty1 -> 
          cons_opt2 
            1 (last_hole_steps_ty uty1)
            0 (fun _ -> last_hole_steps_pat p)
        | None -> cons_opt 0 (last_hole_steps_pat p)
        end
      end
    | ZExp.InjZ(_, ze0) -> cons_opt 0 (prev_hole_steps ze0)
    (* | ZExp.ListLitZ ((prefix, ze0), _) -> 
      let prefix_length = List.length prefix in 
      begin match prev_hole_steps ze0 with 
      | Some ns -> Some (prefix_length :: ns)
      | None -> last_hole_steps (UHExp.Tm NotInHole (UHExp.ListLit prefix))
      end*)
    | ZExp.CaseZE(ze, rules) -> 
      cons_opt 0 (prev_hole_steps ze)
    | ZExp.CaseZR(e1, zrules) -> 
      let zr = ZList.prj_z zrules in 
      let prefix = ZList.prj_prefix zrules in 
      let prefix_len = List.length prefix in 
      begin match zr with 
      | ZExp.RuleZP(zp, e) -> 
        begin match prev_hole_steps_pat zp with 
        | Some ns -> Some ((prefix_len + 1)::0 :: ns)
        | None ->
          begin match last_hole_steps_rules prefix with
          | Some ns -> Some ns
          | None -> cons_opt 0 (last_hole_steps e1)
          end
        end
      | ZExp.RuleZE(p, ze) -> 
        begin match prev_hole_steps ze with 
        | Some ns -> Some ((prefix_len + 1)::1 :: ns)
        | None -> 
          begin match last_hole_steps_pat p with 
          | Some ns -> Some ((prefix_len + 1)::0 :: ns)
          | None -> 
            begin match last_hole_steps_rules prefix with 
            | Some ns -> Some ns
            | None -> cons_opt 0 (last_hole_steps e1)
            end
          end
        end
      end
    | ZExp.OpSeqZ(_, ze_n, surround) ->
      let n = OperatorSeq.surround_prefix_length surround in
      begin match prev_hole_steps ze_n with
      | Some ns -> Some (n :: ns)
      | None ->
        let ue_n = ZExp.erase ze_n in
        let opseq = OperatorSeq.opseq_of_exp_and_surround ue_n surround in
        let m = OperatorSeq.surround_suffix_length surround in
        last_hole_steps_opseq opseq (m+1)
      end
    | ZExp.ApPaletteZ(_, _, _) -> None (* TODO(figure, out, tab, order) protocol *)
    end
  | ZExp.ParenthesizedZ ze0 -> cons_opt 0 (prev_hole_steps ze0)
  end

let rec prev_hole_path (ze : ZExp.t) : t option =
  begin match prev_hole_steps ze with
  | None -> None
  | Some path -> Some (path, Before)
  end

