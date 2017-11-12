{
  let vNum: HazelPrelude.num = HazelPrelude.EHole 0;
  let _plus: HazelPrelude.num => HazelPrelude.num => HazelPrelude.num =
    fun n_ n_2 => n_ + n_2;
  let l001:
    HazelPrelude.sum
      (HazelPrelude.num => HazelPrelude.num => HazelPrelude.num) HazelPrelude.num =
    HazelPrelude.L _plus;
  let r:
    HazelPrelude.sum
      (HazelPrelude.num => HazelPrelude.num => HazelPrelude.num) HazelPrelude.hole =
    HazelPrelude.R vNum;
  HazelPrelude.NEHole
    1
    (
      (
        _plus (
          switch l001 {
          | HazelPrelude.L l2 => (l2 7) 0
          | HazelPrelude.R r2 => r2
          }
        )
      ) 3
    )
  [@implicit_arity]
};
