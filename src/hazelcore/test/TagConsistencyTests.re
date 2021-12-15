let empty_tag_hole = (u: int): UHTag.t => EmptyTagHole(u);
let tag = (t: string): UHTag.t => Tag(NotInTagHole, t);
let tag_hole =
    (t: string, reason: TagErrStatus.HoleReason.t, u: MetaVar.t): UHTag.t =>
  Tag(InTagHole(reason, u), t);

let%test_module "tag consistency tests" =
  (module
   {
     ();

     let%test "? ~ ?" =
       UHTag.consistent(empty_tag_hole(1), empty_tag_hole(2));

     let%test "? ~ A" = UHTag.consistent(empty_tag_hole(1), tag("A"));
     let%test "A ~ ?" = UHTag.consistent(tag("A"), empty_tag_hole(1));

     let%test "A ~ A" = UHTag.consistent(tag("A"), tag("A"));

     let%test "A !~ B" = !UHTag.consistent(tag("A"), tag("B"));
     let%test "B !~ A" = !UHTag.consistent(tag("B"), tag("A"));

     let%test "a ~ a" =
       UHTag.consistent(
         tag_hole("a", InvalidName, 1),
         tag_hole("a", InvalidName, 2),
       );

     let%test "a ~ b" =
       UHTag.consistent(
         tag_hole("a", InvalidName, 1),
         tag_hole("b", InvalidName, 2),
       );
     let%test "b ~ a" =
       UHTag.consistent(
         tag_hole("b", InvalidName, 1),
         tag_hole("a", InvalidName, 2),
       );

     let%test "a ~ A" =
       UHTag.consistent(tag_hole("a", InvalidName, 1), tag("A"));
     let%test "A ~ a" =
       UHTag.consistent(tag("A"), tag_hole("a", InvalidName, 1));
   });
