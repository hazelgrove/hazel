let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         1364c852-d21c-4d28-b18f-718ec5e93666)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         ef7a9db2-1103-4456-9dd1-cdedbf7e7974)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b6cd541-ba32-4b41-bb27-51a3f4d3c9d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         b50f2b06-465b-4e06-8370-9d79c3e82dd4)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         7dc12e96-d431-4861-a408-f06fdc803303)(content(Whitespace\"\\n\"))))(Secondary((id \
         c418fa2d-2cdb-4358-839a-10cdfa577b80)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         0b7ec5d3-f3d7-469c-b150-7694dbb3b2f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         84cfd564-fee2-4f7c-9bbb-dc5fc8f4e01a)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         03035532-28a8-46d4-833d-24b91cea8d2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         30774d88-9c79-4113-bebf-7b84c5c3e9d1)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         947cf57d-25ef-40a7-9b2c-4ab65baa9db1)(content(Whitespace\"\\n\"))))(Secondary((id \
         30504328-cb57-42f3-acfb-0e24b118db60)(content(Whitespace\"\\n\"))))(Tile((id \
         d434ce37-62a6-42db-9d4f-32b27fd67ee6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         256c5d28-4371-40be-b6af-c43230c8b62d)(content(Whitespace\" \
         \"))))(Tile((id \
         a9866c47-bef1-4842-9d6d-e1c6f7fc88c0)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dc37aea6-988b-42b6-9188-671131060324)(content(Whitespace\" \
         \")))))((Secondary((id \
         5a3aff00-ea46-42d7-83c0-5e513d9bfdd6)(content(Whitespace\" \
         \"))))(Tile((id 7038a023-3c79-4f3b-9848-8a9d27e97066)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         479cb1ee-322f-4bb9-9fef-954f167a5a46)(content(Whitespace\" \
         \"))))(Tile((id \
         e31be637-f801-4c99-8b8e-c9ce71958377)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         731e58e0-43be-434b-88a2-4b13bd600331)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fed4a6d5-ef81-4dc0-bf40-f0ec687a1b9b)(content(Whitespace\"\\n\"))))(Tile((id \
         bf493bd1-2837-44f9-b35d-a9fa19dd441a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         29d447d0-2f65-4d02-8dc6-b1ad49bf605d)(content(Whitespace\" \
         \"))))(Tile((id \
         3101fe2b-8bc2-426a-9c1b-9218f726293a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7b0cb80c-9c0b-43a1-ad82-443a6b0959ef)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         eea0946b-8475-4da9-acf9-0bcf7e3c060f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a0ed4a3c-e32b-4836-a2bb-924ec4675b0c)(content(Whitespace\" \
         \"))))(Tile((id \
         341af5af-87fd-4b3b-b3a8-4acc22fbc5a4)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b5a132ec-777e-426e-a2f2-63f69ab7af03)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d0d40ef-7c42-4490-8a27-2e1755097dd2)(content(Whitespace\" \
         \"))))(Tile((id \
         bf6b5f74-fa9b-4f90-9d3b-d8eabde75401)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f9424f1-27e2-40c1-b034-14848211161f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         88116e9e-7c89-4b93-9126-40b0c2b306c8)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f832d15a-d434-4aa6-948d-9783442f6f17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         984b775c-63f1-41cd-bda9-a1758b9feb04)(content(Whitespace\"\\n\"))))(Tile((id \
         dad13edf-3d29-4a6a-93a2-276615d653cb)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4d675d6c-011d-460c-9ce6-b91e98b6488d)(content(Whitespace\" \
         \"))))(Tile((id \
         1815533c-533a-4b32-9fd2-e67ec1d430b1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         25e1797c-f48e-4b56-91b6-4c450a5cad47)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2e784e85-a08e-4c4d-9dca-5812276d3b62)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         31ceebef-6763-4996-9466-c2cc23204a5b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6372987f-2d75-431c-96f0-92a496118f3f)(content(Whitespace\" \
         \"))))(Tile((id \
         5edfdfb6-7ce9-4701-af92-78c81d8e9e91)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         786ddda1-d7c6-4225-9cff-8d85268b862f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9ca254b6-1c99-4660-af83-a4deeb989112)(content(Whitespace\" \
         \"))))(Tile((id \
         0f3f76ba-53ad-4b88-baff-26d691bf258b)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a6e587a0-5f90-4837-93e3-51bc42a0c27b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3528a68a-ed56-4429-947d-a610d59e754b)(content(Whitespace\"\\n\"))))(Tile((id \
         fd3999f3-019d-4b14-92f0-4b0d0a060b81)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b42b567a-202e-41e3-a484-a055e9dd783c)(content(Whitespace\" \
         \"))))(Tile((id \
         a6cc2922-0e65-4766-91aa-838259ccce22)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         93fe1fb3-956a-4ecb-8396-290e89069609)(content(Whitespace\" \
         \")))))((Secondary((id \
         41b6b72b-e08c-4ea1-9f65-b03719b440de)(content(Whitespace\" \
         \"))))(Tile((id \
         bd52b6a7-eca8-4d55-8622-ce84d6ffc378)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1e48dc17-7228-45af-b02d-e1f631d0caf8)(content(Whitespace\" \
         \"))))(Tile((id \
         283028b5-dab3-462f-9679-9b48829d602d)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4df779d7-663a-489d-b3bc-76e5f0f1b9b1)(content(Whitespace\" \
         \"))))(Tile((id \
         ba02b6f3-d617-4f83-bbeb-e5219bf5c0c8)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6808fe6b-6a32-456e-9bb2-af1b11836629)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ed9731fc-1f0d-45d7-8480-c868ce073db2)(content(Whitespace\"\\n\"))))(Tile((id \
         4888bc29-cc50-4089-b3bd-d29b45bc3492)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1e416938-9d23-4536-99c9-ee23715cfd8f)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca1c1f6d-6f93-43b0-9c8d-2f58b36e19de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b7f519f-cacd-4856-86b2-7da3f98ede8c)(content(Whitespace\" \
         \"))))(Tile((id \
         cbab5064-52b5-4772-8938-10d4538f59c5)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c181d8e-a057-475a-9016-ebe85f0b9b2d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         096b47ee-8e95-4157-ab48-a25e2fbafecd)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec514ae0-3ca5-4e0f-9389-557ef937d85a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e95eb4b-adad-470d-b5b4-18cf12d05135)(content(Whitespace\" \
         \"))))(Tile((id 4e891b4d-3195-4515-a87f-278a7719be77)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         409d8a08-761d-40dc-8f9b-fa173e9cc92d)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
         45310f23-70b4-42c1-9d4b-59cfd6fcfaca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         daa9425a-294e-4907-a3da-2b4a8bdbcf05)(content(Whitespace\"\\n\"))))(Tile((id \
         c192ec71-8cb6-44d1-94df-62cd9be735a4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8fd11a43-90e3-4f48-999a-63f636b52acb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4106a7ee-a2a3-42fa-98a1-3138e47ca31e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6511e412-1419-4852-9f4d-9b265d1eb5e1)(content(Whitespace\" \
         \"))))(Tile((id \
         9da1cf70-58a6-439e-bc1b-f725266c7d76)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1bee4ff0-7a5f-4335-ab94-4dfcb299141e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a85543c6-a87b-4240-8e32-68c3e66baf46)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         31462517-fd6c-462d-a1e5-b32fb5f6d00f)(content(Whitespace\"\\n\"))))(Tile((id \
         f228b0f3-480e-46f1-8244-ab6247337d7d)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b9e9fcb-50c1-459f-b1a5-bdaaa11a4915)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         890e3e49-a011-4be1-bb14-5580036aa556)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5407df8-10a3-485a-a0ac-3640af596f8e)(content(Whitespace\"\\n\"))))(Tile((id \
         af925415-5c8f-478e-987e-f5b14b0dab91)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bea69481-8f80-47db-a47c-c1c9da2db19d)(content(Whitespace\"\\n\"))))(Tile((id \
         14a50e67-0160-4c07-b46e-8983ec0bd251)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1fcdcdf-5b92-436e-82a7-7455bdacdd9e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6c10284d-6c00-4bd7-8b06-7c305269c160)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4de5704b-5a85-4287-be27-231dc25cc41b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5795be3-9877-4318-b21f-14a6436dcd25)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a13c863-b087-4d58-8b40-91189f53b8f6)(content(Whitespace\" \
         \"))))(Tile((id \
         1a31eeb5-e62c-4184-a3d2-e0a699facdbc)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         37384764-0215-45ab-a9a7-0a6da795bcba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd2ebe03-5c26-4ce0-8db9-27ea2293c280)(content(Whitespace\" \
         \"))))(Tile((id \
         c52971b5-c918-4120-984d-b48a28e6c73f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fcdf1138-4231-4094-b98e-9063e2365d30)(content(Whitespace\"\\n\"))))(Tile((id \
         46980e5a-7bf5-40f3-86bb-cede00a9ef13)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0278109-4112-4d7a-91d8-d74417af3aca)(content(Whitespace\" \
         \"))))(Tile((id 37c3a566-d886-4bbe-85dd-af1eb01d3b0c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6368817c-7878-4ec5-a22e-10cdf11fe82b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41811892-1dbc-4f95-a487-313aa22005b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f338ecc0-6516-416e-83e7-7fd64c4285c4)(content(Whitespace\" \
         \"))))(Tile((id \
         5474c0a9-9744-41d0-a0e7-046e21523607)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bcfdcefa-858c-4358-b634-6ef873b17d4f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed2a10d4-6abc-41b4-bf88-623f5a69ff47)(content(Whitespace\" \
         \"))))(Tile((id \
         5a1936f4-44a5-4fda-8d39-babf6bc96f3f)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         96ea9b9a-1e93-4539-a1a2-5f9dda013c61)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a3191cf7-4cf4-48e7-8cc3-bb365b24bc2e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         56c897d6-362c-4b5e-b981-e224c8d80ec5)(content(Whitespace\"\\n\"))))(Secondary((id \
         3dd7f3d6-e18f-43cf-8c82-bd55227b477e)(content(Whitespace\"\\n\"))))(Tile((id \
         ea3ec154-bbd2-4fff-a3cc-e7a4504e184c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3dbac293-a40f-4f31-8b28-7dc7f4a77913)(content(Whitespace\"\\n\"))))(Tile((id \
         234c4f01-e44d-4668-ba0f-186e8a43161e)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94ba2bfb-b97f-49b3-8dce-0a3ced1ee3b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6ee6461-594b-48b5-a3f3-350bffc1d06d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1c2958ef-dea9-42ad-a261-b129b36b08cc)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8f5958b3-7939-459a-8e33-6e5792a4d4ce)(content(Whitespace\"\\n\"))))(Tile((id \
         fc6fb261-4786-486b-8fdd-7e4289e1541c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         efa53959-640b-460f-b272-282304f25a14)(content(Whitespace\" \
         \"))))(Tile((id 72b05aba-2102-4d32-a22a-f5816e881d03)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         47c60cbf-e681-4904-9de3-9ee6079bb51e)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b4c71a64-524a-4e7e-92a1-833132293979)(content(Whitespace\"\\n\")))))))))(Tile((id \
         463c325b-b985-43f8-baf7-5e081373ce4e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6174938d-f9ba-48dc-aa02-46b459002167)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0e3cf76-b434-4880-97d4-376b0b3beff2)(content(Whitespace\"\\n\"))))(Tile((id \
         4992a80d-7e4d-4651-a2d7-9f21bdc107bc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ddb54741-1f2b-4c41-872a-fbf360fb5a95)(content(Whitespace\"\\n\"))))(Tile((id \
         2dd01c1b-7f04-4a2f-b332-8a99c3a058a2)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3f7092f-1594-4697-92f8-f57cf048f532)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         763c8ac3-81bc-4e46-9300-75ba563f428d)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cb2e408d-5179-4493-93af-1e39d9ae2c84)(content(Whitespace\"\\n\"))))(Tile((id \
         eae61ddb-ac74-4ad4-8e0e-37daf7659930)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c25b0905-fd5b-43c6-8834-0505ed727b2d)(content(Whitespace\" \
         \"))))(Tile((id \
         4a52670e-d890-45ac-b3d2-0ac4c1dd651c)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         02a87353-c09a-4ef1-a8a8-e8d2f938228c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5445feb4-c6ae-47cb-8f83-07664b3d6ff2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46ef8ac2-1c73-49c8-b830-da255f3c5194)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fd67d95-6b20-43b6-bd03-3b6f6feb1558)(content(Whitespace\"\\n\"))))(Tile((id \
         51f5c9d1-c8f3-43dd-b8a6-4c8327c9d1df)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3c6788ed-3099-4d9d-bf6e-dc492ded4f6e)(content(Whitespace\"\\n\"))))(Tile((id \
         889d5e95-3d56-4caf-af50-3a68696a8995)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d5b5eb47-2c75-460b-b741-d83ea3393fe7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9504d561-ceac-40bd-9360-9806946f3441)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         36a17f14-9d7e-4485-b359-a2e590542916)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c13f9ec1-48c1-4773-99aa-afb05921c71a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1ff4aea-735a-4219-af07-ddb586b37445)(content(Whitespace\" \
         \"))))(Tile((id \
         313a9547-afa8-4405-9f95-1fd6aee4d809)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d6839b5-16bd-4c53-950a-03a05b75d41e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c47b7e47-b7fd-41b3-87f3-d80359646919)(content(Whitespace\" \
         \"))))(Tile((id \
         cd1fcae7-12af-46a2-979c-9c5c37a11eb1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a30cb804-a44b-4014-a6e5-577266645993)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2a5d0e9-f5f0-404e-9f34-fad9ccb3adf7)(content(Whitespace\" \
         \"))))(Tile((id \
         4cf3e2bd-98cc-441c-91d7-217e80450d69)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         7e542601-24dd-451a-9d61-5e5503e60787)(content(Whitespace\"\\n\"))))(Tile((id \
         a8a62684-2e5d-4df6-9f79-29027cdfa473)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1fb3318-4bd3-4c82-9eb0-5635aef9b4e4)(content(Whitespace\" \
         \"))))(Tile((id 878a9f77-0fbc-44e8-8412-90163c7b5ccf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9f8ba8fc-0923-456d-8ff2-d3b6d0421914)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         486020fa-aad6-4b3b-af28-dae5bc8de821)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         efcd7ee8-bc51-4c98-9f1c-ffda3ad94218)(content(Whitespace\" \
         \"))))(Tile((id \
         b082a6b9-3621-4d5d-8821-830b584faed1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         feea09cc-4619-4fb9-ae4e-e907d5ccd22a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3d0140b-81a8-43b5-bec8-5a1399cfc49d)(content(Whitespace\" \
         \"))))(Tile((id \
         e0613f7c-f188-4df1-99a3-88f58735225e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce007be9-d21a-48d9-9aad-d20e0cc88322)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39c2e309-e41f-4bcf-9d20-7fc38fc18246)(content(Whitespace\" \
         \"))))(Tile((id \
         f4058745-8daf-49b9-8a61-a436e921852c)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f9f11843-f45f-43d3-9032-046d2c3fcfd7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         dfb8015f-6137-445c-ad31-daed0887cce4)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# RUNNING SUM - SOLUTION #\n\n\
         # Uses fold_left with a tuple accumulator:         #\n\
         # (running_total, result_list_so_far)              #\n\
         # On each step, add current element to total,      #\n\
         # append new total to result list.                 #\n\n\
         let running_sum = fun nums ->\n\
         let (_, result) = fold_left(nums,\n\
         fun ((total, acc), x) ->\n\
         let new_total = total + x in\n\
         (new_total, append(acc, [new_total])),\n\
         (0, [])\n\
         ) in\n\
         result\n\
         in\n\n\
         test\n\
         running_sum([1, 2, 3])\n\
         == [1, 3, 6]\n\
         end;\n\n\
         test\n\
         running_sum([5])\n\
         == [5]\n\
         end;\n\n\
         test\n\
         running_sum([])\n\
         == []\n\
         end;\n\n\
         test\n\
         running_sum([1, 1, 1, 1])\n\
         == [1, 2, 3, 4]\n\
         end\n";
      refractors = "()";
    } )
