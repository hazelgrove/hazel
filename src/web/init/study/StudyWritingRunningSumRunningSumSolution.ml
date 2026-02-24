let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         bd2ecb98-693c-4662-8e6d-9556f98b1d86)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         14bca373-a975-45e8-a786-0a306380f1ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad93998d-2179-4c79-b513-488332031c83)(content(Whitespace\"\\n\"))))(Secondary((id \
         e78e4e0f-edc3-4ca6-9330-95ff990730ee)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         9c97d94e-aa44-45d2-a90a-799388d6c5ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         c26ed350-ba22-4aa0-bcb9-48ddb7044568)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         4d56e72d-e7be-4b06-9a0d-c119f846fbb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         3380249d-7f45-4d59-84df-83f5cfdbd6b6)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         2f0ff760-6e83-47b4-bc8f-0cefc9451d66)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7361327-303c-4cfb-99cb-c253ad779568)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         7f89d253-b763-4c10-be12-fad1aeebc0b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         765c64b8-52bc-464d-bd87-9302cabb92f2)(content(Whitespace\"\\n\"))))(Tile((id \
         d57493d4-523e-466d-91a3-9f4de2b06ec1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9513004f-3eab-4957-914e-961da37c6b72)(content(Whitespace\" \
         \"))))(Tile((id \
         e1326047-5d7f-452b-93a6-9f04f7d0179f)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9e0dfeb6-81e9-475a-9d0a-edb3c5c11ac2)(content(Whitespace\" \
         \")))))((Secondary((id \
         e08030ec-f5ea-4c4f-ad4e-e592d7b725c1)(content(Whitespace\" \
         \"))))(Tile((id 9dbca383-103e-41ef-9ce9-809e528fd14f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5be53c90-ffb1-4df8-936b-2b517e1721e7)(content(Whitespace\" \
         \"))))(Tile((id \
         fc7248a3-4c6a-4ced-8ad1-53e733688d53)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ee9bf094-eb4b-461a-bc05-cd2e46841be6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         47fc2466-f5ee-4351-8383-1747679d2b3e)(content(Whitespace\"\\n\"))))(Tile((id \
         fbcb7e8c-c550-4bcc-bdc5-16c4ceecbf0e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3f87bf7e-54ec-4404-898b-a62f998edc3b)(content(Whitespace\" \
         \"))))(Tile((id \
         79aae6f5-c5a0-473e-8d47-9b0d2398e4f9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2fc60722-71d5-49fe-8792-2746ad2fdcb3)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         15b8cb2c-e964-477c-a968-39ec67a057cb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ee86c8ff-0b7e-48be-95de-ffa227c3f45b)(content(Whitespace\" \
         \"))))(Tile((id \
         7d02d373-16d8-42fe-a1f9-06e65f02587c)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d7184db8-7464-4790-81b6-30acee69fb49)(content(Whitespace\" \
         \")))))((Secondary((id \
         a0b80d07-42f3-4f9c-baa0-38014d57578e)(content(Whitespace\" \
         \"))))(Tile((id \
         dd45de40-8185-4f33-b3b5-25e9c750e3fc)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef3342ac-0cd5-42e8-b68a-8de344565846)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         49e657aa-72b0-45e1-b492-0e8da1624c91)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e2039bf-0618-4b98-b6be-55345f397bba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74d9d58f-a7e1-4bce-9b96-7036fe4b1c8d)(content(Whitespace\"\\n\"))))(Tile((id \
         e5e6cfc2-1272-4785-a92b-e1678231cf1d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         574100e1-e49c-4ee6-be13-ad1bb78c3ea5)(content(Whitespace\" \
         \"))))(Tile((id \
         daf22222-d6c4-4f14-8a6a-a49eb176d23e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c8af5715-0ab7-466e-808e-25c036f65497)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         dd1d8689-3f09-49f3-ad63-91fc88d9e384)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fa291ee7-eb83-4a09-a9b9-8e7a61961df6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         34d3e7be-701a-4e66-8b51-657b848e8899)(content(Whitespace\" \
         \"))))(Tile((id \
         14070efb-16bf-4893-8ac5-df89f22c5ca0)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         79645639-df83-4286-b371-fce251c072a7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bb333aad-89ba-43d8-9df0-adaf8bbc0449)(content(Whitespace\" \
         \"))))(Tile((id \
         b88ebff1-b313-4cd9-a883-a7bfa571f4f0)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3d87223e-c3ea-4c37-bd30-643ef84b3602)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9bf8e041-bc35-44b8-b2f8-a1f14a1d7223)(content(Whitespace\"\\n\"))))(Tile((id \
         5f7508ba-293f-4a31-9077-96cdf3a2ae60)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a2d543c7-887d-4a60-b48d-0598af674173)(content(Whitespace\" \
         \"))))(Tile((id \
         c7cacc1b-ca0a-4497-a039-eba366fb3ffb)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         31b857ae-cdef-4286-9945-0bff21fd4295)(content(Whitespace\" \
         \")))))((Secondary((id \
         3a18afac-e502-4d3d-9368-4fdfe260d879)(content(Whitespace\" \
         \"))))(Tile((id \
         5d25fcc1-ae63-4318-a6ad-671f9440623c)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e021ecf6-546d-4920-8c29-4247651298a3)(content(Whitespace\" \
         \"))))(Tile((id \
         aac171cc-465f-40b9-a814-b9efbaed9b0c)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c952000-63eb-4c12-9a83-846c95a5f69d)(content(Whitespace\" \
         \"))))(Tile((id \
         889cb879-1c80-459d-b90a-0c662ad048d1)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3ceca23-4ea2-44f7-98f2-dafee32e7081)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1c1a4592-6f92-4e33-a486-5aefea935711)(content(Whitespace\"\\n\"))))(Tile((id \
         bc667208-ade0-4e36-82df-7ad11737383d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a8ae2d35-2007-4eee-a5b4-9cc8be5dcb41)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c19ed00e-73e0-4bac-821d-4b1d22f636ff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c24c022f-9f0c-4ec6-8d88-500de31993cf)(content(Whitespace\" \
         \"))))(Tile((id \
         abbca820-d245-445d-86ee-3df03c3d26aa)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a6f5b0b-f7ce-4c90-84fe-3f017bca75fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         efeda9b5-70db-4cd0-bb18-4db8d699904f)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         119e275d-4971-4870-b6cc-23a91e828c1b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bf2541e-faac-49fa-8138-c5eae1f2988a)(content(Whitespace\" \
         \"))))(Tile((id ad67e62e-7588-42a8-a410-609f5dcc0bd0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ef352f4-0204-4468-94e4-e831ec679c2c)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
         410fd27b-d657-446a-b704-5f1bc9382128)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         586214ae-2da3-468c-a3d9-b3bba0185831)(content(Whitespace\"\\n\"))))(Tile((id \
         1a4e8025-6d9d-4eed-afef-8b42f5ff23d6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         14a87b1c-eb80-4dee-b657-02f3a65b808b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4ad19cc-ed94-454c-b064-0747c053d5d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7affeedc-0e39-4533-85dd-57811a22fa6d)(content(Whitespace\" \
         \"))))(Tile((id \
         279e8780-5073-4e11-ade8-d9a1912b65b0)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3e535520-841b-4e7a-8509-5680a20b159e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2981c30b-e3ec-44f9-9cf0-abe39ca25867)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         43c2f998-62cb-417c-896f-7cb1cf6fefe8)(content(Whitespace\"\\n\"))))(Tile((id \
         50199d7f-1145-4a47-9b70-18b14c80fe41)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8084b946-eaea-4c05-891e-7f6ae9f5bb6e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         980e8958-a42a-4029-ae2e-06007bb04bb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e5d49fc-4da3-4543-b563-3eb2b921ab97)(content(Whitespace\"\\n\"))))(Tile((id \
         b7e4de89-0094-4bf0-8dee-eeec6b1f946f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0b9a60ce-b5a6-4ea0-8276-fced424e00fb)(content(Whitespace\"\\n\"))))(Tile((id \
         3e50bbd5-893a-4353-bed5-66a255276d9b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99e007cf-0439-4b65-bce5-562f2ae9dda2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ec218237-7487-41c7-b3e2-bab79e2b9a1d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6027d673-f2e7-48e5-bc07-9863130e885a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c23f495-842e-410a-9642-5625c3b7a685)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6c68aac-3b53-40a0-ae0e-ea041013b832)(content(Whitespace\" \
         \"))))(Tile((id \
         45e8b6b2-1fc8-4ead-821e-1bfa6eebb7b8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7647f45b-1cc3-43d8-b09b-d8167ab0674e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb4e535b-050c-4ef4-8ecc-c58aa0f12986)(content(Whitespace\" \
         \"))))(Tile((id \
         545f2471-855a-49da-9429-41be007b3938)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         7909f1d5-9cb6-4cbf-bd14-f7dae8ca56cb)(content(Whitespace\"\\n\"))))(Tile((id \
         b3a014c7-8a0b-4d64-9887-e3d1249fb813)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57ef598b-0c89-4172-8976-614c68941b2a)(content(Whitespace\" \
         \"))))(Tile((id 604c08f1-0d60-4709-a406-b5e7047d5e16)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         282bdfd9-a8a8-4f53-8a47-d945a74cef41)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0ee3ba2-cf3c-40a4-9a7c-8e3c8b824a3d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6042e80-c47e-474d-abe6-31d71c36efc4)(content(Whitespace\" \
         \"))))(Tile((id \
         7e130541-b5f5-443e-85db-79d134feb953)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1d647ba-ec2d-4705-abde-84f0e431d1ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f21349a3-6045-4f59-85d9-60084e417127)(content(Whitespace\" \
         \"))))(Tile((id \
         1b0791e3-ea58-4edd-8338-605db6315fee)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8559eecf-8b26-4cde-8533-c64da8ac828b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         96201490-829a-4bee-aa81-127464ad86e9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02deb925-cdd3-43f4-a1ad-84e140c77d6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d7e1958-e85c-4466-91f0-d972aa3eeebf)(content(Whitespace\"\\n\"))))(Tile((id \
         1fb42f76-d35c-4bbb-8a73-15199eac0c42)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9c79b0ef-4f6d-4d65-a1b3-bc39b68cafb6)(content(Whitespace\"\\n\"))))(Tile((id \
         b835bb72-2f1f-489b-a869-56842dff4e1e)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cb92a98-a890-461f-9870-1d0f6264d44d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         738f9f8d-d962-47d6-b729-a2ddb11bdca8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b336e224-52a9-4049-8839-bcac4e0f5f43)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         16c1dc58-fe92-4d9d-9c10-ce5a6cbebc49)(content(Whitespace\"\\n\"))))(Tile((id \
         2f308f06-477d-4f11-8ea5-dd05ac7ca7ae)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e3587691-019b-4ccf-bb63-562251fa5483)(content(Whitespace\" \
         \"))))(Tile((id c6b9c18d-3d72-42e2-8048-8423b1f85fc7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1e3418b2-03e4-4b46-bec1-212a7cd7ec97)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ff8a3978-79ad-4489-b334-d4891bc22e0e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         231555b0-5385-4952-bed5-e7890e4ece27)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6972bd53-f829-41aa-aa0f-3e90c960ad38)(content(Whitespace\"\\n\"))))(Secondary((id \
         3849ac17-60c6-469f-a6b0-b3d94b9b9230)(content(Whitespace\"\\n\"))))(Tile((id \
         41d99124-0b92-4c0e-a24d-c53d4827ecb1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cc6bbf43-4c62-4542-8201-059366b8626f)(content(Whitespace\"\\n\"))))(Tile((id \
         07615954-85d6-4f84-9d75-c7655083ff45)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8597d2cd-933c-4005-ae65-0f8d6e1ee794)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         babae9ae-094f-4596-abe0-d9fd2405c555)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         40f7554a-9b63-4110-9e61-e013c890a612)(content(Whitespace\"\\n\"))))(Tile((id \
         54f0374f-8e9b-450f-8dea-cab557c48ab2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ae97b468-e43f-462d-a896-a83b5e381347)(content(Whitespace\" \
         \"))))(Tile((id \
         337d7e57-4a44-4980-be2e-1c5c43cb7e45)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         75bfb2fa-e360-4897-95d9-2afa571f4b06)(content(Whitespace\"\\n\")))))))))(Tile((id \
         65823a8d-1827-4c89-8d97-9392f5a18cf0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ba3075a-379b-4328-8a07-62e226f49883)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb81db1f-1580-4e75-9d5c-d1b0cb6a8c58)(content(Whitespace\"\\n\"))))(Tile((id \
         bee6ee3b-1c1b-471c-a300-e704dc2af85b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8f4f2c2b-3a66-4d0f-9a67-53442a0969d7)(content(Whitespace\"\\n\"))))(Tile((id \
         c40d0802-61b1-4855-9f6e-d04bf4585bea)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         188b7964-90c9-440a-8c04-514ac9b103b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         27282a01-e240-44f4-9237-f07adcbbdf6a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         148ec4d4-bcb3-4ce8-945e-78e00fb2d9b6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         917bb7de-ee6c-446e-ba83-69fd13752940)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd3b9b45-17da-4c83-9048-d5e3873c294b)(content(Whitespace\" \
         \"))))(Tile((id \
         9748d5ce-f91b-4024-9051-2471df2e91eb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a398b83f-89cb-44b0-a277-8d94cbbc7347)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         363c72e3-3729-494d-babb-eb04a0ab0296)(content(Whitespace\" \
         \"))))(Tile((id \
         63b1b2ed-0411-450f-a000-7c00fbf27a1b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5fbe21d9-45be-4b48-b948-94dae34e1350)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0036ec87-2b85-4ae9-adb9-e08c33a575b0)(content(Whitespace\" \
         \"))))(Tile((id \
         e1f2bf44-29fa-4d13-968c-70ac3f600768)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d36a98ad-438c-4f0b-bb3f-845513e0ac7c)(content(Whitespace\"\\n\"))))(Tile((id \
         a00d5e7c-ccb9-4af4-aff3-9dba0067f46b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93b8bc31-fad6-4e71-9922-c18c83c53d91)(content(Whitespace\" \
         \"))))(Tile((id 9caafa29-76b0-41ac-8b32-205af0f3c65c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         29af302c-5cbc-4056-995f-a9017d4cf298)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8e236176-635a-4900-8249-5a81376d0961)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b02eafa6-57bf-46f1-ad3c-7064d3eeddf0)(content(Whitespace\" \
         \"))))(Tile((id \
         54226509-5c55-4fc8-8255-0375ea27bb47)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c08d2641-1683-4c42-aad6-2e3d50712f23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47552e4e-3e31-4058-b5d7-6e6cd18253ed)(content(Whitespace\" \
         \"))))(Tile((id \
         c2807fe9-63c9-41b8-80c0-948da06ea952)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         605a5168-5ac4-4f7e-89c7-d3f6b7db8a70)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9d4a9c4-d38f-4534-8472-c82f247524f5)(content(Whitespace\" \
         \"))))(Tile((id \
         ab638350-cc7e-41e9-86f9-63e7ae2b4c89)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d90bf107-735e-458d-8073-1e6044bcdd84)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         39e16dd5-d670-4f69-b626-98394a4670b8)(content(Whitespace\"\\n\")))))";
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
