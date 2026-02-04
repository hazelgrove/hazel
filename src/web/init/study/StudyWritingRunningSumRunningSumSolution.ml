let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         9f4dd104-01d2-4984-8be2-b1236b3f68cc)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         16fd9328-4f21-415f-87cc-878fec714da5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0144ea8a-d20a-4a40-9f1b-0ceab40a99ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2ec3bd0-5c32-4cf1-8c55-7fecae36c89e)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         83e666b0-0f98-4b1b-bf6b-9691a38a0e58)(content(Whitespace\"\\n\"))))(Secondary((id \
         435794f5-5385-4bbc-854f-439307a018df)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         044a0d50-88aa-4bef-950c-aa6581e9ade2)(content(Whitespace\"\\n\"))))(Secondary((id \
         82f6bfe3-260f-49dd-8048-091e5623cbeb)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         20775406-4d09-4092-9edf-f9d795fd1e98)(content(Whitespace\"\\n\"))))(Secondary((id \
         d37de318-d23c-4435-b3a4-2b2b6cf2c5e8)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         035736a0-4853-4cfe-b935-f88e13363579)(content(Whitespace\"\\n\"))))(Secondary((id \
         a55001fb-6be0-478c-80f4-7978903132e4)(content(Whitespace\"\\n\"))))(Tile((id \
         2c8c8176-f2b6-4d3f-8916-eab5b40a77e4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4803b2da-ed11-4f56-b47e-12eb513475d5)(content(Whitespace\" \
         \"))))(Tile((id \
         e37859b4-f3b9-41a7-ba8c-a1f091589572)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a660bdd1-703f-42ec-b359-67e1ead4f1e7)(content(Whitespace\" \
         \")))))((Secondary((id \
         bb8c4943-53e1-4620-8b4d-e12618e2156a)(content(Whitespace\" \
         \"))))(Tile((id d906458b-bce4-4fcd-8096-d45513dc7539)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         55f1e9e2-d88f-4394-912c-e574d0c56886)(content(Whitespace\" \
         \"))))(Tile((id \
         aa20e326-68e3-4d60-b37e-bda685389d53)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd71b30e-9f62-4549-9e3f-ea8c157fa7c4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         710e7c6d-9404-4b90-a633-e5ebffc228fa)(content(Whitespace\"\\n\"))))(Tile((id \
         fe3aefc6-f5f3-456c-ac4d-d9f8b6bb1a52)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ef31f1b-2584-46e1-85e3-1a32f08ab9c5)(content(Whitespace\" \
         \"))))(Tile((id \
         e734cf87-debc-404b-acfd-6ab38a2ebedf)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         fc3e117c-ce39-4398-919c-3202566e01a9)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b6ef7a2c-f1cf-4df6-995d-199954c4167d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5ae3f296-297a-4bcb-9098-3412ed4c2893)(content(Whitespace\" \
         \"))))(Tile((id \
         8d28c401-9b93-466c-b0ba-d809fe37385a)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a4edee3e-708a-4a63-a1f1-68de186000dd)(content(Whitespace\" \
         \")))))((Secondary((id \
         5287fe13-e647-4183-b0d8-ba47e7609329)(content(Whitespace\" \
         \"))))(Tile((id \
         edfbd40b-255f-4b3d-9a66-a6e079ac8123)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1569836c-1d11-4bf1-b37e-955e9c044e0e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         83ca67ea-8369-4941-b336-e1e148e423c7)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         adac4173-10db-4a40-8739-831873f0d77f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af8a3d3b-ca28-4577-a9c0-6ccbe51d7077)(content(Whitespace\"\\n\"))))(Tile((id \
         cfc1ce80-eb11-44e0-b46f-fd7997ffd956)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1664fbb7-44e0-48fe-a253-4c1299d72823)(content(Whitespace\" \
         \"))))(Tile((id \
         804f4493-f406-46ed-83f5-86ad6039e37d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         30d07817-102d-494d-95e6-f5ab3ae4f826)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d769f1ae-edbb-41c5-89d6-dccf36be444f)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         92c4b20e-d408-49b7-8ca1-855c29fc96be)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4bd2f086-b9d2-44bf-b9e7-6ae714f4606e)(content(Whitespace\" \
         \"))))(Tile((id \
         c76debd8-b0fb-437e-9b6f-95e5b6b88efd)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         508833bf-af78-4ae2-b515-ff754c76b492)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6c6232a3-5fbd-4655-b210-a51cc7660d92)(content(Whitespace\" \
         \"))))(Tile((id \
         39d3b734-8a83-4292-86e2-b208a5328b19)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d0a3fd47-2ae9-491d-a90a-1e3876ba16d5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e4566dc0-dc5f-4b97-8800-4669732b7b6e)(content(Whitespace\"\\n\"))))(Tile((id \
         46956679-323e-4172-addb-895e52893a1a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d3b2f2fe-8dbd-4a27-a792-7ff1f5649be2)(content(Whitespace\" \
         \"))))(Tile((id \
         52751f54-97dd-466a-b1b8-c4e4b31c9d03)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cc8b1792-5981-4384-ad53-1b9c0df0d8d4)(content(Whitespace\" \
         \")))))((Secondary((id \
         2cdea5fc-867f-49a3-acd9-f5b3fec76941)(content(Whitespace\" \
         \"))))(Tile((id \
         44402e81-9136-471f-b22b-b14b71318bd0)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         49a1697c-af55-4b0e-895f-d9530382afd9)(content(Whitespace\" \
         \"))))(Tile((id \
         08a94d83-934f-480a-b5a2-efaba9660285)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d887f4d-48d1-4750-b071-4ac1adebf772)(content(Whitespace\" \
         \"))))(Tile((id \
         ba6ef856-49c0-472b-a981-f7ba4fbc9e83)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2b398d1b-c7af-4ae5-a298-3daef99cb23f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7afe54a2-6126-4aa6-ad83-3e61a493f6f7)(content(Whitespace\"\\n\"))))(Tile((id \
         15f46691-2722-4d04-9793-7f8013339b14)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         217dac7a-7f44-4e35-827d-af078922a6bb)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         38178f85-5475-46e2-9d19-f6b87c762d90)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f187e674-56de-4143-89b0-70ad53b7e4b0)(content(Whitespace\" \
         \"))))(Tile((id \
         c3b7752c-8d33-42bf-93bd-4d30c8d08a30)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5622366d-fe5c-40f1-ac28-d447120c8692)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e7f66b9-a20c-4120-b1db-37b78d07d23b)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b37aa9d9-f02a-451e-8c09-43c522c1890e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bbd374a6-13ce-4f6a-9954-739a13c0487c)(content(Whitespace\" \
         \"))))(Tile((id aced1b7b-3b85-4d30-83ed-b66cc03d6aac)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea66b00c-d3f3-440b-a6eb-4d22eaa5e1c1)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
         b84e6c00-621d-428e-a834-01b89c849906)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edd35467-e7fd-4f51-85c9-f51b56295d2f)(content(Whitespace\"\\n\"))))(Tile((id \
         aa3997bd-581c-4a5c-b999-fc08dff4eddd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         407b4be0-ffcb-4a92-b81e-af86dab3356e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c51e0fe3-a614-4c89-acd1-b0ed8ad2943e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         58c41c00-86c2-4dc9-b1e3-4f707fef479c)(content(Whitespace\" \
         \"))))(Tile((id \
         4e298031-3e43-45f4-8559-cac7e394503c)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7346c0fa-076d-4fd5-b876-1b26c25a1d1f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf4f38a2-3f23-4e18-9dba-d5f8c710fd98)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a1eb68ed-49c4-4812-ab8e-0836fb61aabc)(content(Whitespace\"\\n\"))))(Tile((id \
         95427194-9e00-45b1-ab61-c10e5145f09d)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         68c769f6-e69e-45c3-986a-d1842ad0a706)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         012f7a4b-7ac5-4ec4-a3ee-fbfece432d7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8cb99e9-d7e0-43d8-a631-658fa0045fc0)(content(Whitespace\"\\n\"))))(Tile((id \
         a91a233b-bdab-4897-8360-df8cb2fd8c3c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         30c31c4f-cd42-4328-830a-4aff97dfb075)(content(Whitespace\"\\n\"))))(Tile((id \
         c17cb735-7303-4dc9-ac7e-ba54b6aa5379)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         659d01fd-d53d-4e86-9b07-e294b9fd0519)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f925371e-35e1-4e96-9ad2-f7d75e83b573)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         83a91086-d107-4c26-bcf3-53d5d9f88f87)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85fb70d6-626f-4572-a8fc-49921bf921c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         606afb36-c580-4c31-9189-6d6e9559979e)(content(Whitespace\" \
         \"))))(Tile((id \
         5bd9c7fc-afd7-4b34-9c12-d8334d6423de)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c2e0eeb-95b5-44d0-b24a-41ea5555caa0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7efaafd2-9ca0-48eb-a84a-85f9dbebc62f)(content(Whitespace\" \
         \"))))(Tile((id \
         e92a128d-24b4-4c60-b925-974a0cd31825)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e14bb045-a20b-4721-a8ee-4b6995135677)(content(Whitespace\"\\n\"))))(Tile((id \
         3eef0bc2-5d95-453c-9033-c6da0bcf9452)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         941e5408-0b58-42c5-8a86-cbed59048cad)(content(Whitespace\" \
         \"))))(Tile((id a74cfee1-63ca-4ad7-a523-a72d01f6e60e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         960be546-7b57-4afd-98db-cda1e35b60f0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b601d2f8-dd21-4b5a-a5bf-443f76862336)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a13b6794-b78b-467a-8cd3-1dc6a05ec713)(content(Whitespace\" \
         \"))))(Tile((id \
         c58411c8-7c38-4bfa-94ad-5ead24494c12)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         638d8023-b215-4f71-bee7-caacb913e3af)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94b9ac73-43c8-416d-970a-b3f56b0a8678)(content(Whitespace\" \
         \"))))(Tile((id \
         25cee201-f017-4b2e-acbf-a362012da8f9)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         732187d7-3446-4e6c-ae7a-23289099c08e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c76f43f0-f191-4e8a-a8fe-3b742fbaf993)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ada37063-1c88-4133-b407-02a446b9b8a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae532ef6-660d-46ba-af93-fdfc1a7a2864)(content(Whitespace\"\\n\"))))(Tile((id \
         4b1b59ae-d9e0-4d16-9bc9-1d8e38af2b57)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         94189969-9f4f-43a6-b5f9-9328e0c85675)(content(Whitespace\"\\n\"))))(Tile((id \
         7abe3979-46bf-4686-b350-f44e9ea8e84b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd4d801d-d94c-45c5-8741-31af2b6346cb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c3a6f2e0-a579-4264-acfc-1a6ed9f29bc6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4bc8afa1-2f1e-4fe8-9b97-34836c2b3d0f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         65598630-261b-44b1-8bcf-3243f2b3e4c3)(content(Whitespace\"\\n\"))))(Tile((id \
         5bcd70d6-d4a0-4396-bd04-f6519c5ad583)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         565a7e46-4cd5-405b-8072-c369e2ac9c59)(content(Whitespace\" \
         \"))))(Tile((id cb680cb5-1ca0-47fb-b093-aaa6a0cbe56b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58d6f1c1-1162-4d67-9168-8ee10f34b6c0)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2f269b17-dd56-4453-9590-63941beee38b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f902771b-2955-4689-8d87-d9a37bf43ea4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fe172b3-83ae-4b22-945c-1362801698e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee390d41-d623-4356-9e8c-da5cba63ce24)(content(Whitespace\"\\n\"))))(Tile((id \
         74c17c07-99fc-494c-a2eb-fc09d684cd59)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bd4ff00f-fb50-49fa-89af-31f1b889616e)(content(Whitespace\"\\n\"))))(Tile((id \
         e2f13330-179e-4bb2-8cd6-61f9ba5bd25b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c4fccb7-1ada-41e8-8f97-99ee7ec9025f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6826f790-821a-4614-8064-a35ffb4752b1)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d2598891-c412-45bd-83f2-04f86ac17511)(content(Whitespace\"\\n\"))))(Tile((id \
         38be2c36-e4bf-47c6-9661-d552ed4920cf)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c2fd3b5-7989-46da-99e3-c4d18dafa77d)(content(Whitespace\" \
         \"))))(Tile((id \
         e3c868ae-3c75-4dc9-9b96-e0edcc10148d)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7066ff23-4fd8-4214-a90f-b5d9f9582172)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2547624f-adaa-4d59-a97a-0853514a45dd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1532e4c5-9a08-4bd0-9c45-c27b55a45591)(content(Whitespace\"\\n\"))))(Secondary((id \
         456c10a9-28ba-4835-b8d9-aafbff61e175)(content(Whitespace\"\\n\"))))(Tile((id \
         19ecc852-01f0-4800-84cb-989fa0e06d8e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         64c34b75-01b1-4f7e-956f-ecb02b1c3b49)(content(Whitespace\"\\n\"))))(Tile((id \
         20ee5468-3fa6-498c-967b-866a5e39f117)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68d4a38a-13c1-44a6-bf54-fdc9e7966d82)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d94929cc-fc51-4d86-962f-c73570128d40)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         069e55ee-ab40-4daa-bbd9-4e0f0010bc1a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3bf3bc9a-8256-45d7-a322-690586930bbc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e0435c4-b644-4c84-8fdf-c37369044acb)(content(Whitespace\" \
         \"))))(Tile((id \
         0af8bc9d-35a4-4980-b789-753bf11888f8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         588a5335-94fc-4837-990a-19087d716f10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0085c68c-5699-4d86-bf26-d3f5e6c32e5b)(content(Whitespace\" \
         \"))))(Tile((id \
         7dc49cec-4a00-4d6e-a331-f8f67525c8e0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43e1b7bf-4e11-4842-aeb5-53e1adee1827)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f36d332-c313-4a3b-ab8d-53f173dc9296)(content(Whitespace\" \
         \"))))(Tile((id \
         dd2a2971-7fc7-4b4c-98e5-a1fdd5e6fb46)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         2f9bdef5-24e2-4f56-aaa6-13cb46e72b12)(content(Whitespace\"\\n\"))))(Tile((id \
         80884319-83d6-4dec-889e-4ea995e868f2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e099bef7-a256-4a95-94fa-53901fe392ed)(content(Whitespace\" \
         \"))))(Tile((id 148dfaa4-f8c6-405e-988d-e25718608d2d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fbc9a896-d31c-47a5-9ef4-544d7a9ef0bc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         087899ca-f622-420d-9742-2fd84537d7f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4297b83b-cfef-47d3-a51d-4c6b6b744935)(content(Whitespace\" \
         \"))))(Tile((id \
         4534cab6-1aae-4a97-99ea-e7d0c625a7cf)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48b1254a-10e0-473d-884b-2f79d7f344b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         042cae37-dfef-4c3a-a6f4-d052f7f1f5bc)(content(Whitespace\" \
         \"))))(Tile((id \
         94342ff1-26ea-465a-a67e-c791da8283b4)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd11f86b-fb6a-4975-aba0-805de88dc9b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bc9e693-4d98-43de-b8a0-feb0fbb8774a)(content(Whitespace\" \
         \"))))(Tile((id \
         b97562db-f62b-4fff-9239-7fa77ef49308)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2bbf71a5-a2fd-4baf-a9ea-f7a5a39e31b9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         99171f29-0717-4f2b-ab1e-3410e0275ce8)(content(Whitespace\"\\n\")))))";
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
