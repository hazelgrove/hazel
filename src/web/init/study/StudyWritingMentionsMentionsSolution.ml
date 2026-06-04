let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         1bcc9a3c-f3a1-4d76-954c-d44857bfbd56)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         2d0644b2-350b-4fc3-80bf-37b91fe7247f)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ef45290-c1c4-48d4-b013-72f3f9694fb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         12d1061c-7c26-4184-8ca0-73b7d8039a40)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         ecd6dd6a-1694-47bb-bd69-1dc01b4c2b53)(content(Whitespace\"\\n\"))))(Tile((id \
         2e27797a-f4b3-4452-84f9-1a4211d5855a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8072f709-e1b7-4053-bbb8-b0c7a7cdf865)(content(Whitespace\" \
         \"))))(Tile((id \
         bd337be0-cb6e-4010-979f-192f0d0e5b24)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cd4b68e0-9556-4a74-9b50-b184b4e834cb)(content(Whitespace\" \
         \")))))((Secondary((id \
         ff1cac58-8ee7-4011-bc7a-8055edc13e91)(content(Whitespace\" \
         \"))))(Tile((id 6edc67a0-52a2-4dfd-8119-3d005ba1a4bc)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         907c4e8e-721c-4247-a7a4-964c380c494c)(content(Whitespace\" \
         \"))))(Tile((id \
         93718880-3312-40f4-b62e-72b5d34e30fc)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         52fd46e5-44b0-4d94-986e-29dc15e7a2de)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         25bbd34b-802d-4979-a10a-08cbf82456c1)(content(Whitespace\"\\n\"))))(Tile((id \
         85749204-7fe4-44a0-b2ed-3ca94633beec)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d1540bb-6f02-402a-941b-dcbc792952cd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f53f289d-fadf-448e-917b-a318f6f95712)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa1b7e4e-1176-41b9-beff-985db8595092)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1add769f-93fc-40a5-8e64-a8c80f801b67)(content(Whitespace\" \
         \"))))(Tile((id \
         497287b2-7d2f-437f-91e2-dde6a99ebee7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a7102b01-e292-438e-943d-5be292a70e87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2610941c-cf79-4e2f-b377-d6741069aa93)(content(Whitespace\" \
         \"))))(Tile((id \
         ee68034e-2035-433a-aa01-92ebdd6b5bf4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d6f983a6-1378-4729-b109-56e8b846d7cd)(content(Whitespace\" \
         \"))))(Tile((id \
         946edd5e-593c-4e73-8eb3-b40b3147791a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eca98a99-097f-4311-b391-0df7b19b6251)(content(Whitespace\" \
         \"))))(Tile((id \
         9736f70d-0387-45f3-bc7e-2146d59d82be)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8261367-aec9-4912-9b16-e363815dd42a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f037e139-1aa5-4e34-a13d-bbf7c4fb37ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8da2da4-df59-401f-ada2-b460213e1a20)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f3486c6-6964-4dc8-b742-bdac3429eee7)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         60a247b6-6cba-498c-b132-08941a589031)(content(Whitespace\"\\n\"))))(Tile((id \
         71c22f30-0610-4b38-83a9-63bd404d8ec1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f1992556-7658-48e9-893e-e4d220a8459a)(content(Whitespace\" \
         \"))))(Tile((id \
         8c49dea6-ccd2-42c5-8146-b932dc1a5a0d)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3262c945-2107-4889-b4fc-6de2326e82fd)(content(Whitespace\" \
         \")))))((Secondary((id \
         12572e8b-75e4-482e-ae05-5064df05305f)(content(Whitespace\" \
         \"))))(Tile((id 732b717e-9626-48ad-a10e-b6b76cd41103)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         707f7ece-249c-46bf-a735-dcc94d5fe0c6)(content(Whitespace\" \
         \"))))(Tile((id \
         bdb05c8b-3c95-4a76-95f1-6c512b2a4de5)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b693cc5f-0748-4ca4-b800-d9df0d82f8c0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         edf21e58-e3e2-450a-b1d0-ede1b54dc570)(content(Whitespace\"\\n\"))))(Tile((id \
         92b57a8e-a0a5-4f82-89d5-0ec6f6f72e59)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e164fce5-834b-4b79-8b40-58eb8c592dd1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a095374f-4b0f-4e22-9b1b-64db8868c4fa)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b4d140ba-fc60-47ec-895e-ff10a981db62)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e82831f-10cb-4e68-8e02-3b4b7beea80b)(content(Whitespace\" \
         \"))))(Tile((id \
         f9822223-b328-48af-b035-9464e64e7002)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef4fa6e5-bde1-4539-8d68-d313513859d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c74f1503-6974-401e-beaf-1febb51fb315)(content(Whitespace\" \
         \"))))(Tile((id \
         b66e0859-e684-49df-80dd-e0beb69a852f)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f4f2406-335c-4ebd-add6-e6622c203099)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42b59ee8-3c2b-4c4e-86ce-3f35a953b92c)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         feca1abc-6a01-4056-9758-f9b2e53e7554)(content(Whitespace\" \
         \"))))(Tile((id \
         33938b18-67f5-4ae8-9b1f-6eb445263cd5)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce4f9b78-e497-44fb-84d9-435f4e25f726)(content(Whitespace\" \
         \"))))(Tile((id \
         263764b1-2566-4d38-9a69-82b63ba05583)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d62082bf-ff8d-466e-8320-c90a9597f22e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         73ce3c81-f28c-4460-b939-f9c858bf3a63)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a1dfad1-f047-447c-be5d-52b830d1060c)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf1ca847-90c8-44eb-a871-f15c49909208)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         33e35d62-a303-4bb1-8b54-be2d997db413)(content(Whitespace\"\\n\"))))(Tile((id \
         93e77bd3-426d-4d7e-a2fa-c1e1c632c071)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         63d6c2ab-68d0-494d-9295-a3a2d616debd)(content(Whitespace\" \
         \"))))(Tile((id \
         292a83b4-b51a-4c4d-af43-0eeee8814a6b)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0117d017-3e36-4983-86e6-d1f649baa4e3)(content(Whitespace\" \
         \")))))((Secondary((id \
         0506e8d1-9c7b-4861-afda-97e7428388c1)(content(Whitespace\" \
         \"))))(Tile((id 46b2d9be-e563-46d3-9868-09099dad3029)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         142b5ddf-9dc4-4724-bd06-2ff92f57b169)(content(Whitespace\" \
         \"))))(Tile((id \
         1b4a75c0-0bcd-48eb-9480-eedc13837fbe)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bddc36c9-5343-4de6-a7c3-632b7bff4aa9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aa383e87-8223-46f4-8fd8-58e01a92a754)(content(Whitespace\"\\n\"))))(Tile((id \
         311005fc-f752-49ad-8b43-2035d6b33516)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1c4a8b85-5d12-4d3c-b25c-d3ec89c1a99a)(content(Whitespace\" \
         \"))))(Tile((id \
         68f0518b-45fc-47d7-a0c8-380a120edffd)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f345dedd-1d51-4c9b-a9a3-7833ee099256)(content(Whitespace\" \
         \")))))((Secondary((id \
         b97a417a-0f0d-4daa-b9f4-c6960820851c)(content(Whitespace\" \
         \"))))(Tile((id \
         cc37cd97-7263-4cb7-9b97-4ef34d9069fa)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06450707-03e3-4ead-a0c6-5a50be394a36)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a5bbec60-4b23-4720-b4f5-ad86b057965a)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbb25192-c5d9-4b0a-83a3-1e4a103af67d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9c801ab-86a8-49f4-a74b-27c41a8edb79)(content(Whitespace\" \
         \"))))(Tile((id \
         742abce6-fc13-442a-aacc-c9ba95c5ff40)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c3080e59-0332-497f-8094-85bf191a2740)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         14f5becd-82d7-4460-a454-166ef4f4c9e5)(content(Whitespace\"\\n\"))))(Tile((id \
         9dca8791-782c-4c22-a0f8-fa582618f6fd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7ebd0055-0845-4574-b98e-59078eead936)(content(Whitespace\" \
         \"))))(Tile((id \
         870a57a9-32ee-42e9-a3c2-57d012eeeac7)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         42f5686a-37cb-40ca-a8af-ede83bc39916)(content(Whitespace\" \
         \")))))((Secondary((id \
         822ea541-ed37-4c00-87e3-50b1498cd203)(content(Whitespace\" \
         \"))))(Tile((id \
         d1015011-1040-431e-bd9e-10307b7da1a6)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         354a1fa3-48bf-480c-afed-99d46ebec032)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e453002-f1e9-4d70-9f93-f95bddb6f12b)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f4711a4-3146-4e95-9fd4-5b8e086bdd00)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9080c191-b0b8-4b7b-8661-5fe0e45b1714)(content(Whitespace\" \
         \"))))(Tile((id \
         1c158980-309b-45d2-a46b-59939f3753ec)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c620fc27-51f5-4ff6-b18a-110533be71de)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         780cae8f-0e65-401d-9a13-027a82b36a9a)(content(Whitespace\"\\n\"))))(Tile((id \
         4722eac6-43d4-48da-b42f-1375219355d0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5c8cce73-118d-440f-9c67-5f08acd7c9ed)(content(Whitespace\" \
         \"))))(Tile((id \
         c1707796-aa2e-4a5d-a6da-af37bd1e401b)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         82d83028-761e-475c-898c-62674c24406c)(content(Whitespace\" \
         \")))))((Secondary((id \
         d0305ec3-955f-4de8-9a53-b77ef3347174)(content(Whitespace\" \
         \"))))(Tile((id \
         5b0ca5bc-030b-4d5b-9c21-d18c394d5316)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee0c7c66-bb3d-443d-8c0f-e4737153d8a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1bb77a9e-c3d0-48f8-ba71-1fac8586eae2)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7e3fa3d-2486-456c-b61e-fa256ec37bc4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cbef19cb-36c2-4955-b4a0-b8f49a4a2e0a)(content(Whitespace\" \
         \"))))(Tile((id \
         01224c2c-bfff-4f50-a2cf-1bbcc9fb70ee)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7e0eb2eb-2263-41de-953d-6939995ed409)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         513ce776-ff44-44e6-9282-6167232df494)(content(Whitespace\"\\n\"))))(Tile((id \
         61fee7c7-c656-426d-ba00-aaa8ffdeca89)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7b481ada-340d-4caa-8ea2-02d894b23c2f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9dc5610d-5099-42ab-82d5-cf14ab630294)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7c2549d-315f-4d2c-be57-19abde6ed6e8)(content(Whitespace\"\\n\"))))(Tile((id \
         788b68be-77d7-4008-99df-70b0bf84cbc6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         67d2d154-8cf7-4aa3-9e17-74c1f2d952d9)(content(Whitespace\"\\n\"))))(Tile((id \
         df8c8199-e7f3-4ea0-ae84-1464ddfe4200)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58f3d464-9882-4206-9041-77045ce40097)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7592abf6-d997-42c1-9933-a1b22522f91b)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         73f833f7-7828-4483-93d4-25baac150c07)(content(Whitespace\"\\n\"))))(Tile((id \
         b39efbbf-787a-446c-924b-ff874662e7f3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b78cd76-194f-41db-9c1b-50b4414fe41b)(content(Whitespace\" \
         \"))))(Tile((id a440e1c4-fc6d-4dfe-8ea1-6193d74c0b54)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c2d66380-2de3-4f7b-8c3a-db0fc2f16440)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b0c0e9ec-1e61-4ec7-a72b-900be267353b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f829e2b1-eb9d-40e2-a0b3-f3ccf5e63334)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e9978f0-ec2a-46a1-a819-8a1c00a84796)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1acb92c-9af0-4d5d-af5e-3023be74a011)(content(Whitespace\"\\n\"))))(Tile((id \
         45bc07d6-4521-41c4-ac9c-28f904d70146)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4f7caf5d-66f9-47ec-837e-4ae4c84f60f7)(content(Whitespace\"\\n\"))))(Tile((id \
         52a6c1b3-4477-40df-8d06-0715698a29b5)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9dd696b-8c3a-4f8f-abcc-8aa610fa6b22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1c7334cb-4dc9-4468-8acb-c280b47a37a6)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b99bedd8-2d0c-4829-ac6b-f2ab32757a48)(content(Whitespace\"\\n\"))))(Tile((id \
         cb764b8e-65c1-47dc-9d17-efcdc3769238)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c45787c6-7571-4144-8082-825774b582af)(content(Whitespace\" \
         \"))))(Tile((id a1f2ca7a-2585-4fee-ae1c-ff0036900ad0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b52a0082-5c48-44b3-8dcd-d1952ae5851a)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4d1d466-ce0f-46f2-8913-9b5c70b9355b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5096968c-602c-4114-a345-9bb0c06941a7)(content(Whitespace\" \
         \"))))(Tile((id \
         06cbaef7-ae86-4049-85e8-2ff48b0c65ff)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         968336da-8793-4ef4-87c8-e5f70609dbb6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6e3428ed-7371-445e-885d-111b22acd84c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55e2f8f0-2e95-4dd3-9c5f-6d826104a0af)(content(Whitespace\"\\n\"))))(Secondary((id \
         22eabcd3-7f81-4ff5-9a4d-610389390628)(content(Whitespace\"\\n\"))))(Tile((id \
         3e4ed537-e15f-46e0-a2b8-0cd691df0e96)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4b45a970-9d54-4dfb-ba1e-50327cfcbaa6)(content(Whitespace\"\\n\"))))(Tile((id \
         fa9bee92-e884-4306-9a48-65f858e62ccc)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ace3226a-8045-4c0c-a37c-69cb2e15d3ac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         59c9682b-2c61-44d0-a9c8-9415a764b5f4)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         15398f09-9dde-409e-8160-12a070e2604a)(content(Whitespace\"\\n\"))))(Tile((id \
         1242c07b-26d0-431e-acab-6efbb819b582)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd713cf4-7cf0-4018-8c3c-f928a978f946)(content(Whitespace\" \
         \"))))(Tile((id \
         2b4e32a2-5f63-48ed-a379-8af301b64330)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e51cabbb-6d7b-4943-ad97-54563e0b4f00)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3e350ae2-d7e7-4080-b4fe-a71a575790c2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         644d6518-4ee2-4a8f-9cb8-4e0159cd8a78)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b078b84-5959-4094-a30a-47f88056f812)(content(Whitespace\"\\n\"))))(Tile((id \
         d1cea68d-4bf1-4903-9938-1b2de9469033)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9a61de4a-d3a3-4525-96e8-8766fe069bbd)(content(Whitespace\"\\n\"))))(Tile((id \
         339d276b-b693-4448-abc9-edea9c12ef54)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1cd71fdc-554e-467c-a5b0-f4264ccc754f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87ce27f4-109a-4b1b-b4a8-4c08472462ba)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8310f6d7-4abe-4433-adcd-084dad996122)(content(Whitespace\"\\n\"))))(Tile((id \
         d2b23394-6bce-4b5d-85e4-d4825e3345d4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12df2420-d48f-4cdf-8431-4849f27d9385)(content(Whitespace\" \
         \"))))(Tile((id 11faa5cd-4b2c-4226-9d36-bcd9abb43b7d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         072e2f0f-d4d0-45e4-86d0-7cf2c3b7801a)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2a8d45ed-3e34-4fd6-a26e-2b514db0e5d1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fb2f11d2-a9c6-442e-a923-77a248f6a499)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR - SOLUTION #\n\n\
         # Check if a word starts with @ #\n\
         let starts_with_at = fun word ->\n\
         string_sub(word, 0, 1) == \"@\"\n\
         in\n\n\
         # Remove the @ prefix (take everything after index 0) #\n\
         let strip_at = fun word ->\n\
         string_sub(word, 1, string_length(word) - 1)\n\
         in\n\n\
         # Extract usernames: split -> filter -> map #\n\
         let extract_mentions = fun message ->\n\
         let words = string_split(\" \", message) in\n\
         let mentions = filter(words, starts_with_at) in\n\
         let usernames = map(mentions, strip_at) in\n\
         usernames\n\
         in\n\n\
         test\n\
         extract_mentions(\"Hey @luna the moonblooms are opening\")\n\
         == [\"luna\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@thorn @moss check the greenhouse\")\n\
         == [\"thorn\", \"moss\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"the night air is still\")\n\
         == []\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@fern\")\n\
         == [\"fern\"]\n\
         end\n";
      refractors = "()";
    } )
