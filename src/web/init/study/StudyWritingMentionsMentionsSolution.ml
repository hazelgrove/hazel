let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         b5507fc6-0b54-47ae-8004-1900e05e430b)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         164a3305-cdc3-49dc-afd8-6adb1516bb73)(content(Whitespace\"\\n\"))))(Secondary((id \
         601d0bb6-3afc-4942-b89e-a141f2cee1c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a6996dc-4221-486c-a129-11d6969846ec)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         cd1196f2-92bc-49fb-b867-38368c52069d)(content(Whitespace\"\\n\"))))(Tile((id \
         2ab2243f-5c83-41ea-aec2-e27b95af33e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8f7a2550-d03f-4b81-8079-6aa02e01de26)(content(Whitespace\" \
         \"))))(Tile((id \
         6cd01c8e-48bc-4b21-a398-2d0158cc2f28)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2c589cab-166e-409b-bd9d-6fa7cb47b3ce)(content(Whitespace\" \
         \")))))((Secondary((id \
         98b5761b-dff7-4d35-902c-0bbb545887d5)(content(Whitespace\" \
         \"))))(Tile((id 32885881-92c2-4b1d-9675-e5cee050760a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         b8a0dda2-33df-43f6-9709-bc6e607bb20b)(content(Whitespace\" \
         \"))))(Tile((id \
         f88f1d10-2fb7-4f0c-a64a-42c71c11eab5)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cc7483e4-3df6-4adb-9e46-ba57f7ad0bd7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c055993f-e98f-4bb9-a860-c2a2a5382540)(content(Whitespace\"\\n\"))))(Tile((id \
         1db535c3-3e10-4336-8028-2ec9e3ef4065)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6130c86d-1966-4072-a25b-42222ade8d56)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         55a90087-7f29-4f80-94c0-cfaf0bc1c166)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         832be0e0-c344-40fe-a2c6-d353513bb893)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d39d8e0-bd99-43d2-8e2b-e1608c72afa6)(content(Whitespace\" \
         \"))))(Tile((id \
         4fdeab31-0dec-4602-9d12-83483fdd4a22)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         04ae3ce8-506e-465a-9001-6096ad3f56d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85d8ae0b-9765-42a1-b40f-a9bee04cecb3)(content(Whitespace\" \
         \"))))(Tile((id \
         6abb9edf-b7b3-478f-9c2e-62167f14e559)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         01df1b38-e09c-45a9-8aa3-6f051ac2c6de)(content(Whitespace\" \
         \"))))(Tile((id \
         f3148368-14c5-40a5-9d93-17f238c0b4ca)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2dff7a78-385f-4858-829a-637a248bb95b)(content(Whitespace\" \
         \"))))(Tile((id \
         853a0c24-9ef5-4852-94bf-e2e1fadbb221)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9a43dc0-f42c-410f-bb12-aedd98a8ad51)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         002d9735-af1f-4e84-a217-49c0925032e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         160aaf80-b4e7-4358-8434-9dc5fbae0319)(content(Whitespace\"\\n\"))))(Secondary((id \
         70a85d0d-9582-4eb0-90c6-73153676c990)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         d32b52db-bb0b-4c50-9b25-f37043a2126d)(content(Whitespace\"\\n\"))))(Tile((id \
         bfab9910-87c5-4306-9e4f-9f7ace5e1b8a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         32e2a6e7-d607-463d-b98f-9bc59a407524)(content(Whitespace\" \
         \"))))(Tile((id \
         79aeadd7-74fc-45ea-9fab-b43b695e610b)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         adf8b0f7-d00e-4ad1-998c-a5cc700c77fa)(content(Whitespace\" \
         \")))))((Secondary((id \
         85ba6dfe-7f1e-44c9-aba2-1dff30d3bf74)(content(Whitespace\" \
         \"))))(Tile((id 70fdb683-2908-43ef-8404-2132d866d6cb)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d03e0ea9-0b35-48f9-bfb9-c12de89089de)(content(Whitespace\" \
         \"))))(Tile((id \
         b9335ed9-4482-44ca-bce5-56c2c3598408)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         69b6a7c2-4aa6-4d42-9a6a-dc8968ec8aee)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         17266db6-480c-4572-a674-5a96bff1c539)(content(Whitespace\"\\n\"))))(Tile((id \
         2ce24385-9809-4b86-94b5-4de87645c89b)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86b7e039-98dd-40d9-a75a-2e93546a387b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b797be50-ab90-48b1-b079-b37992f69e24)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e248f87-cd8b-4a0c-9707-83b0d063552f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5690531-297f-4faa-9659-16cae2a82ba9)(content(Whitespace\" \
         \"))))(Tile((id \
         a3a59898-82f5-4425-8e13-584fe376e1cf)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92fadae5-f4e5-40ed-9ca1-5fc6be53f619)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbb9b926-c5f9-4ca5-b6c7-8a2475a767a1)(content(Whitespace\" \
         \"))))(Tile((id \
         0290d2c5-312f-4798-8230-6f715f36cb62)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94dd55a4-b883-4baf-b72c-f2cabd75f37e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c800edd0-ccf8-40bb-9038-0089100dc27a)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         679ef843-5078-414a-a6bf-9b7cfa05ee74)(content(Whitespace\" \
         \"))))(Tile((id \
         244feedf-2d8c-4137-b125-13bf3aa3a715)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4711a17d-1650-42c6-973f-030b8eb50b00)(content(Whitespace\" \
         \"))))(Tile((id \
         6ed9d537-40f9-45ce-88f7-77ed4e595de2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         920abe6d-9497-498a-aad1-b62be2ec8cd5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ee057753-ddb1-4918-a189-625d1eff0e77)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb47fc74-3f1d-43c0-9cc5-f555383eeb74)(content(Whitespace\"\\n\"))))(Secondary((id \
         643f2698-5c1b-4f96-940c-6475d4bf232f)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         959ec885-7d41-4de2-9a98-57ef6ae73518)(content(Whitespace\"\\n\"))))(Tile((id \
         d9d8e91e-9605-440b-8b10-882467bf019f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b6eb81ac-ced0-4617-9cdf-e46a2d4df485)(content(Whitespace\" \
         \"))))(Tile((id \
         a17ccedf-8a71-4025-af90-e800cf3ff3a6)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0d249c02-fef9-4a0c-badf-fc4492a6012a)(content(Whitespace\" \
         \")))))((Secondary((id \
         46ba345c-1645-4ada-b150-3d75cb392f91)(content(Whitespace\" \
         \"))))(Tile((id 492e46ef-61bb-42c2-9aba-4ed0c8445d7d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a27594fc-c83f-45f2-bb7c-deb5d2acaa82)(content(Whitespace\" \
         \"))))(Tile((id \
         b5b45f4b-e7b7-45ef-ae9e-711726ccad05)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a46f63f8-54ad-4fc4-9b07-71845ae9efcb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5c381087-686a-4ddb-b818-84fe214cfec5)(content(Whitespace\"\\n\"))))(Tile((id \
         7920fa03-6358-4e79-b8db-27984749510e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bf4cabc7-3759-424e-9c5a-aab3cb612133)(content(Whitespace\" \
         \"))))(Tile((id \
         81193854-02fc-4977-bc3a-da0f50406c09)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd582756-6803-4849-b9a0-27e66dad5fa2)(content(Whitespace\" \
         \")))))((Secondary((id \
         4c60926a-e15e-4a22-a45b-97408000d0ae)(content(Whitespace\" \
         \"))))(Tile((id \
         c7404ee7-5f64-4343-9660-63ac8c63a405)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef19f1f6-9a4e-427d-aff9-b1cc0fc3cb46)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20e5cf44-e65f-47d2-b8b8-52a861b55c66)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e009b284-5e07-424a-b048-df5b33f537fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         048a9dba-0fd9-43d6-92f6-f4ea29dbcb0a)(content(Whitespace\" \
         \"))))(Tile((id \
         d3203aa6-4bfd-4f31-9572-8b0bb6b8f786)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         33569ea9-e5c8-4a43-8eb0-76561a1e4075)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         98675f3b-438e-427f-981c-7fbe8d780770)(content(Whitespace\"\\n\"))))(Tile((id \
         7e4b15d9-a3a6-4858-b909-0442503a03d7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cf94ae80-e947-4ca3-be9e-d511d52534a8)(content(Whitespace\" \
         \"))))(Tile((id \
         386a4df5-86db-404e-9f9d-4dc71f0ba18a)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         79343434-e6bf-4091-8d63-c8f077e38cde)(content(Whitespace\" \
         \")))))((Secondary((id \
         40237854-8167-476d-a5e7-a9ec89358c85)(content(Whitespace\" \
         \"))))(Tile((id \
         8ed1b83c-5424-4085-99d4-2ee185cb8f6c)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae305929-1216-4681-aecd-56ce07963597)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0b356f0b-aaef-46b3-8f73-b5c908207c11)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a25e5419-9da8-4901-bf94-5706b3e03182)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e06bce5-ec11-43bf-b8dc-8131a83f92cd)(content(Whitespace\" \
         \"))))(Tile((id \
         b822a937-552e-4f7d-99ee-fb253f74cfa4)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a55e8985-a78c-42b9-b2fa-aaecba8c61f8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1f4f4cf2-dcee-487c-a1b3-96fa96cf1c91)(content(Whitespace\"\\n\"))))(Tile((id \
         a01a65f8-f92f-4b0a-aa9d-7efa3e750926)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c50e226d-cfa4-40f2-87a4-43272dc50619)(content(Whitespace\" \
         \"))))(Tile((id \
         84c90db9-264e-4399-9d9c-f87bf683bb77)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         735fde72-8297-4293-bd20-8541cedd0886)(content(Whitespace\" \
         \")))))((Secondary((id \
         87db9110-f27c-4601-bbb7-83d5ad52dcf5)(content(Whitespace\" \
         \"))))(Tile((id \
         d2c92a5b-0631-4ad6-9ab0-df712cb8a311)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ec109f1-3639-472b-b9b7-22c927a62eda)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d3255b0b-f5eb-479b-8b3b-1d049e846774)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ae270ad-957a-4c13-ac1a-fb3a61042bae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         545d6955-7b94-4aab-9103-ee5255fb8773)(content(Whitespace\" \
         \"))))(Tile((id \
         17a44f88-b21e-47b9-9dc5-1dcfb680bef2)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5ab23d58-c773-4d5e-a763-38ad09a7fe3c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         89cee9db-845f-45ad-8be2-3494ad0bd387)(content(Whitespace\"\\n\"))))(Tile((id \
         21b2725a-01c9-46f5-94b3-5f8777063321)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ba2d7f03-7ff3-4549-b36e-06f2c12ed3e5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         39c7dead-0bca-4add-8c9e-2d5487cdb913)(content(Whitespace\"\\n\"))))(Secondary((id \
         538c64c3-f8eb-480b-b1de-07c15192adce)(content(Whitespace\"\\n\"))))(Tile((id \
         51586c87-f7f5-4ced-9aee-1a8bec45ac02)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         dc039bfd-aa24-490f-a430-4c103100c0e2)(content(Whitespace\"\\n\"))))(Tile((id \
         4e86695e-2ca5-455c-b6fb-4b6ebe965612)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4b3d86e-c8b0-4e16-93e0-30a87977cc2d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         85b5966e-d614-4cc0-b1a0-e79a763d39b4)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f7773932-6bdb-4b1d-a517-3a0efaa27de8)(content(Whitespace\"\\n\"))))(Tile((id \
         42692b03-5c10-4ade-9fca-c9db015643c2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4c5812e-3ae7-4a51-8168-7dd9d758de80)(content(Whitespace\" \
         \"))))(Tile((id e454c5ca-597f-4f02-8a5a-7db36a675651)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14b9f739-7d3d-4557-bbca-4de6b45a7d49)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dae07f27-492c-42d2-8a11-df044db76372)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ea7a28fe-5199-4d5c-a9ea-d9553ddfdda8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fb64714-7c6e-47c5-999b-2b33890617bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         4103e9e5-e595-4956-9eff-6247ffcbd635)(content(Whitespace\"\\n\"))))(Tile((id \
         9ef1c805-0a42-431f-a308-bfef6a26e6ab)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f915cb59-4f20-4521-ae65-f29b9fa20ef2)(content(Whitespace\"\\n\"))))(Tile((id \
         cabb0420-6738-4306-a033-787164095b33)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11da13b8-5c5f-4c10-b67f-8ad24dbd667e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e745aeb0-622e-4197-a784-298a42d951a0)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4cc7ff1d-4ebf-4a68-917f-ca767e5f3d5f)(content(Whitespace\"\\n\"))))(Tile((id \
         a281ed03-0631-4ce9-9022-4db31c8472cb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f234b60-45ba-4103-95ea-b5d03e34a01a)(content(Whitespace\" \
         \"))))(Tile((id 518fa18b-82aa-4052-b121-04e159cd220c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d06dc630-4e02-46fc-8cf5-79a5db8d7967)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29eda352-3853-4595-acb7-a9af71145f02)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7ffc325-de96-4a75-8f4a-51ab45ad46ea)(content(Whitespace\" \
         \"))))(Tile((id \
         27917d9e-ec22-4da6-9725-4f1a25231adc)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3c9ccd65-a9f7-4142-8b51-4c075fa65415)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3aafda4e-d3c3-4156-a25f-6df8527c0598)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         530db5ae-b1fd-42a2-b756-795f3f4a4356)(content(Whitespace\"\\n\"))))(Secondary((id \
         44412a4b-8f47-4515-8bb6-16ad69381661)(content(Whitespace\"\\n\"))))(Tile((id \
         b88dabc7-1619-4b7d-92c9-c2291561d759)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f102bad8-7f1c-46c9-8383-843e0142bc23)(content(Whitespace\"\\n\"))))(Tile((id \
         fdd11163-2bf5-4497-ade1-d5b9f40016a8)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e306f2f-32a2-4588-a485-f07b367de5d4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8dce8aef-a232-4a98-882d-960e119130a1)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         904badbf-45fe-4838-944f-bf45f0d973a2)(content(Whitespace\"\\n\"))))(Tile((id \
         1b9be9f2-1e79-4e98-843d-995ac81a606a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42de4347-cf27-4339-9134-3368053ee96b)(content(Whitespace\" \
         \"))))(Tile((id \
         aaa12c40-4df3-4749-843d-2980a8209adc)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         15e1136a-6257-41ee-b2cf-3ef2dd1fd22d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cb914120-a08c-485a-a355-dccf2cbfa889)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c43422e-663c-4ed2-ba18-757101f3d259)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d66e55e-0151-480f-81b0-157bfed33918)(content(Whitespace\"\\n\"))))(Tile((id \
         6ea049a0-2515-4b42-bec3-977cc9c0d77c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         224ff3ed-c781-418c-9723-9914eda52833)(content(Whitespace\"\\n\"))))(Tile((id \
         e7d15c38-252f-42c8-991d-c2d503dd854d)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         174571b9-7442-40d3-84fc-d528cca98c43)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e5153f1c-5f39-4027-a2f1-677a7b08b9b4)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d3fafbd6-aaec-4c7d-ac4a-f56201412db3)(content(Whitespace\"\\n\"))))(Tile((id \
         691cf218-40de-4f3a-ac67-bb73af5b8678)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b2fd2c1-4ac5-4555-9f2d-fefaee1ceb5d)(content(Whitespace\" \
         \"))))(Tile((id ab3e9505-d4cb-4ee5-b534-19561220b54c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b49cbbdf-a180-455c-946e-23f8dcb11b5f)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         85782db1-86bc-4ff9-b1f3-96b7a763a64a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b6ebe75d-f96b-45a1-93a4-b42fc045e4f5)(content(Whitespace\"\\n\")))))";
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
