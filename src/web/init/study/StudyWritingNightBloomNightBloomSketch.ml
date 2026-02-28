let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / night-bloom / night-bloom-sketch",
    {
      segment =
        "((Secondary((id \
         8344e312-5233-4b4d-ba52-0331809cb9ff)(content(Comment\"# NIGHT BLOOM \
         FILTER TASK                        #\"))))(Secondary((id \
         e5df0b3b-ca82-4cf7-9b3b-89a1277d9f7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         56901bae-9e84-4f39-8cb2-8324f4124dfa)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         541e9075-772c-4c54-a59f-29d863c640fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e49543c-3e99-4ed9-ad41-bcaade3e2e25)(content(Comment\"# A plant \
         catalog has entries like:              #\"))))(Secondary((id \
         07dd5ddc-966a-4ef2-b9bf-9717fafc1b18)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d684e61-0b22-465b-a75b-6821dc1aa8fd)(content(Comment\"#   \
         \\\"Starbloom [night] 200ml\\\"                    \
         #\"))))(Secondary((id \
         5d91276d-3237-4148-bd82-100dfd8b9c76)(content(Whitespace\"\\n\"))))(Secondary((id \
         92ea54d6-5fc3-455c-8303-d9d541e91e0c)(content(Comment\"#   \
         \\\"Sunfern [day] 150ml\\\"                        \
         #\"))))(Secondary((id \
         4070c861-5d6c-43c4-a434-00e6cc391f0f)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a08f4b1-7f78-421a-8698-7a1f14a79cf7)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         f2045cdb-46fa-48e1-bd48-e6937587576e)(content(Whitespace\"\\n\"))))(Secondary((id \
         de47f444-f2ba-478f-9c98-135d6b0f0ce6)(content(Comment\"# Filter to \
         night-blooming plants and extract    #\"))))(Secondary((id \
         d7b11002-987d-4ad8-9810-1c71c8636127)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e89258a-aa33-47d1-b9ef-d77bb5a3b0df)(content(Comment\"# just their \
         names: [\\\"Starbloom\\\", \\\"Moonvine\\\"]    #\"))))(Secondary((id \
         ef5115f0-d0a6-4d0b-9e10-6f02220e03fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         caf8c097-f941-45fa-8dcc-9ea7f9c96770)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         47d5084c-7c7b-4329-a12e-3f8b0714c5c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         1999d7cb-c403-4f27-84e9-f565a0e1ef2b)(content(Comment\"# \
         Steps:                                         #\"))))(Secondary((id \
         8aa85b58-1290-436c-8bb5-22373a14159c)(content(Whitespace\"\\n\"))))(Secondary((id \
         da1d0cbf-577d-45ba-9745-b637efc8455c)(content(Comment\"#   1. \
         is_night: check if entry contains \\\"night\\\" #\"))))(Secondary((id \
         64e6a169-a224-45a3-8e94-c082f42a6d62)(content(Whitespace\"\\n\"))))(Secondary((id \
         86b8c7a6-2560-4717-87ab-6807ad779af1)(content(Comment\"#   2. \
         extract_name: get the first word          #\"))))(Secondary((id \
         864a62be-1fab-4800-bfbc-ec8858fc2cd2)(content(Whitespace\"\\n\"))))(Secondary((id \
         342e1e62-c2b8-43ff-9a29-5299748c17b4)(content(Comment\"#   3. Combine \
         with filter and map               #\"))))(Secondary((id \
         f6000ff0-212b-4ce5-9966-8bfb768538a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         850983d8-5e5f-4b39-85ab-d2ceb8c21c85)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         260522fa-261a-491a-979a-b2785ee6759c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c2eb57e9-11b8-499e-aa8f-0649989ab289)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         c9d560a2-d8a4-4f7c-8e67-dc98b02cfb15)(content(Whitespace\"\\n\"))))(Secondary((id \
         03bf44e9-b20e-455b-aad7-dcb286c5f430)(content(Comment\"#   \
         string_match(pattern, str) -> Bool           #\"))))(Secondary((id \
         f2949f46-c96c-41a2-a36f-81e82dfe7d91)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbeec2c0-a459-4877-92a1-739c025f33ce)(content(Comment\"#   \
         string_split(separator, str) -> [String]     #\"))))(Secondary((id \
         77d84894-fdb7-4229-b979-2a69c9e1e953)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d05775d-dd1d-4c96-b9f8-86d3a74738d2)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         e83c15e2-6cbb-447f-bf4a-0b7d1497e33a)(content(Whitespace\"\\n\"))))(Secondary((id \
         dc7824fa-ba13-4c1b-aec5-2f7260a8aa2b)(content(Comment\"#   \
         filter(list, predicate) -> list              #\"))))(Secondary((id \
         fc03c7d1-5ae6-45aa-9dd2-6271a73dc268)(content(Whitespace\"\\n\"))))(Secondary((id \
         79482168-f479-4d69-a8e5-f8fef3cc1710)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         a06e8e85-f0cb-401a-b83c-abc5d26f481a)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d13bb4d-c71b-4343-92cf-bab54865166b)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         0e6dd3ef-7a7c-4440-8e2e-f6750671d0fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ddaaaf7-3837-43f1-a1e8-b05a6ca881f8)(content(Comment\"# Note: \
         string_match uses regex patterns.        #\"))))(Secondary((id \
         08c809a8-f549-4525-9e02-ab3994920ba1)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8131ac4-ea82-4983-8002-e340bc7a2e72)(content(Comment\"# The pattern \
         \\\"[abc]\\\" matches any of a, b, c.   #\"))))(Secondary((id \
         6331727e-d247-41e5-83cb-bd56d04d5d2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         8cbb6e6c-ad95-47a7-ad87-7e9c33f3d817)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         47fed6a2-e46f-477e-bbab-b712b1bff1da)(content(Whitespace\"\\n\"))))(Secondary((id \
         6642f92b-32e6-4891-a408-09424a2acd26)(content(Comment\"# Tip: Use \
         probes to see what your pattern       #\"))))(Secondary((id \
         f63aac01-5030-485e-af4f-dd7a5aed6530)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d44b9b3-4f23-4954-a5d4-ee4b2b5d869c)(content(Comment\"# actually \
         matches -- regex can be surprising!   #\"))))(Secondary((id \
         0d46e161-1450-4a22-bc30-b51c4d6b2076)(content(Whitespace\"\\n\"))))(Secondary((id \
         b395a118-dd52-4d85-a1c1-f8f64f6b6fd6)(content(Whitespace\"\\n\"))))(Tile((id \
         d312f112-52b0-49a8-9be8-0b7b1b78c225)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         90445d65-23d1-4997-95a8-d61ad33b03df)(content(Whitespace\" \
         \"))))(Tile((id \
         f25b0201-bf7c-436e-91f9-e365f7e36014)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bdc3da73-6d21-4177-85bc-7b66634818be)(content(Whitespace\" \
         \")))))((Secondary((id \
         6d852e97-dd7b-4105-8f26-0bd3fd0d4464)(content(Whitespace\" \
         \"))))(Tile((id b6fd1557-0d1d-4d39-8d38-1dc64f8a30bc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         966e27a1-4730-49bf-be52-4c7ac50befae)(content(Whitespace\"\\n\"))))(Tile((id \
         5854fbab-8ff0-47e4-9fd7-060c9a8d68e4)(label(\"\\\"Starbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         09fe752e-45e0-4d74-9539-9fdbc899ab6c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ba479d2-a7de-4526-a31f-5d76e43ab025)(content(Whitespace\"\\n\"))))(Tile((id \
         3f69a83f-bac3-4dff-a7b2-a5333b7a3daf)(label(\"\\\"Sunfern [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         ea5d6db6-c4e4-4940-a91c-ab99591912e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e55ddfa-2871-4da2-acde-1f0460a57ba8)(content(Whitespace\"\\n\"))))(Tile((id \
         0517418f-cae0-48b6-b08f-943d1b119a1a)(label(\"\\\"Moonvine [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1f60b8cc-933f-4699-8491-7db567a57066)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cedc7c2c-bdc2-4085-8dd4-c92516645391)(content(Whitespace\"\\n\"))))(Tile((id \
         c13c5f07-663e-418c-ac91-bfcb2b637245)(label(\"\\\"Thornrose [day] \
         100ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         48c22788-bfaf-4a7b-a79a-693b273e5972)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         386c2f55-eea7-46cb-9235-ee02e0ebd913)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         50831fa4-9e4f-40d8-be54-d5b93e9a1660)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a35c9a5-0b7e-4c0d-8c7b-d7a0c6c2facb)(content(Whitespace\"\\n\"))))(Secondary((id \
         501ecc56-facd-460e-9450-a1b1b5e85357)(content(Comment\"# Check if \
         entry is a night-blooming plant #\"))))(Secondary((id \
         2cb56fed-c4db-459d-ba86-9f62a49cbd0b)(content(Whitespace\"\\n\"))))(Tile((id \
         d78975df-53f4-4c52-b6ef-e6fc22bf69de)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cad290ab-1269-4285-9e61-63fcd718e06a)(content(Whitespace\" \
         \"))))(Tile((id \
         1867db90-705d-4cd0-8282-3f74065b9eea)(label(is_night))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8264b012-3a78-41d9-bc75-7c416ad60e65)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e7db911d-53bb-45b4-92a9-0d6400063823)(content(Whitespace\" \
         \"))))(Tile((id \
         f23a72f6-0b92-42a8-8a3e-59c8f183f24f)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         62305642-260f-4048-8336-dc50e85535f6)(content(Whitespace\" \
         \"))))(Tile((id \
         c543266a-0814-4713-9727-65f86db4691e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         53f5c8e3-b441-4533-8531-d979ad1c20eb)(content(Whitespace\" \
         \"))))(Tile((id \
         5df382f0-2536-4746-a2b5-ebb8258ae62f)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         003e4bc0-f08f-461d-bee2-7a9466132988)(content(Whitespace\" \
         \")))))((Secondary((id \
         850283b2-9c9d-47c7-9618-7dbaf65eb16e)(content(Whitespace\" \
         \"))))(Tile((id 4ab74f59-efd8-4c78-9663-282cd85ad592)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3bfe699c-d06a-4ab8-997d-6ee583fc6929)(content(Whitespace\" \
         \"))))(Tile((id \
         a75a847b-0537-4814-9cd9-31e484edd427)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         68ba42ba-8b7f-4c11-8507-36714f7ac752)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         adefb70e-e67b-4ca4-9050-9fcefe1bc74c)(content(Whitespace\"\\n\"))))(Tile((id \
         7940eafb-85f0-4cc3-a1da-e26652e95ce1)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         00134331-7154-457b-ba30-de1e337a80c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         90641597-6843-4731-83e1-a78992c4e9d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         9bccc533-1753-435a-a821-2f8545b0861a)(content(Whitespace\"\\n\"))))(Secondary((id \
         808c52b3-1908-472c-a304-7e8366c1ec13)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bb2f7d9b-e43a-4283-b0c7-047572fcced7)(content(Whitespace\"\\n\"))))(Secondary((id \
         26a9c4aa-54e7-47fb-bc16-76444e2c60b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         4260e987-8116-4bd0-bdd9-f406aba26b8d)(content(Comment\"# Extract just \
         the plant name from an entry #\"))))(Secondary((id \
         441c6415-cbf4-4b7e-98c6-8cabf66721d7)(content(Whitespace\"\\n\"))))(Tile((id \
         b1ad29d0-1d69-47bb-a659-91478b347ebd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bc4b95be-394f-412a-82d4-97c74ecf2b4d)(content(Whitespace\" \
         \"))))(Tile((id \
         e3274195-e58a-4c6d-9df9-b40321bf4505)(label(extract_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         76361133-a8f4-4d42-9c48-8c5f6eef9e5e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e78d55a7-dd00-43e3-a819-60b7fc585d9f)(content(Whitespace\" \
         \"))))(Tile((id \
         bd2f7695-f6e4-4ca4-86aa-be9bffe4e6e9)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         54f11871-f3b4-4944-9c91-d21570b538b2)(content(Whitespace\" \
         \"))))(Tile((id \
         0d506548-d6fa-4577-a673-71f4f072f867)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9c8712f1-91b6-4bdc-8716-88d95e7d2004)(content(Whitespace\" \
         \"))))(Tile((id \
         23cc2eee-1aa0-4257-b81b-287f4d6de710)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         94a5e8cd-ac13-4fa6-abbc-09080912cfa2)(content(Whitespace\" \
         \")))))((Secondary((id \
         6b22325c-0236-41c8-b156-c33ecd9f6fcb)(content(Whitespace\" \
         \"))))(Tile((id 8e9d01a8-8594-491b-abb0-f7080fb7cbcc)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e75d4d32-0b32-491e-a02e-286f651397c6)(content(Whitespace\" \
         \"))))(Tile((id \
         733f88f2-5e34-48da-8d3c-743396b05311)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ce36e641-4b77-4f88-8603-c3f788668477)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ff5a151a-d9e0-43d6-8ba5-ba80c5bbad36)(content(Whitespace\"\\n\"))))(Tile((id \
         24a1b45b-755f-4ed6-aa6a-40a93fe2f1c4)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c57eac54-2ea9-41a3-a3d3-55ddfe53fa38)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1737fe1-5616-4d28-a737-ad13213652c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4615ab4-2395-460a-9665-a75fbb2dad0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         3902b95d-7f0d-4a5d-8c1d-c6e3be0ca510)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5c0e4610-3a04-4e51-8f67-2e9bb1bcc7cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9ed6dcb-33f1-4703-90f4-cf5f741324ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbfebe91-175f-4ee6-9e0e-35126c3584f8)(content(Comment\"# Combine: \
         filter night entries, then extract names #\"))))(Secondary((id \
         c1f9c18e-e74a-4de1-982f-0d723113b110)(content(Whitespace\"\\n\"))))(Tile((id \
         c9217f14-e94c-462b-8ca1-37f007e51365)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fd6b6c7c-2298-42a0-a27b-7bc45677e118)(content(Whitespace\" \
         \"))))(Tile((id \
         fe0b39c7-8139-4b2d-bcce-c0247bb45f9f)(label(night_names))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a9fbd5b7-ef00-4cc6-b1f6-fec326557f5a)(content(Whitespace\" \
         \")))))((Secondary((id \
         cb6070b1-5860-4d8b-b22c-ddbcf2e36515)(content(Whitespace\"\\n\"))))(Tile((id \
         b8d3ce8d-3ed0-445b-9034-bb89faf812c4)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f8abb07e-870c-4c6d-88cd-fa1fe9102861)(content(Whitespace\"\\n\"))))(Secondary((id \
         557ef533-b6d9-4549-8e87-5e319df07e12)(content(Whitespace\"\\n\"))))(Secondary((id \
         70eef255-f9c8-44ab-9812-579b4481c0b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         040ef719-bdb5-47f1-bc48-0fdbda4d10c5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6627db7d-6e66-4ace-b448-d89ee69dd9c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         771b6d87-905a-4e9e-8fa2-b25f0dc2e7f7)(content(Whitespace\"\\n\"))))(Tile((id \
         596be7f8-9361-482b-8a83-6ad63eddd1b9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         393d4ab3-93e9-4687-9c12-7c80bf84f2a9)(content(Whitespace\" \
         \"))))(Tile((id \
         1cb3fe27-5e1d-4cfe-80a5-43514b1e4653)(label(night_names))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9442c869-9766-48df-a106-a030dbdcca0c)(content(Whitespace\" \
         \"))))(Tile((id \
         91790723-dc4a-41c1-a2ae-a90e24c5a63f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         feb5b25c-32f3-4903-9eae-25d94b95c59f)(content(Whitespace\" \
         \"))))(Tile((id a1faf96d-24d7-4a55-b745-8d4e016e8fe4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d7cc6c06-2db2-4753-b458-d5963c661d2d)(label(\"\\\"Starbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc7e25f9-a1d7-4b41-8042-e1a4515ff92a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c4c5f4e-f14f-4b93-8156-5663e80342f8)(content(Whitespace\" \
         \"))))(Tile((id \
         9318d2b5-dfed-492a-84e0-6296d9aa2b97)(label(\"\\\"Moonvine\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ea0ed86f-ac17-4066-9acd-6bd6c5ede801)(content(Whitespace\" \
         \")))))))))(Tile((id \
         9a1e7d1f-af74-4d48-a7d2-2704e471017c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b64d887-12d7-492b-8b95-6419490e0043)(content(Whitespace\"\\n\"))))(Secondary((id \
         e620d342-5164-42b3-900c-36898dabe593)(content(Whitespace\"\\n\"))))(Tile((id \
         71f602bf-0abf-4b67-8dbd-8d52bdd78b06)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         eb7cce1c-6068-48aa-ab78-5ddf81065d4c)(content(Whitespace\" \
         \"))))(Tile((id \
         31a50944-5433-4b89-a3a3-099a47f05730)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e416ada1-7c28-4abb-a0a3-a7acb99469ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d58d8b1f-c423-4a44-bc6a-9d61fccf793b)(label(\"\\\"Starbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1beb00a7-ff08-4585-b3ea-ba2df31018d1)(content(Whitespace\" \
         \"))))(Tile((id \
         a0d5b244-ecce-4cd4-9114-17d29be7fe9b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14491a16-8c21-4241-bb5e-dc1509e62fe3)(content(Whitespace\" \
         \"))))(Tile((id \
         3c0a7809-a3ec-47c7-90a5-edb877405f6b)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f218891e-1f6c-4952-ab1d-40404c3a1fdb)(content(Whitespace\" \
         \")))))))))(Tile((id \
         cf91132f-29eb-4986-9608-f19784e6df08)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05b89764-048f-4a12-bcd2-e7c6b367b867)(content(Whitespace\"\\n\"))))(Secondary((id \
         bee5e693-36f3-4ab0-9346-ddd9a9e1819b)(content(Whitespace\"\\n\"))))(Tile((id \
         a9320bbe-4838-4717-abce-82523625462c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         23d1d90a-5411-4321-ab36-96723c5d92bb)(content(Whitespace\" \
         \"))))(Tile((id \
         f454a098-5cd9-4e52-b981-0fd9cbd77a7f)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5ec594c-c319-4fcd-a788-97dc34d10bec)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3ac76e6-7950-493b-bef2-c76e469b6b2a)(label(\"\\\"Sunfern [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c2310d03-f090-4e0f-b2a1-a2bcb44c058a)(content(Whitespace\" \
         \"))))(Tile((id \
         7e061349-7574-457c-9842-1eaa35dd5252)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d52fcf04-4c00-440c-bb4e-4958516d3f28)(content(Whitespace\" \
         \"))))(Tile((id \
         f0139c33-1602-4fc3-9d46-d92018e6c00f)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b865b958-30cb-4b5a-bc4a-1f5b3276b9f1)(content(Whitespace\" \
         \")))))))))(Tile((id \
         3cc1fc3f-a3c7-4217-8c49-d34cff3edfca)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14a63c62-0b34-49fe-a521-f6767732765a)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cb95701-69ad-43c2-8cad-7927ef0139ad)(content(Whitespace\"\\n\"))))(Tile((id \
         e71b5dc7-fbec-4c14-9668-94d0464738fa)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         01dba9cf-786c-4e4e-94be-1113827b7df9)(content(Whitespace\" \
         \"))))(Tile((id \
         8310dd70-18f7-41d0-98b4-93cc35ca3ee6)(label(extract_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5c98f9e-cce8-428b-ae94-75c21b559ff9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72da2246-ab46-46d9-8dfb-29bd169d0f74)(label(\"\\\"Moonvine [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         66bd6f55-44e6-484e-a8eb-0a52f3b11677)(content(Whitespace\" \
         \"))))(Tile((id \
         78a7d385-065e-42d4-905b-1375bae29b8b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         010f21b2-bd73-4338-8700-9f3a004e68ce)(content(Whitespace\" \
         \"))))(Tile((id \
         2029c3dd-b2fc-47fb-8ba2-18eca17a403f)(label(\"\\\"Moonvine\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9141395c-3a36-4723-9c91-1c1690e95390)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d31f19e4-0f9b-4d38-827a-3bf12538ef1d)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# NIGHT BLOOM FILTER TASK                        #\n\
         #                                                #\n\
         # A plant catalog has entries like:              #\n\
         #   \"Starbloom [night] 200ml\"                    #\n\
         #   \"Sunfern [day] 150ml\"                        #\n\
         #                                                #\n\
         # Filter to night-blooming plants and extract    #\n\
         # just their names: [\"Starbloom\", \"Moonvine\"]    #\n\
         #                                                #\n\
         # Steps:                                         #\n\
         #   1. is_night: check if entry contains \"night\" #\n\
         #   2. extract_name: get the first word          #\n\
         #   3. Combine with filter and map               #\n\
         #                                                #\n\
         # Available functions:                           #\n\
         #   string_match(pattern, str) -> Bool           #\n\
         #   string_split(separator, str) -> [String]     #\n\
         #   nth(list, index) -> element                  #\n\
         #   filter(list, predicate) -> list              #\n\
         #   map(list, fn) -> list                        #\n\
         #                                                #\n\
         # Note: string_match uses regex patterns.        #\n\
         # The pattern \"[abc]\" matches any of a, b, c.   #\n\
         #                                                #\n\
         # Tip: Use probes to see what your pattern       #\n\
         # actually matches -- regex can be surprising!   #\n\n\
         let entries = [\n\
         \"Starbloom [night] 200ml\",\n\
         \"Sunfern [day] 150ml\",\n\
         \"Moonvine [night] 175ml\",\n\
         \"Thornrose [day] 100ml\"\n\
         ] in\n\n\
         # Check if entry is a night-blooming plant #\n\
         let is_night: String -> Bool = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Extract just the plant name from an entry #\n\
         let extract_name: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Combine: filter night entries, then extract names #\n\
         let night_names =\n\
         ?\n\n\n\n\
         in\n\n\
         test night_names == [\"Starbloom\", \"Moonvine\"] end;\n\n\
         test is_night(\"Starbloom [night] 200ml\") == true end;\n\n\
         test is_night(\"Sunfern [day] 150ml\") == false end;\n\n\
         test extract_name(\"Moonvine [night] 175ml\") == \"Moonvine\" end\n";
      refractors = "()";
    } )
