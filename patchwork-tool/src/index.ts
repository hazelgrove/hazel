// Patchwork loads this entry module inside a Web Worker and structuredClones
// each plugin entry: metadata only at the top level, code behind `load()`,
// and no runtime imports here (the worker has no importmap).
import type { Datatype, Tool } from "@inkandswitch/patchwork-plugins";
import type { HazelDoc } from "../../collab/src/schema";

export const plugins: (Datatype<HazelDoc> | Tool<HazelDoc>)[] = [
  {
    type: "patchwork:datatype",
    id: "hazel",
    name: "Hazel",
    icon: "TreeDeciduous",
    async load() {
      return (await import("./datatype")).datatype;
    },
  },
  {
    type: "patchwork:tool",
    id: "hazel",
    name: "Hazel",
    icon: "TreeDeciduous",
    supportedDatatypes: ["hazel"],
    async load() {
      return (await import("./tool")).default;
    },
  },
];
