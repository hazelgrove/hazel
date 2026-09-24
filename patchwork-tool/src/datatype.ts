import type { DatatypeImplementation } from "@inkandswitch/patchwork-plugins";
import { initDoc, type HazelDoc } from "../../collab/src/schema";

// A Hazel program as items with Automerge text leaves (docs/collab-modular.md).
export const datatype: DatatypeImplementation<HazelDoc> = {
  init(doc) {
    initDoc(doc, crypto.randomUUID());
  },
  getTitle(doc) {
    return doc.title || "Untitled Hazel";
  },
  setTitle(doc, title) {
    doc.title = title;
  },
};
