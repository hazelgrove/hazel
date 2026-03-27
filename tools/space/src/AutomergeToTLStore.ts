/**
 * Apply automerge patches to a tldraw TLStore.
 *
 * Adapted from tldraw4's AutomergeToTLStore.ts.
 * Patches arrive from automerge docs shaped as { store: { ... }, schema: { ... } },
 * so relevant patches have path[0] === "store".
 */

import type { TLRecord, RecordId, TLStore } from "tldraw";
import * as Automerge from "@automerge/automerge";

export function applyAutomergePatchesToTLStore(
  patches: Automerge.Patch[],
  store: TLStore
) {
  const toRemove: TLRecord["id"][] = [];
  const updatedObjects: { [id: string]: TLRecord } = {};

  patches.forEach((rawPatch) => {
    let patch = rawPatch;

    if (!isStorePatch(patch)) return;

    const id = pathToId(patch.path.map((p) => `${p}`));
    const record = updatedObjects[id] || structuredClone(store.get(id) || {});

    switch (patch.action) {
      case "insert": {
        updatedObjects[id] = applyInsertToObject(patch, record);
        break;
      }
      case "put":
        updatedObjects[id] = applyPutToObject(patch, record);
        break;
      case "splice": {
        updatedObjects[id] = applySpliceToObject(patch, record);
        break;
      }
      case "del": {
        if (patch.path.length === 2) {
          // Deleting the whole record (path is ["store", "recordId"])
          toRemove.push(id as TLRecord["id"]);
        } else {
          // Deleting a property inside a record
          updatedObjects[id] = applyDelToObject(patch, record);
        }
        break;
      }
      default: {
        console.log("Unsupported patch:", patch);
      }
    }
  });
  const toPut = Object.values(updatedObjects);

  // put / remove the records in the store

  store.mergeRemoteChanges(() => {
    if (toRemove.length) store.remove(toRemove);
    if (toPut.length) store.put(toPut);
  });
}

const isStorePatch = (patch: Automerge.Patch): boolean => {
  return patch.path[0] === "store" && patch.path.length > 1;
};

// path: ["store", "camera:page:page", "x"] => "camera:page:page"
const pathToId = (path: string[]): RecordId<any> => {
  return path[1] as RecordId<any>;
};

const applyInsertToObject = (
  patch: Automerge.InsertPatch,
  object: any
): TLRecord => {
  const { path, values } = patch;
  let current = object;
  const insertionPoint = path[path.length - 1];
  const pathEnd = path[path.length - 2];
  const parts = path.slice(2, -2);
  for (const part of parts) {
    if (current[part] === undefined) {
      throw new Error("NO WAY");
    }
    current = current[part];
  }
  // splice is a mutator... yay.
  const clone = current[pathEnd].slice(0);
  clone.splice(insertionPoint, 0, ...values);
  current[pathEnd] = clone;
  return object;
};

const applyPutToObject = (patch: Automerge.PutPatch, object: any): TLRecord => {
  const { path, value } = patch;
  let current = object;
  // special case
  if (path.length === 2) {
    // this would be creating the object, but we have done
    return object;
  }

  const parts = path.slice(2, -2);
  const property = path[path.length - 1];
  const target = path[path.length - 2];

  if (path.length === 3) {
    return { ...object, [property]: value };
  }

  // default case
  for (const part of parts) {
    current = current[part];
  }
  current[target] = { ...current[target], [property]: value };
  return object;
};

const applySpliceToObject = (
  patch: Automerge.SpliceTextPatch,
  object: any
): TLRecord => {
  const { path, value } = patch;
  let current = object;
  const insertionPoint = path[path.length - 1];
  const pathEnd = path[path.length - 2];
  const parts = path.slice(2, -2);
  for (const part of parts) {
    if (current[part] === undefined) {
      throw new Error("NO WAY");
    }
    current = current[part];
  }
  // TODO: we're not supporting actual splices yet because TLDraw won't generate them natively
  if (insertionPoint !== 0) {
    throw new Error("Splices are not supported yet");
  }
  current[pathEnd] = value; // .splice(insertionPoint, 0, value)
  return object;
};

const applyDelToObject = (patch: Automerge.Patch, object: any): TLRecord => {
  const { path } = patch;
  if (path.length === 3) {
    delete object[path[2]];
    return object;
  }
  let current = object;
  const parts = path.slice(2, -1);
  for (const part of parts) {
    if (current[part] === undefined) return object;
    current = current[part];
  }
  delete current[path[path.length - 1]];
  return object;
};
