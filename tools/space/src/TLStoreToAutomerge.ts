/**
 * Apply tldraw store changes to an automerge doc.
 *
 * Adapted from tldraw4's TLStoreToAutomerge.ts.
 * The doc is shaped as { store: { ... }, schema: { ... } }.
 * Changes are written into doc.store.
 */

import type { RecordsDiff, TLRecord } from "tldraw";

export type TldrawDoc = {
  store: Record<string, any>;
  schema: any;
  "@patchwork"?: { type: "tldraw" };
};

/** Prepares a value for storing in Automerge (deep recursively)
 *  For now, all it does is convert strings to RawStrings.
 *  This is critical for performance because TLDraw can generate large
 *  strings for inline assets, which create huge documents.
 *  There's also no support for string merging anyway in TLDraw,
 *  so raw strings work fine.
 */
export function tldrawValueToAutomergeValue(value: any): any {
  if (Array.isArray(value)) {
    return value.map(tldrawValueToAutomergeValue);
  }
  if (value != null && typeof value === "object") {
    const obj: any = {};
    for (const key in value) {
      obj[key] = tldrawValueToAutomergeValue(value[key]);
    }
    return obj;
  }
  return value;
}

export function applyTLStoreChangesToAutomerge(
  doc: TldrawDoc,
  changes: RecordsDiff<TLRecord>
) {
  Object.values(changes.added).forEach((record) => {
    doc.store[record.id] = tldrawValueToAutomergeValue(record);
  });

  Object.values(changes.updated).forEach(([_, record]) => {
    deepCompareAndUpdate(doc.store[record.id], record);
  });

  Object.values(changes.removed).forEach((record) => {
    delete doc.store[record.id];
  });
}

function deepCompareAndUpdate(objectA: any, objectB: any) {
  if (Array.isArray(objectB)) {
    if (!Array.isArray(objectA)) {
      objectA = objectB.map(tldrawValueToAutomergeValue);
    } else {
      for (let i = 0; i < objectB.length; i++) {
        if (i >= objectA.length) {
          objectA.push(tldrawValueToAutomergeValue(objectB[i]));
        } else {
          if (
            (objectB[i] != null && typeof objectB[i] === "object") ||
            Array.isArray(objectB[i])
          ) {
            deepCompareAndUpdate(objectA[i], objectB[i]);
          } else if (objectA[i] !== objectB[i]) {
            objectA[i] = tldrawValueToAutomergeValue(objectB[i]);
          }
        }
      }
      if (objectA.length > objectB.length) {
        objectA.splice(objectB.length);
      }
    }
  } else if (objectB != null && typeof objectB === "object") {
    for (const key in objectB) {
      const value = objectB[key];
      if (objectA[key] === undefined) {
        objectA[key] = tldrawValueToAutomergeValue(value);
      } else {
        if (
          (value != null && typeof value === "object") ||
          Array.isArray(value)
        ) {
          deepCompareAndUpdate(objectA[key], value);
        } else if (objectA[key] !== value) {
          objectA[key] = tldrawValueToAutomergeValue(value);
        }
      }
    }
    for (const key in objectA) {
      if ((objectB as any)[key] === undefined) {
        delete objectA[key];
      }
    }
  }
}
