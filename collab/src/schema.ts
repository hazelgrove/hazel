// The shared document: a Hazel program as items with text leaves.
// See docs/collab-modular.md.
import { generateKeyBetween } from "fractional-indexing";

export type Kind = "def" | "type" | "module" | "stmt" | "tail";
export type Leaf = "lead" | "header" | "body";
export const LEAVES: Leaf[] = ["lead", "header", "body"];

export type Item = {
  // module item id, or null for the top level
  parent: string | null;
  // fractional index; siblings sort by (order, id)
  order: string;
  kind: Kind;
  // Automerge text. `lead`: comments before a def-like item.
  // `header`: pattern + annotation / type name / module name ("" for stmt, tail).
  // `body`: RHS / statement / trailing expression.
  lead: string;
  header: string;
  body: string;
};

export type HazelDoc = {
  "@patchwork"?: { type: string };
  title: string;
  version: 1;
  items: { [id: string]: Item };
};

// An item as exchanged with Hazel: plain values, id included.
export type ItemSnapshot = Item & { id: string };

// What Hazel sends when creating an item (order/parent are placement).
export type NewItem = {
  id: string;
  kind: Kind;
  lead?: string;
  header?: string;
  body?: string;
};

export const itemPath = (id: string) => ["items", id];
export const leafPath = (id: string, leaf: Leaf) => ["items", id, leaf];

export function snapshot(doc: HazelDoc, id: string): ItemSnapshot | undefined {
  const it = doc.items?.[id];
  if (!it) return undefined;
  return {
    id,
    parent: it.parent ?? null,
    order: String(it.order),
    kind: it.kind,
    lead: String(it.lead ?? ""),
    header: String(it.header ?? ""),
    body: String(it.body ?? ""),
  };
}

const byOrder = (a: ItemSnapshot, b: ItemSnapshot) =>
  a.order < b.order ? -1 : a.order > b.order ? 1 : a.id < b.id ? -1 : a.id > b.id ? 1 : 0;

// Children of `parent` in program order. A block's tail always sorts last,
// whatever its order key says, so concurrent inserts can't land after it.
export function children(doc: HazelDoc, parent: string | null): ItemSnapshot[] {
  const out: ItemSnapshot[] = [];
  for (const id of Object.keys(doc.items ?? {})) {
    const s = snapshot(doc, id)!;
    if ((s.parent ?? null) === parent) out.push(s);
  }
  out.sort(byOrder);
  const tails = out.filter((s) => s.kind === "tail");
  return [...out.filter((s) => s.kind !== "tail"), ...tails];
}

// Every item, parents before children, siblings in program order.
export function allItems(doc: HazelDoc): ItemSnapshot[] {
  const out: ItemSnapshot[] = [];
  const visit = (parent: string | null) => {
    for (const s of children(doc, parent)) {
      out.push(s);
      visit(s.id);
    }
  };
  visit(null);
  return out;
}

// An order key placing an item right after `after` (null = first) among
// `parent`'s children, never past the tail.
export function orderAfter(
  doc: HazelDoc,
  parent: string | null,
  after: string | null,
  exclude?: string,
): string {
  const sibs = children(doc, parent).filter((s) => s.kind !== "tail" && s.id !== exclude);
  const i = after === null ? -1 : sibs.findIndex((s) => s.id === after);
  const lo = i >= 0 ? sibs[i].order : null;
  const hi = i + 1 < sibs.length ? sibs[i + 1].order : null;
  // Equal neighbouring keys (possible after concurrent inserts) have no key
  // strictly between them; fall back to just above the lower one.
  if (lo !== null && hi !== null && lo >= hi) return generateKeyBetween(lo, null);
  return generateKeyBetween(lo, hi);
}

// Seed a document from an ordered list of items (e.g. Hazel's current
// program). Called inside a change.
export function seedItems(doc: HazelDoc, items: NewItem[], parent: string | null = null) {
  let prev: string | null = null;
  for (const it of items) {
    const order: string = it.kind === "tail" ? "~" : generateKeyBetween(prev, null);
    if (it.kind !== "tail") prev = order;
    doc.items[it.id] = {
      parent,
      order,
      kind: it.kind,
      lead: it.lead ?? "",
      header: it.header ?? "",
      body: it.body ?? "",
    };
  }
}

export function initDoc(doc: HazelDoc, tailId: string) {
  doc.title ??= "Untitled Hazel";
  doc.version = 1;
  doc.items ??= {};
  if (!Object.values(doc.items).some((it) => it.kind === "tail" && it.parent === null)) {
    seedItems(doc, [{ id: tailId, kind: "tail" }]);
  }
}
