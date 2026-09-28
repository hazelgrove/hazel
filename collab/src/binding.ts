// Binds one Hazel to whichever document it should currently show.
//
// The embedding host doesn't hand Hazel a fixed document. Patchwork resolves
// the document a view was opened on to a *backing* that can change while the
// view stays mounted: the per-draft clone when a draft is checked out, and a
// url pinned to historical heads while the history scrubber is open (see
// patchwork-tool/src/tool.ts for where those come from). This is the Hazel
// side of that: `point(url)` swaps the session underneath Hazel, and the
// editing API forwards to whichever session is current.
//
// Sessions are chained through `firstSeq`, so Hazel's bases stay meaningful
// across a switch (an edit against the old document is dropped, not merged
// into the new one). Pointing at a url with heads yields a read-only view.
import type { AutomergeUrl, DocHandle, Repo } from "@automerge/automerge-repo/slim";
import type { HazelDoc, Leaf, NewItem } from "./schema";
import { CollabSession, type Caret, type HazelSide, type Identity } from "./session";

export class HazelBinding {
  #session: CollabSession | null = null;
  #target: AutomergeUrl | null = null; // what we point at (maybe still resolving)
  #destroyed = false;

  constructor(
    readonly repo: Repo,
    readonly hazel: HazelSide,
    public identity: Identity,
  ) {}

  // The current session, if a document has resolved.
  get session(): CollabSession | null {
    return this.#session;
  }

  get url(): AutomergeUrl | null {
    return this.#target;
  }

  get readOnly(): boolean {
    return this.#session?.readOnly ?? false;
  }

  // Show the document at `url` (a plain url, a draft clone's url, or a url
  // with heads for a read-only view). Resolves once Hazel has been sent the
  // new program; a newer `point` supersedes an unresolved older one.
  async point(url: AutomergeUrl): Promise<void> {
    if (this.#destroyed || url === this.#target) return;
    this.#target = url;
    const handle = await this.repo.find<HazelDoc>(url);
    if (this.#destroyed || this.#target !== url) return; // superseded meanwhile
    this.#swap(handle);
  }

  #swap(handle: DocHandle<HazelDoc>) {
    const prev = this.#session;
    prev?.destroy();
    this.#session = new CollabSession(handle, this.hazel, this.identity, {
      firstSeq: prev ? prev.lastSeq + 1 : 1,
    });
  }

  // ---- Hazel's editing API, forwarded to the current session ----

  edit(basis: number, id: string, leaf: Leaf, text: string): number {
    return this.#session?.edit(basis, id, leaf, text) ?? basis;
  }

  insert(item: NewItem, after: string | null): number {
    return this.#session?.insert(item, after) ?? 0;
  }

  remove(id: string) {
    this.#session?.remove(id);
  }

  move(id: string, after: string | null) {
    this.#session?.move(id, after);
  }

  caret(c: Caret | null) {
    this.#session?.caret(c);
  }

  destroy() {
    this.#destroyed = true;
    this.#session?.destroy();
    this.#session = null;
  }
}
