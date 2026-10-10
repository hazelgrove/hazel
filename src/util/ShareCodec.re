/* Sending the eval worker only what it does not have yet.

   Each request carried the whole program and its statics, ~740 KB of
   Marshal, though a livelit release changes one small item: encoding it
   was most of a release's commit on the main thread, and decoding it a
   cost on the worker. Both sides now keep a table of the values the worker
   holds, keyed by Merkle hash (ValueHash, which gives a copy the hash of
   its original):

   - the CLIENT replaces each value it has sent before (one of at least
     [min_size] parts) with a small reference block carrying its hash, and
     remembers the hashes of the ones it sends now;
   - the WORKER, after unmarshaling, swaps each reference for the value it
     kept, and keeps the new ones.

   So a request is the changed paths plus references, and the worker's
   request shares the unchanged values with the last one (its reuse check
   then finds them the same by identity).

   The two tables must agree. The client resets both (the reset flag on a
   message) when the worker is new, after a failure, or when the table
   grows past [max_entries]; the worker answers a reference it cannot
   resolve with an error, and the client then resends in full. Values are
   assumed immutable once sent. */
open Js_of_ocaml;

let min_size = 16;
let max_entries = 200_000;

let impl: Lazy.t(Js.Unsafe.any) =
  lazy(
    Js.Unsafe.fun_call(
      Js.Unsafe.js_expr(
        {js|(function (hash, MIN) {
  const MARK = "\u0001hazel-share-ref";
  const key = h => h[0] + ":" + h[1];
  const isRef = v => Array.isArray(v) && v.length === 4 && v[1] === MARK;

  /* client: hashes of the values the worker has */
  const sent = new Set();
  const encode = root => {
    const copies = new Map(), fresh = [];
    const walk = v => {
      if (!Array.isArray(v)) return v;
      const done = copies.get(v);
      if (done !== undefined) return done;
      const h = hash(v);
      if (h !== null && h[2] >= MIN && sent.has(key(h))) {
        const r = [0, MARK, h[0], h[1]];
        copies.set(v, r);
        return r;
      }
      const out = new Array(v.length);
      copies.set(v, out);           /* before the fields: keeps sharing */
      for (let i = 0; i < v.length; i++) out[i] = i === 0 ? v[0] : walk(v[i]);
      if (h !== null && h[2] >= MIN) fresh.push(key(h));
      return out;
    };
    const out = walk(root);
    for (const k of fresh) sent.add(k);
    return out;
  };

  /* worker: the values themselves, by hash */
  const table = new Map();
  const decode = root => {
    const resolved = new Set();
    const resolve = v => {
      if (!Array.isArray(v)) return v;
      if (isRef(v)) {
        const o = table.get(v[2] + ":" + v[3]);
        if (o === undefined) throw new Error("ShareCodec: unknown reference");
        return o;
      }
      if (resolved.has(v)) return v;
      resolved.add(v);
      for (let i = 1; i < v.length; i++) v[i] = resolve(v[i]);
      return v;
    };
    const out = resolve(root);
    /* keep the new values: the ones just unmarshaled, not those already
       kept (which their parents now share) */
    try {
      const keep = v => {
        if (!Array.isArray(v) || !resolved.has(v)) return;
        resolved.delete(v);
        const h = hash(v);
        if (h !== null && h[2] >= MIN && !table.has(key(h))) table.set(key(h), v);
        for (let i = 1; i < v.length; i++) keep(v[i]);
      };
      keep(out);
    } catch (_) {
      /* too deep to keep: forget everything; the client's next reference
         fails, and it resends in full */
      table.clear();
    }
    return out;
  };

  return {
    encode, decode,
    resetClient: () => sent.clear(), clientSize: () => sent.size,
    resetWorker: () => table.clear(), workerSize: () => table.size,
  };
})|js},
      ),
      [|ValueHash.js_hash(), Js.Unsafe.inject(min_size)|],
    )
  );

let call = (name, args) =>
  Js.Unsafe.fun_call(Js.Unsafe.get(Lazy.force(impl), name), args);

module Client = {
  /* v with every value the worker already has replaced by a reference;
     what it sends now is remembered as sent. Can throw (a value too deep
     to walk): then send v as it is, and reset. */
  let encode = (v: 'a): 'a => call("encode", [|Js.Unsafe.inject(v)|]);
  let reset = (): unit => call("resetClient", [||]);
  let size = (): int => call("clientSize", [||]);
};

module Worker = {
  /* v (unmarshaled) with every reference swapped for the value kept for
     it; raises on a reference it has no value for */
  let decode = (v: 'a): 'a => call("decode", [|Js.Unsafe.inject(v)|]);
  let reset = (): unit => call("resetWorker", [||]);
  let size = (): int => call("workerSize", [||]);
};
