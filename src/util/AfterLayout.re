open Js_of_ocaml;

/* Reading layout without forcing it. A ResizeObserver reports after the
   browser lays a frame out and before it paints, so reads there cost no
   layout of their own. One observer serves every hook, in passes: watched
   elements whose size changed go to their watchers and the reads scheduled
   for the frame run, then the writes these return run, so no read follows
   a write. A scroll counts as a read: it needs layout and doesn't change
   it. Schedule and watch from outside a pass (after_display, an event):
   the observer would deliver from inside it a frame late. Only a frame
   that schedules a read re-arms the observer, so an idle frame costs
   nothing here. */

type write = unit => unit;
type read = unit => write;

/* gets the elements it watches whose border box changed size (each first
   once watched, unless 0×0); it reads, as part of a pass */
type watcher = list(Js.t(Dom_html.element)) => write;

let pending: ref(list(read)) = ref([]);
let watchers: ref(array(watcher)) = ref([||]);

let guard = (name: string, f: unit => 'a, default: 'a): 'a =>
  try(f()) {
  | exn =>
    print_endline("AfterLayout " ++ name ++ ": " ++ Printexc.to_string(exn));
    default;
  };

/* a watched element's watcher, as its index, in a WeakMap so a removed
   element can go */
let ids = lazy(Js.Unsafe.new_obj(Js.Unsafe.global##._WeakMap, [||]));
let id_of = (el: Js.t(Dom_html.element)): option(int) =>
  Js.Optdef.to_option(
    Js.Unsafe.meth_call(Lazy.force(ids), "get", [|Js.Unsafe.inject(el)|]),
  );

let pass =
    (
      entries: Js.t(Js.js_array(Js.t(ResizeObserver.resizeObserverEntry))),
      _,
    )
    : unit => {
  let changed = Array.make(Array.length(watchers^), []);
  for (i in entries##.length - 1 downto 0) {
    Js.Optdef.iter(
      Js.array_get(entries, i),
      e => {
        let el: Js.t(Dom_html.element) = Js.Unsafe.coerce(e##.target);
        switch (id_of(el)) {
        | Some(w) when w < Array.length(changed) =>
          changed[w] = [el, ...changed[w]]
        | _ => ()
        };
      },
    );
  };
  let reads = List.rev(pending^);
  pending := [];
  let watched =
    List.filter_map(
      w =>
        switch (changed[w]) {
        | [] => None
        | els => Some(guard("watcher", () => (watchers^[w])(els), () => ()))
        },
      List.init(Array.length(changed), Fun.id),
    );
  let writes =
    watched @ List.map(read => guard("read", read, () => ()), reads);
  List.iter(write => guard("write", write, ()), writes);
};

/* made on first use: there is no ResizeObserver outside a browser */
let observer: ref(option(Js.t(ResizeObserver.resizeObserver))) = ref(None);
let root = () => Dom_html.document##.documentElement;
let get = (): Js.t(ResizeObserver.resizeObserver) =>
  switch (observer^) {
  | Some(o) => o
  | None =>
    let o = ResizeObserver.observe(~node=root(), ~f=pass, ());
    observer := Some(o);
    o;
  };

/* [read] runs after the next layout, before that frame's paint; the write
   it returns runs once every read of the pass has. The root always has a
   size, so observing it afresh always reports. */
let schedule = (read: read): unit => {
  switch (pending^) {
  | [] =>
    let o = get();
    o##unobserve(root());
    o##observe(root());
  | _ => ()
  };
  pending := [read, ...pending^];
};

/* registers a watcher, for [watch] */
let watcher = (f: watcher): int => {
  watchers := Array.append(watchers^, [|f|]);
  Array.length(watchers^) - 1;
};

let watch = (w: int, el: Js.t(Dom_html.element)): unit =>
  if (id_of(el) == None) {
    ignore(
      Js.Unsafe.meth_call(
        Lazy.force(ids),
        "set",
        [|Js.Unsafe.inject(el), Js.Unsafe.inject(w)|],
      ),
    );
    let options = ResizeObserver.empty_resize_observer_options();
    options##.box := Js.string("border-box");
    get()##observe_withOptions(el, options);
  };

let unwatch = (el: Js.t(Dom_html.element)): unit =>
  if (id_of(el) != None) {
    ignore(
      Js.Unsafe.meth_call(
        Lazy.force(ids),
        "delete",
        [|Js.Unsafe.inject(el)|],
      ),
    );
    get()##unobserve(el);
  };
