/* Utilities for serializing BigInteger.js objects through web worker postMessage.
   BigInteger.js objects don't survive structured clone - their prototype
   methods (like .lt()) are lost. We convert to tagged strings and back. */

let serialize_js: Obj.t => Obj.t =
  Js_of_ocaml.Js.Unsafe.pure_js_expr(
    {|
    (function() {
      function serializeBigInts(obj) {
        if (obj === null || obj === undefined) return obj;
        if (typeof obj === 'object' && obj.caml_custom === '_z') {
          return { __hazel_bigint__: obj.toString() };
        }
        if (Array.isArray(obj)) return obj.map(serializeBigInts);
        if (typeof obj === 'object') {
          var result = {};
          for (var key in obj) {
            if (obj.hasOwnProperty(key)) result[key] = serializeBigInts(obj[key]);
          }
          return result;
        }
        return obj;
      }
      return serializeBigInts;
    })()
    |},
  );

let deserialize_js: Obj.t => Obj.t =
  Js_of_ocaml.Js.Unsafe.pure_js_expr(
    {|
    (function() {
      function deserializeBigInts(obj) {
        if (obj === null || obj === undefined) return obj;
        if (typeof obj === 'object' && obj.__hazel_bigint__ !== undefined) {
          // Use the zarith runtime's function to reconstruct BigInts
          var runtime = globalThis.jsoo_runtime;
          if (runtime && runtime.jsoo_z_of_js_string_base) {
            return runtime.jsoo_z_of_js_string_base(10, obj.__hazel_bigint__);
          }
          console.error('jsoo_runtime.jsoo_z_of_js_string_base not available');
          return obj;
        }
        if (Array.isArray(obj)) return obj.map(deserializeBigInts);
        if (typeof obj === 'object') {
          var result = {};
          for (var key in obj) {
            if (obj.hasOwnProperty(key)) result[key] = deserializeBigInts(obj[key]);
          }
          return result;
        }
        return obj;
      }
      return deserializeBigInts;
    })()
    |},
  );

let serialize = (x: 'a): 'a => Obj.magic(serialize_js(Obj.magic(x)));
let deserialize = (x: 'a): 'a => Obj.magic(deserialize_js(Obj.magic(x)));
