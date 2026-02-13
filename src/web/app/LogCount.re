/* Cached log entry count to avoid async queries on every render */

let count = ref(0);

let get = (): int => count^;

let set = (n: int): unit => {
  count := n;
};

let increment = (): unit => {
  count := count^ + 1;
};

let clear = (): unit => {
  count := 0;
};
