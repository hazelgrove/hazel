type t = int;

let t = ref(0);

let tick = (): t => {
  let time = t^;
  t := time + 1;
  time;
};
