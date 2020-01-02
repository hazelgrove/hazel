type timeRepo = {
  day: int,
  prevDay: list(int),
};

let changeRepo = (t: timeRepo, a: int): timeRepo => {
  {
    day: t.day - 1,
    prevDay: {
      switch (t.prevDay) {
      | [] => [a]
      | myList => [a, ...myList]
      };
    },
  };
};
