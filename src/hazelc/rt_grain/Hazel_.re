let here_path = "hazel";

module WithoutImpl = (M: Make.M) =>
  Make.WithoutImpl({
    let name = M.name;
    let path = Filename.concat(here_path, M.path);
  });

module WithImpl = (M: Make.M) =>
  Make.WithImpl({
    let name = M.name;
    let path = Filename.concat(here_path, M.path);
  });
