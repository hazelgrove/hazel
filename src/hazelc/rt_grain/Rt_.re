let here_path = "rt";

module WithoutImpl = (M: Make.M) =>
  Hazel_.WithoutImpl({
    let name = M.name;
    let path = Filename.concat(here_path, M.path);
  });

module WithImpl = (M: Make.M) =>
  Hazel_.WithImpl({
    let name = M.name;
    let path = Filename.concat(here_path, M.path);
  });
