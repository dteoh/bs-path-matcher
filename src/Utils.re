module Option = {
  let else_: (option('a), unit => option('a)) => option('a) =
    (result, altFn) =>
      switch (result) {
      | Some(_) as r => r
      | None => altFn()
      };
};