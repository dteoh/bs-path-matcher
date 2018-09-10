type t =
  | Static(string)
  | Int(string)
  | String(string)
  | Regex(string, Js_re.t);

module PrefixMatcher = {
  let tryStatic = (prefix, path) =>
    if (Js.String.startsWith(prefix, path)) {
      Some(path |> Js.String.substr(~from=Js.String.length(prefix)));
    } else {
      None;
    };
  let tryInt = path =>
    switch (Js.String.match([%re "/^(\\d+)/"], path)) {
    | Some([|_, ds|]) =>
      Some((
        int_of_string(ds),
        Js.String.substr(~from=Js.String.length(ds), path),
      ))
    | Some(_)
    | None => None
    };
  let tryString = path =>
    switch (Js.String.match([%re "/^([^/\\n\\r]+)/"], path)) {
    | Some([|_, ss|]) =>
      Some((ss, Js.String.substr(~from=Js.String.length(ss), path)))
    | Some(_)
    | None => None
    };
  let tryRegex = (re, path) =>
    switch (Js.String.match(re, path)) {
    | Some(matches) =>
      Some((
        matches[0],
        Js.String.substr(~from=Js.String.length(matches[0]), path),
      ))
    | None => None
    };
};

let extractIntDefinitions = str =>
  switch (Js.String.match([%re "/{\\w+:int}/g"], str)) {
  | Some(matches) =>
    matches
    |. Belt.Array.map(match => (match, Js.String.indexOf(match, str)))
    |. Belt.List.fromArray
  | None => []
  };

let extractStringDefinitions = str =>
  switch (Js.String.match([%re "/{\\w+:str}/g"], str)) {
  | Some(matches) =>
    matches
    |. Belt.Array.map(match => (match, Js.String.indexOf(match, str)))
    |. Belt.List.fromArray
  | None => []
  };

let extractRegexDefinitions = str =>
  switch (Js.String.match([%re "/{\\w+:re\\(.+\\)}/g"], str)) {
  | Some(matches) =>
    matches
    |. Belt.Array.map(match => (match, Js.String.indexOf(match, str)))
    |. Belt.List.fromArray
  | None => []
  };

let extractDynamics = str => {
  let intDefs = extractIntDefinitions(str);
  let strDefs = extractStringDefinitions(str);
  let regexDefs = extractRegexDefinitions(str);
  intDefs
  |. Belt.List.concat(strDefs)
  |. Belt.List.concat(regexDefs)
  |. Belt.List.sort((d1, d2) => {
       let (_, p1) = d1;
       let (_, p2) = d2;
       p1 - p2;
     })
  |. Belt.List.map(((def, _)) => def);
};

let makeInt = str =>
  switch (Js.String.match([%re "/{(\\w+):int}/"], str)) {
  | Some([|_, name|]) => Some(Int(name))
  | Some(_)
  | None => None
  };

let makeString = str =>
  switch (Js.String.match([%re "/{(\\w+):str}/"], str)) {
  | Some([|_, name|]) => Some(String(name))
  | Some(_)
  | None => None
  };

let makeRegex = str =>
  switch (Js.String.match([%re "/{(\\w+):re\\((.+)\\)}/"], str)) {
  | Some([|_, name, re|]) => Some(Regex(name, Js_re.fromString("^" ++ re)))
  | Some(_)
  | None => None
  };

let make = str =>
  str
  |. makeInt
  |. PM_Utils.Option.else_(() => makeString(str))
  |. PM_Utils.Option.else_(() => makeRegex(str))
  |. PM_Utils.Option.else_(() => Some(Static(str)))
  |. Belt.Option.getExn;

let tryMatch: (t, string) => option((string, list(PM_Argument.t))) =
  (segment, path) =>
    switch (segment) {
    | Static(prefix) =>
      switch (PrefixMatcher.tryStatic(prefix, path)) {
      | None => None
      | Some(rest) => Some((rest, []))
      }
    | Int(name) =>
      switch (PrefixMatcher.tryInt(path)) {
      | Some((integer, rest)) =>
        Some((rest, [PM_Argument.Int(name, integer)]))
      | None => None
      }
    | String(name) =>
      switch (PrefixMatcher.tryString(path)) {
      | Some((str, rest)) => Some((rest, [PM_Argument.String(name, str)]))
      | None => None
      }
    | Regex(name, re) =>
      switch (PrefixMatcher.tryRegex(re, path)) {
      | Some((str, rest)) => Some((rest, [PM_Argument.String(name, str)]))
      | None => None
      }
    };