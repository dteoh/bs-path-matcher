module Argument = {
  type t =
    | Int(string, int);
};

module Segment = {
  type t =
    | Static(string)
    | Int(string);
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
  };
  let extractIntDefinitions = str =>
    switch (Js.String.match([%re "/{\\w+:int}/g"], str)) {
    | Some(matches) =>
      matches
      |. Belt.Array.map(match => (match, Js.String.indexOf(match, str)))
      |. Belt.List.fromArray
    | None => []
    };
  let extractDynamics = str =>
    str |. extractIntDefinitions |. Belt.List.map(((match, _)) => match);
  let makeInt = str =>
    switch (Js.String.match([%re "/{(\\w+):int}/"], str)) {
    | Some([|_, name|]) => Some(Int(name))
    | Some(_)
    | None => None
    };
  let make = str =>
    switch (makeInt(str)) {
    | Some(s) => s
    | None => Static(str)
    };
  let tryMatch: (t, string) => option((string, list(Argument.t))) =
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
          Some((rest, [Argument.Int(name, integer)]))
        | None => None
        }
      };
};

module Matcher = {
  type t = list(Segment.t);
  let splitDynamic: (string, string) => (option(string), option(string)) =
    (path, dynamic) => {
      let result = Js.String.splitAtMost(dynamic, ~limit=2, path);
      switch (result) {
      | [|"", ""|] => (None, None)
      | [|"", after|] => (None, Some(after))
      | [|before, ""|] => (Some(before), None)
      | [|before, after|] => (Some(before), Some(after))
      | _ => (None, None)
      };
    };
  let transformDynamic: (string, string) => (t, option(string)) =
    (path, dynamic) =>
      switch (splitDynamic(path, dynamic)) {
      | (None, None) => ([Segment.make(dynamic)], None)
      | (None, Some(_) as after) => ([Segment.make(dynamic)], after)
      | (Some(before), None) => (
          [Static(before), Segment.make(dynamic)],
          None,
        )
      | (Some(before), Some(_) as after) => (
          [Static(before), Segment.make(dynamic)],
          after,
        )
      };
  let rec transform: (string, list(string), t) => t =
    (path, dynamicSegments, segments) =>
      switch (dynamicSegments) {
      | [] => segments
      | [dynamic, ...rest] =>
        switch (transformDynamic(path, dynamic)) {
        | (parsed, Some(path)) =>
          transform(path, rest, Belt.List.concat(segments, parsed))
        | (parsed, None) => Belt.List.concat(segments, parsed)
        }
      };
  let make: string => t =
    str =>
      switch (Segment.extractDynamics(str)) {
      | [] => [Static(str)]
      | _ as definitions => transform(str, definitions, [])
      };
  let tryMatch: (t, string) => option(list(Argument.t)) =
    (matchSpec, path) => {
      let rec matcher = (specs, path, args) =>
        switch (specs) {
        | [] =>
          if (path == "") {
            Some(args);
          } else {
            None;
          }
        | [spec, ...rest] =>
          switch (Segment.tryMatch(spec, path)) {
          | Some((suffix, parsedArgs)) =>
            matcher(rest, suffix, Belt.List.concat(args, parsedArgs))
          | None => None
          }
        };
      matcher(matchSpec, path, []);
    };
};