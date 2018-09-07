module Matcher = {
  module Segment = {
    type t =
      | Static(string)
      | Int(string);
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
  };
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
  let transformDynamic:
    (string, string) => (list(Segment.t), option(string)) =
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
  let rec transform:
    (string, list(string), list(Segment.t)) => list(Segment.t) =
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
  let make: string => list(Segment.t) =
    str =>
      switch (Js.String.match([%re "/{\\w+:int}/g"], str)) {
      | Some(dynamicParts) =>
        transform(str, Belt.List.fromArray(dynamicParts), [])
      | None => [Static(str)]
      };
};