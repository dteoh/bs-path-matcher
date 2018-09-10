type t = list(PM_Segment.t);

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
    | (None, None) => ([PM_Segment.make(dynamic)], None)
    | (None, Some(_) as after) => ([PM_Segment.make(dynamic)], after)
    | (Some(before), None) => (
        [Static(before), PM_Segment.make(dynamic)],
        None,
      )
    | (Some(before), Some(_) as after) => (
        [Static(before), PM_Segment.make(dynamic)],
        after,
      )
    };

let rec transform: (string, list(string), t) => t =
  (path, dynamicSegments, segments) =>
    switch (dynamicSegments) {
    | [] =>
      if (path == "") {
        segments;
      } else {
        Belt.List.concat(segments, [Static(path)]);
      }
    | [dynamic, ...rest] =>
      switch (transformDynamic(path, dynamic)) {
      | (parsed, Some(path)) =>
        transform(path, rest, Belt.List.concat(segments, parsed))
      | (parsed, None) => Belt.List.concat(segments, parsed)
      }
    };

let make: string => t =
  str => transform(str, PM_Segment.extractDynamics(str), []);

let tryMatch: (t, string) => option(list(PM_Argument.t)) =
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
        switch (PM_Segment.tryMatch(spec, path)) {
        | Some((suffix, parsedArgs)) =>
          matcher(rest, suffix, Belt.List.concat(args, parsedArgs))
        | None => None
        }
      };
    matcher(matchSpec, path, []);
  };