open Jest;

open Expect;

module A = PM_Argument;

module S = PM_Segment;

describe("extractDynamics", () =>
  test("returns list of valid dynamic segment definitions", () =>
    expect(
      S.extractDynamics(
        "/bar/{x:int}/foo/zoo/{y:bbb}/{z:int}/lolo/{a:re(\\d+)}",
      ),
    )
    |> toEqual(["{x:int}", "{z:int}", "{a:re(\\d+)}"])
  )
);

describe("makeInt", () => {
  test("can make int matcher", () =>
    expect(S.makeInt("{xyz:int}")) |> toEqual(Some(S.Int("xyz")))
  );
  test("will not make int matcher when not specified as such", () =>
    expect(S.makeInt("{xyz:str}")) |> toBe(None)
  );
});

describe("makeString", () => {
  test("can make string matcher", () =>
    expect(S.makeString("{xyz:str}")) |> toEqual(Some(S.String("xyz")))
  );
  test("will not make string matcher when not specified as such", () =>
    expect(S.makeString("{xyz:int}")) |> toBe(None)
  );
});

describe("makeRegex", () => {
  test("can make regex matcher", () =>
    expect(S.makeRegex("{def:re(\\d+)}"))
    |> toEqual(Some(S.Regex("def", Js_re.fromString("^\\d+"))))
  );
  test("will not make regex matcher when not specified as such", () =>
    expect(S.makeRegex("{xyz:int}")) |> toBe(None)
  );
});

describe("make", () => {
  test("can make int matcher", () =>
    expect(S.make("{xyz:int}")) |> toEqual(S.Int("xyz"))
  );
  test("can make string matcher", () =>
    expect(S.make("{abc:str}")) |> toEqual(S.String("abc"))
  );
  test("can make regex matcher", () =>
    expect(S.make("{abc:re(\\d+)}"))
    |> toEqual(S.Regex("abc", Js_re.fromString("^\\d+")))
  );
  test("will default to static matcher", () =>
    expect(S.make("{xyz:wtf}")) |> toEqual(S.Static("{xyz:wtf}"))
  );
});

describe("tryMatch", () => {
  test("can match static prefix", () =>
    expect(S.tryMatch(Static("/foo"), "/foo/bar"))
    |> toEqual(Some(("/bar", [])))
  );
  test("will not match non-matching static prefix", () =>
    expect(S.tryMatch(Static("/bar"), "/foo/bar")) |> toBe(None)
  );
  test("can match integer prefix", () =>
    expect(S.tryMatch(Int("something"), "123/bar"))
    |> toEqual(Some(("/bar", [A.Int("something", 123)])))
  );
  test("will not match non-matching integer prefix", () =>
    expect(S.tryMatch(Int("something"), "b123/bar")) |> toBe(None)
  );
  test("can match prefix characters before /", () =>
    expect(S.tryMatch(String("foo"), "abc-123-def-456/ghi/jkl"))
    |> toEqual(Some(("/ghi/jkl", [A.String("foo", "abc-123-def-456")])))
  );
  test("will not match empty prefix", () =>
    expect(S.tryMatch(String("foo"), "/ghi/jkl")) |> toBe(None)
  );
  test("can match regex prefix", () =>
    expect(S.tryMatch(Regex("foo", [%re "/\\d+/"]), "123/bar"))
    |> toEqual(Some(("/bar", [A.String("foo", "123")])))
  );
});