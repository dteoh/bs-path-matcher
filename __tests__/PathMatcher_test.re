open Jest;

open Expect;

describe("Matcher", () => {
  module M = PathMatcher.Matcher;
  module S = PathMatcher.Segment;
  module A = PathMatcher.Argument;
  describe("make", () => {
    test("can create matcher for static paths", () =>
      expect(M.make("/foo/bar")) |> toEqual(S.[Static("/foo/bar")])
    );
    test("can create matcher for paths with int placeholders", () =>
      expect(M.make("/{x:int}")) |> toEqual(S.[Static("/"), Int("x")])
    );
    test("can create matcher for paths with string placeholders", () =>
      expect(M.make("/{x:str}")) |> toEqual(S.[Static("/"), String("x")])
    );
    test("can create matcher for paths with trailing static segments", () =>
      expect(M.make("/{x:int}/foo"))
      |> toEqual(S.[Static("/"), Int("x"), Static("/foo")])
    );
    test("can create matcher for paths with regex placeholders", () =>
      expect(M.make("/{x:re(\\d+)}"))
      |> toEqual(S.[Static("/"), Regex("x", [%re "/^\\d+/"])])
    );
    test(
      "can create matcher for paths with static, int, string and regex placeholders",
      () =>
      expect(M.make("/foo/{x:int}/{bar:str}/{y:int}/zoo/bee/{doo:re(\\d+)}"))
      |> toEqual(
           S.[
             Static("/foo/"),
             Int("x"),
             Static("/"),
             String("bar"),
             Static("/"),
             Int("y"),
             Static("/zoo/bee/"),
             Regex("doo", [%re "/^\\d+/"]),
           ],
         )
    );
  });
  describe("tryMatch", () => {
    test("returns None if the path does not match", () => {
      let matcher = M.make("/foo/bar");
      expect(matcher |. M.tryMatch("/foo/bz")) |> toBe(None);
    });
    test("returns Some if the path matches", () => {
      let matcher = M.make("/foo/bar");
      expect(matcher |. M.tryMatch("/foo/bar")) |> toEqual(Some([]));
    });
    test("can match paths with static, int, string and regex placeholders", () => {
      let matcher =
        M.make("/foo/{x:int}/{lol:str}/{y:int}/zzz/{z:re(\\d+)}/fff");
      expect(matcher |. M.tryMatch("/foo/123/bar/456/zzz/9098/fff"))
      |> toEqual(
           Some([
             A.Int("x", 123),
             A.String("lol", "bar"),
             A.Int("y", 456),
             A.String("z", "9098"),
           ]),
         );
    });
    test("returns None if only partial match", () => {
      let matcher = M.make("/foo/{x:int}/baz");
      expect(matcher |. M.tryMatch("/foo/123/baz/456")) |> toBe(None);
    });
  });
});

describe("Segment", () => {
  module A = PathMatcher.Argument;
  module S = PathMatcher.Segment;
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
});