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
    test("can create matcher for paths with static and int placeholders", () =>
      expect(M.make("/foo/{x:int}/bar/{y:int}"))
      |> toEqual(
           S.[Static("/foo/"), Int("x"), Static("/bar/"), Int("y")],
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
    test("can match paths with static and int placeholders", () => {
      let matcher = M.make("/foo/{x:int}/bar/{y:int}");
      expect(matcher |. M.tryMatch("/foo/123/bar/456"))
      |> toEqual(Some([A.Int("x", 123), A.Int("y", 456)]));
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
      expect(S.extractDynamics("/bar/{x:int}/foo/zoo/{y:bbb}/{z:int}"))
      |> toEqual(["{x:int}", "{z:int}"])
    )
  );
  describe("makeInt", () => {
    test("can make int matcher", () =>
      expect(S.makeInt("{xyz:int}")) |> toEqual(Some(S.Int("xyz")))
    );
    test("will not make int matcher if it is not specified as such", () =>
      expect(S.makeInt("{xyz:str}")) |> toBe(None)
    );
  });
  describe("make", () => {
    test("can make int matcher", () =>
      expect(S.make("{xyz:int}")) |> toEqual(S.Int("xyz"))
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
  });
});