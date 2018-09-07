open Jest;

open Expect;

describe("Matcher", () => {
  module M = PathMatcher.Matcher;
  module S = PathMatcher.Matcher.Segment;
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
});

describe("Matcher.Segment", () => {
  module S = PathMatcher.Matcher.Segment;
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
});