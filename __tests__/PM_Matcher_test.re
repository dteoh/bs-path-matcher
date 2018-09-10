open Jest;

open Expect;

module M = PM_Matcher;

module S = PM_Segment;

module A = PM_Argument;

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