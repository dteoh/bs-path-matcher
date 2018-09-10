open Jest;

open Expect;

describe("Option", () => {
  module O = Utils.Option;
  describe("else_", () => {
    test("will return the first argument when Some", () =>
      expect(O.else_(Some(123), () => Some(456))) |> toEqual(Some(123))
    );
    test("will evaluate second arg when first arg is None", () =>
      expect(O.else_(None, () => Some(456))) |> toEqual(Some(456))
    );
  });
});