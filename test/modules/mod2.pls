@module("mod2");
@import("mod1");

export let add_inst = struct::Add::<int32> {
  a: 10,
  b: 20,
};
