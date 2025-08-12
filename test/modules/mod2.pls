@module("mod2");

export let mod1 = @import("mod1");

export let add_inst = struct::mod1::Add {
  a: 10,
};
