@module(mod2);
let mod1 = @import(mod1);

let add = struct::mod1::Add {
  a: 10,
  b: 20,
};
