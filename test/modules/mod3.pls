@module("mod3");

let mod2 = @import("mod2");

export func test() {
    mod2::mod1::check();
}
