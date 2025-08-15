@module("mod3");

let mod2 = @import("mod2");

export func test() {
    return mod2::mod1::check();
}
