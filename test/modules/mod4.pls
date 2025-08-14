@module("mod4");

let mod3 = @import("mod3");
let mod1 = @import("mod1");

export func main::<T>() {
    let my_struct = struct::mod1::Add {
        a: 10,
    };

    let val = 1 + "test";


    let result = mod3::test();
    return result;
}
