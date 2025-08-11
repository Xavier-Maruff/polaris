@module("my_module");

struct MyStruct::<T> {
    field1: int32,
    field2: string,
}

export let core: int32 = 2;
let test = core.check;

interface Addable::<T> {
    func add(a: T, b: int32): int32;
}

export func add::<X, T(Addable)>(a: T, b: int32): int32 {
    let check: T = test;
    return a + b;
}

struct MyStruct::<T> {
    field1: int32,
    field2: string,
}

/*
impl MyStruct::<T> {
    func my_func(param: int32): String {
        return "Hello, World!";
    }
}

actor Actor {
    x: int32,
    async func actor_func() {
        for let mod i in range(0, 10) {
            self.x &&= 12;
        }
    }
}

enum MyEnum {
    Variant1,
    Variant2(i32),
    Variant3{ field: string, },
}

func main() {
    let mod x: MyStruct::<i32> = struct::MyStruct::<i32> {
        field1: 42,
        field2: "test",
    };

    let y = ref x;
    x.my_func(x.field1);

    let b = [12, 3, x.field1][0];

    let my_actor = actor::Actor {
        x: 10,
    };

    block my_actor.actor_func();

    (func[ref x]() {
        x.field1 += 1;
    })();
}



@module polaris;
@schemes SEAL, TFHE;

struct Test {
    a: int32,
}

actor Actor {
    b: int32,
    async func test(): int32 {
        c += 2;
        return 12 + b;
    }
}


@gpu;
async func test() {
    let a = block b.test();
    c = struct::Test { a: 12, };
    e = actor::Actor { b: 12, };

    (func[ref x]() {
                    x.field1 += 1;
                })();
}


/*
func test_closure(val: ref int32): int32 {
    let mod a: int32 = 12?;
    let test_func = func[ref a](val: int32): int32 {
        return a + val;
    };

    let my_list = [-1, 2, 3, 4, 5];
    //let val = my_list[2];
    my_list.push(6)[8][expr] = test.val;

    let val = MyEnum::Variant2(struct {
        a: 12,
        b: 14,
    });

    //return test_func(val);
}


/*
func ref_semantics(i: ref int32, j: weak int32) {
    i = 12;
    let b: ref int32 = ref i;

    *b = -14;

    //must unwrap weak ref
    let inner = match j {
        Some(ref inner) => inner,
        None => {
            return;
        },
    };

    let enum_val = MyEnum::Variant1(inner);
    let enum_val2 = MyEnum::Variant2(struct {
        a: 12,
        b: 14,
    });
    let enum_val3 = MyEnum::Variant3;
    let enum_val4 = MyEnum::Variant4(12, 14);
}

/*

func test() {
  let a: int32 = 12;
  a = 12;
  return (12+test(b))/c;
}

enum Result::<T, E> {
  Ok(T),
  Err(E),
}

@module polaris;
@import std::io;

struct Add::<T> {
  a: int32::<T>,
  b: int32,
}

interface AddInterface {
  func add(a: int32, b: int32): int32;
  func subtract(a: int32, b: int32): int32 {
    //return a - b;
    test::val::<int32>;
  }
}

impl AddInterface for Add::<T> {
  func add(a: int32, b: int32): int32 {
    return a;
    for let mod a in b {
       b;
    }

    return struct {
       test: "val",
    };

    return struct::Add {
      a: 12 + b,
      b: 14,
    };
  }

  func subtract(a: int32, b: int32): int32 {
    if a {
        return b;
    } else if b {
        return a;
    } else {
        return 0;
    }
  }
}



type SomeType = Result::<int32, string>;


impl AddInterface for Add {
  func add(a: int32, b: int32): int32 {
    return a + b;
  }
}

func add(a: int32, b: int32): int32 {
    return a+b;
}

func main() {
  let a: int32 = io::input("First number: ");
  let b: int32 = io::input("Second number: ");
}

*/
