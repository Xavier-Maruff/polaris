# 🔵 Polaris

Polaris is a privacy-first statically-typed compiled language producing homomorphically encrypted code. This means that the value of every variable and every constant is encrypted for the entire life of the program, and never decrypted within the program. All computations are performed on the encrypted data thanks to homomorphic encryption schemes like CKKS, BGV, TFHE.

## I/O in a homomorphic world

The question arises, if everything everywhere all the time is encrypted, how can we interact with the outside world? Polaris' approach to this is the host harness. Function declarations marked with a special attribute `host` are required to be implemented in this harness, functions like reading from stdin, network requests, etc. Host functions all receive encrypted data as arguments, and are expected to respond with encrypted data. Where the cryptographic boundaries lie is up to the harness implementation - chuck in some over-the-network separation, a TEE enclave, whatever your requirements are. Check out our standard harnesses for common use cases here, and a guide for developing your own harnesses here.

## Getting started

Some examples can be found in the `examples` directory, which will walk you through the main concepts, syntax, and semantics of Polaris.

To install the compiler:

```bash
# macos
brew install polaris-lang
# debian/ubuntu
sudo apt install polaris-lang
```

Then to create a new project and run it:

```bash
polaris new myproject --bin
cd myproject
polaris run
# or to test
polaris test
# or just to build
polaris build
```

## A guide to the Polaris language

Aside from its security focus, Polaris is a modern language with a syntax inspired by Rust, Go, and Swift, and GC-free automatic memory management with opt-in manual memory escape hatches. It has a strong type system with support for generics and interfaces, and a regulated macro system for metaprogramming.

### Program structure

Every Polaris program is a collection of modules, which can depend on each other. Modules can contain functions, structs, enums, and interfaces. The main entry point of a Polaris program is the `main` function, which is required to be defined in the root module. Modules are declared with the `@module` directive, and can be imported with the `@import` directive.

### Variables, Constants, and Types

Variables in Polaris are declared with the `let` keyword, and are immutable by default, but can be made mutable with the `mod` keyword. Constants are declared with the `const` keyword and must be evaluable at compile time:

```go
let my_var: int32;
let my_initialised_var = 42; // type is inferred as int32
let mod my_mutable_var: int32 = 42;
```

As you saw, types are written after the variable name, and can be inferred by the compiler. Polaris has a rich set of built-in types, including integers, floats, booleans, strings, and arrays. It also supports user-defined types like structs and enums:

```go
let my_string: string = "Hello, Polaris!";
let my_array: array::<int32> = [1, 2, 3, 4, 5];
let my_struct: MyStruct = struct::MyStruct { field1: 42, field2: "Hello" };

let my_enum: MyEnum = MyEnum::Variant1;
let my_tagged_enum: TaggedEnum = TaggedEnum::Variant2(3.14);
```

### Functions

Function are first-class citizens in Polaris, and can be defined by:

```go
//some required imports
@import core::io;
@import core::interfaces as cif;

//simple function with an optional return type
func my_function(arg1: float32, arg2: float32): float32 {
	return arg1 + arg2;
}

//function with a generic type parameter
//the `impls` keyword is used to specify that the type must implement an interface
func my_generic_function<T impls cif::Add>(arg1: T, arg2: T): T {
	return arg1 + arg2;
}

//function with a host attribute, to be implemented in the host harness
@host;
func read_stdin(): string;

//a function containing a closure that captures a local variable by reference
func my_closure_function() {
	let mod x = 42;
	let closure = func[ref x]() {
		x += 1;
		io::println(x);
	};
	closure();
}
```

### Interfaces and Impls

With interfaces, you can define a set of methods that a type must implement. This allows for compile-time polymorphism, where you can write functions that work with any type that implements a specific interface.

```go
//defining an interface
interface Add {
	//a method that takes another instance of the same type and returns a new instance
	// the `Self` type refers to the type that implements the interface
	func add(self, other: Self): Self;
}

//implementing the interface for a specific type
impl Add for MyStruct {
	func add(self, other: Self): Self {
		return struct::MyStruct {
			field1: self.field1 + other.field1,
			field2: self.field2 + other.field2,
		};
	}
}
```

### Structs and Enums

Structs are defined with the `struct` keyword, and can have fields of any type. Enums are defined with the `enum` keyword, and can have variants with associated data.

```go
struct MyStruct {
	field1: int32,
	field2: string,
}

//or with generics
struct MyGenericStruct::<T> {
	field1: T,
	field2: string,
}

//then instantiating later on
let my_var = struct::MyStruct {
	field1: 42,
	field2: "Hello, Polaris!",
};

//same for enums
enum MyEnum {
	Variant1,
	Variant2(string),
	Variant3({
		field1: int32,
		field2: float64,
	}),
}

//instantiating
let my_enum: MyEnum = MyEnum::Variant1;
let my_tagged_enum: MyEnum = MyEnum::Variant2("Hello, Polaris!");
let my_struct_enum: MyEnum = MyEnum::Variant3(struct {
	field1: 42,
	field2: 3.14,
});
```

Enums must be unwrapped with a `match` expression to access underlying data, with a syntax stolen entirely from Rust:

```go
match my_enum {
	MyEnum::Variant1 => {
		io::println("Variant1");
	},
	MyEnum::Variant2(data) => {
		io::println(data);
	},
	MyEnum::Variant3(data) => {
		io::println(data.field1);
		io::println(data.field2);
	},
}
```

### References and Memory Management

Polaris uses automatic reference counting (ARC) for memory management, which means that you don't have to worry about manually allocating and deallocating memory unless you opt-in to manual memory management with the `manual` keyword. References are created with the `ref` keyword, and are mutable if the underlying variable is mutable:

```go
let mod my_var = 42;
let my_ref = ref my_var; // creates a reference to my_var
my_ref += 1; // modifies the underlying variable
io::println(my_var); // prints 43
```

If we have two structs that reference each other, we can use the `weak` keyword to create a weak reference that does not affect the reference count, to avoid circular references:

```go
struct FirstStruct {
	second: ref SecondStruct,
}

struct SecondStruct {
	first: weak FirstStruct,
}

let mod first = struct::FirstStruct {
	second: ref second,
};
let mod second = struct::SecondStruct {
	first: weak first, // weak reference to avoid circular reference
};
```

Weak references could have a deallocated underlying resource, so they must be checked before use. They are equivalent in the Polaris type system to `Option<ref T>`, so can be unwrapped with a `match` expression:

```go
let num = 42;
let my_weak_ref = weak num;

match my_weak_ref {
	Some(ref value) => {
		io::println(value); // prints 42
	},
	None => {
		io::println("Weak reference is null");
	},
}
```

### Loops and Iterators

Polaris only supports `for` loops, which can exist in three modes: iterating over an iterable, iterating until a condition evaluates to false, or iterating infinitely:

```go
//iterating over an iterable
let my_array = @vec[1, 2, 3, 4, 5];
for item in my_array {
	io::println(item);
}

//iterating until a condition evaluates to false
let mut i = 0;
for i < 5 {
	io::println(i);
	i += 1;
}

//iterating infinitely
let mut j = 0;
for {
	io::println(j);
	j += 1;
}
```
