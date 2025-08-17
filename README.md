# 🔵 Polaris

> [!CAUTION]
> Polaris is a work in progress, and is not yet even capable of codegen. Watch this space for updates!

Polaris is a high-security procedural compiled language for sensitive computation. All operations are performed homomorphically on encrypted data, meaning that a Polaris program can operate on sensitive data in untrusted environments without ever exposing plaintexts, even to the program itself - in fact, no Polaris program can ever deal with plaintext data.

## I/O in a homomorphic world

The question arises, if everything everywhere all the time is encrypted, how can we interact with the outside world? Polaris' approach to this is the host harness. Extern function declarations marked with a special attribute `@host` are required to be implemented in this harness, functions like reading from stdin, network requests, etc. Host functions all receive encrypted data as arguments, and are expected to respond with encrypted data. Where the cryptographic boundaries lie is up to the harness implementation - behind the scenes, your harness could be talking to a trusted enclave, a remote server, or even just decrypting the data and handling it in plaintext, as long as the Polaris program never sees plaintext data itself. We will provide some standard harness implementation for common use cases, as well as a guide on how to implement your own harness.

## Getting started

Some examples can be found in the `examples` directory, which will walk you through the main concepts, syntax, and semantics of Polaris.

## A short guide to the Polaris language

Aside from its security focus, Polaris is a modern language with a syntax inspired by Rust, Go, and Swift, and GC-free automatic memory management with opt-in manual memory escape hatches. It has a strong type system with support for generics and interfaces, a regulated macro system for metaprogramming, and an actor-based concurrency model.

### Program structure

Every Polaris program is a collection of modules, which can depend on each other. Modules can contain functions, structs, enums, and interfaces. The main entry point of a Polaris program is the `main` function, which is required to be defined in the root module. Modules are declared with the `@module` directive, and can be imported with the `@import` directive:

```zig
//declaring a module, which must be unique
@module("my_module");

//importing a module to the global scope of this module
@import("core::io");

//importing a module as a named alias
let io = @import("core::io");
```

### Variables, Constants, and Types

Variables in Polaris are declared with the `let` keyword, and are immutable by default, but can be made mutable with the `mod` keyword. Constants are declared with the `const` keyword and must be evaluable at compile time:

```zig
const MY_CONSTANT: int32 = 42; // must be evaluable at compile time
let my_var: int32;
let my_initialised_var = 42; // type is inferred as int32
let mod my_mutable_var: int32 = 42;
```

As you saw, types are written after the variable name, and can be inferred by the compiler. Polaris has a rich set of built-in types, including integers, floats, booleans, strings, vectors, and arrays. It also supports user-defined types like structs and enums:

```go
let my_string: string = "Hello, Polaris!";
let my_vec: vector::<int32> = [1, 2, 3, 4, 5];
let my_struct: MyStruct = struct::MyStruct {
	field1: 42,
	field2: "Hello"
};

let my_enum: MyEnum = MyEnum::Variant1;
let my_tagged_enum: TaggedEnum = TaggedEnum::Variant2(3.14);
```

### Functions

Function are first-class citizens in Polaris, and can be declared/defined by:

```zig
//some required imports
@import("core::io");
@import("core::interfaces");

//simple function with an optional return type
func my_function(arg1: float32, arg2: float32): float32 {
	return arg1 + arg2;
}

//function with a generic type parameter
//the `impls` keyword is used to specify that the type must implement an interface
func my_generic_function<T impls Add>(arg1: T, arg2: T): T {
	return arg1 + arg2;
}

//function with a host attribute, to be implemented in the host harness
@host;
extern func read_stdin(): string;

//closures by default capture environment by value,
//but can also capture by reference using the `ref` keyword in the capture list
func my_closure_function() {
	let mod x = 42;
	let closure = func[ref x]() {
		x += 1;
	};
	closure();
  @println("Closure captured x by reference, new value: {}", x);
}
```

### Interfaces and Impls

With interfaces, you can define a set of methods that a type must implement. This allows for compile-time polymorphism, where you can write functions that work with any type that implements a specific interface.

```zig
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

```zig
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

```zig
match my_enum {
	MyEnum::Variant1 => {
		@println("Variant1");
	},
	MyEnum::Variant2(data) => {
		@println(data);
	},
	MyEnum::Variant3(data) => {
		@println(data.field1);
		@println(data.field2);
	},
}
```

### Loops and Iterators

Polaris only supports `for` loops, which can exist in three modes: iterating over an iterable, iterating until a condition evaluates to false, or iterating infinitely:

```zig
//iterating over an iterable
let my_vec = [1, 2, 3, 4, 5];
for let mod item in my_vec {
	@println(item);
}

//iterating until a condition evaluates to false
let mod i = 0;
for i < 5 {
	@println(i);
	i += 1;
}

//iterating infinitely
let mod j = 0;
for {
	@println(j);
	j += 1;
}
```

### Error Handling

Error handling in Polaris is the same as Rust, using the `Result` type. Functions can return a `Result` type, which can be either `Ok` or `Err`. The `?` operator can be used to propagate errors:

```zig
let io = @import("core::io");
@import("core::result");


func read_file(path: string): Result::<string, string> {
	//the error type of all core functions is string
	let file = io::open(path)?;
	let content = file.read_to_string()?;
	return Ok(content);
}
```

### References and Memory Management

Polaris uses automatic reference counting (ARC) for memory management, meaning you don't have to worry about memory management _or_ the overhead of a GC (though really the performance bottleneck will always be the homomorphic operations). References are created with the `ref` keyword, and can be used to mutate the underlying value via the dereference operator `*`, or its fields with the `.` operator. References can be created to any type as long as its instantiation is marked as mutable, including structs, enums, and arrays. Polaris will error during compilation if you try to create a ref to an immutable variable - this is because is no reason to create a reference to an immutable variable, as immutable variables are already default passed by reference if they are larger than a pointer size (which turns out to be all types in Polaris, as polynomials are the fundamental data type used internally by the homomorphic encryption schemes).

```zig
let mod my_var = 42;
let my_ref = ref my_var; // creates a reference to my_var

*my_ref += 1; // modifies the underlying variable
@println(my_var); // prints 43
```

If we have two structs that reference each other, we can use the `weak` keyword to create a weak reference that does not affect the reference count, to avoid circular references creating memory leaks.

```zig
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

```zig
let num = 42;
let my_weak_ref = weak num;

match my_weak_ref {
	Some(ref value) => {
		@println(value); // prints 42
	},
	None => {
		@println("Weak reference is null");
	},
}
```

Manual memory management can only be performed in special `unsafe` blocks, which are used to escape the automatic memory management system. This is useful for low-level operations or when interfacing with external libraries that require manual memory management.

```zig
let mem = @import("core::mem");

unsafe {
  let my_ptr = mem::alloc::<int32>();
  *my_ptr = 42;
	// do something with my_ptr
	@println(*my_ptr);
	// free the memory
	mem::free(my_ptr);
}


// We can also allocate on the stack using `alloca`:
unsafe {
	let my_stack_ptr = mem::alloca::<int32>();
	*my_stack_ptr = 42;
	@println(*my_stack_ptr);
	// no need to free the memory, it will be automatically freed when the block ends
}

//Or use an arena to preallocate memory for a specific scope and avoid fragmentation:
unsafe {
	let arena = mem::Arena::<int32>::new();
	let my_arena_ptr = arena.alloc(42);
	@println(*my_arena_ptr);
	// no need to free the memory, it will be automatically freed when the arena is dropped
	// but memory can still be manually freed to free space in the arena
	arena.free(my_arena_ptr);
}
```

### Concurrency and Actors

Polaris uses an asynchronous actor-based concurrency model, async functions are declared with the `async` keyword, and can be awaited with the `await` keyword. Async functions can be launched from synchronous functions via a `spawn` statement, which will return a `Future::<T>` that can be awaited later.
Actors are defined with the `actor` keyword, and are the only way that async functions are allowed to mutate state that they do not own. Actors own an atomic queue of method calls, which are processed in the order they are received, eliminating the need for locks or other synchronization primitives while allowing for safe concurrent access to shared state and removing the risk of data races.

```zig
@import("core::io");

actor MyActor {
	my_val: int32,

	async func increment(mod ref self) {
		//self if a reference to the actor instance
		self.my_val += 1;
	}

  //all actor methods must be async
	async func get_value(ref self): int32 {
		return self.my_val;
	}
}


async func my_func(shared_state: ref MyActor, index: int32) {
	await shared_state.increment();

	let value = await shared_state.get_value();
	@println("Task {} completed, value: {}", index, value);
}

func main() {
	let mod my_actor = actor::MyActor { my_val: 0 };

  let futures = [];
	for i in range(0, 10) {
		//spawning async tasks that will run concurrently
		futures.push(spawn my_func(ref my_actor, i));
	}

	for let mod future in futures {
		//awaiting the completion of all spawned tasks
		block future;
	}

	let final_value = block my_actor.get_value();
	@println("Final value: {}", final_value);
}
```

#### Blocking Functions

The internal implementation of the async runtime includes a primary executor alongside a set of blocking executors, which are used to run compute-intensive or synchronous i/o-bound tasks. The compiler will, via static analysis, try to automatically offload what it deems to be blocking tasks to the blocking executors, but you can also manually signal to the compiler that a function is blocking by using the `@blocking` directive, or that a function is not blocking by using the `@nonblocking` directive. It is recommended to use these directives to help the compiler make better decisions about task scheduling and improve the performance of your program.

```zig
@blocking;
func blocking_function() {
   //e.g. something compute-intensive
}

@nonblocking;
func nonblocking_function() {
   //e.g. something that does not block the async runtime
}
```

### GPU-acceleration

Polaris automatically compiles an optimal subset of the program to GPU kernels, and handles the data transfer between CPU and GPU memory. This means that Polaris programs are by-default massively parallel, something which is essential for homomorphic encryption to be performant.
The Polaris compiler tries to automatically parallelise where it makes performance sense, but you can also force a function to be compiled to a GPU kernel by using the `@gpu` directive:

```zig
@import("core::range");

@gpu;
func sum_vecs(vec1: vector::<int32>, vec2: vector::<int32>) {
	assert(vec1.len() == vec2.len(), "Vectors must be of the same length");

	let mod result = vector::<int32>::new(arr1.len());
	for let mod i in range(0, vec1.len()) {
		result[i] = vec1[i] + vec2[i];
	}
	return result;
}
```

If the compiler cannot generate a GPU kernel for a function annotated with `@gpu`, it will emit an error.
