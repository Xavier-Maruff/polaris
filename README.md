# <img width="30" height="30" alt="Polaris Logo" src="https://github.com/user-attachments/assets/b62872c0-521b-4d59-8407-c852d2af2a81" /> Polaris

> [!CAUTION]
> Polaris is a work in progress, and is not yet even capable of codegen. Watch this space for updates!

Polaris is a high-security, interpreted, statically typed, primarily functional language for sensitive computation (excuse how many adjectives that was). All sensitive operations are performed homomorphically on encrypted data, meaning that a Polaris program can operate on sensitive data in untrusted environments without ever exposing plaintexts, even to the program itself - in fact, no Polaris program can ever deal with plaintext data (though there is nuance to this point, as will be explained later)

## I/O in a homomorphic world

You are no doubt thinking, "if everything everywhere all the time is encrypted, how can a Polaris program interact with the outside world?". The general approach that Polaris takes is to offload I/O operations onto a runtime harness. This harness can be execution-context specific, and defines where the cryptographic boundary lies - for example, the harness could live in a Trusted Execution Envrionment, or proxy calls to a trusted on-prem server, leaving the Polaris program itself to run securely in any context.

## A short guide to the Polaris language

Polaris is heavily inspired by OCaml and Gleam, but with a few key differences required by the unique constraints of homomorphic encryption.

### Types

Polaris uses algebraic datatypes, and supports parametric polymorphism. Type declaration syntax is very similar to Gleam:

```gleam
//Result is a sum type, parametrised by free type variables 'a' and 'b'
type Result(a, b) {
  Ok(a)
  Error(b)
}

//Test is a product type
type Test {
  Test(
    name: String,
    func: fn() -> Result(None, String)
  )
}
```

Like in Gleam, all type names are capitalised, and lower case type identifiers are treated as type variables for parametric polymorphism (in the Result type above, `a` and `b` were type variables, that can be replaced with arbitrary concrete types at instantiation).

There is one major point of divergence from the Gleam type system, that being the `nocrypt` modifier.

This tells the compiler to, by default, not encrypt the underlying value (though the runtime will also load an encrypted version if involved in arithmetic with a standard encrypted value).
Use `nocrypt` for large (10-100x) performance benefits when not dealing with sensitive data - for example, routing a web request.

#### Intrinsics

Intrinsic types in Polaris include:

```gleam
// Default integer type - pretty narrow, but necessary
Int = I16
//Other integer widths - implemented as multi-limb U/I16 under the hood
I8, U8, U16, I32, U32, I64, U64
//Approx. real numbers - for exact values use fixed
Real
//Fixed-point real numbers - scaled integers under the hood
Fixed(exponent: nocrypt Int)
//Boolean
Bool

//Heap-allocated dynamic array, indexable
//Not mutable (like all in Polaris), but the compiler will
//optimise away redundant copies
Array(t)
//Classic linked cons-list
List(t)
//Hash map
Map(k, v)
//Tuple
#(a, b, ...)

//UTF-8 string - wrapper over Array(U8)
String
//Unicode code point - non-compact representation, ~ U64
Char

//Intrinsic monadic types - support the bind operator
Option(t)
Result(a, b)
```

With corresponding literals:

```gleam
let a: Int = 34
let b: Real = 3.4
let c: Fixed(2) = 3.14
let d: Bool = True
let e: Array(Int) = [1, 2, 3]
let f: List(Int) = [1, 2, 3]
let g: Map(String, Int) = {
  "key_1": 1,
  "key_2": 2
}
let h: #(Int, Int) = #(1, 2)
let i: String = "my_string"
let j: Char = "a"
```

### Bindings

Polaris is an immutable language. You can declare bindings (like variables in mutable languages) like so:

```gleam
let my_binding = some_func()
//bindings can be redeclared in the same scope
let my_binding = my_binding |> other_func
```

#### The other, monad-y kind of binding

Polaris also has two intrinsic monadic types that come with a monad-bind operator '?': `Option(t)` and `Result(a, b)`. This is the same as Rust's early return operator, though expanded to option types:

```gleam
//will return None if arg == None, due to '?' monad binding
fn opt_example(arg: Option(String)) -> Option(String) {
  let t = arg?
  let t = t |> string.reverse
  Some(t)
}

//The monad binding will early return the Error type, so we can change the Ok type
fn result_example(arg: Result(Int, String)) -> Result(String, String) {
  let t = arg? |> int.to_string
  Ok(t)
}
```

### Control flow

Polaris has three major control flow constructs: if/else, match, and the for loop. Before getting into the details though, there is an important point to be made regarding control flow when data is under homomorphic encryption.
Namely, if, for example, `x` and `y` are two encrypted integers, then how can we make sense of something like:

```gleam
if x < y {
  some_func()
}
```

We can of course homomorphically evaluate `sign(y - x)` to discover this ordering, but there's a problem - the outcome of this operation is also encrypted, so we can't know how to branch (not really a problem in the true sense, this is the whole point of HE!). Polaris takes four different approaches to solving this problem based on the context:

1. If the branch condition is `nocrypt`, the problem disappears (nice), and we can just evaluate the branch in the standard way.
2. If the branches are all pure, and the compiler doesn't detect complexity explosion, all branches are evaluated independently, and then the correct branch is selected after the fact via arithmetic predication. This is constant-time, so the overhead isn't that wild compared to the minimal overhead of just doing FHE at all.
3. If the branches perform I/O, the branch condition is handed off to the harness, which decrypts that value and returns an opaque single-use 'branch token' that is cryptographically tied to that specific branch point. This token tells the runtime which branch to take, but is not accessible from within the program. This leaks `# branches` bits to the runtime (though not the language-level program, as the compiler will not let you bind to a `nocrypt` label anything that depends on the result of a secret branch condition).
