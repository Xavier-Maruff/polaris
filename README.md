<br/>
<div align="center">
  <a href="https://github.com/github_username/repo_name">
    <img width="80" height="80" alt="Polaris Logo" src="https://github.com/user-attachments/assets/81e1bc3f-032d-48a0-bdea-7b68786ce0d3" />
  </a>

<h1 align="center">Polaris</h1>
<div align="center">Encryption-oriented programming</div>
</div>
<br/><br/>

> [!CAUTION]
> Polaris is a work in progress, and is not yet even capable of codegen. Watch this space for updates!

Polaris is a high-security, bytecode interpreted, statically typed, primarily functional language for sensitive computation (what an insane amount of adjectives). All sensitive operations are performed under homomorphic encryption schemes, meaning that a Polaris program can operate on sensitive data in untrusted environments without ever exposing plaintexts, even to the program itself - in fact, no Polaris program can ever interact with plaintext sensitive data (though there is nuance to this point, as will be explained later)

## I/O in a homomorphic world

You are no doubt thinking, "if everything everywhere all the time is encrypted, how can a Polaris program interact with the outside world?". The general approach that Polaris takes is to offload I/O operations onto a runtime harness. This harness can be execution-context specific, and defines where the cryptographic boundary lies - for example, the harness could live in a Trusted Execution Envrionment, or proxy calls to a trusted on-prem server, leaving the Polaris program itself to run securely in any context.

## A short guide to the Polaris language

> [!NOTE]
> This guide is also half a language design doc - a friendlier get-started guide is on the way.

Polaris is heavily inspired by OCaml and Gleam, but with a few key differences required by the unique constraints of homomorphic encryption.

### Types

Polaris uses algebraic datatypes, and supports parametric polymorphism. Type declaration syntax is very similar to Gleam:

```gleam
//Result is a sum type, parametrised by free type variables 'a' and 'b'
type Result(a, b) {
  Ok(a)
  Error(b)
}

//Test is a record, which is sugar for a product type
type Test {
  Test(
    name: String,
    func: fn() -> Result(Void, String)
  )
}

//can be instantiated by
let t = Test(name: "my_test", func: fn(){ Ok(Void) })
```

Like in Gleam, all type names are capitalised, and lower case type identifiers are treated as type variables for parametric polymorphism (in the Result type above, `a` and `b` were type variables, that can be replaced with arbitrary concrete types at instantiation).

> [!NOTE]
> In order to maintain security in Polaris' approach to [control flow under homomorphic encryption](#the-branching-problem), Polaris isomorphises all sum type variants to the same underlying representation, though at the language-level this is not exposed. Keep this in mind when designing types that will be used in performance critical code - it is a good idea to minimise the difference between sum type variants (a good heuristic is asking 'independent of ordering and labelling, how many changes must I make to each of these variants so that they all wrap the same types', and designing your types to minimise that value).

#### Nocrypt

```gleam
let a: nocrypt Int = 12
```

Nocrypt is a type modifier that tells the compiler to, by default, not encrypt the underlying value (though the runtime can also load an encrypted version from the harness if involved in arithmetic with a standard encrypted value - note however that the compiler will error if a nocrypt `Real` is used in an encrypted context, due to a vulnerability in the underlying CKKS scheme used to implement real arithmetic).
Use `nocrypt` for large (multiple orders of magnitude) performance benefits when not dealing with sensitive data - for example, routing a web request. For ultra-high security applications, the use of nocrypt can be disabled via the compiler flag `--no-nocrypt`.

#### Intrinsics

Intrinsic types in Polaris include:

```gleam
// Empty unit type
Void

// Default integer type - pretty narrow, but necessary
Int = I16
//Other integer widths - implemented as multi-limb U/I16 under the hood
I8, U8, U16, I32, U32, I64, U64
//Approx. real numbers - for exact values use fixed
Real
//Fixed-point real numbers - scaled integers under the hood
Fixed1(t), Fixed2(t), Fixed4(t)
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

//function types are written as
fn(Arg1Type, Arg2Type, ...) -> ReturnType
```

With corresponding literals:

```gleam
let a: Int = 314
let b: Real = 3.141
let c: Fixed2(Int) = 3.14
let d: Bool = True
let e: Array(Int) = [1, 2, 3]
let f: List(Int) = [1, 2, 3]
let g: Map(String, Int) = #{
  "key_1": 1,
  "key_2": 2
}
let h: #(Int, Int) = #(1, 2)
let i: String = "my_string"
let j: Char = "a"
//closure literal
let k: fn(Int) -> Int = fn(a) { a + 1 }
```

> [!NOTE]
> Any implicit numeric conversions or narrowings will result in a compiler error, and this includes literals. This is because type coercion is expensive under homomorphic encryption, and involves switching schemes in most cases. To convert a value to a different numeric type, use the relevant intrinsic conversion function, e.g. `int.to_real`, `fixed2.to_int`, etc.

### Bindings

Polaris is an immutable language, as this allows for certain important FHE optimisations. You can declare bindings like so:

```gleam
let my_binding = some_func()
//bindings can be redeclared in the same scope
let my_binding = my_binding |> other_func
```

### Functions

Functions in Polaris are declared using `fn`:

```gleam
fn my_func(a: Int) -> Int {
  a + 12
}

```

As discussed earlier, we can make a function generic by using an implicit type variable instead of a concrete type:

```gleam
fn my_func(a: Result(Int, t)) -> Result(Int, t) {
 match a {
   Ok(i) -> Ok(i + 12)
   Error(m) -> Error(m)
 }
}
```

Functions can be declared to exist in the encrytped runtime context or in the harness host context. To declare a function that lives on the harness:

```gleam
harness fn my_func(a: Int) -> Result()
```

Harness functions must return either `Void` or `Result(a, b)`, as any call to the harness is implicitly effectful - we are either just going to dump an effect to the harness and not care about what happens later, or need some data back after I/O and/or decryption in the harness, both of which could fail.

Otherwise extern functions as of yet cannot be declared due to the unique layout and encoding of data in the Polaris runtime.

### Monad-y goodness

Polaris has two intrinsic monadic types that come with a monad bind operator '?': `Option(t)` and `Result(a, b)`. This is the same as Rust's early return operator, though expanded to option types:

```gleam
//will return None if arg == None, due to '?' monad binding
fn opt_example(arg: Option(String)) -> Option(String) {
  let t = arg?
  let t = t |> string.reverse
  Some(t)
}

//The monad binding will early return the Error type, so we are free to change the Ok type
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

#### The branching problem

We can of course homomorphically evaluate `sign(y - x)` to discover this ordering, but there's a problem - the outcome of this operation is also encrypted, so we can't know how to branch (not really a problem in the true sense, this is the whole point of HE!). Polaris takes four different approaches to solving this problem based on the context:

1. **Nocrypt condition**:
   If the branch condition is `nocrypt`, the problem disappears (nice), and we can just evaluate the branch in the standard way.
2. **Pure branches**:
   If the branches are all pure, and the compiler doesn't detect complexity explosion (configurable via the predication cost budget flag `--predication-budget), all branches are evaluated independently, and then the correct branch is selected after the fact via arithmetic predication. This is constant-time, so the overhead isn't that wild compared to the minimal overhead of just doing FHE at all.
3. **Effectful branches**:
   If the branches perform I/O, or are heuristically detected to involve intense computation, the branch condition is handed off to the harness, which decrypts the value and returns a continuation token to the runtime, which then executes the correct branch associated with that token. The mapping from condition outcome to branch token is only available to the harness, as part of the harness contract generated at compile time, meaning this doesn't leak any information (outside of potential side-channels, which will be addressed at some point before v1.0) about the condition outcome to the untrusted runtime. Additionally, no knowledge about which branch was executed can be propagated outside the branch at the language level, as the compiler forbids binding nocrypt values to anything that depends on secret branch conditions.
4. **Strict mode**:
   If the compiler flag `--strict-branch` is set, and the branches perform I/O, instead of the harness decrypting the condition and returning a continuation token, the harness tells the runtime to execute all branches and select the result via arithmetic predication, like strategy 2 - the difference this time is that the harness, knowing the correct branch, will dummy I/O operations on the other branches.

#### If/Else

```gleam
let result = if some_condition {
  Ok(3)
} else {
  Error("Condition failed")
}
```

#### Match

```gleam
type MyNumericType {
  Thing1(x: Fixed2(Int))
  Thing2(x: Real)
}

let unwrapped = match some_value {
  Thing1(x) -> x |> fixed2.to_real
  Thing2(x) -> x
}
```

#### For loop

For loops in Polaris are similar in semantics to those in OCaml - they just expand to repeated evaluation of the body of the loop, and said body must be of type `Void`. Loops can also not be broken out of, so all iterations must be executed.

```gleam
for i in 1 to 10 {
  do_something(i)
}
```

### Module system

This is another facet of Polaris' design that is just fully ~~stolen~~ borrowed from Gleam. Polaris code is organised into packages, which are library or executable-level structures, and modules, which are granular within packages. Modules are imported by their filepath relative to the package source root, and otherwise relative import paths are not allowed.
Packages are directories that have a `polaris.toml` file, in which you can configure compiler and package settings.

#### Imports

Imports are scoped to the module name by default:

```gleam
import core/io

fn main() {
  io.println("hello")
}
```

Or can be relabelled using `as`:

```gleam
import core/io as something_funky

fn main() {
  something_funky.println("hello")
}
```

### Memory management

Due to the need for immutability for Polaris' FHE optimisation pipeline, memory management is handled by the runtime. This also keeps the language simpler, and helps maintain the level of abstraction necessary for the actual atomic types implementations to be so decoupled from their standard counterparts that the language user has in their head (for example, integers are not integers, but massive polynomials - we don't wan't the user to have to think about memory management of a single integer though, things would get too crazy).
The compiler and runtime use a combination of GC, reference counting, and lifetime analysis to handle allocations and frees, as well as secretly mutating data behind the scenes when a move is detected. For example:

```gleam
fn add_1(val: Int) -> Int {
  val + 1
}

let my_arr: Array(Int) = [1, 2, 3]
//array.apply_at is nominally pure, but the compiler will optimise this
//to mutate the underlying array rather than copy
let my_arr = my_arr |> array.apply_at(0, add_1)
```

## PMIR and PLIR

Polaris has two block-argument SSA-based intermediate representations: PMIR, the mid-level IR, and PLIR, the low-level IR, before codegen to the bytecode interpreted by the Polaris runtime. PMIR abstracts FHE schemes and operations and retains the value semantics of the source alongside branch conditions, applying language-level optimisations like tail-calls, while PLIR involves explicit FHE schemes, accelerator intrinsics, mapping from value semantics to buffer semantics, and branch anonymisation.

### Examples

The following are some examples of PMIR/PLIR codegen from Polaris source, mostly for internal reference at the moment (these are not good docs).

```gleam
import core/array

const some_const = 12
const other_const = "hello"
const clear_const: nocrypt Int = 42

harness fn log_something(arg: String) -> Result(Void, String)

fn some_pure_func(arg: Int) {
  arg + some_const
}

//use intrinsic array.reduce which maps directly to a VM op
fn sum_array_opt(arr: Array(Int)) -> Int {
  arr |> array.reduce(fn(a, b) { a + b }, 0)
}

//manual tail recursive sum
fn sum_array_rec(arr: Array(Int)) -> Int {
  arr |> sum_array_internal(0)
}

fn sum_array_internal(arr: Array(Int), acc: Int) -> Int {
  match arr {
    [] -> acc
    [first, ...rest] -> rest |> sum_array_internal(acc + first)
  }
}


fn main() {
  let val = some_pure_func(some_const)
  log_something(other_const)

  if val > some_const {
    log_something("val is big")
  } else {
    log_something("val is small")
  }
}

```

```llvm
module hello_world

harness_contract {
  ; constants that must be injected by the harness
  const_manifest {
    %some_const: enc.int16 = 12
    %other_const: enc.str = "hello"
    %$0: enc.str = "val is big"
    %$1: enc.str = "val is small"
    %$2: enc.int16 = 0
    %clear_const: nc.int16 = 42
  }

  ; functions that must be implemented by the harness
  fn_manifest {
    log_something(arg: enc.str): enc.result(nc.void, enc.str)
  }
}

fn some_pure_func(arg: enc.int16): enc.int16 {
  ^entry():
  %arg0: enc.int16 = enc.add_int16 arg %some_const
  ret %arg0
}

fn sum_array(arr: enc.array(enc.int16)): enc.int16 {
  ^entry():
  %0: enc.int16 = array.reduce arr $lambda_0 %$2
  ret %0
}

fn $lambda_0(a: enc.int16, b: enc.int16): enc.int16 {
  ^entry():
  %0: enc.int16 = enc.add_int16 a b
  ret %0
}

fn sum_array_rec(arr: enc.array(enc.int16)): enc.int16 {
  ^entry():
  %0: enc.int16 = sum_array_internal arr %$2
  ret %0
}

; tail-call optimised
fn sum_array_internal(arr: enc.array(enc.int16), acc: enc.int16): enc.int16 {
  br ^entry(arr, acc)

  ^entry(%arr, %acc):
  ; array metadata is nocrypt in the current design
  %0: nc.bool = array.is_empty %arr
  %1: branch_id = nc.decide_branch %0 {nc.bool.true: ^branch_1, nc.bool.false: ^branch_2}
  br %1


  ^branch_1():
  ret acc

  ^branch_2():
  ; access takes a nocrypt index
  %2: enc.int64 = array.access %arr 0
  %3: nc.int64 = array.length %arr
  %4: nc.int64 = nc.int64.sub %3 1
  %5: enc.array(enc.int16) = array.slice %arr 1 %4
  %6: enc.int16 = enc.add_int16 %acc %2
  br ^entry(%5, %6)
}

fn main(): nc.void {
  ^entry():
  %0: enc.int16 = call some_pure_func(%some_const)
  call harness.log_something(%other_const)

  %1: enc.bool = enc.gt_int16 %0 %some_const
  ; decide_branch [decision id] [condition] {variant: branch_label, ...}
  %2: branch_id = harness.decide_branch $main_1 %1 {enc.bool.true: ^branch_1, enc.bool.false: ^branch_2}

  br %2

  ^branch_1():
  call harness.log_something(%$0)
  br ^end()

  ^branch_2():
  call harness.log_something(%$1)
  br ^end()

  ^end():
  ret nc.void.void
}

```

## Planned enhancements

### Strict-mode side-channel resistance

The strict-mode branching strategy could be susceptible to side-channel attacks via (1) timing differences between real I/O and dummy I/O, and (2) data size differences between a real I/O op and its dummy counterpart.
Polaris tries to address this potential vulnerability via I/O profiles - harnesses can record timing and response size distributions per I/O operation for a given context during an explicit profiling stage in development, and then apply this to its dummy I/O operations during runtime in production.
The goal of this is to make dummy I/O operations virtually statistically indisitnguishable from real I/O operations, reducing side-channel risk.
