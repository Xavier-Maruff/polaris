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
