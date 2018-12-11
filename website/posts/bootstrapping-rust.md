title: Bootstrapping Rust
date: 2018-12-11 14:36
author: Danny Milosavljevic
tags: Bootstrapping, Reproducible builds
---
Slowly, systems programming languages are getting better in the sense
of giving more guarantees and automating what can be automated without
downsides.

Rust is one of the more promising system programming languages.  Its
central advantage is that it enforces memory safety and thread safety
at compile time, without incurring any runtime overhead.

The part that enforces memory safety is called "the borrow checker".

It has been a long-standing tradition to develop a language far enough
to be able to write the language's compiler in the same language, and
Rust does the same.  Rust is nowadays written in Rust.

We've tracked down the earlier Rust versions, which were written
in OCaml, [1] and were planning to use these to bootstrap Rust.
But in parallel, John Hudge (Mutabah) developed a Rust compiler,
called "mrustc", written in C++ [2].

mrustc is now good enough to compile rust 1.19.0.

We now got a bootstrap chain like this:

    rust@1.28.0
        ^
        |
    rust@1.27.2
        ^
        |
    rust@1.26.2
        ^
        |
    rust@1.25.0
        ^
        |
    rust@1.24.1
        ^
        |
    rust@1.23.0
        ^
        |
    rust@1.22.1
        ^
        |
    rust@1.21.0
        ^
        |
    rust@1.20.0
        ^
        |
    rust@1.19.0
        ^
        |
    mrustc@0.8.0
        ^
        |
       g++

#### Limitations

* mrustc currently does no borrow checking -- so memory safety
of our rust@1.19.0 is mostly guaranteed in the sense of "someone else
built rust@1.19.0 using another Rust compiler and thus ran the borrow
checker already".

* The bootstrap chain is rather long.  There are plans to extend
mrustc to support newer Rust, but it turned out to be difficult.

* Rust takes reproducible builds seriously, but there are some
reproducibility problems left in earlier compilers that pop up
very sporadically (mostly because of LLVM, and some because of
Rust hashtable poisoning).  Help wanted, especially from LLVM
people!

* Each target we want to support has to have support in LLVM,
AND mrustc needs to have a specification of the alignment and
sizes of the base types.

[1]: https://github.com/rust-lang/rust/commit/ef75860a0a72f79f97216f8aaa5b388d98da6480
[2]: https://github.com/thepowersgang/mrustc
