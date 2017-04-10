# Feldspar

Feldspar is a programming language with the following goals:

 - All programs are referentially transparent
 - Type inference, higher-kinded types, and rank-N types
 - Very fast compiler, and no separate compilation step
 - Batteries included

The compiler will be written in Rust. The initial version will use Lua as its
virtual machine. This is merely an implementation detail. There is no way to
interact with Lua from within Feldspar.
