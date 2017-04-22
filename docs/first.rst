First Program
=============

When learning a new programming language, it is customary to start with a
program that prints "Hello, world!" to the screen.

Because there is currently no standard library, we will be using built-in
primitive values. Names of these values end with ``%``.

.. highlight:: sml

::

    write% stdout% (to_utf8% "Hello, world!")

If you are not familiar with functional programming, this syntax may surprise
you. Function application is written by juxtaposing functions with their
arguments. Function application is left-associative, so ``f a b`` is equivalent
to ``(f a) b``. Parentheses are used for grouping only, not for function
application.

Variables
---------

We may define variables with ``let``/``val``, as in the following example::

    let
      val message = "Hello, world!"
    in
      write% stdout% (to_utf8% message)
    end

Functions may be written as ``fun parameter -> body end``, and multiple
variables may be defined within the same ``let`` expression::

    let
      val print = fun message -> write% stdout% (to_utf8% message) end
      val message = "Hello, world!"
    in
      print message
    end

The programs demonstrated above are all equivalent in behavior.

Types
-----

Even though no type annotations appear in the above programs, Feldspar is a
typed language. Each expression has an associated type, and if the types are
not consistent with each other, the program will be rejected *prior to
execution*. For example, consider the following ill-typed program::

    let
      val print = fun message -> write% stdout% (to_utf8% message) end
    in
      print stdout% "Hello, world!"
    end

When we attempt to execute this program, we will get a type error:

.. highlight:: text

::

    feldspar: cannot unify type
      file_handle
    with type
      str

The mistake is that ``print`` already assumes a certain file handle, so we need
not pass one as an argument.
