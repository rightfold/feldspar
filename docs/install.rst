Installation
============

There is currently no installer for Feldspar. If you want to try out Feldspar,
you can build it yourself. You will need to have Git, a Rust compiler, and
Cargo installed.

.. highlight:: bash

::

    git clone git@github.com:rightfold/feldspar.git
    cd feldspar
    make build
    export PATH="$PATH:$PWD/target/debug"

You can now run Feldspar programs.

::

    feldspar example/hello.fls
