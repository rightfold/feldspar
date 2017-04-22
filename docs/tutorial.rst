Tutorial
========

Once you have installed Feldspar, it is time to write your first program.
Feldspar programs are usually stored in text files with the ``.fls`` file
extension, although the implementation does not care what you actually name
your files.

.. highlight:: sml

We will begin with a simple program that writes a message to the screen::

    write% stdout% (to_utf8% "My first Feldspar program!")

.. highlight:: text

Once you have stored this program on your file system, you can have it compiled
and interpreted in one step::

    $ feldspar first.fls
    My first Feldspar program!
