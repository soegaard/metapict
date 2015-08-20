MetaPict
========

A graphics library for producing beautiful pictures using Racket.
MetaPict programs mimick the MetaPost/TikZ style known in LaTeX-circles.

The documentation is being written. See the progress here:

    http://soegaard.github.io/docs/metapict/metapict.html

Please report any errors - including grammatical and spelling errors.

I'd like to have a collection of user contributed examples, so consider
sharing any MetaPict programs you write.

Installation
============

The easiest way to install MetaPict is to use the Package Manager in DrRacket.
After opening the Package Manager (from the "File" menu) click the "Available from Catalog" tab.
That tab lists all packages available from pkgs.racket-lang.org.

Now enter metapict in the filter. The MetaPict package is now easy to spot.
Click the line with metapict and then click the "Install" button.

You will see that the package manager downloads metapict, compiles it,
and, generates the documentation. The last bit - generating the documentation -
takes a while for MetaPict due to the large amount of images used in the examples.

The documentation builder will give a few warnings similar to:

    raco setup: WARNING: undefined tag in <pkgs>/metapict/metapict/scribblings/metapict.scrbl:
    raco setup:  ((lib "metapict/main.rkt") --)
    ...
    raco setup:  ((lib "metapict/main.rkt") above)
    ...

These are harmless - they mean that I need improve the documentation at some point.

Any other errors, I'd like to hear about.

/soegaard

Examples
========

Old note (is this still needed?):

Run the examples directly in DrRacket, but do this first:

  - In the menu "Language" choose the item "Choose Language"
  - Click "Show Details"
  - Remove the tick in "Populate Compiled Directories"
