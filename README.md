# Inravina

A portable and extensible Common Lisp pretty printer.

This is a work in progress and not ready to replace the pretty printer
in an existing Common Lisp implementation. Most of the pretty printer
interface and functionality has been implemented. If you want to kick
the tires and light the fires than continue reading!

# Requirements

[ABCL][], [Clasp][], [ECL][], and [SBCL][] is the only current
implementations that this system has been tested on. The intention is
to include this as the stock pretty printer in [SICL][] eventually.

In addition to a clone of this repository in a location that is
discoverable by ASDF you will also need a clone of
[nontrivial-gray-streams][] and [Incless][].

# Usage

The core functionality is in the `inravina` package, but the Common
Lisp-like interface of `pprint-newline`, `pprint-dispatch` is in the
`inravina-extrinsic` package and system. To call pprint on a form try
the following in SBCL:

```
* (asdf:load-system :inravina-extrinsic)
T
* (incless-extrinsic:pprint '(loop for i in '(a b c) 
                                    unless (eq i b) do (stuff i) (quux i) and collect i))

(LOOP FOR I IN '(A B C)
      UNLESS (EQ I B)
        DO (STUFF I)
           (QUUX I)
        AND COLLECT I)
```

Inravina can also replace replace an implementation's pretty
printer. This is done with the `inravina-intrinsic` system. Because
pretty printer works with PRINT-METHOD and must participate in circle
detection it must be coupled with the the printer and to some extent
with FORMAT. This is done via [Incless][] which has its own
intrinsic/extrinsic interface. In the case of replacing the pretty
printer of an implementation, Inravina will interface to the printer
via the incless-native system. To load all the required systems along
with the required glue code one would use the `inravina-shim` system.

```
* (asdf:load-system :inravina-shim)
T
* (pprint '(loop for i in '(a b c) 
                 unless (eq i b) do (stuff i) (quux i) and collect i))

(LOOP FOR I IN '(A B C)
      UNLESS (EQ I B)
        DO (STUFF I)
           (QUUX I)
        AND COLLECT I)
```

There are also some examples of pretty printing with [cl-pdf][] and
[McCLIM][] in the examples directory.

# Core Functionality

Almost everything works currently. There are probably bugs abound and
nothing has been optimized yet. The dispatch tables are functional but
very inefficient.  There is a variety of form specific printers but
not all forms that need a specific printer in the `common-lisp`
package have been covered yet.

In the case of no available printer in the dispatch table for function
or macro calls there is a fallback printer. This printer will use
introspection on the appropriate lambda list declaration to determine
non-key argument counts, body forms, and embedded lambda list
desctructuring arguments for macros. Given that information it will
attempt to do things like keeping key-values together on a single line
or printing body forms correctly, etc.

# Extended Functionality

Inravina is being developed with a "client" extensibility design
pattern. It is also being designed with an eye to correct shortcomings
in the original XP algorithm.

Some extended functionality is already available. Specifically,
Inravina can typeset code using proportional fonts. For an sample of
this see the PDF code in the examples directory.

Additionally, font styles can be injected into the instruction
sequence normally created by `pprint-logical-block`, `pprint-newline`,
`pprint-tab`, `pprint-indent` and calls to `write-char` or
`write-string` on the pretty stream. A sample of this being done is
available in the McCLIM code in the examples directory.

[ABCL]: https://armedbear.common-lisp.dev/
[CLASP]: https://github.com/clasp-developers/clasp
[ECL]: https://ecl.common-lisp.dev/
[Incless]: https://github.com/s-expressionists/Incless
[McCLIM]: https://github.com/McCLIM/McCLIM
[SBCL]: http://sbcl.org
[SICL]: https://github.com/robert-strandh/SICL
[cl-pdf]: https://github.com/mbattyani/cl-pdf
[nontrivial-gray-streams]: https://github.com/yitzchak/nontrivial-gray-streams
