# Pretty Printer Issues

Some issues I have noticed as I have been working on Inravina.

## Issues with the ANSI standard 

1. There is no statement about what is in the "initial" or the
   "standard" pretty printer dispatch table. The original AIM paper on
   the XP printer makes several statements:
    1. This table is initialized with a number of entries that specify
       how to pretty print all the built-in Common Lisp macros and
       special forms.
    3. It is expected that the table may contain entries whose type
       specifiers partially overlap in various ways. For example, the
       standard print dispatch table contains a catchall entry for
       printing lists in general and a number of entries for printing
       specific knds of lists.
    5. **Predefined pretty printing functions.** To support
       traditional Lisp pretty printing style, XP provides pretty
       printing functions for all of the Common Lisp macros and
       standard forms. The user can change the way any griven kind of
       list is printed by defining a new list pretty printing function
       for it. To facilitate the correct utilization of priorities.
       Figure 41 summarizes the contents of the standard print
       dispatch table initially defined by XP.

        | Priority | Type Specifier                         | Pretty Printing Action      |
        |---------:|:---------------------------------------|:----------------------------|
        |        0 | (cons (member *symbol*))               | ~60 printers for Lisp code. |
        |       -5 | (cons (and symbol (satifies fboundp))) | Print as function call      |
        |      -10 | cons                                   | Print using **fill-style**  |

2. There is no statement that calling SET-PPRINT-DISPATCH with the
   default priority of zero is sufficient to override the values in
   the initial dispatch table. Nor are there any statements about what
   priority should be used to guarantee overriding the initial values
   if a zero priority is not sufficient.
3. There is no standardized way to create an empty dispatch table or
   iterate through the entries of an existing table.
4. There is no direct way to make a copy of or query the contents of
   the standard dispatch table. One should be able make a copy of the 
   standard dispatch table via
   ```lisp
   (with-standard-io-syntax (copy-dispatch-table *readtable*))
   ```
5. The glossary entry for **standard pprint dispatch table** states
   that it "must never be modified by any program." This implies that
   dispatch tables can be read-only but there is no standardized way
   of checking to see if an existing table is read-only, or a
   standardized way to make a table read-only. Also, there is no
   statement in the "Exceptional Situations" section of
   SET-PPRINT-DISPATCH about read-only tables and the error that
   should result from attempting to modify one.

## Issues with ansi-test

* COPY-PPRINT-DISPATCH.1, COPY-PPRINT-DISPATCH.2,
  COPY-PPRINT-DISPATCH.3 and COPY-PPRINT-DISPATCH.4 all assume that
  priority zero with a type specifier of EQL will override any
  existing entries.
* PPRINT-DISPATCH.3, PPRINT-DISPATCH-5, PPRINT-DISPATCH-8 and
  PPRINT-DISPATCH-9 assume that `(EQL |X|)` as a type specifier with a
  priority of zero will override any entries in the initial
  table. This seems to assume that there is no `SYMBOL` type specifier
  in the initial table at priority zero or higher.
* PPRINT-DISPATCH.4 and PPRINT-DISPATCH-7 makes the same assumptions
  as PPRINT-DISPATCH-3 except that they allow any priority. Therefore
  they assume that there is no `SYMBOL` type specifier in the initial
  table at all.
