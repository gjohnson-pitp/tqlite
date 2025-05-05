
# TQLite (Thtructured Query Language Lite) – SQLite with a Lisp

I've found that existing Lisp bindings to C libraries tend to do too much, forcing you to adopt very specific workflows in order to accommodate the library rather than the library accommodating you. This binding aims, on the one hand, to mirror the underlying SQLite API as much as possible, treating any extra macros or functions as convenient shortcuts rather than unavoidable parts of the interface; and on the other hand, to use CLOS and other Lisp features to actually enforce the rules that C libraries allow to be broken and lead to undefined behavior.

## Installation

This package is not on Quicklisp at the moment, and I don't feel ready to have it put there yet, so you'll have to copy the source code to ~/common-lisp, ~/quicklisp/local-projects, or any other directory your ASDF / Quicklisp is aware of. At a terminal, cd into whatever directory you chose and git clone into this repository:

    git clone https://github.com/gjohnson-pitp/tqlite.git

Now, you should be able to load TQLite with `(ql:quickload "tqlite")` or `(asdf:load-system :tqlite)`.

(If you already have Lisp open before git cloning, you'll probably need to restart it to make ASDF / Quicklisp aware of the new system. To do this in SLIME, on a new, empty REPL line, hit comma and enter `restart-inferior-lisp`. I always just type `rest` and hit space a couple times, and it autocompletes. There may be an ASDF function to refresh its cache of local systems, but if so I don't remember it at the moment, and nothing in the index of functions looked like what I was looking for.)

I recommend loading with Quicklisp. The only direct dependencies are `cffi` and `trivial-garbage`, but loading the dependencies of the dependencies is the real hassle.

## Usage

Open a database connection with

    (open-database "name-of-database")

This will return a database connection object. Supposing you save this to a variable, say `*connection*`, you can then perform a single SQL statement with no parameters or result set using `execute-sql`:

    (execute-sql *connection* "CREATE TABLE foo(id INTEGER PRIMARY KEY)")

You can prepare a statement using `prepare-statement`:

    (prepare-statement *connection*
                       "SELECT * FROM foo")

This must be freed with `finalize-statement`, but any unfinalized statement will automatically be freed when it's garbage collected, so there's no need to stress out *too* much. You should probably still free them yourself rather than depending on garbage collection, though.

If a statement is only intended to be used once, then instead of preparing and finalizing it yourself, you can use the macro `with-statement`:

    (with-statement (statement *connection* "SELECT * FROM foo")
      ...)

Alternatively, if you want to prepare a statement to be used repeatedly and only finalized when the program is finished, you can use `prepare-persistent-statement`. This function's arguments and return value look completely identical to the non-persistent version; the only difference is in how SQLite stores them.

In any case, once you have a statement, you can step it with `step-statement`. Besides the obvious statement argument, `step-statement` also accepts two closures as keyword arguments, `:if-row` and `:if-done`. If stepping a statement gives a result row, then the `:if-row` closure is called with a row object as its argument. The `:if-done` closure can be used to, e.g., escape from loops with `return-from`. For example:

    ;; Assume we already have a statement object
    (loop
      (step-statement statement
                      :if-row (lambda (row) ...)
                      :if-done (lambda () (return nil))))

To save you typing, there's a shortcut macro, `do-rows`, which will automatically step a statement until it's done and execute its body with the given variable bound to each row:

    (do-rows (row statement)
      ...)

There are really only two things to do with rows: get the number of columns they have, and get the value of a particular column. These are accomplished with `column-count` and `get-column`, respectively. So, e.g., you can print all the columns of each row like this:

    (do-rows (row statement)
      (fresh-line)
      (dotimes (i (column-count row))
        (format t "~A " (get-column row i))))

You can bind parameters to a statement with `bind-parameter`. This takes three arguments: the statement, an indicator of which parameter to bind, and the value to bind. The indicator can be given either as an integer index, or as a string if the parameter is named. Note that in SQLite, the parameter numbering starts at 1!

What type the parameter should be bound as is inferred from the type of the value argument:

| argument type | SQLite type |
| --- | --- |
| integer | int |
| float | double |
| nil | null |
| string | text |
| any other vector | BLOB |

Note that A. binding `int64`'s is not supported at the moment, and B. in the case of BLOB's, the vector elements must all be unsigned bytes, or else an error will be thrown.

So we can do things like this:

    (defun get-by-name-and-age (name age)
      (with-statement (statement *connection*
                                 "SELECT * FROM example
                                  WHERE age = :age AND name = :name")
        (bind-parameter statement ":age" age)
        (bind-parameter statement ":name" name)
        (step-statement statement
                        :if-row (lambda (row) ...))))

Once you've stepped a statement at least once, you *have* to reset it before you can bind new parameters to it! (Apparently in SQLite, statements are automatically reset if you try to step them again after they've finished, but they aren't automatically reset if you try to bind to them again after stepping them. Go figure.)

Note that resetting a statement does *not* reset parameter bindings; stepping the statement again will replay it with the same parameters that were bound to it last time. To make sure bindings are reset, you need to use `clear-bindings`. But note that in SQLite, unbound parameters are implicitly bound to nulls, so it's not as though clearing parameter bindings puts the statement in an unsteppable state or something.

### Not Supported (As of Now)

- SQL code containing multiple SQL statements (as it is, any statement after the first will be ignored)
- The UTF-16 versions of functions
- The distinction between `int` and `int64`
- Opening BLOB's as streams
- Getting the value of a column in a data type other than what SQLite says it is by default
- `sqlite3_last_insert_rowid`; at the moment, you'll have to access it from SQL: `SELECT LAST_INSERT_ROWID()`

### Other TODO's

- Enforce requirements on string encoding (as it is, UTF-8 is simply assumed)
- Maybe to make `step-statement` easier to use in loops, in addition to the `if-done` callback, give the option of checking the return value to see if the statement is done?
- Give more informative error messages when an invalid operation is attempted (see Design Principles below)

## Design Principles

This library was written to prioritize ease of reasoning above all else, and it accomplishes this using the Curry–Howard correspondence. The Curry–Howard correspondence essentially says that a program is equivalent to a logical proof if we view types as propositions and functions from type A to type B as proofs that A implies B. Thus, this library is written so that most every proposition of interest to the program corresponds to a class; generic functions are used extensively as a replacement for conditional statements (I think you'll find there are hardly any conditionals in this library) and to ensure that the preconditions of functions are met; and even runtime class changing is used to indicate changes of state that affect an object's valid operations, in particular to indicate when database connections are closed and statements are finalized. So I haven't benchmarked it or anything, but I'm afraid I can't make any guarantees about the performance of this library.

A side-effect of this approach is that any attempt to perform an operation whose preconditions aren't met results in a `no-applicable-method` error, which seems uninformative until you take a closer look at the function and arguments that triggered the error. I might add extra methods to remedy this later on, but at the moment this is the state of the library.

As stated above, this library was written to provide a direct interface to the SQLite API. Thus for the most part, it makes no opinionated decisions like making result rows accessible only as association lists, or forcing you to write all your SQLite code in a macro with a name like `with-sqlite`. I think the only place where I departed significantly from this approach—and to be honest, I'm already starting to second-guess it—is in the `step-statement` function, where result rows and the end of the statement are indicated by calling closures. The purpose of this is, on the one hand, to save the user from having to make conditional checks on the return value; and on the other hand, to emphasize that result rows are temporary objects that will quickly be invalidated and so shouldn't be saved.

No class names are exported from this library's package, for two reasons. One is to ensure that class invariants are upheld. The other is because for someone who isn't already thoroughly acquainted with the code, all the different-but-similar classes might be confusing. For example, the first class you turn to to make a statement object might be `statement`, but that's probably not what you want; what you're looking for is more likely `unfinalized-statement`.

So if you want to inherit from or specialize methods on classes from this library, then you'll have to check the source code and directly access internal symbols. (Incidentally, note that `import` is not restricted to importing external symbols; it can also import another package's internal symbols. Just saying.) But as always when dealing with non-public code, you would be doing so at your own risk.

### A Word on Errors

Error classes in this library are based on *what operation* failed, not *why* a failure occurred, i.e. you'll find error names like `bind-parameter-failed` or `step-statement-failed`, not `sqlite-busy` or `type-error`. The reason for this is because the class name of the error is the only information readily available to the program at runtime, and so (I believe) should contain the information most helpful for the program to automatically choose a recovery strategy. While knowing the cause of a failure is helpful to the programmer, it is not helpful to the program; "array out of bounds" or "divide by zero" gives us no information about how we can recover. For that, we need to know what operation was being attempted when the error occurred. If the array out of bounds occurred while we were trying to read an array of users, then maybe it would be appropriate to return a null user object with a name like "Guest," but if it occurred while we were trying to read an array of test scores, then maybe it would be appropriate to return 0.

Providing information on the cause of a failure is generally left to the stack trace and the error messages returned by SQLite.

### A Word on Docstrings

The way I see it, docstrings are supposed to be miniature man pages; they let you get information on how to use the system from within the system, without having to read the source code or resort to external sources. Accordingly, all public functions and macros in this library have (or will have) docstrings complete with lambda list information and a "see also" section.

Since, as mentioned above, class names are not exported from this package, descriptions on how to use each data type are given in the functions that instantiate that data type. The "see also" section for such functions will list all the possible operations on the data type instantiated.

A pet peeve of mine is when Lispers write their docstrings under the assumption that the user has SLIME's autodocs available. By which I mean, docstrings like this:

    (documentation 'foo 'function)
    "Insert BAR into BAZ. If QUUX is not provided, it defaults to
    *standard-output*. If FRED is not provided, it defaults to 3.14159."

This kind of docstring is all well and good if I'm in SLIME, where the autodocs will show me the lambda list. But if I happen to be running Lisp directly in the terminal because I wanted to do a quick experiment and firing up Emacs and SLIME felt like overkill, or if I'm in my very-nice-to-have [cl-repl Android app](https://play.google.com/store/apps/details?id=org.eql5.android.repl&utm_source=na_Med), which doesn't have a minibuffer, then I have no way of knowing what order the arguments come in. I can see that `quux` and `fred` are optional arguments with default values, but I have no way of knowing whether they're `&optional` arguments or `&key` arguments. Anyways, even if I am in SLIME, I still have to start typing out a dummy call to the function before I can see the lambda list.

The solution to this is extremely simple—just write the lambda list in the docstring:

    (documentation 'foo 'function)
    "(foo bar baz &key quux fred) => successp
    
    Insert BAR into BAZ. If QUUX is not provided, it defaults to
    *standard-output*. If FRED is not provided, it defaults to 3.14159."

Can you imagine if man pages didn't have the "synopsis" section at the top showing the general form of invocation for the program, and the "description" section expected you to just know where all the positional arguments go? Well, that's exactly the type of documentation you're writing when you write a docstring like the first example above!

To be fair, I just found out while writing this section that on SBCL, if you use `describe` instead of `documentation`, then SBCL will show you that function's lambda list. But, for one thing, the output of `describe` is almost completely unspecified by the standard and so can't be counted on to show the information you expect on different implementations; the aforementioned cl-repl app, for example, which uses ECL under the hood, does not show the lambda lists of functions. (In fact, on that implementation, the lambda lists for standard functions are actually included in the docstrings, just like I'm saying they should be!) Docstrings are reliable and under the complete control of the programmer, so they should be used for all vital information. And for another thing, even in SBCL, if you used the same symbol for multiple purposes that all have documentation strings, e.g. a class, a function, and a slot, then `describe` will show them all, cluttering the output with information you weren't looking for.

So for all these reasons, lambda lists are included in the docstrings for all public functions / macros in this package.
