# HOPL3 - Hasksentials of Programming Languages

This is a Haskell Port of the EOPL3 codebase from Daniel P. Friedman, Mitchell Wand, et al. Original Racket implementation is available at [https://github.com/mwand/eopl3](https://github.com/mwand/eopl3).

**Coming soon: An alternate version using Parsec in place of Happy+Alex!**

## Prerequisites

To work with HOPL3, you must have one of the following configurations installed:

 * GHC + Alex + Happy; or
 * [Haskell Platform](https://www.haskell.org/platform/) (includes all of the above)

You must also have the following additional tools:

 * [Git Source Code Management](http://git-scm.com/)
 * Command shell (such as BASH, Windows Command Prompt, etc.)
 * Your favorite programmer's plain-text editor (*not* Notepad!)

## Haskell Translation of _Essentials of Programming Languages_

This public repository provides two specific resources freely available
to anyone interested in Haskell as well as the EOPL3 approach to studying
the design and implementation of programming languages.

* [HOPL3 Codebase - Haskell Translation of EOPL3 Codebase](https://bitbucket.org/hopl3/hopl3-old)
* [Companion Wiki for HOPL3](https://bitbucket.org/hopl3/hopl3-wiki)

Additionally, to help my students manage using an alternate functional
programming language with the EOPL3 textbook, I have done my best to
translate the early Chapters of the book (from Ch. 1 to 3.2) into Haskell,
including all embedded code examples. This partial translation is contained
in a private repository only accessible to students in my class, who will
have their own copy of the original textbook from which to continue on to
the later Chapters. If you are enrolled in my class, then you can find
that private repository here:

* [Hasksentials of Programming Languages - Early Chapters](https://bitbucket.org/hopl3/hopl3-book)

### Building and Running the Interpreter

In your Terminal or Git Bash, within the code directory:

    $ ./build.sh NAME_OF_LANG

This will compile all the files associated with that language and generate three executables - Tester, Interp, and Repl. To run the Tester, from Terminal/Git Bash within the code directory:

    $ NAME_OF_LANG/Tester

To run the Repl, from Terminal/Git Bash within the code directory:

    $ NAME_OF_LANG/Repl

The Interp executable can be used to parse and evaluate a file containing a source program.

    $ NAME_OF_LANG/Interp FILE_CONTAINING_LANG_SOURCE

## Why Haskell?

While I love the EOPL approach to investigating the syntax and semantics of a
programming language from the ground up, I have found the use of *Racket*
(formerly *Scheme*) somewhat cumbersome. Racket is an excellent *functional*
language with *dynamic typing* but, to paraphrase Christopher Walken in
*The Rundown*, "That's a lotta parentheses."

In my classes, I eventually eventually switched over to *Haskell* (another
functional language, but with *static typing*) as the host language for our
work in this course. I made this decision for the following reasons:

- **Haskell's syntax is neater.** Haskell's syntax brings its own challenges,
    but I like the fact that there is very little clutter inthe code.
- **Haskell's syntax is very close to the mathematical notations that we use
    in our written specifications.** There is a lot of formal notation in
    this course, and I believe it will be easier to follow if mathematical
    specifications align closely with the code implementation.
- **Haskell's types are exactly what the doctor (i.e., author) ordered.** The
    textbook frequently relies on *contracts* written in code comments. In
    Haskell, such contracts are not merely embedded in code comments, they
    are exactly the type signatures for our functions!
- **I also use Haskell in CMPT 333.** This way, I don't have to juggle so many
    languages, and students who also have taken/are taking/will take *Language
    Study* can benefit from having seen the language before.
- **I love Haskell. Haskell brings me joy.** Enough said.
