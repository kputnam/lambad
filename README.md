## Syntax

The syntax follows ordinary notation for the pure lambda calculus. Feel free to
skip this section if you're already familiar with the notation. Expressions are
built from variables, applications, abstractions, or parenthesized expressions.

    E ::= x
        | E E
        | lambda x. E
        | λx. E
        | ( E )

### Variables

Variables are a sequence of characters not containing whitespaces, periods, or
parentheses.

### Applications

Function application is denoted by whitespace, and is left-associative. For
example, `f x y` is parsed as `((f x) y)`, which means apply `y` to the result
of applying `f` to `x`.

You can use parentheses to override the default associativity. For instance,
`g (f x)` means apply `g` to the result of applying `f` to `x`.

### Abstractions

Abstractions are written using either the string `lambda ` or the character
`λ`, followed by one or more variable names separated by whitespace, followed by
the `.` character to separate the parameters from expression body which follows
the `.` character.

For example, the identity function is written `λx. x`. The K combinator is
written either `λx. λy. x` or it can be abbreviated `λx y. x`. The ladder is
desugared into the same expression as the former.

Abstraction is right-associative and has a higher precedence than application,
so `lambda x. lambda lambda y. E` is parsed as `(λx. (λlambda. (λy. E)))`,
instead of `(λx. lambda) (λy. E)`. Note `λ` and `lambda` are not reserved
identifiers.

You can use parentheses to override default associativity and precedence. For
instance `lambda x. E F` is parsed as `(lambda x. (E F))`, denoting abstraction.
Parentheses allow you to write `(lambda x. E) F`, which denotes application.

## How to Install

    $ git clone git@github.com:kputnam/lambad.git
    $ cd lambad
    $ cabal install --enable-tests

## How to Use

There are two functions defined for use in GHCi: `eval` and `pval`. Both take
an evaluation order (callByName, callByValue, normalOrder, applicativeOrder,
hybridApplicative, hybridNormal, and headSpine) and a Text string denoting a
lambda calculus expression.

`eval` returns a value of type `(Either Text Expression, [Step Expression])`,
where first element is either an error message or the result of the computation
and the second element is a list of reduction steps.

`pval` returns a value of type `Either Text Expression` and pretty-prints the
reduction steps to the console as a side effect.

### Example

Here's an example which adds two Church-encoded numerals, 2 and 2. The
line breaks are added for readability, but they aren't required.

    $ ghci
    > :set prompt "> "
    > pval applicativeOrder "(lambda Z S +. + (S (S Z)) (S (S Z)))
                               (lambda f x. x)
                               (lambda n f x. f (n f x))
                               (lambda n m f x. m f (n f x))"

    ...
              => f (f x)
              >> f (f (f (f x)))
                >> f
                => f
                >> f (f (f x))
                  >> f
                  => f
                  >> f (f x)
                    >> f
                    => f
                    >> f x
                      >> f
                      => f
                      >> x
                      => x
                    => f x
                  => f (f x)
                => f (f (f x))
              => f (f (f (f x)))
            => f (f (f (f x)))
          => λx. f (f (f (f x)))
        => λf x. f (f (f (f x)))
      => λf x. f (f (f (f x)))
    => λf x. f (f (f (f x)))
    276.0 steps

    Right (Abstraction "f"
            (Abstraction "x"
              (Application
                (Variable "f")
                (Application
                  (Variable "f")
                  (Application
                    (Variable "f")
                    (Application
                      (Variable "f")
                      (Variable "x")))))))

The result is the Church-encoded representation of 4: `λf x. f (f (f (f x)))`.

### Reading from a File

    > import qualified Data.Text.IO as T
    > pval applicativeOrder =<< T.readFile "example.plc"
