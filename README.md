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
