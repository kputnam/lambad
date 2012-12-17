## Demonstrating Lambda Calculus Reduction

http://www.itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf

                                       Reduce under λ
          +------------------+-----------------------+
    Strict|                Y |                     N |
    +-----+------------------+-----------------------+
    |   Y |      Normal form |      Weak normal form |
    |     |  E ∷= λx.E , x E |       E ∷= λx.e , x E |
    |     |                  |                       |
    |     |   ao, no, ha, hn |                    bv |
    +-----+------------------+-----------------------+
    |   N | Head normal form | Weak head normal form |
    |     |  E ∷= λx.E , x e |       E ∷= λx.e , x e |
    |     |                  |                       |
    |     |               he |                    bn |
    +-----+------------------+-----------------------+

                                  e ∷= x | λx.e | e e
* ao: `applicativeOrder`
* no: `normalOrder`
* ha: `hybridApplicative`
* hn: `hybridNormal`
* bv: `callByValue`
* he: `headSpine`
* bn: `callByName`

## How to Install

    $ git clone git@github.com:kputnam/lambad.git
    $ cd lambad
    $ cabal install --enable-tests

## How to Use

There are three functions defined for use in GHCi. Both `eval` and `pval` take
an reduction strategy (callByName, callByValue, normalOrder, applicativeOrder,
hybridApplicative, hybridNormal, and headSpine) and a Text string denoting a
lambda calculus expression.

`eval` returns a value of type `(Either Text Expression, [Step Expression])`,
where first element is either an error message or the result of the computation
and the second element is a list of reduction steps.

`pval` returns a value of type `Either Text Expression` and pretty-prints the
reduction steps to the console as a side effect.

`aval` takes only a Text string and evaluates the expression using each strategy
and prints a table showing the number of subexpressions evaluated and the
evaluated result.

### Example

Here's an example of adding the Church-encoded numerals, 2 and 2. The
line breaks are added for readability, but they aren't required.

    $ ghci
    > :set prompt "> "

    > aval "(lambda Z S +. (+ (S (S Z)) (S (S Z))))
              (lambda f x. x)
              (lambda n f x. n f (f x))
              (lambda m n f x. m f (n f x))"

    ao (284): λf x. f (f (f (f x)))
    no (310): λf x. f (f (f (f x)))
    ha (146): λf x. f (f (f (f x)))
    hn (125): λf x. f (f (f (f x)))
    bv (28): λf x. (λf x. (λf x. (λf x. x) f (f x)) f (f x)) f ((λf x. (λf x. (λf x. x) f (f x)) f (f x)) f x)
    he (75): λf x. f (f ((λn f x. n f (f x)) ((λn f x. n f (f x)) (λf x. x)) f x))
    bn (11): λf x. (λn f x. n f (f x)) ((λn f x. n f (f x)) (λf x. x)) f ((λn f x. n f (f x)) ((λn f x. n f (f x)) (λf x. x)) f x)


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
