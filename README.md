my-cond
=======

a version of cond that supports a for/cond-clause form, allowing it to iterate though conditions

[![Build Status](https://travis-ci.org/AlexKnauth/my-cond.png?branch=master)](https://travis-ci.org/AlexKnauth/my-cond)

my-cond is like cond, but with the ability to use things like for/cond-clause to iterate through
cond-clauses like for would.
my-cond also allows easy internal definitions with things like #:defs [def ...] and
 #:let ([var val] ...).

As soon as one of the conditions evaluates to a true value, it returns whatever cond
would return as the result of that clause.

Otherwise, it goes on to the next clause, if there is one.

If it reaches the end of a my-cond expression where none of the conditions returned a
true value, the my-cond expression returns void.

If it reaches the end of a for/cond-clause or for*/cond-clause form, then it goes
through the for/cond-clause form again for the next iteration (if there is one).

If there is no "next iteration," then it goes on to the next clause after the for/cond-
clause form (if there is one).

An else clause can only appear as the last clause of a my-cond (or cond) form, and
cannot appear inside of a for/cond-clause or for*/cond-clause form.

Examples:

    > (require my-cond)
    > (my-cond (for/cond-clause ([i (in-range 0 5)])
                 [(<= 3 i) i]
                 [(<= 2 i) (number->string i)]))
    "2"
    > (my-cond (for/cond-clause ([i (in-range 0 5)])
                 [(<= 2 i) i]
                 [(<= 3 i) (number->string i)]))
    2
