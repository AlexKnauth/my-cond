#lang scribble/manual

@(require scribble/eval)

@(require (for-label racket
                     my-cond))

@title[#:tag "my-cond.scrbl"]{my-cond}

@defmodule[my-cond]

source code: @url["https://github.com/AlexKnauth/my-cond"]

@section[#:tag "my-cond.my-cond"]{my-cond}

@defform[#:literals (else => for/cond-clause for*/cond-clause)
         (my-cond my-cond-clause ...)
         #:grammar ([my-cond-clause normal-cond-clause
                                    for/cond-clause-form
                                    for*/cond-clause-form
                                    [cond-expander-id stuff ...]
                                    (code:line #:defs [def ...])
                                    (code:line #:let ([var val] ...))
                                    (code:line #:begin [def-or-expr ...])
                                    ]
                    [normal-cond-clause [condition-expr then-body ...+]
                                        [else then-body ...+]
                                        [condition-expr => proc-expr]
                                        [condition-expr]]
                    [for/cond-clause-form (for/cond-clause (for-clause ...) my-cond-clause ...)]
                    [for*/cond-clause-form (for*/cond-clause (for-clause ...) my-cond-clause ...)]
                    [for-clause [id seqence-expr]
                                [(id ...) sequence-expr]
                                (code:line #:when guard-expr)
                                (code:line #:unless guard-expr)
                                (code:line #:break guard-expr)
                                (code:line #:final guard-expr)])]{
like @racket[cond], but with the ability to use things like @racket[for/cond-clause] to iterate
through cond-clauses like @racket[for] would.  It does this using @racket[cond-expander]s.

@racket[my-cond] also allows easy internal definitions with things like @racket[#:defs [def ...]]
and @racket[#:let ([var val] ...)].  

As soon as one of the conditions evaluates to a true value, it
returns whatever @racket[cond] would return as the result of that clause.  

Otherwise, it goes on to the next clause, if there is one.  

If it reaches the end of a @racket[my-cond] expression where none of the conditions returned a true
value, the @racket[my-cond] expression returns @|void-const|.

If it reaches the end of a @racket[for/cond-clause] or @racket[for*/cond-clause] form, then it goes
through the @racket[for/cond-clause] form again for the next iteration (if there is one).  

If there is no "next iteration," then it goes on to the next clause after the @racket[for/cond-clause]
form (if there is one).  

An @racket[else] clause can only appear as the last clause of a @racket[my-cond] (or @racket[cond])
form, and cannot appear inside of a @racket[for/cond-clause] or @racket[for*/cond-clause] form.  

@examples[
  (require my-cond)
  (my-cond (for/cond-clause ([i (in-range 0 5)])
             [(<= 3 i) i]
             [(<= 2 i) (number->string i)]))
  (my-cond (for/cond-clause ([i (in-range 0 5)])
             [(<= 2 i) i]
             [(<= 3 i) (number->string i)]))
]}

@deftogether[[@defform[(for/cond-clause (for-clause ...) my-cond-clause ...)]
              @defform[(for*/cond-clause (for-clause ...) my-cond-clause ...)]]]{
these are defined as @racket[cond-expander]s.
}

@section{cond-expanders}

@defproc[(cond-expander [proc (syntax? . -> . syntax)]) cond-expander?]{
returns a cond-expander that uses @racket[proc] to transform the @racket[my-cond] form.

@examples[
  (require my-cond racket/match (for-syntax racket/base))
  (define-syntax $m
    (cond-expander
     (syntax-rules ()
       [(my-cond [$m val pat body ...] clause ...)
        (match val
          [pat body ...]
          [_ (my-cond clause ...)])])))
  (define x '(1 2 3))
  (my-cond [$m x (list a b c) (+ a b c)]
           [else #f])
  (my-cond [$m 5 (list a b c) (+ a b c)]
           [else #f])
]}

@defthing[prop:cond-expander (struct-type-property/c (-> cond-expander? (-> syntax? syntax?)))]{
a struct-type property for @racket[cond-expander]s.
}

@defproc[(cond-expander? [v any/c]) boolean?]{
returns @racket[#t] if @racket[v] is a cond-expander created by @racket[cond-expander] or
@racket[prop:cond-expander], returns @racket[#f] otherwise.
}

@defproc[(syntax-local-cond-introduce [stx syntax?]) syntax?]{
like @racket[syntax-local-introduce], but for @racket[cond-expander]s.
}

