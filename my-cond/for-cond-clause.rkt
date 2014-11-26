#lang racket/base

(provide for/cond-clause
         for*/cond-clause
         )

(require (for-syntax racket/base
                     (only-in typed/untyped-utils syntax-local-typed-context?)
                     "cond-expander.rkt"
                     "parse-for-cond-clause.rkt"
                     (prefix-in typed: "../typed/my-cond/parse-for-cond-clause.rkt")
                     ))

(define-syntax for/cond-clause
  (cond-expander
   (lambda (stx)
     (cond [(syntax-local-typed-context?)
            (typed:parse-for/cond-clause stx)]
           [else
            (parse-for/cond-clause stx)]))))

(define-syntax for*/cond-clause
  (cond-expander
   (lambda (stx)
     (cond [(syntax-local-typed-context?)
            (typed:parse-for*/cond-clause stx)]
           [else
            (parse-for*/cond-clause stx)]))))

