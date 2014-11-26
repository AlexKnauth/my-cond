#lang racket/base

(provide parse-for/cond-clause
         parse-for*/cond-clause
         )

(require syntax/parse
         (for-template racket/base
                       "failure-sym.rkt"
                       ))

(define (make-for/cond-clause-parser for-id)
  (with-syntax ([for-id for-id])
    (define (parse-for/cond-clause stx)
      (syntax-parse stx
        [(my-cond (for/cond-clause:id ~! (for-clause ...) looped-cond-clause ...)
                  clause ...)
         (syntax/loc stx
           (let/cc return
             (for-id
              (for-clause ...)
              (define maybe-result
                (my-cond looped-cond-clause ...
                         [else failure-sym]))
              (if (not (failure-sym? maybe-result))
                  (return maybe-result)
                  #f))
             (my-cond clause ...)))]))
    parse-for/cond-clause))

(define parse-for/cond-clause (make-for/cond-clause-parser #'for))
(define parse-for*/cond-clause (make-for/cond-clause-parser #'for*))

