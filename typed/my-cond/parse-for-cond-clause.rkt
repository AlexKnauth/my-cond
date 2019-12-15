#lang racket/base

(provide parse-for/cond-clause
         parse-for*/cond-clause
         )

(require syntax/parse
         (for-template typed/racket/base
                       "failure-sym.rkt"
                       ))

(define (make-for/cond-clause-parser for-id)
  (with-syntax ([for-id for-id])
    (define (parse-for/cond-clause stx)
      (syntax-parse stx #:literals (:)
        [(my-cond (for/cond-clause:id
                   ~!
                   (~or (~seq (for-clause ...)
                              (~optional (~seq : type) #:defaults ([type #'Any])))
                        (~seq (~optional (~seq : type) #:defaults ([type #'Any]))
                              (for-clause ...)))
                   looped-cond-clause ...)
                  clause ...)
         (syntax/loc stx
           (call/cc
            (lambda ([return : (type -> Nothing)])
              (for (for-clause ...)
                (: maybe-result : (Maybe type))
                (define maybe-result
                  (call/cc
                   (lambda ([mreturn : ((Maybe type) -> Nothing)])
                     (some
                      (my-cond looped-cond-clause ...
                               [else (mreturn none)])))))
                (if (some? maybe-result)
                    (return (ann (some-value maybe-result) type))
                    #f))
              (my-cond clause ...))))]))
    parse-for/cond-clause))

(define parse-for/cond-clause (make-for/cond-clause-parser #'for))
(define parse-for*/cond-clause (make-for/cond-clause-parser #'for*))

