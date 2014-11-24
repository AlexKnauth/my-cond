#lang racket/base

(provide my-cond cond/local-def
         for/cond-clause
         for*/cond-clause
         )
(require (for-syntax racket/base racket/contract/base))
(begin-for-syntax
  (provide (contract-out
            [prop:cond-expander
             (struct-type-property/c [cond-expander? . -> . (syntax? . -> . syntax?)])]
            [cond-expander [(syntax? . -> . syntax?) . -> . cond-expander?]]
            [cond-expander? [any/c . -> . boolean?]]
            [syntax-local-cond-introduce [syntax? . -> . syntax?]]
            )))

(require racket/local
         (for-syntax racket/base
                     racket/local
                     syntax/parse
                     "cond-expander-prop.rkt"
                     "cond-expander.rkt"
                     ))

(module+ test
  (require rackunit))

(define failure-sym
  (gensym 'failure))

(define-syntax for/cond-clause
  (cond-expander
   (lambda (stx)
     (syntax-parse stx #:literals (for/cond-clause)
       [(my-cond (for/cond-clause ~! (for-clause ...) looped-cond-clause ...)
                 clause ...)
        (syntax/loc stx
          (let/cc return
            (for (for-clause ...)
              (define maybe-result
                (my-cond looped-cond-clause ...
                         [else failure-sym]))
              (if (not (eq? maybe-result failure-sym))
                  (return maybe-result)
                  #f))
            (my-cond clause ...)))]))))

(define-syntax for*/cond-clause
  (cond-expander
   (lambda (stx)
     (syntax-parse stx #:literals (for*/cond-clause)
       [(my-cond (for*/cond-clause ~! (for-clause ...) looped-cond-clause ...)
                 clause ...)
        (syntax/loc stx
          (let/cc return
            (for* (for-clause ...)
              (define maybe-result
                (my-cond looped-cond-clause ...
                         [else failure-sym]))
              (if (not (eq? maybe-result failure-sym))
                  (return maybe-result)
                  #f))
            (my-cond clause ...)))]))))

(begin-for-syntax
  (define-syntax-class cond-exp
    [pattern id:id
             #:attr exp (syntax-local-value #'id (λ () #f))
             #:when (cond-expander? (attribute exp))
             #:attr proc (cond-expander-proc (attribute exp))]))

(define-syntax my-cond
  (lambda (stx)
    (syntax-parse stx #:literals (else)
      [(my-cond)
       (syntax/loc stx (cond))]
      [(my-cond [id:cond-exp . stuff] clause ...)
       (let* ([proc (attribute id.proc)]
              [intro (make-syntax-introducer)]
              [form (intro (syntax-local-introduce stx))]
              [new-form
               (parameterize ([current-cond-introducer intro])
                 (proc form))])
         (syntax-property
          (syntax-local-introduce (intro new-form))
          'disappeared-use (list (syntax-local-introduce #'id))))]
      [(my-cond [else ~! body:expr ...+])
       (syntax/loc stx (cond [else body ...]))]
      [(my-cond #:defs ~! [def ...] clause ...)
       (syntax/loc stx (local [def ...] (my-cond clause ...)))]
      [(my-cond #:let ~! ([var val] ...) clause ...)
       (syntax/loc stx (let ([var val] ...) (my-cond clause ...)))]
      [(my-cond #:begin [expr:expr ...] clause ...)
       (syntax/loc stx (begin expr ... (my-cond clause ...)))]
      [(my-cond clause1
                clause ...)
       (syntax/loc stx
         (cond clause1
               [else
                (my-cond clause ...)]))]
      )))

(define-syntax cond/local-def
  (lambda (stx)
    (syntax-parse stx
      [(cond/local) #'(cond)]
      [(cond/local #:defs [def ...] clause ...)
       #'(local [def ...]
           (cond/local clause ...))]
      [(cond/local #:local [def ...] clause ...)
       #'(local [def ...]
           (cond/local clause ...))]
      [(cond/local #:let ([id val] ...) clause ...)
       #'(let ([id val] ...)
           (cond/local clause ...))]
      [(cond/local #:let* ([id val] ...) clause ...)
       #'(let* ([id val] ...)
           (cond/local clause ...))]
      [(cond/local #:letrec ([id val] ...) clause ...)
       #'(letrec ([id val] ...)
           (cond/local clause ...))]
      [(cond/local #:letrec-syntaxes+values ([trans-ids trans-vals] ...) ([ids vals] ...)
                   clause ...)
       #'(letrec-syntaxes+values ([trans-ids trans-vals] ...) ([ids vals] ...)
           (cond/local clause ...))]
      [(cond/local #:parameterize ([parameter val] ...) clause ...)
       #'(parameterize ([parameter val] ...)
           (cond/local clause ...))]
      [(cond/local #:with-handlers ([pred handler] ...) clause ...)
       #'(with-handlers ([pred handler] ...)
           (cond/local clause ...))]
      [(cond/local kw:keyword stuff clause ...)
       (with-syntax ([let-id (datum->syntax stx (string->symbol (keyword->string (syntax->datum #'kw))))])
         #'(let-id stuff
                   (cond/local clause ...)))]
      [(cond/local clause)
       #'(cond clause)]
      [(cond/local clause0 clause ...)
       #'(cond clause0
               [else
                (cond/local clause ...)])]
      )))

(module+ test
  (check-equal? (my-cond) (cond))
  (let ([test-sym (gensym 'test-sym)])
    (check-equal? (my-cond [#true test-sym]) test-sym)
    (check-equal? (my-cond [else test-sym]) test-sym)
    
    (check-equal? (my-cond #:defs [(define b #t) (define x test-sym)]
                           [b x])
                  test-sym)
    (check-equal? (my-cond #:let ([b #t] [x test-sym])
                           [b x])
                  test-sym)
    
    (check-equal? (my-cond (for/cond-clause ()
                             [#true test-sym]))
                  test-sym)
    (check-equal? (my-cond (for/cond-clause ([var (in-list '())])
                             [#true 'ntaaoeux]))
                  (void))
    
    (check-equal? (my-cond (for/cond-clause ([var (in-list '(#t))])
                            [var test-sym]))
                  test-sym)
    )
  
  (check-equal? (my-cond (for/cond-clause ([var (in-range 0 5)])
                           [(<= 3 var) var]
                           [(<= 2 var) (number->string var)]))
                "2")
  (check-equal? (my-cond (for/cond-clause ([var (in-range 0 5)])
                           [(<= 3 var) (define x var) x]
                           [(<= 2 var) (define x (number->string var)) x]))
                "2")
  (check-equal? (my-cond (for/cond-clause ([var (in-range 0 5)])
                           [(<= 3 var) (define x var) x]
                           [(<= 2 var) (define x (number->string var)) x]))
                "2")
  (check-equal? (my-cond #:defs [(define b #f)]
                         (for/cond-clause ([i (in-range 0 5)])
                           [b i])
                         [(not b) "this-thing"])
                "this-thing")
  
  )
