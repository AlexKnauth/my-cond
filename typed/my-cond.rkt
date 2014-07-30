#lang typed/racket/base

(provide my-cond cond/local-def
         for/cond-clause
         for*/cond-clause
         )

(require racket/local
         (for-syntax racket/base
                     racket/local
                     racket/syntax
                     syntax/parse))

(module+ test
  (require typed/rackunit))

(require (only-in my-cond/main for/cond-clause for*/cond-clause))

(define-syntax define-gensym-type
  (lambda (stx)
    (syntax-parse stx
      [(define-gensym-type id:id)
       (with-syntax ([sym (gensym (syntax-e #'id))])
         #'(begin
             (define-type id 'sym #:omit-define-syntaxes)
             (define id : id 'sym)))])))

(define-syntax my-cond
  (lambda (stx)
    (syntax-parse stx #:literals (for/cond-clause for*/cond-clause : else)
      [(my-cond)
       (syntax/loc stx (cond))]
      [(my-cond [else ~! body:expr ...+])
       (syntax/loc stx (cond [else body ...]))]
      [(my-cond #:defs ~! [def ...] clause ...)
       (syntax/loc stx (local [def ...] (my-cond clause ...)))]
      [(my-cond #:let ~! stuff clause ...)
       (syntax/loc stx (let stuff (my-cond clause ...)))]
      [(my-cond (for/cond-clause ~! (~optional (~seq : type) #:defaults ([type #'Any]))
                  (for-clause ...) looped-cond-clause ...)
                clause ...)
       (syntax/loc stx
         (local [(define-gensym-type success-sym)
                 (define-gensym-type failure-sym)
                 (define-type Success (Pairof success-sym type))
                 (define-type Failure failure-sym)
                 (define (success? val)
                   (if (pair? val)
                       (eq? (car val) success-sym)
                       #f))
                 (define (failure? val)
                   (eq? val failure-sym))
                 (: success : [type -> Success])
                 (define (success val)
                   (cons success-sym val))
                 (: success-val : [Success -> type])
                 (define (success-val s)
                   (cdr s))
                 (define failure failure-sym)
                 (define maybe-success
                   (for/or : (U Success #f) (for-clause ...)
                     (let ([maybe-result : (U type Failure)
                                         (my-cond looped-cond-clause ...
                                                  [else failure])])
                       (if (not (failure? maybe-result))
                           (success maybe-result)
                           #f))))]
           (if maybe-success
               (success-val maybe-success)
               (my-cond clause ...))
           ))]
      [(my-cond (for*/cond-clause ~! (~optional (~seq : type) #:defaults ([type #'Any]))
                  (for-clause ...) looped-cond-clause ...)
                clause ...)
       (syntax/loc stx
         (local [(define-gensym-type success-sym)
                 (define-gensym-type failure-sym)
                 (define-type Success (Pairof success-sym type))
                 (define-type Failure failure-sym)
                 (define (success? val)
                   (if (pair? val)
                       (eq? (car val) success-sym)
                       #f))
                 (define (failure? val)
                   (eq? val failure-sym))
                 (: success : [type -> Success])
                 (define (success val)
                   (cons success-sym val))
                 (: success-val : [Success -> type])
                 (define (success-val s)
                   (cdr s))
                 (define failure failure-sym)
                 (define maybe-success
                   (for*/or : (U Success #f) (for-clause ...)
                     (let ([maybe-result : (U type Failure)
                                         (my-cond looped-cond-clause ...
                                                  [else failure])])
                       (if (not (failure? maybe-result))
                           (success maybe-result)
                           #f))))]
           (if maybe-success
               (success-val maybe-success)
               (my-cond clause ...))
           ))]
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
  
  )