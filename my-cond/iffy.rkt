#lang sweet-exp racket

provide if else-if else define
        all-from-out "main.rkt"

require syntax/parse/define
        only-in racket/base [if rkt:if] [define rkt:define]
        "main.rkt"
module+ test
  require rackunit

begin-for-syntax
  struct proc+condexp (proc condexp) #:transparent
    #:property prop:procedure
    struct-field-index proc
    #:property prop:cond-expander
    λ (this)
      proc+condexp-condexp this

define-syntax if
  proc+condexp
    syntax-parser
      (~and stx (if t:expr a:expr b:expr))
        syntax/loc #'stx (rkt:if t a b)
    syntax-parser
      (~and stx (my-cond [if condition:expr body:expr ...+] . stuff))
        syntax/loc #'stx (my-cond [condition body ...] . stuff)

define-syntax else-if
  cond-expander
    syntax-parser
      (~and stx (my-cond [else-if condition:expr body:expr ...+] . stuff))
        syntax/loc #'stx (my-cond [condition body ...] . stuff)

define-syntax define
  proc+condexp
    syntax-parser
      (~and stx (define stuff:expr ...+))
        syntax/loc #'stx (rkt:define stuff ...)
    syntax-parser
      (~and stx (my-cond (~and def (define :expr ...+)) . stuff))
        syntax/loc #'stx (my-cond #:defs [def] . stuff)

module+ test
  define test-sym (gensym 'test-sym)
  check-equal?
    if {2 < 1} 1 2
    2
  check-equal?
    my-cond
      if {3 < 1}
        1
      else-if {2 < 1}
        2
      else
        3
    3
  check-equal?
    my-cond
      if {3 < 1}
        1
      else-if {2 < 1}
        2
      else
        4
        3
    3
  check-equal?
    my-cond
      if {3 < 1}
        1
      else-if {2 < 1}
        2
      else
        add1 2
    3
  check-equal?
    my-cond
      if {3 < 1}
        1
      else-if {2 < 1}
        2
      else
        define f(x)
          {x + 1}
        f 2
    3
  check-equal?
    my-cond
      #:defs
        define b #t
        define x test-sym
      if b x
    test-sym
  check-equal?
    my-cond
      for/cond-clause ([var (in-range 0 5)])
        if {3 <= var}
          var
        else-if {2 <= var}
          number->string(var)
    "2"
