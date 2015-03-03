#lang sweet-exp racket

provide my-cond if else-if else

require syntax/parse/define
        only-in racket/base [if rkt:if]
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

module+ test
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
