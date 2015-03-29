#lang racket/base

(provide (struct-out proc+condexp))

(require (only-in "cond-expander.rkt" prop:cond-expander))

(struct proc+condexp (proc condexp) #:transparent
  #:property prop:procedure
  (struct-field-index proc)
  #:property prop:cond-expander
  (λ (this)
    (proc+condexp-condexp this)))

