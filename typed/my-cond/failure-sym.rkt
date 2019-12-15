#lang typed/racket/base

(provide Maybe
         none none?
         some some? some-value)

;; use list as maybe

(define-type (Maybe a) (U Null (List a)))

(: none (Maybe Nothing))
(: none? (-> (Maybe Any) Boolean))
(define none '())
(define none? null?)

(: some (∀ (a) (-> a (Maybe a))))
(: some? (-> (Maybe Any) Boolean))
(: some-value (∀ (a) (-> (Maybe a) a)))
(define (some a) (list a))
(define some? pair?)
(define (some-value m) (car m))
