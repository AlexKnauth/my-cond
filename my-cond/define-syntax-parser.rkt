#lang racket/base

(provide define-syntax-parser)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/stxparam
                     (for-syntax racket/base
                                 )))

;; define-syntax-parser idea from
;; https://github.com/jackfirth/generic-bind/commit/863d39229fb42395face91de733870c0d02f0922#diff-3252674930bbd0c4e113856a2a3a5747R118
(define-syntax define-syntax-parser
  (syntax-parser
    [(define-syntax-parser id:id #:stx stx-id:id option-or-clause ...)
     #'(define-syntax id
         (lambda (stx-id)
           (syntax-parameterize ([this-syntax (make-rename-transformer #'stx-id)])
             (syntax-parse stx-id option-or-clause ...))))]
    [(define-syntax-parser id:id option-or-clause ...)
     #:with stx-id:id (generate-temporary #'stx)
     #'(define-syntax-parser id #:stx stx-id option-or-clause ...)]))

