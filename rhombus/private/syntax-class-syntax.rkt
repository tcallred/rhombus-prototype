#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     )
         syntax/parse
         (submod "quasiquote.rkt" convert)
         (submod "syntax-class.rkt" for-syntax-class-syntax)
         "definition.rkt"
         "name-root.rkt")

(provide
 (rename-out [class stx_class]))

(module+ for-macro
  (provide syntax))

(define-simple-name-root syntax
  class)

(define-syntax class
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (alts group quotes)
        [(form-id class-name (alts alt ...)
                  . tail)
         (list
          #`(begin-for-syntax
              (define-splicing-syntax-class class-name
                #:datum-literals (block group quotes)
                #,@(for/list ([alt-stx (in-list (syntax->list #'(alt ...)))])
                     (define-values (p idrs can-be-empty?)
                       (convert-pattern #:splice? #t
                                        (syntax-parse alt-stx
                                          #:datum-literals (alts group quotes)
                                          [(block (group (quotes in-quotes))) #'in-quotes])))
                     #`(pattern #,p))))
          #'(define-syntax class-name (rhombus-syntax-class 'term #'class-name)))]
        [_
         (raise-syntax-error #f "expected alternatives" stx)]))))