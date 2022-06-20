#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/set)
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

(define-for-syntax (generate-pattern-and-attributes stx)
  (syntax-parse stx
    #:datum-literals (alts group quotes)
    [(block (group (quotes in-quotes)))
     (define-values (p idrs sidrs can-be-empty?)
       (convert-pattern #:splice? #t
                        #'in-quotes))
     (with-syntax ([((attr ...) ...)
                    (map (lambda (binding) (cons '#:attr binding)) idrs)])
       (values #`(pattern #,p attr ... ...)
               (map (lambda (binding) (syntax-e (car (syntax->list binding)))) idrs)))]))

(define-syntax class
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (alts group quotes block pattern description)
        ;; Classname and patterns shorthand
        [(form-id class-name (alts alt ...)
                  . tail)
         (let-values ([(patterns attributes)
                       (for/lists (patterns attributes
                                            #:result (values patterns (set->list (list->set (flatten attributes)))))
                                  ([alt-stx (in-list (syntax->list #'(alt ...)))])
                         (generate-pattern-and-attributes alt-stx))])
           (list
            #`(begin-for-syntax
                (define-splicing-syntax-class class-name
                  #:datum-literals (block group quotes)
                  #,@patterns))
            #`(define-syntax class-name (rhombus-syntax-class 'term #'class-name '#,attributes))))]
        ;; Specify patterns with "pattern" keyword (incomplete)
        [(form-id class-name
                  (block (group pattern (alts alt ...)))
                  . tail)
         (list
          #`(begin-for-syntax
              (define-splicing-syntax-class class-name
                #:datum-literals (block group quotes)
                #,@(for/list ([alt-stx (in-list (syntax->list #'(alt ...)))])
                     (generate-pattern-and-attributes alt-stx))))
          #'(define-syntax class-name (rhombus-syntax-class 'term #'class-name)))]
        [_
         (raise-syntax-error #f "expected alternatives" stx)]))))