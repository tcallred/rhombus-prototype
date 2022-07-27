#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/set)
         syntax/parse
         (submod "quasiquote.rkt" convert)
         (submod "syntax-class.rkt" for-syntax-class-syntax)
         "definition.rkt"
         "name-root.rkt"
         "parse.rkt"
         "parsed.rkt"
         (only-in "value.rkt" val))

(provide (rename-out [rhombus-syntax syntax]))

(define-simple-name-root rhombus-syntax
  class)

(define-for-syntax (generate-pattern-and-attributes stx)
  (syntax-parse stx
    #:datum-literals (alts group quotes block)
    [(block (group (quotes in-quotes)))
     (define-values (p idrs sidrs can-be-empty?)
       (convert-pattern #:splice? #t
                        #'in-quotes))
     (with-syntax ([((attr ...) ...)
                    (map (lambda (binding) (cons '#:attr binding)) idrs)])
       (values #`(pattern #,p attr ... ...)
               (map (lambda (binding) (syntax-e (car (syntax->list binding)))) idrs)))]
    [(block (group (quotes in-quotes)
                   (block groups ...)))
     (define-values (p idrs sidrs can-be-empty?)
       (convert-pattern #:splice? #t
                        #'in-quotes))
     (define-values (pattern-body explicit-attrs)
       (for/fold ([body-forms null]
                  [attrs null]
                  #:result
                  (values
                   (reverse (cons #`(group (parsed (values #,@(reverse attrs)))) body-forms))
                   (reverse attrs)))
                 ([g (in-list (syntax->list #'(groups ...)))])
         (syntax-parse g
           #:datum-literals (group block)
           [(group #:attr attr-id (block in-block ...))
            (values
             (cons #`(group val attr-id (block in-block ...)) body-forms)
             (cons #'attr-id attrs))]
           [other
            (values (cons #'other body-forms) attrs)])))
     (define all-attrs (append idrs (map (lambda (attr) #`[#,attr #,attr]) explicit-attrs)))
     (with-syntax ([((attr ...) ...)
                    (map (lambda (binding) (cons '#:attr binding)) all-attrs)]
                   [(body-form ...) pattern-body]
                   [((id id-ref) ...) idrs])
       (values #`(pattern #,p #:do [(define-values #,explicit-attrs (let ([id id-ref] ...) (rhombus-body body-form ...)))] attr ... ...)
               (map (lambda (binding) (syntax-e (car (syntax->list binding)))) all-attrs)))]))

(define-for-syntax (generate-syntax-class class-name alts description)
  (let-values ([(patterns attributes)
                (for/lists (patterns attributes
                                     #:result (values patterns (apply set-intersect attributes)))
                           ([alt-stx (in-list alts)])
                  (generate-pattern-and-attributes alt-stx))])
    (list
     #`(define-splicing-syntax-class #,class-name
         #:description #,(if description #`(rhombus-body #,description) #f)
         #:datum-literals (block group quotes)
         #,@patterns)
     #`(define-syntax #,(in-syntax-class-space class-name)
         (rhombus-syntax-class 'term #'#,class-name '#,attributes #f)))))

(define-syntax class
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (alts group quotes block pattern description)
        ;; Classname and patterns shorthand
        [(form-id class-name (alts alt ...))
         (generate-syntax-class #'class-name (syntax->list #'(alt ...)) #f)]
        ;; Specify patterns with "pattern"
        [(form-id class-name
                  (block
                   (~optional (group description (block class-desc)))
                   (group pattern (alts alt ...))))
         (generate-syntax-class #'class-name (syntax->list #'(alt ...)) (attribute class-desc))]
        [_
         (raise-syntax-error #f "expected alternatives" stx)]))))
