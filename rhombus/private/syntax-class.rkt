#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "introducer.rkt")
         syntax/parse
         "operator-parse.rkt"
         "name-root.rkt"
         "definition.rkt")

(provide Term
         Id
         Op
         Id_Op
         Keyw
         Group
         Block
         Multi)

(module+ for-quasiquote
  (provide (for-syntax in-syntax-class-space
                       rhombus-syntax-class?
                       rhombus-syntax-class-kind
                       rhombus-syntax-class-class
                       rhombus-syntax-class-attributes)))
                       
(module+ for-syntax-class-syntax
  (provide (for-syntax rhombus-syntax-class)))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer/add 'rhombus/syntax-class))

  (struct rhombus-syntax-class (kind class attributes)))  

(define-syntax Term (rhombus-syntax-class 'term #f null))
(define-syntax Id (rhombus-syntax-class 'term #'identifier null))
(define-syntax Op (rhombus-syntax-class 'term #':operator null))
(define-syntax Id_Op (rhombus-syntax-class 'term #':operator-or-identifier null))
(define-syntax Keyw (rhombus-syntax-class 'term #'keyword null))
(define-syntax Group (rhombus-syntax-class 'group #f null))
(define-syntax Multi (rhombus-syntax-class 'multi #f null))
(define-syntax Block (rhombus-syntax-class 'block #f null))
