#lang racket/base

(require racket/match
         racket/string
         (only-in parser-tools/lex position-line position-col)
         "ast.rkt")

(provide err errt)

(define (err msg pos)
  (eprintf "Error on line ~a col ~a: ~a.\n"
           (position-line pos)
           (position-col pos)
           msg)
  (exit 1))

(define (errt expected given pos)
  (err (format "type mismatch (expected ~a, given ~a)"
                expected given) pos))
