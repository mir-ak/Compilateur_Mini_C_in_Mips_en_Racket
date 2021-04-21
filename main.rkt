#lang racket/base

(require "parser.rkt"
         "analyzer.rkt"
         "simplifier.rkt"
         "compiler.rkt"
         "mips-printer.rkt"
         )

(define argv (current-command-line-arguments))
(cond
  [(= (vector-length argv) 1)
   (define src (open-input-file (vector-ref argv 0)))
   (define prs (parse src))
   (close-input-port src)
   (define ast (analyze prs))
   (define smp (simplify ast))
   (define asm (compile smp))
   (with-output-to-file #:exists 'replace
    (string-append (vector-ref argv 0) ".s")
     (lambda ()
      (mips-print asm)))]
  [else
   (eprintf "Usage: racket liec.rkt <file>.\n")
   (exit 1)])
