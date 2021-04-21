#lang racket/base

(require parser-tools/yacc
         parser-tools/lex
         "lexer.rkt"
         "ast.rkt"
         "helper.rkt")

(provide parse)

(define parse-syntax
  (parser
   (src-pos)
   (tokens constants keywords operators)
   (start prog)
   (end Leof)
   (grammar
    (prog
     [(instr Lsemicol)       (list $1)]
     [(instr Lsemicol prog)  (cons $1 $3)])
     (instr
     [(Lident Lident Lassign expr) (Passign $2 $4 $2-start-pos)]
     [(Lident Lopar args Lcpar) (Pcall $1 $3 $1-start-pos)])
    (expr
     [(Lnum)            (Pnum $1 $1-start-pos)]
     [(Lstr)            (Pstr $1 $1-start-pos)]
     [(Lident)          (Pvar $1 $1-start-pos)]
     [(Lbool)           (Pbool $1 $1-start-pos)]
     [(Ltype)           (Pvar $1 $1-start-pos)]

     ;; Arithmétiques
     [(expr Lplus expr) (Pcall '%add (list $1 $3) $2-start-pos)]
     [(expr Lsub expr)  (Pcall '%sub (list $1 $3) $2-start-pos)]
     [(expr Lmul expr)  (Pcall '%mul (list $1 $3) $2-start-pos)]
     [(expr Ldiv expr)  (Pcall '%div (list $1 $3) $2-start-pos)]
     [(expr Lmod expr)  (Pcall '%mod (list $1 $3) $2-start-pos)]
     ;; Comparaisons
     [(expr Leq expr)   (Pcall '%eq  (list $1 $3) $2-start-pos)]
     [(expr Lneq expr)  (Pcall '%neq (list $1 $3) $2-start-pos)]
     [(expr Llt expr)   (Pcall '%lt  (list $1 $3) $2-start-pos)]
     [(expr Lgt expr)   (Pcall '%gt  (list $1 $3) $2-start-pos)]
     [(expr Llte expr)  (Pcall '%lte (list $1 $3) $2-start-pos)]
     [(expr Lgte expr)  (Pcall '%gte (list $1 $3) $2-start-pos)]
     ;; Bit-à-bit
     [(expr Lband expr) (Pcall '%band (list $1 $3) $2-start-pos)]
     [(expr Lbor expr)  (Pcall '%bor (list $1 $3) $2-start-pos)]
     [(expr Lxor expr)  (Pcall '%xor (list $1 $3) $2-start-pos)]
     [(expr Ltld expr)  (Pcall '%nor (list $1 $3) $2-start-pos)]
     ;; Logiques
     [(expr Land expr)  (Pcall '%and (list $1 $3) $2-start-pos)]
     [(expr Lor expr)   (Pcall '%or  (list $1 $3) $2-start-pos)]
     [(Lnot expr)       (Pcall '%not (list $2) $1-start-pos)]
     [(Lident Lopar args Lcpar) (Pcall $1 $3 $1-start-pos)]
      ;; Condition
     [(Lif Lopar expr Lcpar expr Lelse expr ) (Pcond $3 $5 $7 $1-start-pos)])
     ;;[(Lif expr Lthen expr Lelse expr) (Pcond $2 $4 $6 $1-start-pos)])

   (args
    [() (list)]
    [(expr) (list $1)]
    ((expr Lcomma args) (cons $1 $3))))
    (precs
     (left Land Lor)
     (right Lnot)
     (left Leq Lneq Llt Lgt Llte Lgte)
     (left Lplus Lsub)
     (left Lmul Ldiv Lmod)
     (left Lband Lbor Lxor Ltld)
     (right Lelse))
   (error
    (lambda (tok-ok? tok-name tok-value spos epos)
      (err (format "syntax error near ~a ~a"
                   tok-name
                   (if tok-value (format " (~a)" tok-value) ""))
                   spos)))))

(define (parse src)
  (port-count-lines! src)
  (parse-syntax (lambda () (get-token src))))