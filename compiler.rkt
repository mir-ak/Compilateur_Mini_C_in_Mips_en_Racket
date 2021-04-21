#lang racket/base

(require racket/match
         "ast.rkt"
         "baselib.rkt"
         "helper.rkt")

(provide compile)

(define (compile-data data)
  (append (list (Asciiz 'nl "\n"))
          (map (lambda (d) (Asciiz (car d) (cdr d))) data)))

(define (compile-expr-and-push expr env)
  (append (compile-expr expr env)
          (list (Addi 'sp 'sp -4)
                (Sw 'v0 (Mem 'sp 0)))))

(define (compile-expr expr env)
  (match expr
    [(Num v)
     (list (Li 'v0 v))]
     [(list (Var v))
        (list (Li 'v0 v))]
    [(Data l)
     (list (La 'v0 (Lbl l)))]
    [(Bool p)
     (list (Li 'v0 p))]
    [(Var n)
     (list (Lw 'v0 (hash-ref env n)))]
    [(Call f as)
     (append
     (apply append(map (lambda (e) (compile-expr-and-push e env))as))
     (hash-ref env f)
     (list (Addi 'sp 'sp (* 4 (length as)))))]

   [(Cond test yes no)
       ;;; On compile le test, le bloc then et le bloc else
       (define ctest (compile-expr-and-push test env ))
       (define cyes (compile-expr-and-push yes env ))
       (define cno (compile-expr-and-push no env ))
       (list
        (append
         ;; On ajoute les instructions MIPS du test
         (car ctest)
         (list (Addi 'sp 'sp -4)
               (Sw 'v0 (Mem 0 'sp)))
         (list (Bnez 't9 'then_ (number->string))
               (Beqz 't9 'else_ (number->string )))
         (list (Label 'then_ (number->string )))
         ;; On ajoute les instructions MIPS du bloc then
         (car cyes)
         (list (B 'endif_ (number->string)))
         (list (Label 'else_ (number->string )))
         ;; On ajoute les instructions MIPS du bloc else
         (car cno)
         ;; On continue sur le label endif la suite du programme
         (list (Label 'endif (number->string))))
        env)]
))

(struct Cinstr (code env fp-sp))

(define (compile-instr instr env fp-sp)
  (match instr
    [(Assign v e)
     (Cinstr (compile-expr-and-push e env)
             (hash-set env v (Mem 'fp (- (+ 4 fp-sp))))
             (+ 4 fp-sp))]
    [(Call _ _)
     (Cinstr (compile-expr instr env)
             env
             fp-sp)]))

(define (compile-prog prog env fp-sp)
  (match prog
    [(list)
     (cons '()
           0)]
    [(cons i p)
     (let* ([ci (compile-instr i env fp-sp)]
            [cp (compile-prog p (Cinstr-env ci) (Cinstr-fp-sp ci))])
       (cons (append (Cinstr-code ci) (car cp))
             (+ (if (= fp-sp (Cinstr-fp-sp ci)) 0 4)
                (cdr cp))))]
    ))

(define (compile ast)
  (Mips
   (compile-data (cdr ast))
   (append *fake-user-code*
           (list (Label 'main)
                 (Addi 'sp 'sp -8)
                 (Sw 'ra (Mem 'sp 0))
                 (Sw 'fp (Mem 'sp 4))
                 (Addi 'fp 'sp 8))
           (let ([cp (compile-prog (car ast) *baselib-builtins* 8)])
             (append (car cp)
                     (list (Addi 'sp 'sp (cdr cp)))))
           (list (Lw 'ra (Mem 'sp 0))
                 (Lw 'fp (Mem 'sp 4))
                 (Addi 'sp 'sp 8)
                 (Jr 'ra)))))
