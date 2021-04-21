#lang racket/base

(require  racket/match
          "ast.rkt")
(provide (all-defined-out))
(struct Fun (ret args))
(struct Ptr_t (t))
(struct Struct_t (name fields))

(define pair (Struct_t 'pair (list (cons 'head 'num)
                                   (cons 'tail (Ptr_t 'pair)))))
(define (sizeof t)
  (match t
    ['num      4]
    [(Ptr_t _) 4]
    ['char     1]
    [(Struct_t _ fields)
     (foldl (lambda (f acc) (+ acc (sizeof (cdr f))))
            0 fields)]))

(define (member-offset s m)
  (define (mo-aux fields offset)
    (match fields
    [(? null? _)
      (error "FAIL")]
    [(? (lambda (f) (eq? (caar f) m)) _)
      offset]
    [else
      (mo-aux (cdr fields) (+ offset (sizeof (cdar fields))))]))
  (mo-aux (Struct_t-fields s) 0))

(define *baselib-types*
  (make-immutable-hash
      ;; Arithmétiques
    (list (cons '%add  (Fun 'num (list 'num 'num)))
	    (cons '%sub  (Fun 'num (list 'num 'num)))
   	  (cons '%mul  (Fun 'num (list 'num 'num)))
   	  (cons '%div  (Fun 'num (list 'num 'num)))
      (cons '%mod  (Fun 'num (list 'num 'num)))
      ;; comparaisons de nombres
      (cons '%eq  (Fun 'num (list 'num 'num)))
      (cons '%neq (Fun 'num (list 'num 'num)))
      (cons '%lt  (Fun 'num (list 'num 'num)))
      (cons '%gt  (Fun 'num (list 'num 'num)))
      (cons '%lte (Fun 'num (list 'num 'num)))
      (cons '%gte (Fun 'num (list 'num 'num)))
      ;; Bit-à-bit
      (cons '%band  (Fun 'num (list 'num 'num)))
      (cons  '%bor  (Fun 'num (list 'num 'num)))
      (cons '%xor  (Fun 'num (list 'num 'num)))
      (cons '%nor  (Fun 'num (list 'num 'num)))
      ;; opérations logiques
      (cons '%and  (Fun 'bool (list 'bool 'bool)))
      (cons  '%or  (Fun 'bool (list 'bool 'bool)))
      (cons '%not  (Fun 'bool (list 'bool)))
      ;; condition

      ;; fonction
      (cons 'print_num (Fun 'void (list 'num)))
      (cons 'print_str (Fun 'void (list 'str)))
      (cons 'print_bool(Fun 'void (list 'bool)))
      (cons 'print_nl  (Fun 'void  (list)))
      (cons 'nil       (Fun (Ptr_t '%) (list)))
      (cons 'pair      (Fun (Ptr_t 'pair) (list 'num (Ptr_t 'pair))))
      (cons 'head      (Fun 'num (list (Ptr_t 'pair))))
      (cons 'tail      (Fun (Ptr_t 'pair) (list (Ptr_t 'pair)))))))

(define *baselib-builtins*
  (make-immutable-hash
           ;; Arithmétiques
    (list (cons '%add (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Add 'v0 't0 't1)))
          (cons '%sub (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Sub 'v0 't0 't1)))
          (cons '%mul (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Mul 'v0 't0 't1)))
          (cons '%div (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Div 'v0 't0 't1)))
          (cons '%mod (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Mod 'v0 't0 't1)))

          ;; comparaisons de nombres
          (cons '%eq (list(Lw 't0 (Mem 'sp 4))
                          (Lw 't1 (Mem 'sp 0))
                          (Seq 'v0 't0 't1)))
          (cons '%neq (list(Lw 't0 (Mem 'sp 4))
                          (Lw 't1 (Mem 'sp 0))
                          (Sne 'v0 't0 't1)))
          (cons '%lt (list(Lw 't0 (Mem 'sp 4))
                          (Lw 't1 (Mem 'sp 0))
                          (Slt 'v0 't0 't1)))
          (cons '%gt (list(Lw 't0 (Mem 'sp 4))
                          (Lw 't1 (Mem 'sp 0))
                          (Sgt 'v0 't0 't1)))
          (cons '%lte (list(Lw 't0 (Mem 'sp 4))
                          (Lw 't1 (Mem 'sp 0))
                          (Sle 'v0 't0 't1)))
          (cons '%gte (list(Lw 't0 (Mem 'sp 4))
                          (Lw 't1 (Mem 'sp 0))
                          (Sge 'v0 't0 't1)))

          ;;; OPERATIONS LOGIQUES ;;;
          (cons '%and (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (And 'v0 't0 't1)))

          (cons '%or (list (Lw 't0 (Mem 'sp 4))
                           (Lw 't1 (Mem 'sp 0))
                           (Or 'v0 't0 't1)))

          (cons '%not (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Not 'v0 't0 't1)))
          ;; Bit-à-bit

          (cons '%band (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Band 'v0 't0 't1)))

          (cons '%bor (list (Lw 't0 (Mem 'sp 4))
                           (Lw 't1 (Mem 'sp 0))
                           (Bor 'v0 't0 't1)))

          (cons '%xor (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Xor 'v0 't0 't1)))

          (cons '%nor (list (Lw 't0 (Mem 'sp 4))
                            (Lw 't1 (Mem 'sp 0))
                            (Nor 'v0 't0 't1)))
          ;; Condition

          ;; fonctions
          (cons 'print_num (list (Lw 'a0 (Mem 'sp 0))
                                  (Li 'v0 PRINT_INT)
                                  (Syscall)))
          (cons 'print_str  (list (Lw 'a0 (Mem 'sp 0))
                                  (Li 'v0 PRINT_STRING)
                                  (Syscall)))
          (cons 'print_nl   (list (La 'a0 (Lbl 'nl))
                                  (Li 'v0 PRINT_STRING)
                                  (Syscall)))

          (cons 'print_bool(list (Lw 'a0 (Mem 'sp 0))
                               (Li 'v0 PRINT_INT)
                               (Syscall)))
          (cons 'nil  (list (Li 'v0 0)))
          (cons 'pair (list (Jal (Lbl 'pair))))
          (cons 'head (list (Jal (Lbl 'head))))
          (cons 'tail (list (Jal (Lbl 'tail))))
          )))
(define *fake-user-code*
  (list (Label 'pair)
        (Addi 'sp 'sp -12)
        (Sw 'ra (Mem 'sp 0))
        (Sw 'fp (Mem 'sp 4))
        (Addi 'fp 'sp 12)
        (Li 'a0 8)
        (Li 'v0 SBRK)
        (Syscall)
        (Sw 'v0 (Mem 'sp 8))
        (Lw 'v0 (Mem 'fp 4))
        (Move 't0 'v0)
        (Lw 'v0 (Mem 'sp 8))
        (Sw 't0 (Mem 'v0 0))
        (Lw 'v0 (Mem 'fp 0))
        (Move 't0 'v0)
        (Lw 'v0 (Mem 'sp 8))
        (Sw 't0 (Mem 'v0 4))
        (Lw 'v0 (Mem 'sp 8))
        (Lw 'ra (Mem 'sp 0))
        (Lw 'fp (Mem 'sp 4))
        (Addi 'sp 'sp 12)
        (Jr 'ra)
        ;;; haid
        (Label 'head)
        (Addi 'sp 'sp -8)
        (Sw 'ra (Mem 'sp 0))
        (Sw 'fp (Mem 'sp 4))
        (Addi 'fp 'sp 8)
        (Lw 'v0 (Mem 'fp 0))
        (Lw 'v0 (Mem 'v0 0))
        (Lw 'ra (Mem 'sp 0))
        (Lw 'fp (Mem 'sp 4))
        (Addi 'sp 'sp 8)
        (Jr 'ra)
        ;;; tail
        (Label 'tail)
        (Addi 'sp 'sp -8)
        (Sw 'ra (Mem 'sp 0))
        (Sw 'fp (Mem 'sp 4))
        (Addi 'fp 'sp 8)
        (Lw 'v0 (Mem 'fp 0))
        (Lw 'v0 (Mem 'v0 4))
        (Lw 'ra (Mem 'sp 0))
        (Lw 'fp (Mem 'sp 4))
        (Addi 'sp 'sp 8)
        (Jr 'ra)))





;; Ici %add est une fonction simple qui est systématiquement inlinée.
;;
;; En pratique pour la plupart des petites fonctions de la stdlib qui
;; correspondent à une instruction MIPS on pourra faire comme ça.
;;
;; Mais pour les fonctions plus complexes, par exemple celles définies
;; par l'utilisateurice, leur code sera à un label à l'endroit de leur
;; définition, et l'appelle de la fonction consistera en un
;; jump-and-link vers ce label.
