#lang racket/base

(provide (all-defined-out))

;;; parsed syntax
(struct Pvar (val pos)                      #:transparent)
(struct Pnum (val pos)                      #:transparent)
(struct Pstr (val pos)                      #:transparent)
(struct Ptype (val pos)                     #:transparent)
(struct Pcall (func args pos)               #:transparent)
(struct Passign (var expr pos)              #:transparent)
(struct Pcond (test t f pos)                #:transparent)
(struct Pbool (val pos)                     #:transparent)
;;; abstact syntax
(struct Num (val)                           #:transparent)
(struct Str (val)                           #:transparent)
(struct Bool(val)                           #:transparent)
(struct Var (name)                          #:transparent)
(struct Call (func args)                    #:transparent)
(struct Assign (var expr)                   #:transparent)
(struct Cond (test yes no)                  #:transparent)
(struct Lst (t)                             #:transparent)

;;; inter lang
(struct Data (lbl)                          #:transparent)

;;; MIPS assembly
(struct Mips (data text)                    #:transparent)

;;; MIPS data
(struct Asciiz (name str))


;;; MIPS instructions
(struct Label (lbl)                        #:transparent)
(struct Move  (dst reg)                    #:transparent)
(struct La    (dst loc)                    #:transparent)
(struct Li    (dst imm)                    #:transparent)
(struct Addi  (dst reg imm)                #:transparent)
(struct Add   (dst rg1 rg2)                #:transparent)
(struct Sub   (dst rg1 rg2)  	             #:transparent)
(struct Mul   (dst rg1 rg2)                #:transparent)
(struct Div   (dst rg1 rg2)                #:transparent)
(struct Mod   (dst rg1 rg2)                #:transparent)
;; comparaisons de nombres
(struct Seq (dst rg1 imm)                  #:transparent)
(struct Sne (dst rg1 imm)                  #:transparent)
(struct Sle (dst rg1 imm)                  #:transparent)
(struct Sge (dst rg1 imm)                  #:transparent)
(struct Slt (dst rg1 imm)                  #:transparent)
(struct Sgt (dst rg1 imm)                  #:transparent)
;;; OPERATIONS LOGIQUES ;;;
(struct Or  (dst rg1 rg2)                  #:transparent)
(struct And (dst rg1 rg2)                  #:transparent)
(struct Not (dst rg1 rg2)                  #:transparent)
;; Bit-à-bit
(struct Band (dst rg1 rg2)                 #:transparent)
(struct Bor  (dst rg1 rg2)                 #:transparent)
(struct Xor  (dst rg1 rg2)                 #:transparent)
(struct Nor  (dst rg1 rg2)                 #:transparent)

(struct Beqz (rs l)                        #:transparent)
(struct Bnez (rs l)                        #:transparent)
(struct Sw (reg loc)                       #:transparent)
(struct Lw (reg loc)                       #:transparent)
(struct Syscall ()                         #:transparent)
(struct Jr (reg)                           #:transparent)
(struct Jal (loc)                          #:transparent)
(struct B (l)                              #:transparent)
;;; MIPS memory location
(struct Mem (reg offset)                   #:transparent)
(struct Lbl (name)                         #:transparent)

;;; MIPS syscall
(define PRINT_INT    1)
(define PRINT_STRING 4)
(define SBRK         9)
