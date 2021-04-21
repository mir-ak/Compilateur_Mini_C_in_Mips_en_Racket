#lang racket/base

(require racket/match
         "ast.rkt")

(provide mips-print)

(define (print-datum datum)
  (match datum
   [(Asciiz l s)
    (printf "~a: .asciiz ~s\n" l s)]))

(define (print-data data)
  (for-each print-datum data))

(define (fmt-loc loc)
  (match loc
    [(Mem r o) (format "~a($~a)" o r)]
    [(Lbl l)   (format "~a" l)]))

(define (print-instr instr)
  (match instr
    [(Label l)    (printf "~a:\n" l)]
    [(Move d r)   (printf "\tmove $~a, $~a\n" d r)]
    [(Li d i)     (printf "\tli $~a, ~a\n" d i)]
    [(La d a)     (printf "\tla $~a, ~a\n" d (fmt-loc a))]
    [(Sw r l)     (printf "\tsw $~a, ~a\n" r (fmt-loc l))]
    [(Lw r l)     (printf "\tlw $~a, ~a\n" r (fmt-loc l))]
     ;; Arithmétiques 
    [(Addi d r i) (printf "\taddi $~a, $~a, ~a\n" d r i)]
    [(Add d r1 r2)(printf "\tadd $~a, $~a, $~a\n" d r1 r2)]
    [(Sub d r s)  (printf "\tsub $~a, $~a, $~a\n" d r s)]
    [(Mul d r s)  (printf "\tmul $~a, $~a, $~a\n" d r s)]
    [(Div d r s)  (printf "\tdiv $~a, $~a, $~a\n" d r s)]
    [(Mod d r s)  (printf "\trem $~a, $~a, $~a\n" d r s)]
     ;; comparaisons de nombres 
    [(Seq d r1 i)(printf "\tseq $~a, $~a, $~a\n" d r1 i)]
    [(Sne d r1 i)(printf "\tsne $~a, $~a, $~a\n"  d r1 i)]
    [(Sle d r1 i)(printf "\tsle $~a, $~a, $~a\n"  d r1 i)]
    [(Sge d r1 i)(printf "\tsge $~a, $~a, $~a\n"  d r1 i)]
    [(Slt d r1 i)(printf "\tslt $~a, $~a, $~a\n"  d r1 i)]
    [(Sgt d r1 i)(printf "\tsgt $~a, $~a, $~a\n"  d r1 i)]
      ;; opérations logiques
    [(And d r s)  (printf "\tand $~a, $~a, $~a\n" d r s)]
    [(Or d r s)  (printf "\tor $~a, $~a, $~a\n" d r s)]
    [(Not d r s)  (printf "\txor $~a, $~a, $~a\n" d r s)]
      ;; Bit-à-bit
    [(Band d r s) (printf "\tand $~a, $~a, $~a\n" d r s)]
    [(Bor d r s)  (printf "\tor $~a, $~a, $~a\n" d r s)]
    [(Xor d r s)  (printf "\txor $~a, $~a, $~a\n" d r s)]
    [(Nor d r s)  (printf "\tnor $~a, $~a, $~a\n" d r s)]
      ;; Condition
    [(Bnez rs l)      (printf "bnez $v0, $0, else\n" rs l)]
    [(Beqz rs l)      (printf "beqz $~a, ~a\n" rs l)]
    [(Jr r)       (printf "\tjr $~a\n" r)]
    [(Jal l)       (printf "\tjal ~a\n" (fmt-loc l))]
    [(Syscall)    (printf "\tsyscall\n")]))

(define (print-text text)
  (for-each print-instr text))

(define (mips-print mips)
  (printf ".data\n")
  (print-data (Mips-data mips))
  (printf "\n.text\n")
  (print-text (Mips-text mips)))
