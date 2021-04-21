#lang racket/base

(require racket/match
         "ast.rkt"
         "baselib.rkt"
         "helper.rkt")

(provide analyze)

(define (type-compat? given expected)
  (match (cons given expected)
    [(cons t t) #t]
    [(cons '% t) #t]
    [(cons t '%) #t]
    [(cons (Ptr_t t1) (Ptr_t t2)) (type-compat? t1 t2)]
    [else #f]))

(define (expr-pos e)
  (match e
    [(Pnum _ p)  p]
    [(Pstr _ p)  p]
    [(Pbool _ p) p]
    [(Pvar _ p) p]
    [(Pcall _ _ p) p]
    [(Pcond _ _ _ pos) pos]))

(define (analyze-function f as pos env proc?)
  (unless (hash-has-key? env f)
    (err (format "unknown function '~a'" f) pos))
  (let ([ft (hash-ref env f)])
    (if proc?
        (unless (eq? 'void (Fun-ret ft))
          (err (format "functions must return void if used as instruction: '~a'" f) pos))
        (when (eq? 'void (Fun-ret ft))
          (err (format "void is not a valid expression type: '~a'" f) pos)))
    (unless (= (length (Fun-args ft)) (length as))
      (err (format "arity mismatch (expected ~a, given ~a)"
                   (length (Fun-args ft))
                   (length as))
           pos))
    (let ([aas (map (lambda (at a)
                      (let ([aa (analyze-expr a env)])
                        (if (type-compat? (cdr aa) at)
                            (car aa)
                            (errt at (cdr aa) (expr-pos a)))))
                    (Fun-args ft)
                    as)])
      (cons (Call f aas)
            (Fun-ret ft)))))

(define (analyze-expr expr env)
  (match expr
    [(Pnum v pos)
     (cons (Num v)
           'num)]
    [(Pstr v pos)
     (cons (Str v)
           'str)]
     [(Pbool b pos)
     (cons (Bool b)
           'bool)]
    [(Pvar v pos)
     (unless (hash-has-key? env v)
       (err (format "unbound variable ~a" v) pos))
     (cons (Var v)
           (hash-ref env v))]
    [(Pcall f as pos)
     (unless (hash-has-key? env f)
      (err (format "unknown function '~a'" f) pos))
     (let ([ft (hash-ref env f)])
       (unless (= (length (Fun-args ft)) (length as))
        (err (format "arity mismatch (expected ~a, given ~a)"
                     (length (Fun-args ft))
                     (length as))
              pos))
        (let ([aas (map (lambda (at a)
                          (let ([aa (analyze-expr a env)])
                            (if (type-compat? (cdr aa) at)
                                (car aa)
                                (errt at (cdr aa) (expr-pos a)))))
                        (Fun-args ft)
                        as)])
          (cons (Call f aas)
                (Fun-ret ft))))]
     [(Pcond t y n pos)
     (let ([at (analyze-expr t env)]
           [ay (analyze-expr y env)]
           [an (analyze-expr n env)])
       (unless (eq? (cdr at) 'bool)
         (errt 'bool (cdr at)(expr-pos t)))
       (unless (eq? (cdr ay) (cdr an))
         (errt (cdr ay) (cdr an)(expr-pos n)))
       (cons (Cond (car at) (car ay) (car an))
             (cdr ay)))]
    ))


(define (analyze-instr instr env)
  (match instr
    [(Passign v e pos)
     (let ([ae (analyze-expr e env)])
       (cons (Assign v (car ae))
             (hash-set env v (cdr ae))))]
    [(Pcall f as pos)
     (cons (car (analyze-function f as pos env #t))
           env)]))

(define (analyze-prog prog env)
  (match prog
    [(list i)
     (list (car (analyze-instr i env)))]
    [(cons i p)
     (let ([ai (analyze-instr i env)])
       (cons (car ai)
             (analyze-prog p (cdr ai))))]))


(define (analyze ast)
  (analyze-prog ast *baselib-types*))
