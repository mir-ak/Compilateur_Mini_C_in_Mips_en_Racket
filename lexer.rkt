#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "helper.rkt")

(provide constants operators keywords get-token)

(define-tokens constants
  (Lident Lnum Lstr Lbool Ltype))

(define-empty-tokens operators
  (Lplus Lsub Lmul Ldiv
   Lmod Land Lor Lnot Lsll
   Leq Lneq Llt Lgt Llte Lgte
   Lsrl Lsup Lseq Lieq Lassign
   Lband Lxor Lbor Ltld Leof))

(define-empty-tokens keywords
  ( Lopar Lcpar Lacc Laco
    Lcomma Lsemicol Lelse Lif Lthen
    Lfor Lwhile Lret Lnil Llist))

(define-lex-abbrev identifier
    (:: alphabetic (:* (:or alphabetic numeric #\_))))

(define-lex-abbrev types
  (:or "int" "char" "bool"))

(define get-token
  (lexer-src-pos
   [(eof)           (token-Leof)]
   [whitespace      (return-without-pos (get-token input-port))]
   ["#"             (return-without-pos (comment-lex input-port))]
   ["//"            (return-without-pos (comment-lex input-port))]
   ("/*"            (return-without-pos (long-comment-lexer input-port)))
   ["("             (token-Lopar)]
   [")"             (token-Lcpar)]
   [","             (token-Lcomma)]
   [";"             (token-Lsemicol)]
   ["}"             (token-Lacc)]
   ["{"             (token-Laco)]
    ;; Opération arithmétiques
   ["+"             (token-Lplus)]
   ["-"             (token-Lsub)]
   ["*"             (token-Lmul)]
   ["/"             (token-Ldiv)]
   ["%"             (token-Lmod)]
    ;; comparaisons
   ("=="            (token-Leq))
   ("!="            (token-Lneq))
   ("<"             (token-Llt))
   (">"             (token-Lgt))
   ("<="            (token-Llte))
   (">="            (token-Lgte))
     ;; Decalage
   ["="             (token-Lassign)]
   ["<<"            (token-Lsll)]
   [">>"            (token-Lsrl)]
     ;; opérations Logiques
   ["&&"            (token-Land)]
   ["||"            (token-Lor)]
   ["!"             (token-Lnot)]
    ;;Bit-à-bit
   ["&"             (token-Lband)]
   ["|"             (token-Lbor)]
   ["^"             (token-Lxor)]
   ["~"             (token-Ltld)]
   ;; condition & boucle 
   ["if"            (token-Lif)]
   ["else"          (token-Lelse)]
   ["while"         (token-Lwhile)]
   ["for"           (token-Lfor)]
   ;; bool
   ["true"          (token-Lbool 1)]
   ["false"         (token-Lbool 0)]
   ["list"          (token-Llist)]
   [identifier      (token-Lident(string->symbol lexeme))]
   [types           (token-Ltype (string->symbol lexeme))]
   ["\""            (token-Lstr  (string-lex input-port))]
   [(:+ numeric)    (token-Lnum (string->number lexeme))]
   [any-char (err (format "unrecognized character '~a'" lexeme)
                  start-pos)]))

(define string-lex
  (lexer
   [(eof) (error 'lexer "eof while reading string")]
   ["\\\""      (string-append "\"" (string-lex input-port))]
   ["\\n"       (string-append "\n" (string-lex input-port))]
   ["\\t"       (string-append "\t" (string-lex input-port))]
   ["\""  ""]
   [any-char    (string-append lexeme (string-lex input-port))]))

(define comment-lex
  (lexer
   [(eof)(get-token input-port)]
   ["\n" (get-token input-port)]
   [any-char (comment-lex input-port)]))

(define long-comment-lexer
  (lexer
   ("*/"     (get-token input-port))
   (any-char (long-comment-lexer input-port))))
