#lang racket
(provide (all-defined-out))
(require "types.rkt" a86/ast)

; gc
(define rsi 'rsi)  ; from space ptr
(define rdx 'rdx)  ; to space ptr
(define rcx 'rcx)  ; n/2 (active heap size in words)
(define r11 'r11)  ; scratch

; both
(define rbx 'rbx) ; heap
(define r8  'r8)  ; scratch
(define rax 'rax) ; return
(define rsp 'rsp) ; stack

; compile-ops
(define eax 'eax) ; 32-bit load/store
(define rdi 'rdi) ; arg
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch
(define r15 'r15) ; stack pad (non-volatile)


(define (type-pred r mask type)
  (let ((l (gensym)))
    (seq (And r mask)
         (Cmp r type)
         (Mov r (value->bits #t))
         (Je l)
         (Mov r (value->bits #f))
         (Label l))))
