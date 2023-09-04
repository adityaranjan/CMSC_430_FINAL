#lang racket
(provide (all-defined-out))
(require a86/ast "types.rkt" "utils.rkt")


(define (compile-box-cons-gc-check size)
    (let ((continuelabel (gensym 'continue)) (endchecklabel (gensym 'endcheck)) (aftergclabel (gensym 'aftergc)))
        (seq
            (Sub rbx rsi)  ; alloc - from
            (Sar rbx 3)    ; alloc / 8
            (Add rbx size) ; alloc + (size of allocation)
            (Cmp rbx rcx)  ; compare alloc with n/2

            ; if there is enough space, restore rbx and continue
            (Jle continuelabel)

            ; push rax as it might be a pointer whose value has to be copied over
            ; also allows for rax to be used during gc
            (Push rax)

            ; if not enough space the 1st time, do gc
            (compile-gc)

            ; check if there is enough space after gc is done
            (Jmp aftergclabel)

            ; restore value of rbx
            (Label continuelabel)
            (Sub rbx size)
            (Sal rbx 3)
            (Add rbx rsi)

            ; jump to the end
            (Jmp endchecklabel)

            ; check if there is enough space after gc
            (Label aftergclabel)

            ; restore rax
            (Pop rax)

            (Sub rbx rsi)  ; alloc - from
            (Sar rbx 3)    ; alloc / 8
            (Add rbx size) ; alloc + (size of allocation)
            (Cmp rbx rcx)  ; compare alloc with n/2

            ; if there is enough space after gc, continue
            ; if there isn't, raise a runtime error
            (Jle continuelabel)
            (Jmp 'raise_error_align)
            
            (Label endchecklabel))))

 (define (compile-gc)
    (seq
        ; swap from-space (rsi) and to-space (rdx)
        (Push rsi)
        (Mov rsi rdx)
        (Pop rdx)

        ; alloc ptr (rbx) = from-space (rsi)
        (Mov rbx rsi)

        ; loop to go through stack and copy over roots
        (compile-copy-roots)
        
        ; loop to scan the heap for nested objects
        (compile-scan-heap)))

(define (compile-copy-roots)
    (let ((copyrootsbegin (gensym 'copyrootsbegin)) (copyrootsend (gensym 'copyrootsend)) (copyrootscons (gensym 'copyrootscons)))
        (seq
            ; rax will be used to traverse through the stack "roots"
            (Mov rax rsp)       

            ; just for initialization purposes     
            (Sub rax 8)             

            (Label copyrootsbegin)
            
            ; increment stack pointer every iteration
            (Add rax 8)

            ; move value in the stack's current position into scratch register
            ; if it matches the sentinel value, end the loop
            (Mov r11 (Offset rax 0))
            (Cmp r11 stack-begin)
            (Je copyrootsend)

            ; check if the root value is a box pointer
            (type-pred r11 ptr-mask type-box)
            (Cmp r11 val-false)

            ; if not a box pointer, check if it's cons
            (Je copyrootscons)

            ; if it is, copy it over and jump back to beginning of loop
            (compile-copy-box)
            (Jmp copyrootsbegin)

            (Label copyrootscons)

            ; check if the root value is a cons pointer
            (Mov r11 (Offset rax 0))
            (type-pred r11 ptr-mask type-cons)
            (Cmp r11 val-false)

            ; if not a cons pointer, jump to beginning of loop
            (Je copyrootsbegin)

            ; if it is, copy it over and jump back to beginning of loop
            (compile-copy-cons)
            (Jmp copyrootsbegin)
            
            (Label copyrootsend))))

(define (compile-scan-heap)
    (let ((scanheapbegin (gensym 'scanheapbegin)) (scanheapend (gensym 'scanheapend)) (scanheapcons (gensym 'scanheapcons)))
        (seq
            ; scan ptr (rax) = from-space (rsi)
            (Mov rax rsi)

            ; just for initialization purposes     
            (Sub rax 8)             

            (Label scanheapbegin)
            
            ; increment scan pointer every iteration
            (Add rax 8)

            ; if scan ptr matches rbx, end the loop
            (Cmp rax rbx)
            (Je scanheapend)

            ; check if the value is a box pointer
            (Mov r11 (Offset rax 0))
            (type-pred r11 ptr-mask type-box)
            (Cmp r11 val-false)

            ; if not a box pointer, check if it's cons
            (Je scanheapcons)

            ; if it is, copy it over and jump back to beginning of loop
            (compile-copy-box)
            (Jmp scanheapbegin)

            (Label scanheapcons)

            ; check if the value is a cons pointer
            (Mov r11 (Offset rax 0))
            (type-pred r11 ptr-mask type-cons)
            (Cmp r11 val-false)

            ; if not a cons pointer, jump to beginning of loop
            (Je scanheapbegin)

            ; if it is, copy it over and jump back to beginning of loop
            (compile-copy-cons)
            (Jmp scanheapbegin)
            
            (Label scanheapend))))

(define (compile-copy-box)
    (let ((normalcopy (gensym 'normalcopybox)) (endcopy (gensym 'endcopybox)))
        (seq
            ; get the raw address of the box pointer
            ; in scratch register r11
            (Mov r11 (Offset rax 0))
            (Xor r11 type-box)

            ; get the value from the heap in r11
            (Mov r11 (Offset r11 0))

            ; check if it is a forward-pointer
            (type-pred r11 ptr-mask type-forward)

            ; copy normally if it isn't
            (Cmp r11 val-false)
            (Je normalcopy)

            ; get value from heap in r11 again (forward pointer)
            (Mov r11 (Offset rax 0))
            (Xor r11 type-box)
            (Mov r11 (Offset r11 0))

            ; get rid of type tag and make it a box pointer
            (Xor r11 type-forward)
            (Or r11 type-box)

            ; update original ptr to contain the new address and jump to end
            (Mov (Offset rax 0) r11)
            (Jmp endcopy)

            ; if value is not a forward pointer
            (Label normalcopy)

            ; get value from heap into r11
            (Mov r11 (Offset rax 0))
            (Xor r11 type-box)
            (Mov r11 (Offset r11 0))

            ; copy the value into the new space
            (Mov (Offset rbx 0) r11)

            ; get the raw address of the pointer
            (Mov r11 (Offset rax 0))
            (Xor r11 type-box)

            ; tag new address as a forward ptr
            ; and put it in the heap's old spot
            (Or rbx type-forward)
            (Mov (Offset r11 0) rbx)

            ; restore rbx to be the raw address
            (Xor rbx type-forward)

            ; take the new location of the pointer
            ; and change the original ptr to be this
            (Or rbx type-box)
            (Mov (Offset rax 0) rbx)

            ; restore rbx to be the raw address
            (Xor rbx type-box)

            ; increment rbx since one word was copied over
            (Add rbx 8)

            (Label endcopy))))


(define (compile-copy-cons)
    (let ((normalcopy (gensym 'normalcopycons)) (endcopy (gensym 'endcopycons)))
        (seq
            ; get the raw address of the cons pointer
            ; in scratch register r11
            (Mov r11 (Offset rax 0))
            (Xor r11 type-cons)

            ; get the first value (second value of cons since reversed) from the heap in r11
            (Mov r11 (Offset r11 0))

            ; check if it is a forward-pointer
            (type-pred r11 ptr-mask type-forward)

            ; copy normally if it isn't
            (Cmp r11 val-false)
            (Je normalcopy)

            ; get value from heap in r11 again (forward pointer)
            (Mov r11 (Offset rax 0))
            (Xor r11 type-cons)
            (Mov r11 (Offset r11 0))

            ; get rid of type tag and make it a cons pointer
            (Xor r11 type-forward)
            (Or r11 type-cons)

            ; update original ptr to contain the new address and jump to end
            (Mov (Offset rax 0) r11)
            (Jmp endcopy)

            ; if value is not a forward pointer
            (Label normalcopy)

            ; get first value from heap into r11
            (Mov r11 (Offset rax 0))
            (Xor r11 type-cons)
            (Mov r11 (Offset r11 0))

            ; copy the first value into the new space
            (Mov (Offset rbx 0) r11)

            ; get second value from heap into r11
            (Mov r11 (Offset rax 0))
            (Xor r11 type-cons)
            (Mov r11 (Offset r11 8))

            ; copy the second value into the new space
            (Mov (Offset rbx 8) r11)

            ; get the raw address of the pointer
            (Mov r11 (Offset rax 0))
            (Xor r11 type-cons)

            ; tag new address as a forward ptr
            ; and put it in the heap's old spot
            (Or rbx type-forward)
            (Mov (Offset r11 0) rbx)

            ; restore rbx to be the raw address
            (Xor rbx type-forward)

            ; take the new location of the pointer
            ; and change the original ptr to be this
            (Or rbx type-cons)
            (Mov (Offset rax 0) rbx)

            ; restore rbx to be the raw address
            (Xor rbx type-cons)

            ; increment rbx since two words were copied over
            (Add rbx 16)

            (Label endcopy))))
