#lang racket

; (let ((x (make-vector 3 (box 6)))) (unbox (vector-ref x 0)))
; (let ((x (cons 5 (cons 3 (cons 1 0))))) (add1 5))

#| (begin
  (let ((x (box 1))) (let ((y (box 1))) (let ((z (box (cons 1 2)))) (add1 5))))
  (begin
    (let ((x (box 1))) (let ((y (box 1))) (let ((z (box (cons 1 2)))) (add1 5))))
    (begin
        (let ((x (box 1))) (let ((y (box 1))) (let ((z (box (cons 1 2)))) (add1 5))))
        (begin
            (let ((x (box 1))) (let ((y (box 1))) (let ((z (box (cons 1 2)))) (add1 5))))
            (add1 5))))) |#

; (let ((x (cons 1 (cons 2 3)))) (add1 5))

; (let ((x (cons 1 (box 1)))) (add1 5))

; (let ((x (box (box 1)))) (add1 5))

#| (begin
    (let ((x (box 1))) (add1 5))
    (begin
        (let ((x (box 1))) (add1 5))
        (begin
            (let ((x (box 1))) (add1 5))
            (begin
                (let ((x (box 1))) (add1 5))
                (begin
                    (let ((x (box 1))) (add1 5))
                    (begin
                        (let ((x (box 1))) (add1 5))
                        (let ((x (box 1))) (add1 5)))))))) |#

; (let ((x (box 1))) (add1 5))

; (let ((x (box (box 1)))) (add1 5))

#| (begin
    (let ((x (box 1))) (add1 5))
    (begin
        (let ((x (box 1))) (add1 5))
        (let ((x (box 1))) (add1 5)))) |#

#| (begin
    (let ((x (box 1))) (add1 5))
    (let ((x (box 1))) (add1 5))) |#


; (let ((x (box 1))) (let ((x (box 1))) (add1 5)))

; (let ((x (box (add1 (unbox (box 1)))))) 6)  ; you would think this needs 2 words, but only really needs 1 word!

; (let ((x (unbox (unbox (box (box 1)))))) 6)

; (let ((x (box 1))) (let ((y (box x))) (let ((z (box y))) 6)))

; (let ((x (box (box 1)))) (add1 5))

; (let ((x (box 1))) (let ((y (box x))) 6))

; (let ((x (box (box 6)))) x)

#| (begin
    (let ((x (box 1))) (let ((y x)) (box 1)))
    (begin
        (let ((x (box 1))) (let ((y x)) (box 1)))
        (begin
            (let ((x (box 1))) (let ((y x)) (box 1)))
            (begin
                (let ((x (box 1))) (let ((y x)) (box 1)))
                (unbox (box 6)))))) |#

; this example is interesting! (you need the scan pointer stuff for this to work correctly and copy over everything that is needed)
; basically, since you have a nested box, it has to copy over the box inside of the box
; (let ((x (box (box 6)))) (box 1))

#| (begin
    (let ((x (cons 1 (cons (box 2) (cons 3 (cons 4 (cons 5 '()))))))) (car (cdr x)))
    (begin
        (let ((x (cons 1 (cons (box 2) (cons 3 (cons 4 (cons 5 '()))))))) (car (cdr x)))
        (let ((x (cons 1 (cons (box 2) (cons 3 (cons 4 (cons 5 '()))))))) (car (cdr x))))) |#

(begin
    (let ((x (box 1))) (add1 5))
    (begin
        (box 1)
        (begin
            (let ((x (box 1))) (add1 5))
            (begin
                (box 1)
                (begin
                    (let ((x (box 1))) (add1 5))
                    (begin
                        (box 1)
                        (begin
                            (let ((x (box 1))) (add1 5))
                            (begin
                                (box 1)
                                (begin
                                    (let ((x (box 1))) (add1 5))
                                    (begin
                                        (box 1)
                                        (begin
                                            (let ((x (box 1))) (add1 5))
                                            (box 1))))))))))))
