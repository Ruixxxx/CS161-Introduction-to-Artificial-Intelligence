; 005230642
; Rui Xu


; Problem 1
; recursively compute the former Padovan numbers needed

; LISP function: PAD
; argument: integer N
; return: the Nth Padovan number
(defun PAD (N) 
    (cond ((= N 0) 1)
          ((= N 1) 1)
          ((= N 2) 1)
          (t (+ (PAD (- N 2)) (PAD (- N 3))))
    )
)

; test on the first 10 Padovan numbers
;(print (PAD 0))
;(print (PAD 1))
;(print (PAD 2))
;(print (PAD 3))
;(print (PAD 4))
;(print (PAD 5))
;(print (PAD 6))
;(print (PAD 7))
;(print (PAD 8))
;(print (PAD 9))
;(print (PAD 10))
; test on larger values
;(print (PAD 20))
;(print (PAD 50))
;(print (PAD 100))
;(print (PAD 1000))
;(print (PAD 10000))

; Problem 2
; recursively compute the former additions needed

; LISP function: SUMS
; argument: integer N
; return: number of additions required by PAD to compute the Nth 
; Padovan number
(defun SUMS (N)
    (cond ((= N 0) 0)
          ((= N 1) 0)
          ((= N 2) 0)
          (t (+ 1 (+ (SUMS (- N 2)) (SUMS (- N 3)))))
    )
)

; test on the first 10 values
;(print (SUMS 0))
;(print (SUMS 1))
;(print (SUMS 2))
;(print (SUMS 3))
;(print (SUMS 4))
;(print (SUMS 5))
;(print (SUMS 6))
;(print (SUMS 7))
;(print (SUMS 8))
;(print (SUMS 9))
;(print (SUMS 10))

; Problem 3
; recursively replace symbols and numbers until the leaf nodes

; LISP function: ANON
; argument: TREE that represents a tree
; return: an anonymized version of the tree with the same structure, but
; all symbols and numbers in the tree are replaced by question marks
(defun ANON (TREE) 
    (cond ((null TREE) nil)
          ((atom TREE) '?)
          (t (cons (ANON(car TREE)) (ANON(cdr TREE))))
    )
)

; test
;(print (ANON '42))
;(print (ANON 'FOO))
;(print (ANON '(((L E) F) T)))
;(print (ANON '(5 FOO 3.1 -0.2)))
;(print (ANON '(1 (FOO 3.1) -0.2)))
;(print (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
;(print (ANON '(R (I (G (H T))))))