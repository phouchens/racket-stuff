#lang at-exp racket
;Tower of hanoi problem. Pegs are labeled 0 1 2. 
;n is the number of disk, f is the peg moving all the disks from,
;t is teh peg we are moving to, u is the remaining peg
;Returns a list of moves. Each element in the list is a list of two elements
;that designate the peg to move from and the peg to move to.
(define (hanoi n f t) 
  (if (= 1 n) (list (list f t)) ;only a single disk to move
      (let* ([u (- 3 (+ f t))]  ;determine unused peg
             [m1 (hanoi (sub1 n) f u)] ;move n-1 disks from f to u
             [m2 (list f t)]  ;move single disc from f to t
             [m3 (hanoi (sub1 n) u t)]) ;move disk from u to t
        (append m1 (cons m2 m3)))))

;Fibonacci easy approach
(define (F n)
  (if (<= n 1) n
      (+ (F (- n 1)) (F (- n 2)))))

;Fibonacci better approach due to tail call optimization
(define (Fib n)
  (define (f a b c)
    (if (= c 0) b
        (f (+ a b) a (- c 1))))
  (f 1 0 n))
