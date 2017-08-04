#lang racket

(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th))))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(define (number-until stream tester)
  (letrec ([answer (stream)])
    (if (tester (car answer))
      0
      (+ 1 (number-until (cdr answer) tester)))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define (ones) (cons 1 ones))

(define (test x y)
  (if (= x 1)
      #t
      #f))
(define (f-2 x) (cons x (lambda () (f-2 (* x 2)))))

(define fib
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                (if ans
                    (cdr ans)
                    (let ([new-ans (if (or (= x 1)(= x 2))
                                      1
                                      (+ (f (- x 1)) (f (- x 2))))])
                      (begin (set! memo (cons (cons x new-ans) memo))
                             new-ans)))))])
    f))
                                      
                    