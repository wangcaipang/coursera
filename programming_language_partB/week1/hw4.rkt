
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (item) (string-append item suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs)(error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([ans (s)])
        (cons (car ans) (stream-for-n-steps (cdr ans) (- n 1))))))

(define funny-number-stream
  (letrec ([f
            (lambda (x)
              (cons
               (if (= (remainder x 5) 0)
                   (- 0 x)
                   x)
               (lambda () (f (+ x 1)))))])
  (lambda () (f 1))))
    

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
  (lambda () (f "dan.jpg"))))


(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (letrec ([ans (s)])
                  (cons (cons 0 (car ans)) (lambda ()(f (cdr ans))))))])
  (lambda () (f s))))


(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda ()(f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([item (vector-ref vec n)])
                      (if (and (pair? item) (equal? (car item) v))
                          item
                          (f (+ n 1))
                      ))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [offset 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                          (if new-ans
                              (begin
                                (vector-set! memo offset new-ans)
                                (set! offset (remainder (+ 1 offset) n))
                                new-ans)
                              new-ans)))))])
    f))