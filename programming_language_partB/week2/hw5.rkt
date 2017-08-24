;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist list)
  (if (null? list)
      (aunit)
      (apair (car list) (racketlist->mupllist (cdr list)))))
(define (mupllist->racketlist list)
  (if (aunit? list)
      null
      (cons (apair-e1 list) (mupllist->racketlist (apair-e2 list)))))
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define arg_tag "__args__")
(define (eval-under-env e env)
  (letrec ([add-to-env (lambda(list e)
                         (cond [(null? list) e]
                               [#t (cons (car list) (add-to-env (cdr list) e))]))]
           [add-arg-env (lambda(arg-list e)
                          (letrec ([helper (lambda(real-arg-list formal-arg-list enviornment)
                                             (cond [(or
                                                     (and (null? real-arg-list) (not (null? formal-arg-list)))
                                                     (and (null? formal-arg-list) (not (null? real-arg-list))))
                                                    (error "extra argument")]
                                                   [(null? real-arg-list) enviornment]
                                                   [#t (helper (cdr real-arg-list) (cdr formal-arg-list) (add-to-env (list (cons (car formal-arg-list) (car real-arg-list))) enviornment))]
                                                   ))])
                            (cond [(null? e) (error "no argument bound to function")]
                                  [(equal? (car (car e)) arg_tag) (helper arg-list (cdr (car e)) e)]
                                  [#t (add-arg-env arg-list (cdr e))]
                                  )))]
           )
    (cond [(var? e) 
           (envlookup env (var-string e))]
          [(add? e) 
           (let ([v1 (eval-under-env (add-e1 e) env)]
                 [v2 (eval-under-env (add-e2 e) env)])
             (if (and (int? v1)
                      (int? v2))
                 (int (+ (int-num v1) 
                         (int-num v2)))

                 (error "MUPL addition applied to non-number")))]
          ;; CHANGE add more cases here
          [(int? e) e]
          [(closure? e) e]
          [(aunit? e) e]
          [(ifgreater? e)
           (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                 [v2 (eval-under-env (ifgreater-e2 e) env)])
             (if (and (int? v1)
                      (int? v2))
                 (if (> (int-num v1)
                        (int-num v2))
                     (eval-under-env (ifgreater-e3 e) env)
                     (eval-under-env (ifgreater-e4 e) env))
                 (error "MUPL ifgreater applied to non-number")))]
          [(fun? e)
           (letrec ([clo (closure env e) ])
             clo)]
          [(call? e)
           (letrec ([clo (eval-under-env (call-funexp e) env)]
                    [arg (eval-under-env (call-actual e) env)])
             (if (closure? clo)
                 (let ([fep (closure-fun clo)]
                       [env (closure-env clo)])
                   (eval-under-env (fun-body fep) (add-arg-env (list arg) (add-to-env (cons (cons arg_tag (list (fun-formal fep))) (if (equal? (fun-nameopt fep) #f)
                                                                                                                                       null
                                                                                                                                       (cons (cons (fun-nameopt fep) clo) null)
                                                                                                                                       )) env))))
                 (error "MUPL call applied to non-closure")
                 ))]
          [(mlet? e)
           (eval-under-env (mlet-body e) (add-to-env (list (cons (mlet-var e) (eval-under-env (mlet-e e) env))) env))]
          [(apair? e)
           (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
          [(fst? e)
           (let ([pr (eval-under-env (fst-e e) env)])
             (if (apair? pr)
                 (apair-e1 pr)
                 (error "MUPL fst applied to non-pair")))]
          [(snd? e)
           (let ([pr (eval-under-env (snd-e e) env)])
             (if (apair? pr)
                 (apair-e2 pr)
                 (error "MUPL fst applied to non-pair")))]
          [(isaunit? e)
           (let ([pr (eval-under-env (isaunit-e e) env)])
             (if (aunit? pr)
                 (int 1)
                 (int 0)))]
          [#t (error (format "bad MUPL expression: ~v" e))])
    )
  )

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))
  
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2
                      (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "map" "list"
            (ifgreater
             (isaunit (var "list"))
             (int 0)
             (aunit)
             (apair (call (var "func") (fst (var "list"))) (call (var "map") (snd (var "list"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "integer"
             (call (var "map") (fun #f "x" (add (var "integer") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([helper (lambda(e)
                     (cond [(int? e) (cons e (set))]
                           [(aunit? e) (cons e (set))]
                           [(apair? e)
                            (letrec ([new-e1 (helper (apair-e1 e))]
                                     [new-e2 (helper (apair-e2 e))])
                              (cons (apair(car new-e1) (car new-e2)) (set-union (cdr new-e1) (cdr new-e2))))]
                           [(isaunit? e)
                            (let ([new-e (helper (isaunit-e e))])
                              (cons (isaunit (car new-e)) (cdr new-e)))]
                           [(fst? e)
                            (let ([new-e (helper (fst-e e))])
                              (cons (fst (car new-e)) (cdr new-e)))]
                           [(snd? e)
                            (let ([new-e (helper (snd-e e))])
                              (cons (snd (car new-e)) (cdr new-e)))]
                           [(mlet? e)
                            (letrec ([new-e (helper (mlet-e e))]
                                     [new-body (helper (mlet-body e))])
                              (cons (mlet (mlet-var e)  (car new-e) (car new-body)) (set-union (cdr new-e) (cdr new-body))))]
                           [(add? e)
                            (letrec ([new-e1 (helper (add-e1 e))]
                                     [new-e2 (helper (add-e2 e))])
                              (cons (add  (car new-e1) (car new-e2)) (set-union (cdr new-e1) (cdr new-e2))))]
                           [(ifgreater? e)
                            (letrec ([new-e1 (helper (ifgreater-e1 e))]
                                     [new-e2 (helper (ifgreater-e2 e))]
                                     [new-e3 (helper (ifgreater-e3 e))]
                                     [new-e4 (helper (ifgreater-e4 e))])
                              (cons (ifgreater  (car new-e1) (car new-e2) (car new-e3) (car new-e4)) (set-union (cdr new-e1) (cdr new-e2) (cdr new-e3) (cdr new-e4))))]
                           [(call? e)
                            (let ([new-funexp (helper (call-funexp e))]
                                  [new-actual (helper (call-actual e))])
                              (cons (call (car new-funexp) (car new-actual)) (set-union (cdr new-funexp) (cdr new-actual))))]
                           [(closure? e)
                            (let ([new-fun (helper (closure-fun e))])
                              (cons (closure (closure-env e) (car new-fun)) (cdr new-fun)))]
                           [(var? e)
                            (cons e (set (var-string e)))]
                           [(fun? e)
                            (letrec ([new-body (helper (fun-body e))]
                                     [freevar (set-remove (set-remove (cdr new-body) (fun-formal e)) (fun-nameopt e))])
                              (cons (fun-challenge (fun-nameopt e) (fun-formal e) (car new-body) freevar) freevar))]
                           ))])
    (car (helper e))))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (letrec ([add-to-env (lambda(list e)
                         (cond [(null? list) e]
                               [#t (cons (car list) (add-to-env (cdr list) e))]))]
           [add-arg-env (lambda(arg-list e)
                          (letrec ([helper (lambda(real-arg-list formal-arg-list enviornment)
                                             (cond [(or
                                                     (and (null? real-arg-list) (not (null? formal-arg-list)))
                                                     (and (null? formal-arg-list) (not (null? real-arg-list))))
                                                    (error "extra argument")]
                                                   [(null? real-arg-list) enviornment]
                                                   [#t (helper (cdr real-arg-list) (cdr formal-arg-list) (add-to-env (list (cons (car formal-arg-list) (car real-arg-list))) enviornment))]
                                                   ))])
                            (cond [(null? e) (error "no argument bound to function")]
                                  [(equal? (car (car e)) arg_tag) (helper arg-list (cdr (car e)) e)]
                                  [#t (add-arg-env arg-list (cdr e))]
                                  )))]
           [get-correct-env (lambda(env freevars)
                              (cond [(set-empty? freevars) null]
                                    [#t (if (set-member? freevars (car (car env)))
                                            (cons (car env) (get-correct-env (cdr env) (set-remove freevars (car (car env)))))
                                            (get-correct-env (cdr env) freevars))]))]
           )
    (cond [(var? e) 
           (envlookup env (var-string e))]
          [(add? e) 
           (let ([v1 (eval-under-env-c (add-e1 e) env)]
                 [v2 (eval-under-env-c (add-e2 e) env)])
             (if (and (int? v1)
                      (int? v2))
                 (int (+ (int-num v1) 
                         (int-num v2)))

                 (error "MUPL addition applied to non-number")))]
          ;; CHANGE add more cases here
          [(int? e) e]
          [(closure? e) e]
          [(aunit? e) e]
          [(ifgreater? e)
           (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
                 [v2 (eval-under-env-c (ifgreater-e2 e) env)])
             (if (and (int? v1)
                      (int? v2))
                 (if (> (int-num v1)
                        (int-num v2))
                     (eval-under-env-c (ifgreater-e3 e) env)
                     (eval-under-env-c (ifgreater-e4 e) env))
                 (error "MUPL ifgreater applied to non-number")))]
          [(fun-challenge? e)
           (letrec ([clo (closure env e) ])
             clo)]
          [(call? e)
           (letrec ([clo (eval-under-env-c (call-funexp e) env)]
                    [arg (eval-under-env-c (call-actual e) env)])
             (if (closure? clo)
                 (letrec ([fep (closure-fun clo)]
                          [env (closure-env clo)]
                          [real-env (get-correct-env env (fun-challenge-freevars fep))])
                   (eval-under-env-c (fun-challenge-body fep) (add-arg-env (list arg) (add-to-env (cons (cons arg_tag (list (fun-challenge-formal fep))) (if (equal? (fun-challenge-nameopt fep) #f)
                                                                                                                                                             null
                                                                                                                                                             (cons (cons (fun-challenge-nameopt fep) clo) null)
                                                                                                                                                             )) real-env))))
                 (error "MUPL call applied to non-closure")
                 ))]
          [(mlet? e)
           (eval-under-env-c (mlet-body e) (add-to-env (list (cons (mlet-var e) (eval-under-env-c (mlet-e e) env))) env))]
          [(apair? e)
           (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
          [(fst? e)
           (let ([pr (eval-under-env-c (fst-e e) env)])
             (if (apair? pr)
                 (apair-e1 pr)
                 (error "MUPL fst applied to non-pair")))]
          [(snd? e)
           (let ([pr (eval-under-env-c (snd-e e) env)])
             (if (apair? pr)
                 (apair-e2 pr)
                 (error "MUPL fst applied to non-pair")))]
          [(isaunit? e)
           (let ([pr (eval-under-env-c (isaunit-e e) env)])
             (if (aunit? pr)
                 (int 1)
                 (int 0)))]
          [#t (error (format "bad MUPL expression: ~v" e))])
    ))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
