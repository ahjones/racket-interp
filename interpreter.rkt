#lang plai

(define-type ExprC
  [numC (n number?)]
  [idC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)])

(define (interp e fds)
  (type-case ExprC e
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [idC (_) (error 'interp "shouldn't get here")]
    [numC (n) n]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define-type FunDefC
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])

(define (get-fundef n fds)
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(symbol=? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define (subst what for in)
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))

(define (parse e)
  (cond
    [(number? e) (numS e)]
    [(list? e)
     (case (first e)
       [(+) (plusS (parse (second e)) (parse (third e)))]
       [(*) (multS (parse (second e)) (parse (third e)))]
       [(-) (bminusS (parse (second e)) (parse (third e)))])]))

(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)]
  [multS (l ArithS?) (r ArithS?)])

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(test 0 (interp (numC 0) '()))
(test 1 (interp (plusC (numC 1) (numC 0)) '()))
(test 6 (interp (multC (numC 2) (numC 3)) '()))
(test 7 (interp (plusC (numC 1) (multC (numC 2) (numC 3))) '()))
(test 2 (interp (desugar (parse '(- 4 2))) '()))

  


