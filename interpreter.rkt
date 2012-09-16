#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define v (interp f env)])
                  (cond
                    [(funV? v) (interp (funV-body v)
                                       (extend-env (bind (funV-arg v)
                                                         (interp a env))
                                                   mt-env))]
                    [else (error 'interp "no function to apply")]))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [fdC (n a b) (funV n a b)]
  ))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "one argument was not a number")]))
  
(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define (lookup [n : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "Couldn't find binding")]
    [else (if (symbol=? n (bind-name (first env))) (bind-val (first env)) 
                   (lookup n (rest env)))]))

(define (get-fundef n fds)
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(symbol=? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define (parse [e : s-expression])
  (cond
    [(s-exp-number? e) (numS (s-exp->number e))]
    [(s-exp-list? e)
     (let ([sl (s-exp->list e)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond
                [(empty? (rest (rest sl))) (uminusS (parse (second sl)))]
                [else (bminusS (parse (second sl)) (parse (third sl)))])]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (n) (plusC (numC 0) (multC (numC -1) (desugar n)))]))

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

 

(test (interp (appC (fdC 'f 'a (idC 'a)) (numC 1)) mt-env) (numV 1))

(test (interp
       (appC (appC (fdC 'g 'b (fdC 'f 'a (idC 'b))) (numC 0)) (numC 10))
       mt-env)
      (numV 0))