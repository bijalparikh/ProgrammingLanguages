#lang plai
;Bijal Parikh
;Value of pi defined as 3.1415

(define-type CFWAE
  (num (n number?))
  (binop (myop symbol?) (lhs CFWAE?) (rhs CFWAE?))
  (id (name symbol?))
  (fun (param symbol?)(body CFWAE?))
  (app (func-expr CFWAE?)(arg-expr CFWAE?))
  (if0C (c CFWAE?)(t CFWAE?)(e CFWAE?))
  (withC (name symbol?) (named-expr CFWAE?) (body CFWAE?))
  (cond0 (cn (listof cond-rec?)) (default CFWAE?))
 )

(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?) (value CFWAE?) (ds DefrdSub?)))

(define-type cond-rec
  (crec (condition CFWAE?) (eval CFWAE?)))

(define-type binop-rec
  (rec (name symbol?) (op procedure?)))

(define binop-rec-table
  (list
   (rec 'add +)
   (rec 'sub -)
   (rec 'div /)
   (rec 'mult *)))

(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (rec-name (car op-table)) op-name)
                     (rec-op (car op-table))
                     (lookup op-name (cdr op-table)))))))

;To parse the list of conditions in cond0
(define parse-cond
  (lambda (sexpr conditions)
    (cond ((list? (first sexpr)) (set! conditions (cons (crec (parse-cfwae (first (first sexpr))) (parse-cfwae (second (first sexpr)))) conditions)) (parse-cond (rest sexpr) conditions))
          (else conditions))))

;To parse the default value at the end of the cond0
(define parse-last
  (lambda (sexpr)
    (cond ((empty? (rest sexpr)) (parse-cfwae (first sexpr)))
          (else (parse-last (rest sexpr))))))

;Parser
(define parse-cfwae
  (lambda(sexpr)
    (cond ((number? sexpr) (num sexpr))
          ((symbol? sexpr) (id sexpr))
          ((list? sexpr) (case (first sexpr)
                           ((+) (binop 'add (parse-cfwae(second sexpr)) (parse-cfwae(third sexpr))))
                           ((-) (binop 'sub  (parse-cfwae(second sexpr)) (parse-cfwae(third sexpr))))
                           ((*) (binop 'mult (parse-cfwae(second sexpr)) (parse-cfwae(third sexpr))))
                           ((/) (binop 'div (parse-cfwae(second sexpr)) (parse-cfwae(third sexpr))))
                           ((if0) (if0C (parse-cfwae (second sexpr)) (parse-cfwae (third sexpr)) (parse-cfwae (fourth sexpr))))
                           ((fun) (fun  (second sexpr) (parse-cfwae (third sexpr))))
                           ((cond0) (cond0 (parse-cond (rest sexpr) '()) (parse-last (rest sexpr))))
                           ((with) (withC (first (second sexpr)) (parse-cfwae (second (second sexpr))) (parse-cfwae (third sexpr)))) 
                           (else (app (parse-cfwae(first sexpr)) (parse-cfwae (second sexpr)))))))))

;Elaborator
(define elab-cfwae
  (lambda (expr)
    (type-case CFWAE expr
      (num (n) expr)
      (id (v) expr)
      (binop(op l r) (binop op (elab-cfwae l) (elab-cfwae r)))
      (if0C(c t e) (if0C (elab-cfwae c) (elab-cfwae t) (elab-cfwae e)))
      (app(func-expr arg-expr) (app (elab-cfwae func-expr) (elab-cfwae arg-expr)))
      (withC (name named-expr body)(app (fun name (elab-cfwae body)) (elab-cfwae named-expr)))
      (fun(param body)(fun param (elab-cfwae body)))
      (cond0 (cn default) (elab-cond cn default)))))

;To convert the cond0 to if0C
(define elab-cond
  (lambda (cn default)
  (cond ((null? cn) (elab-cfwae default))
        (else (if0C (elab-cfwae (crec-condition (first cn))) (elab-cfwae (crec-eval (first cn))) (elab-cond (rest cn) default))))))
       
;Lookup in Defered Substituion list
(define lookupds
  (lambda (n ds)
    (type-case DefrdSub ds
      (mtSub() (error 'lookupds "find"))
      (aSub (name value nextds) (if (symbol=? n name) value (lookupds n nextds))))))

;Interpretor
;Same interpretor as interp-cfae . Difference in name is just to handle the type cfwae defined in this file. Does not include contsructs for with and cond0 as they would have been handled by elab
(define interp-cfwae
  (lambda (expr ds)
    (type-case CFWAE expr
      (num(n) expr)
      (binop(op l r) (interp-op op (interp-cfwae l ds) (interp-cfwae r ds)))
      (if0C(c t e) (if (= (num-n (interp-cfwae c ds)) 0) (interp-cfwae t ds) (interp-cfwae e ds)))
      (fun(param body) expr)
      (app (func-expr arg-expr) (local ((define fun-val(interp-cfwae func-expr ds)))
                                  (interp-cfwae (fun-body fun-val) (aSub (fun-param fun-val) (interp-cfwae arg-expr ds) ds))))
      (id(name) (lookupds name ds))
      (else (error 'interp-cfwae "invalid")))))

(define interp-op
  (lambda (op expr1 expr2)
    (num ((lookup op binop-rec-table) (num-n expr1) (num-n expr2)))))

;Prelude called after elaboation
(define prelude
  (lambda (ds)
    (aSub 'area (fun 'x (binop 'mult (num 3.1415)(binop 'mult (id 'x)(id 'x)))) (aSub 'inc  (fun 'x (binop 'add (id 'x) (num 1))) (aSub 'pi (num 3.1415) ds)))))

;Evaluation function
(define eval-cfwae
  (lambda(sexpr)
    (interp-cfwae (elab-cfwae (parse-cfwae sexpr)) (prelude (mtSub)))))

;Test Cases
(test (eval-cfwae '{+ 1 2}) (num 3))
(test (eval-cfwae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfwae '{{fun x x} 3}) (num 3))
(test (eval-cfwae '{{fun x {+ x 1}} 1}) (num 2))
(test (eval-cfwae '{if0 0 1 2}) (num 1))
(test (eval-cfwae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfwae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfwae '{with {x 10} {+ x 5}}) (num 15))
(test (eval-cfwae '{with {f {fun x {+ x 1}}} {f 2}}) (num 3))
(test (eval-cfwae '{cond0 {1 2} {0 15} 0}) (num 15))
(test (eval-cfwae '{with {add1 {fun x {+ x 1}}} {cond0 {{add1 0} 5} {3 4} {0 {add1 2}} 52} 2}) (num 3))
(test (eval-cfwae '{inc pi}) (num 4.141592653589793))
(test (eval-cfwae '{with {x 2} {with {inc {fun x {+ x 2}}} {inc x}}}) (num 4))
(test (eval-cfwae '{area 2}) (num 12.566370614359172))
(test (eval-cfwae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))
(test (eval-cfwae '{with {g {fun f {f 3}}} {g {fun x {+ x 1}}}}) (num 4))