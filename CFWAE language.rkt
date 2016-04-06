;Bijal Parikh 
#lang plai

(define-type CFAE
  (num (n number?))
  (binop (myop symbol?) (lhs CFAE?) (rhs CFAE?))
  (id (name symbol?))
  (fun (param symbol?)(body CFAE?))
  (app (func-expr CFAE?)(arg-expr CFAE?))
  (if0C (c CFAE?)(t CFAE?)(e CFAE?))                    
 )

(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?) (value CFAE?) (ds DefrdSub?)))

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
;Parser
(define parse-cfae
  (lambda(sexpr)
    (cond ((number? sexpr) (num sexpr))
          ((symbol? sexpr) (id sexpr))
          ((list? sexpr) (case (first sexpr)
                           ((+) (binop 'add (parse-cfae(second sexpr)) (parse-cfae(third sexpr))))
                           ((-) (binop 'sub  (parse-cfae(second sexpr)) (parse-cfae(third sexpr))))
                           ((*) (binop 'mult (parse-cfae(second sexpr)) (parse-cfae(third sexpr))))
                           ((/) (binop 'div (parse-cfae(second sexpr)) (parse-cfae(third sexpr))))
                           ((if0) (if0C (parse-cfae (second sexpr)) (parse-cfae (third sexpr)) (parse-cfae (fourth sexpr))))
                           ((fun) (fun (second sexpr) (parse-cfae (third sexpr))))
                           (else (app (parse-cfae(first sexpr)) (parse-cfae (second sexpr)))))))))

;Lookup in Defered Substituion list
(define lookupds
  (lambda (n ds)
    (type-case DefrdSub ds
      (mtSub() (error 'lookupds "find"))
      (aSub (name value nextds) (if (symbol=? n name) value (lookupds n nextds))))))


;Interpretor
(define interp-cfae
  (lambda (expr ds)
    (type-case CFAE expr
      (num(n) expr)
      (binop(op l r) (interp-op op (interp-cfae l ds) (interp-cfae r ds)))
      (if0C(c t e) (if (= (num-n (interp-cfae c ds)) 0) (interp-cfae t ds) (interp-cfae e ds)))
      (fun(param body) expr)
      (app (func-expr arg-expr) (local ((define fun-val(interp-cfae func-expr ds)))
                                  (interp-cfae (fun-body fun-val) (aSub (fun-param fun-val) (interp-cfae arg-expr ds) ds))))
      (id(name) (lookupds name ds)))))

(define interp-op
  (lambda (op expr1 expr2)
    (num ((lookup op binop-rec-table) (num-n expr1) (num-n expr2)))))


;Evaluation function
(define eval-cfae
  (lambda(sexpr)
    (interp-cfae (parse-cfae sexpr) (mtSub))))


;TEST -CASES
(test (eval-cfae '{+ 1 2}) (num 3))
(test (eval-cfae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfae '{{fun x x} 3}) (num 3))
(test (eval-cfae '{{fun x {+ x 1} } 1}) (num 2))
(test (eval-cfae '{if0 0 1 2}) (num 1))
(test (eval-cfae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))