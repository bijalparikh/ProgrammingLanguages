;Submission for EECS 662 Project 1 Exercise 1
;Bijal Parikh

#lang plai
(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (name symbol?) (named_expr WAE?) (body WAE?))
  (id (name symbol?))
  )
;Parse concrete syntax for with by recursively calling parse-wae for second and third argument.
(define parse-wae
  (lambda(sexp)
   (cond ((number? sexp) (num sexp))
         ((symbol? sexp) (id sexp))
         ((list? sexp) (case (first sexp) ((+) (add (parse-wae(second sexp))
                                                   (parse-wae(third sexp))))
                                         ((-) (sub (parse-wae(second sexp))
                                                   (parse-wae(third sexp))))             
                         ((with) (with (first(second sexp))
                                                      (parse-wae (second(second sexp))) (parse-wae(third sexp)))))))))

(define subst-wae
  (lambda(e i v)
    (type-case WAE e
      (num(n) (num n))
      (add (l r) (add (subst-wae l i v) (subst-wae r i v)))
      (sub (l r) (sub (subst-wae l i v) (subst-wae r i v)))
      (with (bound_id named_expr bound_body)
            (if(symbol=? bound_id i)
               (with bound_id
                     (subst-wae named_expr i v) bound_body)
               (with bound_id
                     (subst-wae named_expr i v)
                     (subst-wae bound_body i v))))
      (id (name)
          (if(symbol=? name i)
              v
              e)))))
;Interpretor         
(define interp-wae
  (lambda(exp)
    (type-case WAE exp
      (num(n) n)
      (add(l r) ( + (interp-wae l) (interp-wae r)))
      (sub(l r) ( - (interp-wae l) (interp-wae r)))
      (with(bound_id named_expr bound_body)
           (interp-wae (subst-wae bound_body bound_id (num (interp-wae named_expr)))))
      (id(v) (error 'interp-wae "FREE variable")))))

(define eval-wae
  (lambda(sexp)
    (interp-wae (parse-wae sexp))))

(test (eval-wae '1) 1)
(test (eval-wae '{+ 1 1}) 2)
(test (eval-wae '{- 1 1}) 0)
(test (eval-wae '{with {x 3} {+ x x}}) 6)
(test (eval-wae '{with {x 3} {with {y 4} {+ x y}}}) 7)
(test (eval-wae '{with {x 3} {with {y 4} {+ x y}}}) 7)
(test (eval-wae '{with {x 3} {with {y {+ x x}} {+ x y}}}) 9)
(test (eval-wae '{with {x 3} {with {y {+ x x}} {with {x 1} {+ x y}}}})7)
(test (eval-wae '{with {x 2} {+ x x}}) 4)
(test (eval-wae '{with{x 5}{+ x{with{x 3}x}}}) 8)