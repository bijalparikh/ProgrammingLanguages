;Submission for EECS 662 Project 1 Exercise 2
;Bijal Parikh



#lang plai
(define-type WAEE
  (num (n number?))
  (binop (op symbol?) (lhs WAEE?) (rhs WAEE?))
  (withC (terms (listof binding?)) (body WAEE?))
  (id (name symbol?)))

(define-type binding
  (bind (name symbol?) (named_expr WAEE?)))

(define-type binop-rec
  (rec (name symbol?) (op procedure?)))

(define binop-rec-table
  (list
   (rec 'add +)
   (rec 'sub -)
   (rec 'div /)
   (rec 'mult *)))

;Code snippet from the blog
(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (rec-name (car op-table)) op-name)
                     (rec-op (car op-table))
                     (lookup op-name (cdr op-table)))))))

;Recursively parse all concrete syntax to create list of bindingterms beginning from an empty list
(define (parse-binding sexp bindingterms)
  (cond ((null? sexp)
        bindingterms)
        (else
        (set! bindingterms (cons (bind (first (first sexp)) (parse-waee (second (first sexp)))) bindingterms))
        (parse-binding (rest sexp) bindingterms))))


;Parse concrete syntax
(define parse-waee
  (lambda(sexp)
    (cond ((number? sexp) (num sexp))
          ((symbol? sexp) (id sexp))
          ((list? sexp) (case (first sexp)
                           ((+) (binop 'add (parse-waee (second sexp)) (parse-waee (third sexp))))
                           ((-) (binop 'sub (parse-waee (second sexp)) (parse-waee (third sexp))))
                           ((*) (binop 'mult (parse-waee (second sexp)) (parse-waee (third sexp))))
                           ((/) (binop 'div (parse-waee (second sexp)) (parse-waee (third sexp))))
                           ((with) (withC (parse-binding (second sexp) '())
                                              (parse-waee(third sexp))))))
          (else (error 'parse-waee " Error in parsing concrete-syntax")))))

;Wrapper function to fetch the id and WAEE expression of a binding term and pass it to subst-waee
(define subst-wrapper
  (lambda (e term) 
      (subst-waee e (bind-name term) (bind-named_expr term))))

;Substitute values for with expression
(define subst-waee
  (lambda(e i v)
    (type-case WAEE e
      (num(n) (num n))
      (binop(op l r) (binop op (subst-waee l i v) (subst-waee r i v)))
      (withC (terms bound_body)
             (withC
              (map (lambda(aterm) (subst-waee (second aterm) i v)) terms)
              (map (lambda(aterm) (if (eq? i (first aterm)) bound_body (subst-waee bound_body i v)))))) 
      (id (name)
          (if(symbol=? name i)
              v
              e)))))

;Substitutes binding instances one by one recursively
(define (substitute-all terms body)
   (cond ((empty? terms)
        body )
         (else
            (set! body (subst-wrapper body (first terms)))
            (substitute-all (rest terms) body))))

;Interpretor
(define interp-waee
  (lambda(exp)
    (type-case WAEE exp
      (num(n) n)
      (binop(op l r) ( (lookup op binop-rec-table) (interp-waee l) (interp-waee r)))
      (withC (terms bound_body)
             (interp-waee (substitute-all terms bound_body)))
      (id(v) (error 'interp-waee "FREE variable")))))

;Evaluation function
(define eval-waee
  (lambda(sexp)
    (interp-waee (parse-waee sexp))))



(test (eval-waee '1) 1)
(test (eval-waee '{+ 1 1}) 2)
(test (eval-waee '{- 1 1}) 0)
(test (eval-waee '{* 2 2}) 4)
(test (eval-waee '{/ 4 2}) 2)
(test (eval-waee '{with {{x 3}} {+ x x}}) 6)
(test (eval-waee '{with {{x 3} {y 4}} {+ x y}}) 7)
(test (eval-waee '{with {{x 2}{y 3}} {+ x y}}) 5)
(test (eval-waee '{with {{x 2}} {+ x x}}) 4)

 