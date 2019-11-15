#lang curly-fn racket

(require redex)

(define-language tonart
  (e ::= n x (+ e e) (in (e e) e) (bind x e))
  (x ::= variable-not-otherwise-mentioned)
  (n ::= number)
  (t-env ::= (((n n) (x n) ...) ...))
  (E ::= hole
     (t-env e ... E e ...)
     (+ E e) (+ n E)
     (in (E e) e) (in (n E) e) (in (n n) E)
     (bind x E)))

(define (within? c1 c2)
  (match* (c1 c2)
    [((list s1 e1) (list s2 e2))
     (and (<= s2 s1) (>= e2 e1) (not (= s1 e2)))]))

(define (insert t-env loc x v) (dict-update t-env loc #{cons (list x v) %} '()))

(define not-found (gensym))

(define (lookup t-env loc x)
  (define bindings
    (for/fold ([bindings '()])
              ([(loc2 v) (in-dict t-env)])
      (if (within? loc loc2)
          (append v bindings)
          bindings)))
  (define result (assq x bindings))
  (if result
      (cadr result)
      not-found))

(define red
  (reduction-relation
   tonart
   (--> (in-hole E (+ n_1 n_2)) (in-hole E ,(+ (term n_1) (term n_2))) "plus")
   (--> (t-env e_1 ... (in-hole E_1 (in (n_s n_e) (in-hole E_2 (bind x n)))) e_2 ...)
        (,(insert (term t-env) (term (n_s n_e)) (term x) (term n))
         e_1 ...
         (in-hole E_1 (in (n_s n_e) (in-hole E_2 n)))
         e_2 ...)
        "bind")
   (--> (t-env e_1 ... (in-hole E_1 (in (n_s n_e) (in-hole E_2 x))) e_2 ...)
        (t-env e_1 ...
               (in-hole E_1
                        (in (n_s n_e)
                          (in-hole E_2
                                   ,(lookup (term t-env) (term (n_s n_e)) (term x)))))
               e_2 ...)
        (side-condition (not (eq? (lookup (term t-env) (term (n_s n_e)) (term x)) not-found)))
        "lookup")))

(traces red
        (term (() (in (0 4) (bind x (+ 3 4))) (in (2 4) (bind y 5)) (in (3 4) (+ 2 (+ x y))))))
