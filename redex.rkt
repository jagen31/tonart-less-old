#lang curly-fn racket

(require redex)

(define-language tonart
  (t ::= (t-env e ...))
  (e ::= n x (+ e e) (in (e e) e) (bind x e))
  (x ::= variable-not-otherwise-mentioned)
  (n ::= number)
  (loc ::= (n n))
  (t-env ::= ((loc (x v) ...) ...))
  (E ::= hole
     (e ... E e ...)
     (+ E e) (+ n E)
     (in (E e) e) (in (n E) e) (in (n n) E)
     (bind x E)
     (t-env e ... E e ...)
     (t-env (E e) e ...)
     (t-env (n E) e ...)
     (t-env (n n) e ... E e ...)))

(define (within? c1 c2)
  (match* (c1 c2)
    [((cons s1 e1) (cons s2 e2))
     (and (<= s2 s1) (>= e2 e1) (not (= s1 s2)))]))

(define (insert t-env loc x v)
  (dict-update
   t-env
   loc
   #{dict-set % x v}
   '()))

(define (lookup t-env loc x)
  (define bindings
    (for/fold ([bindings '()])
              ([(loc2 v) (in-dict t-env)])
      (if (within? loc loc2)
          (append v bindings)
          bindings)))
  (dict-ref bindings x #f))

(define red
  (reduction-relation
   tonart
   (--> (in-hole E (+ n_1 n_2))
        (in-hole E ,(+ (term n_1) (term n_2)))
        "plus")
   (--> (in-hole E (t-env e_1 ... (in (n_s n_e) e) e_2 ...))
        (e_1 ... (in-hole E (t-env (n_s n_e) e)) e_2 ...)
        "init")
   (--> (t-env
         loc
         (in-hole E x))
        (in-hole E ,(lookup (term t-env) (term loc) (term x)))
        "lookup")
   (--> (t-env
         loc
         (in-hole E (bind x n)))
         (,(insert (term t-env) (term loc) (term x) (term n))
          loc
          (in-hole E n)))
   (--> (t-env
         (n_ps n_pe)
         (in-hole E (in (n_cs n_ce) e)))
        (t-env
         ((+ n_cs n_ps) (+ n_ce n_ps))
         (in-hole E e))
        "loc")))

(traces red
        (term (()
               (in (2 4)
                 (in (0 1)
                   (bind x 5)))
               (in (2 3) x))))