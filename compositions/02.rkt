#lang curly-fn racket

(require "../music.rkt")
(require "../eval.rkt")

(define (foldl1 f li)
  (when (null? li)
    (error 'foldl1 "empty list provided"))
  (foldl f (car li) (cdr li)))

(in (0 18 (S))
  (bind basis
    (list (cons 'c 0) (cons 'd 2) (cons 'e 4) (cons 'f 5) (cons 'g 7) (cons 'a 9) (cons 'b 11)))
  (bind major-scale
    (list (pindex 0 0) (pindex 1 0) (pindex 2 0) (pindex 3 0) (pindex 4 0) (pindex 5 0) (pindex 6 0)))
  (bind major-mode
    (mode major-scale
          (list (cons 'I  (harmony 0 (list (pindex 2 0) (pindex 4 0))))
                (cons 'ii (harmony 1 (list (pindex 2 0) (pindex 4 0))))
                (cons 'V  (harmony 4 (list (pindex 2 0) (pindex 4 0)))))))
  (define harmonies
    (list (cons 'I 0) (cons 'V 1) (cons 'I 1) (cons 'ii 1) (cons 'V 0) (cons 'I 2)))
  (define contexts (map #{(in (0 3 (S)) (bind harmony %) (bind arpeggio 'down))} harmonies))
  (foldl1 sequence contexts))

(define (realize-arpeggios ctxt)
  (for/fold ([new-ctxt '()])
            ([(coord bindings) (in-dict (context-bindings ctxt))])
    3))
