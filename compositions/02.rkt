#lang racket

(require "../eval.rkt")
(require "../music.rkt")
(require "../perform.rkt")
(require rsound)

(define (secret o)
  (-- ()
    (1 (note 'd 0 o))
    (1 (note 'e 0 o))
    (1 (note 'f 0 o))
    (1 (note 'g 0 o))
    (2 (note 'e 0 o))
    (1 (note 'c 0 o))
    (4 (note 'd 0 o))))

(define comp
  (|| ([voices '(s a t b)])
    (-- ([voices '(s)])
      (2 (note 'a 0 4))
      (4 (note 'b -1 4))
      (2 (note 'a 0 4))
      (4 (note 'a 0 4)))
    (in ([voices '(a)]) (secret 4))
    (-- ([voices '(t)])
      (4 (note 'f 0 3))
      (4 (note 'e 0 3))
      (4 (note 'f 0 3)))
    (-- ([voices '(b)])
      (2 (note 'd 0 3))
      (2 (note 'b -1 2))
      (2 (note 'g 0 2))
      (2 (note 'a 0 2))
      (4 (note 'd 0 2)))))
  
  
(play (perform comp 240))