#lang racket

(require "../eval.rkt")
(require "../music.rkt")
(require "../perform.rkt")

(define my-comp
  (-- ([voices '(S A T B)])
    (8 (|| ()
         (in ([end 4]) (note 'a 0 4))
         (in ([end 2]) (note 'c 0 5))
         (in ([end 8]) (note 'e 0 3))))
    (8 (|| ()
         (in ([end 8]) (note 'g 1 4))
         (in ([end 4]) (note 'b 0 5))))))

(play (perform my-comp 240))