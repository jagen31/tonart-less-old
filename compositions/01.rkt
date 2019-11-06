#lang racket

(require "../my-expand.rkt")
(require "../my-player.rkt")
(require rsound)

(set-output-device! 4)

(define my-comp
  (in (0 32 (S A T B))
    (define theme1
      (in (0 16 ())
        (in (0 4 ()) (bind note (note 'a 0 0)))
        (in (4 8 ()) (bind note (note 'b 0 0)))
        (in (8 12 ()) (bind note (note 'c 0 1)))
        (in (12 16 ()) (bind note (note 'd 0 1)))))
    (define theme2
      (in (0 16 ())
        (in (0 3 ()) (bind note (note 'e 0 0)))
        (in (3 3.5 ()) (bind note (note 'd 0 0)))
        (in (3.5 4 ()) (bind note (note 'c 0 0)))
        (in (4 7 ()) (bind note (note 'd 0 0)))
        (in (7 7.5 ()) (bind note (note 'f 1 0)))
        (in (7.5 8 ()) (bind note (note 'g 1 0)))
        (in (8 12 ()) (bind note (note 'a 0 0)))
        (in (12 16 ()) (bind note (note 'g 1 0)))))
    (apply-voices (transpose-octave theme1 4) '(A))
    (apply-voices (transpose-octave (time-offset theme1 8) 3) '(T))
    (apply-voices (transpose-octave theme2 5) '(S))
    (apply-voices (transpose-octave (time-offset theme2 8) 2) '(B))
    (in (20 24 (B)) (bind note (note 'e 0 2)))
    (in (24 32 (S)) (bind note (note 'a 0 5)))
    (in (24 32 (A)) (bind note (note 'c 1 4)))
    (in (24 32 (T)) (bind note (note 'e 0 3)))
    (in (24 32 (B)) (bind note (note 'a 0 2)))))

(play (perform my-comp 240))
