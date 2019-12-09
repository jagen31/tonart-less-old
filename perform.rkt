#lang racket
(require racket/generic)
(require rsound)
(require (for-syntax syntax/parse))
(require "eval.rkt")
(require "music.rkt")

(provide play perform)

(require rsound
         rsound/piano-tones
         rsound/envelope)

(define (note->midi n0te)
  (match n0te
    [(note pitch accidental octave)
     (+ (match pitch ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11])
        accidental
        (* 12 (+ octave 1)))]))

(define (tone-length d tempo)
  (exact-round (/ (* FRAME-RATE 60 d) tempo)))

(define (note->piano n d tempo)
  (define len (tone-length d tempo))
  (clip
   (rs-mult
    (piano-tone (note->midi n))
    ((adsr 2 1.0 2 1.0 (exact-round (* 1/2 len))) len))
   0 len))

(define (duration->rest d tempo)
  (silence (tone-length d tempo)))

;; convert a musical context into a sound. a more advanced renderer would get tempos from the context.
;; a more advanced renderer would also not be stupid, avoiding overlaying a new sound for each note.
(define (perform context tempo)
  (define (perform c0ntext acc)
    (match c0ntext
      [(list (cons (coordinate start end voices) n0te) more-frames ...)
       (println start)
       (println end)
       (println voices)
       (println n0te)
       (define piano (note->piano n0te (- end start) tempo))
       (perform
        more-frames
        (rs-overlay
         (if (zero? start)
             piano
             (rs-append
              (duration->rest start tempo)
              piano))
         acc))]
      [else acc]))
  (perform (match-context context note?) (silence 1)))

