#lang racket
(require racket/generic)
(require rsound)
(require (for-syntax syntax/parse))
(require "my-expand.rkt")

(provide note perform)

(struct note [pitch accidental octave] #:transparent)
(struct key [tonic mode] #:transparent)

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

(define-syntax define-homo-match-expander
  (syntax-parser
    [(_ name transformer)
     #'(define-match-expander name transformer transformer)]))

(define-homo-match-expander loc
  (syntax-parser
    [(_ start end voices)
     #'(list (interval-coordinate start end) (subset-coordinate voices))]))

;; convert a musical context into a sound. a more advanced renderer would get tempos from the context.
;; a more advanced renderer would also not be stupid, avoiding overlaying a new sound for each note.
(define (perform context tempo)
  (define (perform c0ntext acc)
    (match c0ntext
      [(list (cons (coordinate start end voices) bindings) more-frames ...)
       (define n0te (dict-ref bindings 'note #f))
       (cond
         [n0te
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
         [else (perform more-frames acc)])]
      [_ acc]))
  (perform (context-bindings context) (silence 1)))

(define (map-coordinates f ctxt)
  (context
   (map
    (λ (fr4me)
      (cons (f (car fr4me)) (cdr fr4me)))
    (context-bindings ctxt))))

(define (map-bindings f ctxt)
  (context
   (map
    (λ (fr4me)
      (cons (car fr4me) (map f (cdr fr4me))))
    (context-bindings ctxt))))

(define (time-offset context offset)
  (map-coordinates
   (match-lambda
     [(coordinate start end voices) (coordinate (+ start offset) (+ end offset) voices)])
   context))

(define (transpose-octave context offset)
  (map-bindings
   (match-lambda
     [(cons key value)
      (if (eq? key 'note)
          (cons 'note (struct-copy note value [octave (+ (note-octave value) offset)]))
          (cons key value))])
   context))

(define (apply-voices context voices)
  (map-coordinates
   (match-lambda
     [(coordinate start end _) (coordinate start end voices)])
   context))

(define my-comp
  (in (0 40 (S A T B))
    (define theme1
      (in (0 16 ())
        (in (0 4 ())
          (bind note (note 'a 0 0)))
        (in (4 8 ())
          (bind note (note 'b 0 0)))
        (in (8 12 ())
          (bind note (note 'c 0 1)))
        (in (12 16 ())
          (bind note (note 'd 0 1)))))
    (apply-voices (transpose-octave theme1 4) '(S))
    (apply-voices (time-offset (transpose-octave theme1 3) 8) '(A))
    (apply-voices (time-offset (transpose-octave theme1 2) 16) '(T))))

(play (perform my-comp 240))
