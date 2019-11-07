#lang racket

(require "eval.rkt")
(provide note key time-offset transpose-octave apply-voices)

(struct note [pitch accidental octave] #:transparent)
(struct key [tonic mode] #:transparent)

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
