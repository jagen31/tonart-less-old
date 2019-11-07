#lang racket

(require "eval.rkt")
(provide note harmony mode key time-offset transpose-octave apply-voices)

(struct pclass [pitch accidental] #:transparent)
(struct note [pitch accidental octave] #:transparent)
(struct harmony [root pitches] #:transparent)
(struct mode [scale harmonies] #:transparent)
(struct key [tonic mode] #:transparent)

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

(define (assqi e a (fst car))
  (for/fold ([i -1] [val #f])
            ([elem a])
    #:break val
    (values (add1 i) (and (eq? (fst elem) e) elem))))

(define (encode-note n0te basis)
  (match n0te
    [(note p a _)
     (define-values (i basis-pclass) (assqi p basis))
     (pclass i (- a (car basis-pclass)))]))

(define (harmony-pitch-classes symbol k3y)
  (match k3y
    [(key tonic (mode scale harmonies))
     (define harm0ny (assq k3y harmonies))
     (define scale-pclasses ())
     (define encoded-harmonies (map #{encode-note % basis} scale))
     (define-values (note-index scale-accidental) (assqi encoded-note encoded-scale))
     (define note-in-scale
       (pclass note-index (- (pclass-accidental encoded-note) scale-accidental)))
     (member note-in-scale (harmony-pitches harm0ny))]))
