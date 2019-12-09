#lang racket

(require "eval.rkt")
(provide note note? harmony harmony? mode mode? key key?
         tremolo tremolo? tremolo-duration g:tremolo g:tremolo?)

(struct pclass [pitch accidental] #:transparent)
(struct pitch-index [index accidental] #:transparent)
(struct note [pitch accidental octave] #:transparent)
(struct harmony [root ] #:transparent)
(struct mode [scale harmonies] #:transparent)
(struct key [tonic mode] #:transparent)
(struct tremolo [duration] #:transparent)
(struct g:tremolo tremolo [] #:transparent)
