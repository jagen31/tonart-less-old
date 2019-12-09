#lang curly-fn racket

(require (for-syntax racket racket/syntax syntax/parse syntax/stx) racket/stxparam)
(provide in match-context coordinate -- ||)

(define *time-ctxt* (make-parameter '()))
(define *break* (make-parameter #f))

(define (normalize coord base-coord)
  (define pstart (dict-ref base-coord 'start #f))
  (define pend (dict-ref base-coord 'end #f))
  (define pvoices (dict-ref base-coord 'voices #f))

  (define cstart (dict-ref coord 'start #f))
  (define cend (dict-ref coord 'end #f))
  (define cvoices (dict-ref coord 'voices #f))

  (define start (if (and cstart pstart) (+ cstart pstart) (or cstart pstart)))
  (define end
    (if (and cend pstart) (+ cend pstart) (or cend pend)))
  (define voices (or cvoices pvoices))

  (filter cdr `((start . ,start) (end . ,end) (voices . ,voices))))
  
(define (within? coord1 coord2)
  (match* (coord1 coord2)
    [((list-no-order `(start . ,start1) `(end . ,end1) `(voices . (,voices1 ...)))
      (list-no-order `(start . ,start2) `(end . ,end2) `(voices . (,voices2 ...))))
     (and (>= start1 start2) (<= end1 end2) (not (= start1 end2)) (subset? voices1 voices2))]
    [(_ _) #f]))

(define (normalize-things coords+things base-coord)
  (for/fold ([things* '()])
            ([(coord thing) (in-dict coords+things)])
    (cons (cons (normalize coord base-coord) thing) things*)))

(define (get-values ctxt coord)
  (for/fold ([vals '()] #:result (reverse vals))
            ([(other-coord things) (in-dict ctxt)])
    (if (within? coord other-coord)
        (append (reverse things) vals)
        vals)))

(define (merge-things coords+things1 coords+things2)
  (for/fold ([coords+things coords+things2])
            ([(coord thing) (in-dict coords+things1)])
    (dict-update coords+things coord #{append thing %} '())))

(struct paused [coord k] #:transparent)
(struct results [coords+vals coords+ks] #:transparent)

(define (meta-eval local-coord things (parent #t))
  (define (meta-eval things ctxt)
    (define-values (ks ctxt* all-paused)
      (for/fold ([ks '()] [ctxt ctxt] [all-paused #t])
                ([thing things])
        (match thing
          [(paused coord k) (values (cons (cons coord k) ks) ctxt all-paused)]
          [(results coords+vals coords+ks)
           (values
            (append (normalize-things coords+ks local-coord) ks)
            (merge-things (normalize-things coords+vals local-coord) ctxt)
            #f)]
          [else
           (values ks (merge-things `((,local-coord . (,thing))) ctxt) #f)])))
    (cond
      [(and all-paused parent) (results ctxt* ks)]
      [all-paused (error 'eval "made no progress in: ~a" ctxt*)]
      [(empty? ks) (results ctxt* '())]
      [else (meta-eval
             (for/list ([(coord k) (in-dict ks)])
               (let/ec break
                 (parameterize ([*time-ctxt* (get-values ctxt* coord)]
                                [*break* break])
                   (k))))
             ctxt*)]))
  (meta-eval things '()))

(begin-for-syntax
  (define-syntax-class dimension
    (pattern (name:id value)))
  
  (define-syntax-class elision
    (pattern (type:id duration))))

(define-syntax-parameter get (syntax-rules ()))

(define (get-recur pred continue coord)
  (define result (findf pred (*time-ctxt*)))
  (if result
      (continue result)
      ((*break*)
       (paused coord
               (λ () (get-recur pred continue coord))))))

(define-syntax in
  (syntax-parser
    [(_ (dimension:dimension ...) expr ...)
     #:with [(defs ...) (expr* ...)] (lift-defines (syntax->list #'(expr ...)))
     #'(let ([coord (list (cons 'dimension.name dimension.value) ...)])
         (syntax-parameterize
             ([get (syntax-parser
                     [(_ pred)
                      #'(let/ec continue (get-recur pred continue coord))])])
           defs ...
           (meta-eval
            coord
            (list (results '() (list expr* ...))))))]))

(define-for-syntax (lift-defines exprs)
  (define expanded-exprs (map #{local-expand % 'top-level (list #'get)} exprs))
  (define-values (dvs others)
    (partition #{syntax-parse %
                  [({~literal define-values} more ...) #t]
                  [expr #f]}
               expanded-exprs))
  #`(#,dvs #,(map (λ (stx) #`(cons '() (λ () #,stx))) others)))

(define-match-expander coordinate
  (syntax-parser
    [(_ startp endp voicesp)
     #'(list-no-order `(start . ,startp) `(end . ,endp) `(voices . ,voicesp))]))

(define (start-of ctxt)
  (match ctxt
    [(results vals _)
     (apply min (map (match-lambda [(cons (coordinate start _ _) _) start]) vals))]))

(define (end-of ctxt)
  (match ctxt
    [(results vals _)
     (apply max (map (match-lambda [(cons (list-no-order `(end . ,end) more ...) _) end]) vals))]))

(define-syntax --
  (syntax-parser
    [(_ (dimensions:dimension ...)
        (durations:number exprs ...) ...)
     #:with [offset-ids ...] (stx-map (λ (_) (gensym)) #'(durations ...))
     (define (get-from-dims key default)
       ;; this is bad
       (datum->syntax
        #'(dimensions ...)
        (car (dict-ref (syntax->datum #'(dimensions ...)) key (list default)))))
     (define voices-id (gensym))
     (define voices-value (get-from-dims 'voices #f))
     (define-values (ins sums)
       (for/fold ([exprs '()] [sums '()] #:result (values (reverse exprs) (reverse sums)))
                 ([offset-id (in-list (cons 0
                                            (syntax->list #'(offset-ids ...))))]
                  [next-offset-id (in-list (syntax->list #'(offset-ids ...)))]
                  [inner-exprs (in-list (syntax->list #'((exprs ...) ...)))]
                  [duration (in-list (syntax->list #'(durations ...)))])
         (values
          (cons
           #`(in ([start #,offset-id]
                  [end #,next-offset-id]
                  #,@(if (syntax-e voices-value) (list #`[voices #,voices-id]) '()))
               #,@inner-exprs)
           exprs)
          (cons #`(+ #,duration #,offset-id) sums))))
     (define/with-syntax [sums* ...] sums)
     #`(let* ([#,voices-id #,(get-from-dims 'voices ''())]
              [offset-ids sums*] ...)
         (in ([start #,(get-from-dims 'start 0)]) #,@ins))]))

(define-syntax ||
  (syntax-parser
    [(_ (dimensions:dimension ...) exprs ...+)
     #:with [expr-ids ...] (stx-map (λ (_) (gensym)) #'(exprs ...))
     (define (get-from-dims key default)
       ;; this is still bad, even when i copy paste it down here
       (datum->syntax
        #'(dimensions ...)
        (car (dict-ref (syntax->datum #'(dimensions ...)) key (list default)))))
     #`(let ([expr-ids exprs] ...)
         (in ([start #,(get-from-dims 'start 0)]
              #,@(let ([voices (get-from-dims 'voices #f)])
                   (if voices (list #`[voices '#,voices]) '()))
              [end (max (end-of expr-ids) ...)])

           expr-ids ...))]))

(define-for-syntax (tee expr)
  (println expr)
  expr)

#;(define (trim-context-start context duration)
    (for/fold ([])))

(define (match-context ctxt pred)
  (match ctxt
    [(results coords+vals _)
     (foldl (λ (coord+vals acc)
              (match coord+vals
                [(cons coord vals)
                 (define result (findf pred vals))
                 (if result (cons (cons coord result) acc) acc)]))
            '()
            coords+vals)]))