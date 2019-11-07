#lang curly-fn racket

(require (for-syntax syntax/parse syntax/stx syntax/apply-transformer))
(provide in context context-bindings coordinate within?
         map-bindings map-coordinates time-offset merge sequence)

(define *local-context* (make-parameter (make-hash)))
(define *time-context* (make-parameter '()))

(struct result [bindings continuation] #:transparent)
(struct results [mapped-bindings mapped-continuations] #:transparent)
(struct context [bindings] #:transparent)
(struct coordinate [start end voices] #:transparent)

(define-syntax in
  (syntax-parser
    [(_ (the-start the-end (voices ...)) exprs ...)
     (define-values (bindings remaining-exprs) (extract-bindings (syntax->list #'(exprs ...))))
     #`(letrec-values (#,@bindings)
         (let ([coord (coordinate the-start the-end '(voices ...))])
           (meta-eval
            (list #,@(stx-map
                      (λ (expr)
                        #`(cons
                           coord
                           (λ (break)
                             (define val #,(my-expander expr))
                             (match val
                               [(? results?) val]
                               [(? context?) val]
                               [_ (result (convert-local-bindings (*local-context*)) #f)]))))
                      remaining-exprs))
            '())))]))

(define (meta-eval mapped-fns time-ctxt (parent #f))
  (define-values (mapped-bindings mapped-ks)
    (for/fold ([mapped-bindings '()] [mapped-ks '()]
               #:result (values mapped-bindings (reverse mapped-ks)))
              ([(coord fn) (in-dict mapped-fns)])
      (define val
          (let/cc break (parameterize ([*time-context* (get-bindings time-ctxt coord)]
                                       [*local-context* (make-hash)])
                          (fn break))))
      (match val
        [(result bindings k)
         (values (if (cons? bindings)
                     (merge-bindings (list (cons coord bindings)) mapped-bindings)
                     mapped-bindings)
                 (if k (cons (cons coord k) mapped-ks) mapped-ks))]
        [(results mapped-bindings* mapped-ks*)
         (values (merge-bindings (normalize-mappings mapped-bindings* coord) mapped-bindings)
                 (append (reverse (normalize-mappings mapped-ks* coord)) mapped-ks))]
        [(context mapped-bindings*)
         (values (merge-bindings (normalize-mappings mapped-bindings* coord) mapped-bindings)
                 mapped-ks)]
        [_ (values mapped-bindings mapped-ks)])))
  (define time-ctxt* (merge-bindings mapped-bindings time-ctxt))
  (cond
    [(null? mapped-ks) (context time-ctxt*)]
    [(and (null? mapped-bindings) parent) (results time-ctxt mapped-ks)]
    [(null? mapped-bindings) (error 'meta-eval "made no progress! bindings: ~s" time-ctxt)]
    [else (meta-eval mapped-ks time-ctxt* parent)]))

(define-for-syntax (tee expr)
  (println expr)
  expr)

(define-for-syntax (extract-bindings exprs)
  (define exprs* (map just-macros exprs))
  (for/fold ([bindings '()] [remaining-exprs '()]
             #:result (values (reverse bindings) (reverse remaining-exprs)))
            ([expr exprs] [expr* exprs*])
    (syntax-parse expr*
      [({~literal define-values} (ids ...) vals)
       (values (cons #`[(ids ...) vals] bindings)
               remaining-exprs)]
      [_ (values bindings (cons expr remaining-exprs))])))

(define-for-syntax (just-macros expr)
  (syntax-parse expr
    [(fun exprs ...)
     (define value (syntax-local-value #'fun (λ () #f)))
     (cond
       [value (just-macros (local-apply-transformer value expr 'top-level))]
       [else expr])]
    [_ expr]))

(define-for-syntax (my-expander stx)
  (syntax-parse stx
    [({~literal let-values} ([(let-ids ...) vals] ...) body-exprs ...)
     (define-values (bindings remaining-exprs) (extract-bindings (syntax->list #'(body-exprs ...))))
     #`(let-values ([(let-ids ...) vals] ...)
              (letrec-value (#,@bindings)
                            #,@(map my-expander remaining-exprs)))]
    [({~literal #%plain-lambda} (args ...) body-exprs ...)
     (define-values (bindings remaining-exprs) (extract-bindings (syntax->list #'(body-exprs ...))))
     #`(lambda (args ...)
         (letrec-values (#,@bindings)
           #,@(map my-expander remaining-exprs)))]
    [({~literal meta-eval} fns li) #'(meta-eval fns li #t)]
    [({~literal bind} name:id expr) #`(dict-set! (*local-context*) 'name #,(my-expander #'expr))]
    [({~literal quote} _) stx]
    [(fun:id exprs ...)
     (define value (syntax-local-value #'fun (λ () #f)))
     (cond
       [value (my-expander (local-apply-transformer value stx 'expression))]
       [else
        (define-values (bindings remaining-exprs) (extract-bindings (syntax->list #'(exprs ...))))
        (define rest-expanded #`(fun #,@(map my-expander remaining-exprs)))
        (if (null? bindings)
            rest-expanded
            #`(let-values (#,@bindings) #,rest-expanded))])]
    [(fun exprs ...)
     (define-values (bindings remaining-exprs) (extract-bindings (syntax->list #'(exprs ...))))
        (define rest-expanded #`(#,@(map my-expander #`(fun #,remaining-exprs))))
        (if (null? bindings)
            rest-expanded
            #`(let-values (#,@bindings) #,rest-expanded))]
    [expr:id #'(decide-if-bound expr break)]
    [_ stx]))

(define-syntax decide-if-bound
  (syntax-parser
    [(_ expr break)
     (if (identifier-binding #'expr)
         #'expr
         #'(let/cc k (lookup 'expr break k)))]))

(define (lookup id break continue)
  (define local-lookup (dict-ref (*local-context*) id #f))
  (define time-lookup (dict-ref (*time-context*) id #f))
  (cond
    [local-lookup (continue local-lookup)]
    [time-lookup (continue time-lookup)]
    [else
     (begin
       (break
        (result
         (convert-local-bindings (*local-context*))
         #{parameterize ([*local-context* (make-hash)])
            (lookup id % continue)})))]))

(define (convert-local-bindings ctxt)
  (for/fold ([bindings '()])
            ([(k v) (in-dict ctxt)])
    (cons (cons k v) bindings)))

(define (within? c1 c2)
  (match* (c1 c2)
    [((coordinate start1 end1 voices1) (coordinate start2 end2 voices2))
     (and (>= start1 start2) (<= end1 end2) (not (= start1 end2)) (subset? voices1 voices2))]))

(define (get-bindings ctxt coord)
  (for/fold ([bindings '()])
            ([(coord* bindings*) (in-dict ctxt)])
    (if (within? coord coord*) (append bindings* bindings) bindings)))

(define (merge-bindings c1 c2)
  (for/fold ([c3 c2])
            ([(coord bindings) (in-dict c1)])
    (dict-update c3 coord (λ (bindings*) (append bindings bindings*)) '())))

(define (normalize-mappings mappeds coord)
  (for/fold ([remapped '()] #:result (reverse remapped))
            ([(mapping mapped) (in-dict mappeds)])
    (match* (mapping coord)
      [((coordinate start end voices) (coordinate parent-start parent-end parent-voices))
       (define new-mapping (coordinate (+ start parent-start) (+ end parent-start) voices))
       (unless (within? new-mapping coord)
         (error 'normalize "child is not subcontext of parent! ~s from ~s in ~s"
                new-mapping mapping coord))
       (cons (cons new-mapping mapped) remapped)])))

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

(define (start-of ctxt)
  (for/fold ([min-start #f])
            ([(coord _) (in-dict (context-bindings ctxt))])
    (match coord
      [(coordinate start _ _) (if min-start (min min-start start) start)])))

(define (end-of ctxt)
  (for/fold ([max-end #f])
            ([(coord _) (in-dict (context-bindings ctxt))])
    (match coord
      [(coordinate _ end _) (if max-end (max max-end end) end)])))

(define (time-offset context offset)
  (map-coordinates
   (match-lambda
     [(coordinate start end voices) (coordinate (+ start offset) (+ end offset) voices)])
   context))

(define (merge c1 c2)
  (match* (c1 c2)
    [((context b1) (context b2)) (context (merge-bindings b1 b2))]))

(define (sequence c1 c2)
  (define start (start-of c1))
  (define end (end-of c1))
  (merge c1 (time-offset c2 (- end start))))
