#lang racket/base

(require racket/class
         racket/contract/base
         racket/set
         (prefix-in ffi: ffi/unsafe)
         "link.rkt"
         "private/object.rkt")

(provide
 dynamic-object<%>
 dynamic-load-context?
 (contract-out
  [load-object (->* (bin:object?) ((or/c dynamic-load-context? #f)) (is-a?/c dynamic-object<%>))]
  [make-dynamic-load-context (->* () ((or/c dynamic-load-context? #f)) dynamic-load-context?)]))

(define dynamic-object<%>
  (interface ()
    [symbols (->m (listof symbol?))]
    [symbol-ref (->m symbol? ffi:cpointer?)]))

(struct dynamic-load-context (symbol-map base))

(define (make-dynamic-load-context [base-context #f])
  (dynamic-load-context make-hasheq base-context))

(define (load-object obj [ctx #f])
  (define dobj
    (new dynamic-object% [object obj] [context ctx]))
  (when ctx
    (define sym-map (dynamic-load-context-symbol-map ctx))
    (for ([sym (send dobj symbols)])
      (when (hash-has-key? sym-map sym)
        (eprintf "Warning: redefining symbol: ~a\n" sym))
      (hash-set! sym-map sym (send dobj symbol-ref sym))))
  dobj)

(struct symbol-reference
  (obj ptr)
  #:property ffi:prop:cpointer (struct-field-index ptr))

(define dynamic-object%
  (class* object% (dynamic-object<%>)
    (init object context)
    (super-new)
    ; Keep references to prevent linked symbols from being garbage-collected
    (field [sections #f]
           [free-procs #f]
           [globals #f]
           [references #f])
    (set!-values (sections free-procs globals references)
      (load/link object context))
    (ffi:register-finalizer this
      (λ (dobj)
        (for ([proc (in-list (get-field free-procs dobj))]
              [ptr (in-list (get-field sections dobj))]
              #:when ptr)
          (proc ptr))))
    (define/public (symbols)
      (hash-keys globals))
    (define/public (symbol-ref sym)
      (symbol-reference
       this
       (hash-ref globals sym
                 (λ ()
                   (error 'symbol-ref "symbol not defined: ~a" sym)))))))

(define (load/link obj ctx)
  (define references (mutable-seteq))
  (define-values (section-ptrs free-procs)
    (for/lists (ptrs procs)
               ([section (in-list (bin:object-sections obj))])
      (define data (bin:section-data section))
      (define size
        (cond
          [(not data) => (bin:section-size section)]
          [(bin:section-size section) => (λ (size)
                                           (unless (eqv? size (bytes-length data))
                                             (error 'load-object "section size mismatch"))
                                           size)]
          [else (and data (bytes-length data))]))
      (define-values (ptr free-proc)
        (cond
          [(not size) (values #f #f)]
          [(bin:section-executable? section) (values (malloc-code size) free-code)]
          [else (values (ffi:malloc size 'raw) ffi:free)]))
      (cond
        [data (ffi:memcpy ptr data size)]
        [size (ffi:memset ptr 0 size)])
      (values ptr free-proc)))
  (define-values (locals globals)
    (for/fold ([locals (hasheq)]
               [globals (hasheq)])
              ([section (in-list (bin:object-sections obj))]
               [base-ptr (in-list section-ptrs)]
               #:when base-ptr
               [sym (in-list (bin:section-symbols section))])
      (define name (bin:symbol-name sym))
      (define sym-ptr
        (cond [(bin:symbol-value sym) => (λ (offset) (ffi:ptr-add base-ptr offset))] [else #f]))
      (case (bin:symbol-binding sym)
        [(local) (values (hash-set locals name sym-ptr) globals)]
        [(global) (values locals (hash-set globals name sym-ptr))]
        [else (values locals globals)])))
  (define (symbol-value name)
    (ffi:cast
     (or (hash-ref locals name #f)
         (hash-ref globals name #f)
         (let loop ([ctx ctx])
           (and ctx (or (hash-ref (dynamic-load-context-symbol-map ctx) name #f)
                        (loop (dynamic-load-context-base ctx)))))
         (error "symbol not found:" name))
     ffi:_pointer ffi:_size))
  (for ([section (in-list (bin:object-sections obj))]
        [section-ptr (in-list section-ptrs)]
        #:when section-ptr)
    (apply-relocations
     (bin:section-relocations section)
     (ffi:cast section-ptr ffi:_pointer ffi:_size)
     symbol-value
     (λ (offset bstr)
       (ffi:memcpy (ffi:ptr-add section-ptr offset) bstr (bytes-length bstr)))))
  (values section-ptrs free-procs globals references))

(define-values (malloc-code free-code)
  (if (eq? 'racket (system-type 'vm))
      (values
       (ffi:get-ffi-obj 'scheme_malloc_code #f (ffi:_fun ffi:_size ffi:-> ffi:_pointer))
       (ffi:get-ffi-obj 'scheme_free_code #f (ffi:_fun ffi:_pointer ffi:-> ffi:_void)))
      ; TODO
      (values
       (λ (_) (error "dynamic loading is not implemented on this version of Racket"))
       void)))
