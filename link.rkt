#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [apply-relocations (-> (listof bin:relocation?)
                         exact-nonnegative-integer?
                         (-> symbol? (or/c exact-nonnegative-integer? #f))
                         (-> exact-nonnegative-integer? bytes? any/c)
                         (listof bin:relocation?))]
  [link-object/local/relative (-> bin:object? bin:object?)]))

(require racket/list
         "private/object.rkt")

(define (apply-relocations rels base get-sym write!)
  (let loop ([rels rels])
    (cond
      [(null? rels) '()]
      [(get-sym (bin:relocation-symbol (car rels)))
       => (λ (sym-v)
            (define rel (car rels))
            (define offset (bin:relocation-offset rel))
            (define rel-v
              (case (bin:relocation-type rel)
                [(offset) (- sym-v (+ base offset))]
                [else sym-v]))
            (write! offset (integer->integer-bytes
                            (+ rel-v (bin:relocation-addend rel))
                            (bin:relocation-size rel)
                            #t
                            ; FIXME: endianness
                            (system-big-endian?)))
            (loop (cdr rels)))]
      [else (cons (car rels) (loop (cdr rels)))])))

(define (link-object/local/relative obj)
  (bin:object
   (for/list ([section (in-list (bin:object-sections obj))])
     (define symbols
       (for/hasheq ([s (in-list (bin:section-symbols section))]
                    #:when (bin:symbol-binding s))
         (values (bin:symbol-name s) (bin:symbol-value s))))
     (define-values (relative absolute)
       (partition (λ (r) (eq? 'offset (bin:relocation-type r)))
                  (bin:section-relocations section)))
     (define text
       (bytes-copy (bin:section-data section)))
     (struct-copy
      bin:section section
      [relocations (append
                    (apply-relocations
                     relative
                     0
                     (λ (s) (hash-ref symbols s #f))
                     (λ (pos bstr) (bytes-copy! text pos bstr)))
                    absolute)]
      [data text]))))
