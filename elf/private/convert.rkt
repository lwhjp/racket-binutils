#lang racket/base

(require racket/class
         racket/contract/base)

(provide
 (contract-out
  [elf->bin:object (-> (is-a?/c elf%) bin:object?)]))

(require "../../base.rkt"
         "elf.rkt")

(define (elf->bin:object elf)
  (define (get-name idx [strtab-idx (get-field string-table-index (get-field header elf))])
    (let ([strtab (vector-ref (get-field sections elf) strtab-idx)])
      (and (is-a? strtab elf:section:string-table%)
           (let ([str (send strtab get-string idx)])
             (and str (not (zero? (bytes-length str))) str)))))
  (define (get-name/sym idx strtab-idx)
    (cond
      [(get-name idx strtab-idx) => (compose1 string->symbol bytes->string/latin-1)]
      [else #f]))
  (define (get-section idx)
    (vector-ref (get-field sections elf) idx))
  (define (get-section-name idx)
    (get-name (get-field header (get-section idx))))
  (define (get-symbol-name/idx idx table-idx)
    (define symtab (vector-ref (get-field sections elf) table-idx))
    (define sym (vector-ref (get-field entries symtab) idx))
    (get-symbol-name sym symtab))
  (define (get-symbol-name sym symtab)
    (cond
      [(eq? 'section (get-field type sym))
       (define sec (get-section (get-field section-index sym)))
       (get-name/sym (get-field name-index (get-field header sec))
                     (get-field string-table-index (get-field header elf)))]
      [else (get-name/sym (get-field name-index sym) (get-field link (get-field header symtab)))]))
  (define (section-has-flag? f s)
    (and (memq f (get-field flags (get-field header s))) #t))
  (define (section-symbols section-idx)
    (for*/list ([symtab (in-vector (get-field sections elf))]
                #:when (eq? 'symbol-table (get-field type (get-field header symtab)))
                [sym (in-vector (get-field entries symtab))]
                #:when (eqv? section-idx (get-field section-index sym)))
      (bin:symbol
       (get-symbol-name sym symtab)
       (get-field value sym)
       (get-field size sym)
       (get-field binding sym)
       (get-field type sym))))
  (define (section-relocations section-idx)
    (for*/list ([reltab (in-vector (get-field sections elf))]
                [reltab-hdr (in-value (get-field header reltab))]
                #:when (memq (get-field type reltab-hdr)
                             '(relocations relocations-with-addend))
                #:when (eqv? section-idx (get-field info reltab-hdr))
                [rel (in-vector (get-field entries reltab))])
      (bin:relocation
       (get-field offset rel)
       #f ; TODO: size
       (get-symbol-name/idx (get-field symbol-table-index rel) (get-field link reltab-hdr))
       #f ; TODO: type
       (if (is-a? rel elf:relocation-with-addend%) (get-field addend rel) 0))))
  (bin:object
   (for/list ([section-idx (in-naturals)]
              [section (in-vector (get-field sections elf))]
              #:when (section-has-flag? 'allocate section))
     (define header (get-field header section))
     (bin:section
      (get-name (get-field name-index header))
      (get-field size header)
      (section-has-flag? 'write section)
      (section-has-flag? 'execute section)
      (and (is-a? section elf:section:bits%) (get-field data section))
      (section-symbols section-idx)
      (section-relocations section-idx)))))
