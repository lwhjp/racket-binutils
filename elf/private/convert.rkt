#lang racket/base

(require racket/class
         racket/contract/base)

(provide
 (contract-out
  [elf->bin:object (-> (is-a?/c elf%) bin:object?)]
  [bin:object->elf (-> bin:object? (is-a?/c elf%))]))

(require racket/list
         racket/vector
         "../../base.rkt"
         "elf.rkt"
         "params.rkt")

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
       ; TODO: arch-specific size helper
       (if (and (eq? 'elf64 (current-elf-class)) (eqv? 1 (get-field type rel))) 8 4)
       (get-symbol-name/idx (get-field symbol-table-index rel) (get-field link reltab-hdr))
       (case (get-field type rel)
         [(1) 'value]
         [(2) 'offset])
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

(define (bin:object->elf obj)
  (define strings (new elf:section:string-table%))
  (set-field! data strings #"\0")
  (define (make-name bstr) (send strings add-string! bstr))
  (define-values (global-symbols local-symbols)
    (partition
     (λ (sym) (eq? 'global (get-field binding sym)))
     (for/list ([section-idx (in-naturals 1)]
                [section (in-list (bin:object-sections obj))]
                #:when #t
                [symbol (in-list (bin:section-symbols section))])
       (define esym (new elf:symbol%))
       (set-field! name-index esym (make-name (string->bytes/latin-1 (symbol->string (bin:symbol-name symbol)))))
       (set-field! type esym (or (bin:symbol-type symbol) 'none))
       (set-field! binding esym (bin:symbol-binding symbol))
       (set-field! section-index esym section-idx)
       (set-field! value esym (bin:symbol-value symbol))
       (set-field! size esym (or (bin:symbol-size symbol) 0))
       esym)))
  (define symbols
    (list->vector
     (append
      (let ()
        ; Null symbol
        (define esym (new elf:symbol%))
        (set-field! name-index esym 0)
        (set-field! type esym 'none)
        (set-field! binding esym 'local)
        (set-field! section-index esym 0)
        (list esym))
      ; omit file & section symbols
      local-symbols
      global-symbols)))
  (define symbol-idx-map
    (for/hasheq ([sym (in-vector symbols)]
                 [idx (in-naturals)])
      (values (string->symbol
               (bytes->string/latin-1
                (send strings get-string (get-field name-index sym))))
              idx)))
  (define null-sections
    (vector
     (let ()
       (define esec (new elf:section:null%))
       (define ehdr (new elf:section-header%))
       (set-field! name-index ehdr 0)
       (set-field! type ehdr 'null)
       (set-field! header esec ehdr)
       esec)))
  (define prog-sections
    (for/vector ([section (in-list (bin:object-sections obj))])
      (define esec (new elf:section:bits%))
      (define ehdr (new elf:section-header%))
      (set-field! name-index ehdr (make-name (bin:section-name section)))
      (set-field! type ehdr 'program-bits)
      (set-field! flags ehdr (append '(allocate)
                                     (if (bin:section-writable? section) '(write) '())
                                     (if (bin:section-executable? section) '(execute) '())))
      (set-field! header esec ehdr)
      (set-field! data esec (bin:section-data section))
      esec))
  (define strtab-idx (+ (vector-length prog-sections) 1))
  (define strtab+symtab
    (vector
     (let ()
       (define ehdr (new elf:section-header%))
       (set-field! name-index ehdr (make-name #".strtab"))
       (set-field! type ehdr 'string-table)
       (set-field! header strings ehdr)
       strings)
     (let ()
       (define esec (new elf:section:table%))
       (define ehdr (new elf:section-header%))
       (set-field! name-index ehdr (make-name #".symtab"))
       (set-field! type ehdr 'symbol-table)
       (set-field! link ehdr strtab-idx)
       (set-field! info ehdr (- (vector-length symbols) (length global-symbols)))
       (set-field! header esec ehdr)
       (set-field! entries esec symbols)
       esec)))
  (define reltabs
    (for/vector ([section-idx (in-naturals 1)]
                 [section (in-list (bin:object-sections obj))]
                 #:unless (null? (bin:section-relocations section)))
      (define relocations
        (for/vector ([rel (in-list (bin:section-relocations section))])
          (define sym-idx (hash-ref symbol-idx-map (bin:relocation-symbol rel)))
          (define erel (new elf:relocation-with-addend%))
          (set-field! offset erel (bin:relocation-offset rel))
          (set-field! symbol-table-index erel sym-idx)
          ; TODO: arch-specific type helper
          (set-field! type erel (case (bin:relocation-type rel)
                                  [(value) 1]
                                  [(offset) 2]))
          (set-field! addend erel (bin:relocation-addend rel))
          erel))
      (define esec (new elf:section:table%))
      (define ehdr (new elf:section-header%))
      (set-field! name-index ehdr (make-name (bytes-append #".rela" (bin:section-name section))))
      (set-field! type ehdr 'relocations-with-addend)
      (set-field! link ehdr (add1 strtab-idx))
      (set-field! info ehdr section-idx)
      (set-field! header esec ehdr)
      (set-field! entries esec relocations)
      esec))
  (define elf (new elf%))
  (define ehdr (new elf:header%))
  (set-field! type ehdr 'relocatable)
  (set-field! string-table-index ehdr strtab-idx)
  (set-field! header elf ehdr)
  (set-field! sections elf (vector-append null-sections prog-sections strtab+symtab reltabs))
  (send elf repack!)
  elf)
