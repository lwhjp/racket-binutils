#lang racket/base

(provide (all-defined-out))

(require binary-class
         racket/class
         racket/vector
         "params.rkt"
         "types.rkt")

(define elf-magic #"\177ELF")

(define elf%
  (class* object% (binary<%>)
    (field [class (current-elf-class)]
           [big-endian? (current-elf-big-endian?)]
           [header #f]
           [program-headers '#()]
           [sections '#()])
    (super-new)
    (define/public (read in args)
      (define ident
        (read-bytes 16 in))
      (unless (bytes=? elf-magic (subbytes ident 0 4))
        (error "invalid ELF magic"))
      (set! class
            (case (bytes-ref ident 4)
              [(1) 'elf32]
              [(2) 'elf64]
              [else (error "unknown ELF class")]))
      (set! big-endian?
            (case (bytes-ref ident 5)
              [(1) #f]
              [(2) #t]
              [else (error "unknown ELF encoding")]))
      (unless (eqv? 1 (bytes-ref ident 6))
        (error "unsupported ELF version"))
      (parameterize ([current-elf-class class]
                     [current-elf-big-endian? big-endian?])
        (set! header (read-value elf:header% in))
        (set! program-headers
              (read-value (ref (get-field program-header-offset header)
                               (array elf:program-header%
                                      (get-field program-header-count header)
                                      #:element-size (get-field program-header-size header)))
                          in))
        (set! sections
              (vector-map
               (λ (section-header)
                 (read-value (ref (get-field offset section-header) elf:section% section-header) in))
               (read-value (ref (get-field section-header-offset header)
                                (array elf:section-header%
                                       (get-field section-header-count header)
                                       #:element-size (get-field section-header-size header)))
                           in)))
        this))
    (define/public (write out)
      (define ident (make-bytes 16))
      (bytes-copy! ident 0 elf-magic)
      (bytes-set! ident 4 (case class
                            [(elf32) 1]
                            [(elf64) 2]))
      (bytes-set! ident 5 (if big-endian? 2 1))
      (bytes-set! ident 6 1)
      (write-bytes ident out)
      (parameterize ([current-elf-class class]
                     [current-elf-big-endian? big-endian?])
        (write-value elf:header% out header)
        (write-value (ref (get-field program-header-offset header)
                          (array elf:program-header%
                                 (get-field program-header-count header)
                                 #:element-size (get-field program-header-size header)))
                     out
                     program-headers)
        (write-value (ref (get-field section-header-offset header)
                          (array elf:section-header%
                                 (get-field section-header-count header)
                                 #:element-size (get-field section-header-size header)))
                     out
                     (vector-map (λ (s) (get-field header s)) sections))
        (for ([section (in-vector sections)])
          (define section-header (get-field header section))
          (write-value (ref (get-field offset section-header) elf:section%) out section))))
    (define/public (set-defaults!)
      (send header set-defaults!)
      (for ([phdr (in-vector program-headers)]) (send phdr set-defaults!))
      (for ([section (in-vector sections)])
        (send (get-field header section) set-defaults!)
        (send section set-defaults!)))
    (define/public (repack!)
      (set-defaults!)
      (define program-header-offset (get-field elf-header-size header))
      (define program-header-count (vector-length program-headers))
      (set-field! program-header-offset header (if (zero? program-header-count) 0 program-header-offset))
      (set-field! program-header-count header (vector-length program-headers))
      ; TODO: program header offsets
      (define section-header-offset (+ program-header-offset
                                       (* (get-field program-header-size header)
                                          program-header-count)))
      (set-field! section-header-offset header section-header-offset)
      (set-field! section-header-count header (vector-length sections))
      (define sections-offset (+ section-header-offset
                                 (* (get-field section-header-size header)
                                    (get-field section-header-count header))))
      (for/fold ([offset sections-offset])
                ([section (in-vector sections)])
        (define shdr (get-field header section))
        (define section-offset
          (bitwise-and
           (+ offset (sub1 (get-field alignment shdr)))
           (bitwise-not (sub1 (get-field alignment shdr)))))
        (define size
          (cond
            [(is-a? section elf:section:bits%) (bytes-length (get-field data section))]
            [(is-a? section elf:section:table%) (* (get-field entry-size shdr)
                                                   (vector-length (get-field entries section)))]
            [else 0]))
        (set-field! offset shdr section-offset)
        (set-field! size shdr size)
        (+ section-offset size))
      (void))
    (inspect #f)))

(define-binary-class elf:header%
  ([type (enum elf-half '((none . 0)
                          (relocatable . 1)
                          (executable . 2)
                          (shared . 3)
                          (core . 4)))]
   [machine               elf-half]
   [version               elf-word]
   [entry                 elf-addr]
   [program-header-offset elf-off]
   [section-header-offset elf-off]
   [flags                 elf-word]
   [elf-header-size       elf-half]
   [program-header-size   elf-half]
   [program-header-count  elf-half]
   [section-header-size   elf-half]
   [section-header-count  elf-half]
   [string-table-index    elf-half])
  (define/public (set-defaults!)
    (set! machine (or machine (case (current-elf-class) [(elf32) 3] [(elf64) 62] [else 0])))
    (set! version (or version 1))
    (set! entry (or entry 0))
    (set! flags (or flags 0))
    (set! elf-header-size (or elf-header-size 64))
    (set! program-header-size (or program-header-size 56))
    (set! section-header-size (or section-header-size 64)))
  (inspect #f))

(define-binary-class elf:program-header%
  ()
  #:dispatch
  (case (current-elf-class)
    [(elf32) (error "TODO")]
    [(elf64) elf64:program-header%])
  (inspect #f))

(define-binary-class elf64:program-header% elf:program-header%
  ([type             (enum elf-word '((null . 0)
                                      (loadable . 1)
                                      (dynamic . 2)
                                      (interpreter . 3)
                                      (note . 4)
                                      (shlib . 5)
                                      (program-header . 6)))]
   [flags            (bit-flags elf-word '((execute . #x1)
                                           (write . #x2)
                                           (read . #x4)))]
   [offset           elf-off]
   [virtual-address  elf-addr]
   [physical-address elf-addr]
   [file-size        elf-xword]
   [memory-size      elf-xword]
   [alignment        elf-xword])
  (define/public (set-defaults!)
    ; TODO
    (void))
  (inspect #f))

(define-binary-class elf:section-header%
  ([name-index elf-word]
   [type       (enum elf-word '((null . 0)
                                (program-bits . 1)
                                (symbol-table . 2)
                                (string-table . 3)
                                (relocations-with-addend . 4)
                                (hash-table . 5)
                                (dynamic . 6)
                                (note . 7)
                                (no-bits . 8)
                                (relocations . 9)
                                (shlib . 10)
                                (dynamic-symbols . 11)))]
   [flags      (bit-flags elf-xword '((write . #x1)
                                      (allocate . #x2)
                                      (execute . #x4)))]
   [address    elf-addr]
   [offset     elf-off]
   [size       elf-xword]
   [link       elf-word]
   [info       elf-word]
   [alignment  elf-xword]
   [entry-size elf-xword])
  (define/public (set-defaults!)
    (set! flags (or flags '()))
    (set! address (or address 0))
    (set! link (or link 0))
    (set! info (or info 0))
    (set! alignment (or alignment 16))
    (set! entry-size (or entry-size
                         (case type
                           [(symbol-table) 24]
                           [(relocations-with-addend) 24]
                           [else 0]))))
  (inspect #f))

(define-binary-class elf:section%
  ()
  #:dispatch
  (case (get-field type header)
    [(null) elf:section:null%]
    [(symbol-table relocations relocations-with-addend) elf:section:table%]
    [(string-table) elf:section:string-table%]
    [else elf:section:bits%])
  (init-field [header #f])
  (inspect #f))

(define-binary-class elf:section:null% elf:section%
  ()
  (define/public (set-defaults!) (void))
  (inspect #f))

(define-binary-class elf:section:bits% elf:section%
  ([data (bytestring (get-field size header))])
  (inherit-field header)
  (define/public (set-defaults!)
    (unless data (set! data #"")))
  (inspect #f))

(define-binary-class elf:section:table% elf:section%
  ([entries (let ([entry-type
                   (case (get-field type header)
                     [(symbol-table) elf:symbol%]
                     [(relocations) elf:relocation%]
                     [(relocations-with-addend) elf:relocation-with-addend%])])
              (binary
               (λ (in)
                 (let ([size (get-field size header)]
                       [entry-size (get-field entry-size header)])
                   (read-value (array entry-type (quotient size entry-size) #:element-size entry-size) in)))
               (λ (out v)
                 (write-value (array entry-type #f) out v))))])
  (inherit-field header)
  (define/public (set-defaults!)
    (unless entries (set! entries #[]))
    (for ([entry (in-vector entries)]) (send entry set-defaults!)))
  (inspect #f))

(define-binary-class elf:section:string-table% elf:section:bits%
  ()
  (define/public (add-string! str)
    (cond
      [(regexp-match-positions
        (byte-regexp (bytes-append (regexp-quote str) #"\0"))
        data)
       => caar]
      [else
       (begin0
         (bytes-length data)
         (set! data (bytes-append data str #"\0")))]))
  (define/public (get-string offset)
    (let ([m (regexp-match #"[^\0]*" data offset)])
      (and m (car m))))
  (inspect #f))

(define elf:symbol%
  (class* object% (binary<%>)
    (field [name-index #f]
           [type #f]
           [binding #f]
           [other 0]
           [section-index #f]
           [value 0]
           [size 0])
    (super-new)
    (define/public (read in args)
      (case (current-elf-class)
        [(elf64)
         (set! name-index (read-value elf-word in))
         (let* ([info (read-value u1 in)]
                [hi (arithmetic-shift info -4)]
                [lo (bitwise-and #x0F info)])
           (set! binding (case hi
                           [(0) 'local]
                           [(1) 'global]
                           [(2) 'weak]
                           [else hi]))
           (set! type (case lo
                        [(0) 'none]
                        [(1) 'object]
                        [(2) 'function]
                        [(3) 'section]
                        [(4) 'file]
                        [else lo])))
         (set! other (read-value u1 in))
         (set! section-index
               (let ([v (read-value elf-half in)])
                 (case v
                   [(#xFFF1) 'absolute]
                   [(#xFFF2) 'common]
                   [else v])))
         (set! value (read-value elf-addr in))
         (set! size (read-value elf-xword in))]
        [else (error 'elf:symbol "unsupported ELF class: ~a" (current-elf-class))])
      this)
    (define/public (write out)
      (case (current-elf-class)
        [(elf64)
         (write-value elf-word out name-index)
         (write-value u1 out
                      (bitwise-ior
                       (case binding
                         [(local) #x00]
                         [(global) #x10]
                         [(weak) #x20]
                         [else (arithmetic-shift binding 4)])
                       (case type
                         [(none) 0]
                         [(object) 1]
                         [(function) 2]
                         [(section) 3]
                         [(file) 4]
                         [else type])))
         (write-value u1 out other)
         (write-value elf-half out
                      (case section-index
                        [(absolute) #xFFF1]
                        [(common) #xFFF2]
                        [else section-index]))
         (write-value elf-addr out value)
         (write-value elf-xword out size)]
        [else (error "TODO")]))
    (define/public (set-defaults!) (void))
    (inspect #f)))

(define-binary-class elf:relocation%
  ([offset elf-addr]
   [(symbol-table-index type) (binary
                               (λ (in)
                                 (let ([v (read-value elf-xword in)])
                                   (values (arithmetic-shift v -32)
                                           (bitwise-and v #xFFFFFFFF))))
                               (λ (out v1 v2)
                                 (write-value elf-xword
                                              out
                                              (bitwise-ior
                                               (arithmetic-shift v1 32)
                                               v2))))])
  (define/public (set-defaults!) (void))
  (inspect #f))

(define-binary-class elf:relocation-with-addend% elf:relocation%
  ([addend elf-sxword])
  (inspect #f))
