#lang racket/base

(provide (all-defined-out))

(require binary-class
         "params.rkt")

(define (array #:element-size [element-size #f]
               base-type count . init-vs)
  ; TODO: skip padding if applicable
  (binary
   (λ (in)
     (for/vector ([i (in-range count)])
       (apply read-value base-type in init-vs)))
   (λ (out vs)
     (for ([v (in-vector vs)])
       (write-value base-type out v)))))

(define (bit-flags base-type lst)
  (binary
   (λ (in)
     (define v (read-value base-type in))
     (for/list ([flag (in-list lst)]
                #:unless (zero? (bitwise-and (cdr flag) v)))
       (car flag)))
   (λ (out flags)
     (write-value
      base-type
      out
      (for/fold ([v 0])
                ([flag (in-list flags)])
        (bitwise-ior
         v
         (cond
           [(assoc flag lst) => cdr]
           [else (error 'bit-flags "unknown flag: ~a" flag)])))))))

(define (enum base-type lst)
  (define lookup-list
    (map (λ (pair) (cons (cdr pair) (car pair))) lst))
  (binary
   (λ (in)
     (define v (read-value base-type in))
     (cond
       [(assoc v lookup-list) => cdr]
       [else v]))
   (λ (out v)
     (write-value
      base-type
      out
      (cond
        [(assoc v lst) => cdr]
        [else v])))))

(define-values (elf-addr elf-off elf-half elf-word elf-sword elf-xword elf-sxword)
  (apply
   values
   (map
    (λ (signed? size/32 size/64)
      (define-syntax-rule (current-size)
        (if (eq? 'elf32 (current-elf-class)) size/32 size/64))
      (binary
       (λ (in)
         (integer-bytes->integer
          (read-bytes (current-size) in)
          signed?
          (current-elf-big-endian?)))
       (λ (out v)
         (write-bytes
          (integer->integer-bytes
           v
           (current-size)
           signed?
           (current-elf-big-endian?))
          out))))
    '(#f #f #f #f #t #f #t)
    '( 4  4  2  4  4  4  4)    ; elf32
    '( 8  8  2  4  4  8  8)))) ; elf64
