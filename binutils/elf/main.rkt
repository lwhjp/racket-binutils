#lang racket/base

(require racket/contract/base
         racket/class)

(provide
 (all-from-out
  "private/elf.rkt"
  "private/params.rkt"
  "private/system.rkt")
 (contract-out
  [read-elf (->* () (input-port?) (is-a?/c elf%))]
  [write-elf (->* ((is-a?/c elf%)) (output-port?) void?)]))

(require binary-class
         "private/elf.rkt"
         "private/params.rkt"
         "private/system.rkt")

(define (read-elf [in (current-input-port)])
  (read-value elf% in))

(define (write-elf v [out (current-output-port)])
  (write-value elf% out v))
