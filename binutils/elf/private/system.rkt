#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [system-elf-class (-> (or/c 'elf32 'elf64))]))

(define (system-elf-class)
  (case (system-type 'word)
    [(32) 'elf32]
    [(64) 'elf64]))
