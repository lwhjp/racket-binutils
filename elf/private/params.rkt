#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [current-elf-class (parameter/c (or/c 'elf32 'elf64))]
  [current-elf-big-endian? (parameter/c boolean?)]))

(require "system.rkt")

(define current-elf-class
  (make-parameter (system-elf-class)))

(define current-elf-big-endian?
  (make-parameter (system-big-endian?)))
