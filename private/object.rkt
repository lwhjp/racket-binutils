#lang racket/base

(provide (all-defined-out))

(struct bin:symbol
  (name
   value
   size
   binding ; (one-of/c #f 'local 'global 'weak)
   type) ; (one-of/c #f 'object 'function)
  #:transparent)

(struct bin:relocation
  (offset
   size
   symbol
   type ; (one-of/c 'value 'offset 'size)
   addend)
  #:transparent)

(struct bin:section
  (name
   size
   writable?
   executable?
   data
   symbols
   relocations)
  #:transparent)

(struct bin:object
  (sections)
  #:transparent)
