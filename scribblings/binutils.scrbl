#lang scribble/manual
@(require (for-label racket/base
                     racket/class
                     racket/contract/base
                     racket/port
                     binary-class
                     binutils
                     binutils/elf
                     (only-in ffi/unsafe cpointer? cast)))

@title{Racket Binutils}

This package is not stable, and is likely to break compatibility in the future.

@defmodule[binutils]

@section{Binary Objects}

@defstruct[bin:object
           ([sections (listof bin:section?)])]{
  Represents a binary object.
}

@defstruct[bin:section
           ([name (or/c bytes? #f)]
            [size (or/c exact-nonnegative-integer? #f)]
            [writable? boolean?]
            [executable? boolean?]
            [data (or/c bytes? #f)]
            [symbols (listof bin:symbol?)]
            [relocations (listof bin:relocation?)])]{
  Represents a section of a binary object.
}

@defstruct[bin:symbol
           ([name symbol?]
            [value exact-integer?]
            [size (or/c exact-nonnegative-integer? #f)]
            [binding (one-of/c 'local 'global 'weak #f)]
            [type (one-of/c 'object 'function #f)])]{
  Represents a symbol for linking.
}

@defstruct[bin:relocation
           ([offset exact-nonnegative-integer?]
            [size exact-nonnegative-integer?]
            [symbol symbol?]
            [type (one-of/c 'address 'offset 'value 'size)]
            [addend exact-integer?])]{
  Represents a relocation.
}

@section{Linking}

@defproc[(link-object/local/relative [obj bin:object?]) bin:object?]{
  Resolves local relative references in @racket[obj].
}

@section{Dynamic Loading}

@bold{This is unsafe (particularly if there are bugs, which is
likely) - it could cause crashes or worse!}

@definterface[dynamic-object<%> ()]{
  Represents an object which has been loaded into memory.
  @defmethod[(symbols) (listof symbol?)]{
    Returns a list of symbols exported by the object.
  }
  @defmethod[(symbol-ref [name symbol?]) cpointer?]{
    Returns a pointer to the requested symbol. This may be
    @racket[cast] into a function type.
  }
}

@defproc[(load-object [obj bin:object?]) (is-a?/c dynamic-object<%>)]{
  Loads @racket[obj] into memory.
}

@section{ELF Support}

@defmodule[binutils/elf]

@subsection{Reading and Writing}

@defproc[(read-elf [in input-port? (current-input-port)]) (is-a?/c elf%)]{
  Reads an ELF binary from @racket[in].
}

@defproc[(write-elf [v (is-a?/c elf%)]
                    [out output-port? (current-output-port)])
         void?]{
  Writes @racket[v] as an ELF binary to @racket[out].
}

@subsection{Conversion}

@defproc[(elf->bin:object [elf (is-a?/c elf%)]) (bin:object?)]{
  Converts @racket[elf] to a generic object. Some information will be
  lost in this process.
}

@defproc[(bin:object->elf [obj bin:object?]) (is-a?/c elf%)]{
  Converts @racket[obj] to an ELF object.
}

@subsection{ELF Data Structures}

These classes represent the ELF format. For details, see the
ELF specification and the source code of this module.

@defclass[elf% object% (binary<%>)]{
  @defmethod[(repack!) void?]{
    Adjusts sizes and offsets to fit the contents of the object.
  }
}

@subsection{Utilities}

@defproc[(system-elf-class) (or/c 'elf32 'elf64)]{
  Returns the appropriate ELF class for the current processor.
}
