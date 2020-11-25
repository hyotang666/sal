# SAL 1.3.0
## What is this?
Trivial serializer. Saving object as S-Exp.

## Alternatives and differences.
### [CL-MARSHALL](https://github.com/wlbr/cl-marshal)
* Human-readable.
* Introducing own notation rules.
* Can not serialize some objects (?). (e.g. `STREAM`).
### [TRIVIAL-HASH-TABLE-SERIALIZE](https://gitlab.com/ediethelm/trivial-hashtable-serialize)
* Human-readable.
* Scoping only `HASH-TABLE`.
### [CL-STORE](https://github.com/kingcons/cl-store)
* Not human-readable.
* Can not serialize some objects. (README says 'not all yet')
### HU.DWIM.SERIALIZER
* Not human-readable.
### [SERIALIZABLE-OBJECT](https://github.com/guicho271828/serializable-object/)
* Not human-readable.
* Scoping only CLOS class objects (?).
* Not portable i.e. you can not load the object which is saved with sbcl with ccl.
### SAL
* Human-readable.
* Can not serialize some objects. (e.g. `STREAM`, `CLOSURE` ... and more?)
#### Against CL-MARSHAL
I do not want to learn its own notation rules.
SAL generates ordinary common lisp form.
#### Against TRIVIAL-HASH-TABLE-SERIALIZE
I need to serialize some other objects.
#### Against CL-STORE
I want a readable one.
#### Against HU.DWIM.SERIALIZER
I want a readable one.
#### Against SERIALIZABLE-OBJECT
I want a readable one.
I do not want to make the object special.

## Usage

```lisp
* (with-open-file (s "path" :direction :output :if-does-not-exist :create)
    (write-object object :stream s :circle t))
NIL

* (load-object "path") => object
```

### &KEY :READABLY
To guarantee not generates unreadable object, specify `:READABLY` with `T`.

### &KEY :PRETTY
To optimize spaces, specify `:PRETTY` with `NIL`.

### &KEY :GENSYM, :CIRCLE.
To write a reloadable form, you must specify one of them.
#### :GENSYM
* Generates human-friendly form.
* *ANY* uninterned symbols become normal symbols.
* Symbols may conflict.
#### :CIRCLE
* Generates less human-friendly form.
* Guarantee correctness.

### OBJECT-FORM
You can add the method `OBJECT-FORM` for your specific object.
`OBJECT-FORM` should return lisp form that regenerates object when `CL:EVAL`ed.

To support circler reference,

1. Store object as key and gensymed symbol as value in the hash-table `*KNOWN-FORM*`.
2. Generate a `LET` form which binds gensymed var with object regeneration form and return it via var.

For details, see [source code](src/sal.lisp).

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with

## Installation

