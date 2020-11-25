(in-package :cl-user)

(defpackage :sal
  (:use :cl)
  (:export #:write-object
           #:load-object
           #:load-objects
           #:object-form
           #:*known-form*))

(in-package :sal)

;;;; SPECIAL VAR

(defvar *known-form* (make-hash-table :test 'eq))

;;;; FORM

(defgeneric object-form (object))

;;; NUMBER

(defmethod object-form ((number number)) number)

;;; CHARACTER

(defmethod object-form ((character character)) character)

;;; SYMBOL

(defmethod object-form ((symbol symbol)) `',symbol)

;;; STRING

(defmethod object-form ((string string)) string)

;;; FUNCTION

(defmethod object-form ((function function))
  (let ((name (millet:function-name function)))
    (if name
        `#',name
        (error "Could not make object-form. ~S" function))))

;;; PATHNAME

(defmethod object-form ((pathname pathname))
  `(make-pathname :host ,(object-form (pathname-host pathname))
                  :device ,(object-form (pathname-device pathname))
                  :directory ,(object-form (pathname-directory pathname))
                  :name ,(object-form (pathname-name pathname))
                  :type ,(object-form (pathname-type pathname))
                  :version ,(object-form (pathname-version pathname))))

;;; CONS

(defmethod object-form ((cons cons))
  (let ((var (gensym "CONS")))
    (setf (gethash cons *known-form*) var)
    `(let ((,var
            (cons
              ,(or (gethash (car cons) *known-form*) (object-form (car cons)))
              ,(or (gethash (cdr cons) *known-form*)
                   (object-form (cdr cons))))))
       ,var)))

;;; PACKAGE

(defmethod object-form ((package package))
  `(find-package ,(package-name package)))

;;; HASH-TABLE

(defmethod object-form ((hash-table hash-table))
  (let* ((var (gensym "HT")))
    (setf (gethash hash-table *known-form*) var)
    `(let ((,var
            (make-hash-table :test ,(object-form (hash-table-test hash-table))
                             :size ,(hash-table-size hash-table)
                             :rehash-size ,(hash-table-rehash-size hash-table)
                             :rehash-threshold ,(hash-table-rehash-threshold
                                                  hash-table))))
       ,@(unless (zerop (hash-table-count hash-table))
           `((setf ,@(loop :for k :being :each :hash-key :of hash-table :using
                                (:hash-value v)
                           :collect `(gethash ,(object-form k) ,var)
                           :collect (or (gethash v *known-form*)
                                        (object-form v))))))
       ,var)))

;;; STANDARD-CLASS

(defmethod object-form ((object standard-object))
  (let ((var (gensym)) (class (class-of object)))
    (setf (gethash object *known-form*) var)
    `(let ((,var
            (make-instance ',(class-name class)
                           ,@(loop :for slot :in (c2mop:class-slots class)
                                   :for name
                                        := (c2mop:slot-definition-name slot)
                                   :for initargs
                                        := (c2mop:slot-definition-initargs slot)
                                   :if (slot-boundp object name)
                                     :if initargs
                                       :collect (car initargs)
                                       :and :collect (let ((v
                                                            (slot-value object
                                                                        name)))
                                                       (or (gethash v
                                                                    *known-form*)
                                                           (object-form v)))))))
       ,var)))

;;; CONDITION

(defmethod object-form ((object condition))
  (let ((var (gensym "CONDITION")) (class (class-of object)))
    (setf (gethash object *known-form*) var)
    `(let ((,var
            (make-condition ',(class-name class)
                            ,@(loop :for slot :in (c2mop:class-slots class)
                                    :for name
                                         := (c2mop:slot-definition-name slot)
                                    :for initargs
                                         := (c2mop:slot-definition-initargs
                                              slot)
                                    :if (slot-boundp object name)
                                      :if initargs
                                        :collect (car initargs)
                                        :and :collect (let ((v
                                                             (slot-value object
                                                                         name)))
                                                        (or (gethash v
                                                                     *known-form*)
                                                            (object-form
                                                              v)))))))
       ,var)))

;;; BIT-VECTOR

(defmethod object-form ((bit-vector bit-vector)) bit-vector)

;;; STRUCTURE

(defmethod object-form ((object structure-object))
  (let ((var (gensym "STRUCTURE")) (class (class-of object)))
    (setf (gethash object *known-form*) var)
    `(let ((,var
            (make-instance ',(type-of object)
                           ,@(loop :for slot :in (c2mop:class-slots class)
                                   :for name
                                        := (c2mop:slot-definition-name slot)
                                   :if (slot-boundp object name)
                                     :collect (intern (symbol-name name)
                                                      :keyword)
                                     :and :collect (let ((v
                                                          (slot-value object
                                                                      name)))
                                                     (or (gethash v
                                                                  *known-form*)
                                                         (object-form v)))))))
       ,var)))

;;; ARRAY

(defun array-contents-form (array)
  (labels ((rec (dimensions acc)
             (if (endp (cdr dimensions))
                 `(list
                    ,@(loop :with indice
                                  := (revappend ; Do not NRECONC!
                                                acc (list nil))
                            :for i :upfrom 0 :below (car dimensions)
                            :do (rplaca (last indice) i)
                            :collect (object-form
                                       (apply #'aref array indice))))
                 `(list
                    ,@(loop :for i :upfrom 0 :below (car dimensions)
                            :collect (rec (cdr dimensions) (cons i acc)))))))
    (rec (array-dimensions array) nil)))

(defmethod object-form ((array array))
  (let ((var (gensym "ARRAY")))
    (setf (gethash array *known-form*) var)
    `(let ((,var
            (make-array ',(array-dimensions array)
                        :element-type ',(array-element-type array)
                        :adjustable ,(adjustable-array-p array)
                        :initial-contents ,(array-contents-form array))))
       ,var)))

;;; STREAM
;;;; WRITE-OBJECT

(defun write-object
       (object
        &key ((:stream *standard-output*) *standard-output*)
        ((:pretty *print-pretty*) *print-pretty*)
        ((:circle *print-circle*) *print-circle*)
        ((:gensym *print-gensym*) *print-gensym*)
        ((:readably *print-readably*) *print-readably*))
  (let ((*known-form* (make-hash-table :test 'eq)))
    (pprint-logical-block (*standard-output* nil :prefix "#.")
      (write (object-form object)))))

;;;; LOAD-OBJECT

(defun load-object (pathname) (with-open-file (s pathname) (read s)))

(defun load-objects (pathname)
  (with-open-file (s pathname)
    (loop :with tag := '#:eof
          :for object := (read s nil tag)
          :until (eq object tag)
          :collect object)))