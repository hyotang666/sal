(in-package :cl-user)

(defpackage :sal
  (:use :cl)
  (:export
    #:write-object
    #:load-object
    #:load-objects
    #:object-form
    #:known-form*
    ))

(in-package :sal)

;;;; SPECIAL VAR

(defvar *known-form* nil)

;;;; FORM

(defgeneric object-form (object))

;;; NUMBER

(defmethod object-form ((number number)) number)

;;; CHARACTER

(defmethod object-form ((character character)) character)

;;; SYMBOL

(defmethod object-form ((symbol symbol)) symbol)

;;; STRING

(defmethod object-form ((string string)) string)

;;; FUNCTION

(defmethod object-form ((function function))
  (let ((name (millet:function-name function)))
    (if name
        `#',name
        (or (function-lambda-expression function)
            (error "Could not make object-form. ~S" function)))))

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
  (labels ((rec (list)
             (typecase list
               (null `(list ,@(mapcar #'object-form cons)))
               (atom
                `(list* ,@(mapcar #'object-form (butlast cons))
                        ,@(let ((cons (last cons)))
                            `(,(object-form (car cons))
                              ,(object-form (cdr cons))))))
               (cons (rec (cdr list))))))
    (rec cons)))

;;; PACKAGE

(defmethod object-form ((package package))
  `(find-package ,(package-name package)))

;;; HASH-TABLE

(defmethod object-form ((hash-table hash-table))
  (let* ((var (gensym "HT")))
    (pushnew (cons hash-table var) *known-form* :key #'car)
    `(let ((,var
            (make-hash-table :test ,(let ((test (hash-table-test hash-table)))
                                      (etypecase test
                                        (symbol `',test)
                                        (function (object-form test))))
                             :size ,(hash-table-size hash-table)
                             :rehash-size ,(hash-table-rehash-size hash-table)
                             :rehash-threshold ,(hash-table-rehash-threshold
                                                  hash-table))))
       ,@(unless (zerop (hash-table-count hash-table))
           `((setf ,@(loop :for k :being :each :hash-key :of hash-table :using
                                (:hash-value v)
                           :collect `(gethash ,(object-form k) ,var)
                           :collect (cond
                                      ((let ((seen? (assoc v *known-form*)))
                                         (when seen?
                                           (cdr seen?))))
                                      (t (object-form v)))))))
       ,var)))

;;; STANDARD-CLASS

(defmethod object-form ((object standard-object))
  (let ((var (gensym)) (class (class-of object)))
    (pushnew (cons object var) *known-form* :key #'car)
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
                                       :and :collect (let* ((v
                                                             (slot-value object
                                                                         name))
                                                            (seen?
                                                             (assoc v
                                                                    *known-form*)))
                                                       (if seen?
                                                           (cdr seen?)
                                                           (form v)))))))
       ,var)))

;;; CONDITION

(defmethod object-form ((object condition))
  (let ((var (gensym "CONDITION")) (class (class-of object)))
    (pushnew (cons object var) *known-form* :key #'car)
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
                                        :and :collect (let* ((v
                                                              (slot-value
                                                                object name))
                                                             (seen?
                                                              (assoc v
                                                                     *known-form*)))
                                                        (if seen?
                                                            (cdr seen?)
                                                            (form v)))))))
       ,var)))

;;; BIT-VECTOR

(defmethod object-form ((bit-vector bit-vector)) bit-vector)

;;; STRUCTURE

(defmethod object-form ((object structure-object))
  (let ((var (gensym "STRUCTURE")) (class (class-of object)))
    (pushnew (cons object var) *known-form* :key #'car)
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
                                                     (or (cdr
                                                           (assoc v
                                                                  *known-form*))
                                                         (form v)))))))
       ,var)))

;;; ARRAY
;;; STREAM
;;;; WRITE-OBJECT

(defun write-object
       (object
        &key ((:stream *standard-output*) *standard-output*)
        ((:pretty *print-pretty*) *print-pretty*)
        ((:circle *print-circle*) *print-circle*)
        ((:gensym *print-gensym*) *print-gensym*)
        ((:readably *print-readably*) *print-readably*))
  (pprint-logical-block (*standard-output* nil :prefix "#.")
    (write (object-form object))))

;;;; LOAD-OBJECT

(defun load-object (pathname) (with-open-file (s pathname) (read s)))

(defun load-objects (pathname)
  (with-open-file (s pathname)
    (loop :with tag := '#:eof
          :for object := (read s nil tag)
          :until (eq object tag)
          :collect object)))
