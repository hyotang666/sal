(defpackage :sal.spec
  (:use :cl :jingoh :sal))
(in-package :sal.spec)
(setup :sal)

(requirements-about WRITE-OBJECT :doc-type function
                    :around (read-from-string
                              (with-output-to-string (*standard-output*)
                                (call-body))))

;;;; Description:

#+syntax (WRITE-OBJECT object
           &key
           ((:stream *standard-output*) *standard-output*)
           ((:pretty *print-pretty*) *print-pretty*)
           ((:circle *print-circle*) *print-circle*)
           ((:gensym *print-gensym*) *print-gensym*)
           ((:readably *print-readably*) *print-readably*))
; => result

;;;; Arguments and Values:

; object := 

; stream := 

; pretty := 

; circle := 

; gensym := 

; readably := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests.
;;; NUMBER
#?(write-object 0) => 0
#?(write-object 1.0) => 1.0
#?(write-object #c(1 2)) => #C(1 2)
#?(write-object 1/2) => 1/2

;;; CHARACTER
#?(write-object #\a) => #\a

;;; SYMBOL
#?(write-object 'a) => A
#?(write-object :a) => :A
#?(write-object '#:a) :satisfies (lambda (result)
                                   (& (symbolp result)
                                      (null (symbol-package result))
                                      (string= "A" result)))

;;; STRING
#?(write-object "string") => "string"
,:test equal

;;; FUNCTION
#?(write-object #'car)
:satisfies (lambda (result)
             (& (functionp result)
                (eq result #'car)))
#?(write-object (lambda ())) :signals sal::give-up
#?(write-object (constantly t)) :signals sal::give-up
#?(write-object (formatter "~A")) :signals sal::give-up
#?(write-object (flet ((a () :a)) #'a)) :signals sal::give-up
#?(write-object (labels ((a () :a)) #'a)) :signals sal::give-up
#?(write-object #'documentation) :satisfies (lambda (result)
                                              (& (typep result 'generic-function)
                                                 (eq result #'documentation)))

;;; PATHNAME
#?(write-object #P"foo/bar.hoge" :circle t) => #P"foo/bar.hoge"
,:test equal

;;; CONS
;; Cons
#?(write-object '(:a . :b) :circle t) => (:A . :B)
,:test equal
;; Proper-list
#?(write-object '(1 2 3) :circle t) => (1 2 3)
,:test equal
;; Dotted list
#?(write-object '(1 2 . 3) :circle t) => (1 2 . 3)
,:test equal
;; Circler-list
#?(write-object (let ((list (list 1 2)))
                  (nconc list list))
                :circle t)
:satisfies (lambda (result)
             (& (listp result)
                (null (list-length result)) ; as circle-listp
                (eql 1 (car result))
                (eql 2 (cadr result))
                (eq result (cddr result))))

;;; PACKAGE
#?(write-object *package*) :satisfies (lambda (result)
                                        (eq result *package*))

;;; HASH-TABLE
#?(write-object (let ((ht (make-hash-table)))
                  (setf (gethash :a ht) 0
                        (gethash :b ht) 1)
                  ht)
                :circle t)
:satisfies (lambda (result)
             (let ((ht (make-hash-table)))
               (setf (gethash :a ht) 0
                     (gethash :b ht) 1)
               (equalp result ht)))

;;; STANDARD-CLASS
#?(defclass test-class () ((a :initarg :a :initform nil :accessor test-class-a)))
:be-the class
,:around nil

#?(write-object (make-instance 'test-class :a 0) :circle t)
:satisfies (lambda (result)
             (& (typep result 'test-class)
                (eql 0 (test-class-a result))))

;;; CONDITION
#?(write-object (make-condition 'simple-error :format-control "format ~S"
                                :format-arguments '(:argument))
                :circle t)
:satisfies (lambda (result)
             (& (typep result 'simple-error)
                (equal "format ~S" (simple-condition-format-control result))
                (equal '(:argument) (simple-condition-format-arguments result))))

;;; BIT-VECTOR
#?(write-object #*10101) => #*10101
,:test equal

;;; STRUCTURE
#?(write-object (make-instance 'jingoh:issue
                               :form '(form)
                               :expected :exprected
                               :actual "actual"
                               :line 0)
                :circle t)
:satisfies (lambda (result)
             (& (typep result 'jingoh:issue)
                (equal '(form) (jingoh::issue-form result))
                (eq :exprected (jingoh::issue-expected result))
                (equal "actual" (jingoh::issue-actual result))
                (eql 0 (jingoh::issue-line result))))

;;; ARRAY
#?(write-object #(1 2 3) :circle t)
=> #(1 2 3)
,:test equalp

#?(write-object #2A((1 2) (3 4)) :circle t)
=> #2A((1 2) (3 4))
,:test equalp

;;; STREAM
#?(write-object *standard-output*) :signals sal::give-up
,:with-restarts use-value

