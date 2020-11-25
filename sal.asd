; vim: ft=lisp et
(in-package :asdf)
(defsystem "sal"
  :version
  "1.3.1"
  :depends-on
  (
   "millet" ; Wrapper for implementation dependent utilities.
   "closer-mop" ; Wrapper for Meta-object-protocols.
   "structure-ext" ; Tiny Structure extensioin.
   "prompt-for" ; Type safe user input.
   )
  :pathname
  "src/"
  :components
  ((:file "sal"))
  :author "SATO Shinichi"
  :license "MIT"
  :description "S-Expression based object serializer."
  )

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "sal").
(defmethod component-depends-on ((o test-op) (c (eql (find-system "sal"))))
  (append (call-next-method) '((test-op "sal.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "sal")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "sal"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
