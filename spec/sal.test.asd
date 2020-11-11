; vim: ft=lisp et
(in-package :asdf)
(defsystem "sal.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "sal")
  :components
  ((:file "sal"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :sal args)))