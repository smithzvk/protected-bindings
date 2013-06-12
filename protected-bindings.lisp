
(defpackage :protected-bindings
  (:use :cl)
  (:export #:with-protected-bindings*))

(in-package :protected-bindings)

(defmacro with-protected-binding ((binding-var binding-form &rest binding-clean-up)
                                  &body body)
  "Set up a single protected binding.  See WITH-PROTECTED-BINDINGS* for more
info."
  `(let ((,binding-var ,binding-form))
     ,(when (equal (first binding-clean-up) :finalize)
        (pop binding-clean-up)
        `(tg:finalize ,binding-var ,(pop binding-clean-up)))
     (unwind-protect
          (progn ,@body)
       ,@binding-clean-up)))

;; @This is particularly useful as this is actually annoying to do correctly.

(defmacro with-protected-bindings* (bindings &body body)
  "This sets up protected bindings, a bindings that has some clean-up code that
will always be run when the binding expires.  A binding is of the form:

 (sym value [:finalize evaluates-to-finalize-thunk] . clean-up-forms)

CLEAN-UP-FORMS will be guaranteed to run via unwind-protect.  If there is
a :FINALIZE value, the finalizer is instantiated."
  (if bindings
      `(with-protected-binding ,(first bindings)
         (with-protected-bindings* ,(rest bindings)
           ,@body))
      `(progn ,@body)))

