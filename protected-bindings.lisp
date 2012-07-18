
(defpackage :protected-bindings
  (:use :cl)
  (:export #:with-protected-binding
           #:with-protected-bindings))

(in-package :protected-bindings)

(defmacro with-protected-binding ((binding-var binding-form &rest binding-clean-up)
                                  &body body)
  "This sets up a protected binding, a binding that has some clean-up code that
will always be run when the binding expires.

Usually, the forms specified at the end of the binding form will be placed in
the clean-up clause of an unwind-protect.  If the clean-up forms starts with
keyword :finalize, then the following code will be exectuted as if in a progn and
the result will be set as a finalization thunk for the object (or produce an
error if finalizers are not supported)."
  (if (equal (first binding-clean-up) :finalize)
      `(let ((,binding-var ,binding-form))
         (tg:finalize ,binding-var (progn ,@(rest binding-clean-up)))
         ,@body)
      `(let ((,binding-var ,binding-form))
         (unwind-protect
              (progn ,@body)
           ,@binding-clean-up))))

;; @This is particularly useful as this is actually annoying to do correctly.

(defmacro with-protected-bindings (bindings &body body)
  "This sets up a protected bindings, a bindings that has some clean-up code
that will always be run when the binding expires.

Usually, the forms specified at the end of the binding forms will be placed in
the clean-up clause of an unwind-protect.  If the clean-up forms starts with
keyword :finalize, then the following code will be exectuted as if in a progn
and the result will be set as a finalization thunk for the object (or produce an
error if finalizers are not supported)."
  (if bindings
      `(with-protected-binding ,(first bindings)
         (with-protected-bindings ,(rest bindings)
           ,@body))
      `(progn ,@body)))

