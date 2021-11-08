#+sbcl (setf sb-ext:*muffled-warnings* 'warning)

(sb-ext:with-unlocked-packages (#:common-lisp)
  (require :inravina/intrinsic))

(sb-ext:with-unlocked-packages (#:sb-kernel)
  (defun sb-impl::output-object (object stream)
    (inravina/intrinsic::write-object object stream)))

(setf sb-pretty::*standard-pprint-dispatch-table* *print-pprint-dispatch*)

#+sbcl (setf sb-ext:*muffled-warnings* nil)    

