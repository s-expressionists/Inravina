#+sbcl (setf sb-ext:*muffled-warnings* 'warning)

(sb-ext:with-unlocked-packages (#:common-lisp)
  (require :incless-intrinsic)
  (require :inravina/intrinsic))

(setf incless:*client* inravina:*client*)

(defmethod incless:print-object-using-client (client (object symbol) stream)
  (print-object object stream))

#+(or)(sb-ext:with-unlocked-packages (#:sb-kernel)
  (defun sb-impl::output-object (object stream)
    (incless:write-object object stream)))

(setf sb-pretty::*standard-pprint-dispatch-table* *print-pprint-dispatch*)

#+sbcl (setf sb-ext:*muffled-warnings* nil)    

