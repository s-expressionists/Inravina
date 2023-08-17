(in-package #:inravina/test)

(defmacro with-env ((stream &key right-margin) &body body)
  `(let ((*print-right-margin* ,right-margin))
     (with-output-to-string (,stream)
       ,@body)))

(defclass test-client ()
  ())

(defparameter *client* (make-instance 'test-client))

(defmethod inravina:make-dispatch-function ((client test-client) (pattern (eql :client-stream-object)) function rest)
  (lambda (stream object)
    (apply function *client* (inravina:make-pretty-stream *client* stream) object rest)))

(defmethod inravina:make-dispatch-function ((client test-client) (pattern (eql :client-object-stream)) function rest)
  (lambda (stream object)
    (apply function *client* object (inravina:make-pretty-stream *client* stream) rest)))

(defmethod inravina:make-dispatch-function ((client test-client) (pattern (eql :stream-object)) function rest)
  (lambda (stream object)
    (apply function (inravina:make-pretty-stream *client* stream) object rest)))

(defmethod inravina:make-dispatch-function ((client test-client) (pattern (eql :object-stream)) function rest)
  (lambda (stream object)
    (apply function object (inravina:make-pretty-stream *client* stream) rest)))

(defmethod incless:write-object ((client test-client) object stream)
  (if *print-pretty*
      (funcall (inravina:pprint-dispatch client *print-pprint-dispatch* object) stream object)
      (call-next-method)))

(defmethod incless:print-object ((client test-client) object stream)
  (declare (ignore client))
  (print-object object stream))

(defmethod incless:handle-circle ((client test-client) object stream function)
  (declare (ignorable client))
  (funcall function object stream))

(defmethod incless:write-unreadable-object
    ((client test-client) object stream type identity function)
  (declare (ignore client))
  (print-unreadable-object (object stream :type type :identity identity)
    (funcall function object stream)))

(defmethod incless:circle-check ((client test-client) object stream)
  (declare (ignore client stream))
  nil)
