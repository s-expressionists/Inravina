(in-package #:inravina-examples/cst)

(defclass cst-client (inravina-extrinsic:extrinsic-client)
  ((cst-map :reader cst-map
            :initform (make-hash-table :test 'eq))))

(defun record-cst (client cst)
  (when (typep cst 'concrete-syntax-tree:cons-cst)
    (setf (gethash (concrete-syntax-tree:raw cst) (cst-map client)) cst)
    (record-cst client (concrete-syntax-tree:first cst))
    (record-cst client (concrete-syntax-tree:rest cst))))

(defmethod incless:write-object :around ((client cst-client) (object concrete-syntax-tree:cst) stream)
  (record-cst client object)
  (incless:write-object client (concrete-syntax-tree:raw object) stream))

(defstruct source-info
  start-line start-column end-line end-column)

(defmethod inravina::layout-block ((client cst-client) stream object (kind (eql :start)) line column)
  (let ((cst (gethash object (cst-map client))))
    (when cst
      (setf (concrete-syntax-tree:source cst) (make-source-info :start-line line :start-column column)))))

(defmethod inravina::layout-block ((client cst-client) stream object (kind (eql :end)) line column)
  (let ((cst (gethash object (cst-map client))))
    (when cst
      (let ((source (concrete-syntax-tree:source cst)))
        (setf (source-info-end-line source) line
              (source-info-end-column source) column)))))
