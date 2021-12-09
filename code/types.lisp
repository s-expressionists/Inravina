(in-package #:inravina)

(deftype newline-kind ()
  `(member :fill :linear :mandatory :miser :fresh
           :literal-fill :literal-linear :literal-mandatory :literal-miser :literal-fresh))

(defun fill-kind-p (kind)
  (and (member kind '(:fill :literal-fill))
       t))

(defun linear-kind-p (kind)
  (and (member kind '(:linear :literal-linear))
       t))

(defun mandatory-kind-p (kind)
  (and (member kind '(:mandatory :literal-mandatory))
       t))

(defun miser-kind-p (kind)
  (and (member kind '(:miser :literal-miser))
       t))

(defun fresh-kind-p (kind)
  (and (member kind '(:fresh :literal-fresh))
       t))

(defun literal-kind-p (kind)
  (and (member kind '(:literal-fill :literal-linear :literal-mandatory :literal-miser :literal-fresh))
       t))

(deftype tab-kind ()
  `(member :line :line-relative :section :section-relative))

(defun line-kind-p (kind)
  (and (member kind '(:line :line-relative))
       t))

(defun section-kind-p (kind)
  (and (member kind '(:section :section-relative))
       t))

(defun relative-kind-p (kind)
  (and (member kind '(:line-relative :section-relative))
       t))

