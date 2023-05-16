(in-package #:inravina)

(deftype newline-kind ()
  `(member :fill :linear :mandatory :miser :fresh
           :fill-literal :linear-literal :mandatory-literal :miser-literal :fresh-literal))

(defun fill-kind-p (kind)
  (and (member kind '(:fill :fill-literal))
       t))

(defun linear-kind-p (kind)
  (and (member kind '(:linear :linear-literal))
       t))

(defun mandatory-kind-p (kind)
  (and (member kind '(:mandatory :mandatory-literal))
       t))

(defun miser-kind-p (kind)
  (and (member kind '(:miser :miser-literal))
       t))

(defun fresh-kind-p (kind)
  (and (member kind '(:fresh :fresh-literal))
       t))

(defun literal-kind-p (kind)
  (and (member kind '(:fill-literal :linear-literal :mandatory-literal :miser-literal :fresh-literal))
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
