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

(deftype quasiquote-form ()
  '(cons (eql #+clasp ext:quasiquote
              #+clisp system::backquote
              #+ecl si:quasiquote
              #+mezzano mezzano.internals::backquote
              #+sbcl sb-int:quasiquote)
    (cons t null)))

(defun quasiquote-form-p (form)
  (typep form 'quasiquote-form))

(deftype unquote-form ()
  '(cons (member #+clasp ext:unquote
                 #+clasp ext:unquote-splice
                 #+clasp ext:unquote-nsplice
                 #+clisp system::unquote
                 #+clisp system::splice
                 #+clisp system::nsplice
                 #+ecl si:unquote
                 #+ecl si:unquote-splice
                 #+ecl si:unquote-nsplice
                 #+mezzano mezzano.internals::bq-comma
                 #+mezzano mezzano.internals::bq-comma-atsign
                 #+mezzano mezzano.internals::bq-comma-dot)
         (cons t null)))

(defun unquote-form-p (form)
  (typep form 'unquote-form))
