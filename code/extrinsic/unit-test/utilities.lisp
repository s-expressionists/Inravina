(in-package #:inravina-extrinsic/unit-test)

(defmacro with-env ((stream
                     &key (array t) (base 10) (case :upcase) circle
                          (escape t) (gensym t) level length lines
                          miser-width (pretty t) readably right-margin)
                    &body body)
  `(let ((*print-array* ,array)
         (*print-base* ,base)
         (*print-case* ,case)
         (*print-circle* ,circle)
         (*print-escape* ,escape)
         (*print-gensym* ,gensym)
         (*print-level* ,level)
         (*print-length* ,length)
         (*print-lines* ,lines)
         (*print-miser-width* ,miser-width)
         (*print-pretty* ,pretty)
         (*print-readably* ,readably)
         (*print-right-margin* ,right-margin))
     (with-output-to-string (,stream)
       ,@body)))
