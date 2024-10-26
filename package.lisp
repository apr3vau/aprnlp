(defpackage pos
  (:use :cl #:for-loop #:log4cl)
  (:import-from #:alexandria
   #:if-let
   #:when-let
   #:shuffle)
  (:import-from #:split-sequence #:split-sequence))
(in-package pos)

;; For LOG4CL
#+lispworks (log-config :stream *standard-output*)