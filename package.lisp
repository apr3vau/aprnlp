;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(defpackage aprnlp
  (:use :cl #:iterate #:log4cl)
  (:import-from #:alexandria
   #:if-let
   #:shuffle
   #:appendf
   #:deletef)
  (:import-from #:serapeum
   #:op
   #:push-end
   #:whitespacep
   #:dict
   #:href
   #:href-default)
  (:import-from #:split-sequence #:split-sequence)
  (:import-from #:trivial-file-size #:file-size-in-octets)
  (:import-from #:anaphora-basic #:awhen #:it))
(in-package aprnlp)

;; For LOG4CL
#+lispworks (ignore-errors (log-config :stream hcl:*background-output*))
