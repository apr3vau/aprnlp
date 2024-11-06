;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(defsystem aprnlp
  :author "April & May"
  :license "0BSD"
  :depends-on (alexandria for-loop log4cl split-sequence serapeum trivial-file-size dexador)
  :components ((:file "package")
               (:file "conllu")
               (:file "util")
               (:file "generic")
               (:file "pos")
               (:file "lemma")
               (:file "dep")))