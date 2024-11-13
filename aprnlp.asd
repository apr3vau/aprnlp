;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(defsystem aprnlp
  :author "April & May"
  :license "0BSD"
  :depends-on (alexandria
               anaphora
               cl-ppcre
               dexador
               iterate
               log4cl
               serapeum
               split-sequence
               trivial-file-size)
  :components ((:file "package")
               (:file "conllu")
               (:file "util")
               (:file "generic")
               (:file "pos")
               (:file "lemma")
               (:file "dep")
               (:file "dep-label")
               (:file "simple-tokenizer")
               (:file "simple-pipeline")))
