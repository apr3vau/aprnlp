;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package aprnlp)

(defun parse-conllu-line (line id)
  (declare (inline parse-conllu-line))
  (destructuring-bind (conllu-id form lemma upos xpos feats head deprel deps misc)
      (split-sequence #\Tab line)
    (declare (ignore deps misc feats))
    (unless (find #\- conllu-id)
      (make-word :id id
                 :form (intern form "POS/WORDS")
                 :lemma (unless (eql (char lemma 0) #\_) (intern lemma  "POS/WORDS"))
                 :upos (intern (string-upcase upos) :keyword)
                 :xpos (intern (string-upcase xpos) :keyword)
                 :head (if (eql (char head 0) #\_) 0 (parse-integer head))
                 :deprel (unless (eql (char deprel 0) #\_) (intern (string-upcase deprel) :keyword))
                 :suffix (intern (subseq form (- (length form) (min 3 (length form)))) "POS/WORDS")
                 :prefix (intern (subseq form 0 (min 3 (length form))) "POS/WORDS")))))

(defun read-conllu-stream (in)
  (declare (inline read-conllu-stream))
  (let ((i 1)
        (result (make-array 0 :element-type 'vector :fill-pointer t :adjustable t))
        (arr (make-array 0 :element-type 'word :fill-pointer t :adjustable t)))
    (iter (for line :in-stream in using #'read-line)
          (when (and (= (length line) 0))
            (setq i 1)
            (when (> (length arr) 0)
              (vector-push-extend arr result)
              (setq arr (make-array 0 :element-type 'word :fill-pointer t :adjustable t))))
          (when (and (> (length line) 0)
                     (not (eq (char line 0) #\#)))
            (when-let ((word (parse-conllu-line line i)))
              (vector-push-extend word arr)
              (incf i))))
    result))

(defun read-conllu-file (path)
  (declare (inline read-conllu-file))
  (with-open-file (in path :if-does-not-exist nil)
    (read-conllu-stream in)))

(defun read-conllu-files (&rest paths)
  (apply #'concatenate 'vector (mapcar #'read-conllu-file paths)))