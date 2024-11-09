;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package aprnlp)

(defmacro-driver (for var :range val)
  (let ((kw (if generate 'generate 'for))
        (start (if (consp val) (first val) 0))
        (end (if (consp val) (second val) (list '1- val)))
        (step (if (consp val) (third val) nil)))
    (apply #'nconc
           (delete nil (list (list kw var :from start)
                             (if end (list :to end))
                             (if step (list :by step)))))))

(defmacro-driver (for kv :in-plist plist)
  (let ((kw (if generate 'generate 'for)))
    (list kw kv :on plist :by '(function cddr))))

(defun print-size (num)
  (if (numberp num)
      (cond ((< num 1024)
             (format nil "~dB" num))
            ((< num 1048576)
             (format nil "~,1fK" (/ num 1024)))
            ((< num 1073741824)
             (format nil "~,1fM" (/ num 1048576)))
            (t (format nil "~,1fG" (/ num 1073741824))))
    ""))

(defun table-to-plist (table)
  (iter (for (key val) :in-hashtable table)
        (if (hash-table-p val)
            (nconcing (list key (table-to-plist val)))
          (nconcing (list key val)))))

(defun plist-to-table (plist)
  (let ((table (make-hash-table :test #'eq)))
    (iter (for (key val) :in-plist plist)
          (if (consp val)
              (setf (gethash key table) (plist-to-table val))
            (setf (gethash key table) val)))
    table))

(defun punct-char-p (c)
  (declare (inline punct-char-p))
  (and (graphic-char-p c)
       (not (or (alphanumericp c) (whitespacep c)))))

(defun numericp (str)
  (or (every (op (or (digit-char-p _1) (member _1 '(#\, #\.)))) str)))

(defun ordinalp (str)
  (declare (inline ordinalp))
  (let ((len (length str)))
    (and (> len 2)
         (every #'digit-char-p (subseq str 0 (- len 2)))
         (member (subseq str (- len 2) len) '("st" "nd" "rd" "th")))))

(defun punctp (str)
  (declare (inline punctp))
  (every #'punct-char-p str))

(defun download-ud-treebanks ()
  (let* ((dir (asdf:system-source-directory :aprnlp))
         (tgz-filename (merge-pathnames "ud-treebanks-v2.14.tgz" (asdf:system-source-directory :aprnlp)))
         (folder (merge-pathnames "ud-treebanks-v2.14/" (asdf:system-source-directory :aprnlp))))
    (unless (probe-file folder)
      (unless (probe-file tgz-filename)
        (log:info "Downloading Universal Dependencies Treebank v2.14 into ~A..." dir)
        (dex:fetch "https://lindat.mff.cuni.cz/repository/xmlui/bitstream/handle/11234/1-5502/ud-treebanks-v2.14.tgz"
                   tgz-filename))
      (log:info "Extracting ~A..." tgz-filename)
      #+(and lispworks mswindows)
      (sys:call-system-showing-output (list "cmd" "/c" "tar" "-xvf" (namestring tgz-filename) "-C" (namestring dir)))
      #+(and lispworks unix)
      (sys:call-system-showing-output (list "/usr/bin/env" "tar" "-xvf" (namestring tgz-filename) "-C" (namestring dir)))
      #+(and (not lispworks) mswindows)
      (uiop:run-program (list "cmd" "/c" "tar" "-xvf" (namestring tgz-filename) "-C" (namestring dir)))
      #+(and (not lispworks) unix)
      (uiop:run-program (list "/usr/bin/env" "tar" "-xvf" (namestring tgz-filename) "-C" (namestring dir)))
      (log:info "UD Treebank downloaded."))))