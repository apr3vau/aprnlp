;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package aprnlp)

(defclass lemmatizer ()
  ((name :initform "unnamed-lemmatizer"
         :accessor lemmatizer-name)
   (dict :initform (make-hash-table :test #'eq)
         :accessor lemmatizer-dict)))

(defparameter *loaded-lemmatizer* nil)

(defmethod load-processor ((class (eql 'lemmatizer)) file)
  #+lispworks (hcl:load-data-file file)
  #-lispworks (load file)
  *loaded-lemmatizer*)

(defmethod save-processor ((lemmatizer lemmatizer) directory)
  (with-slots (name dict) lemmatizer
    (let ((filename (make-pathname :name name :type "fasl" :defaults directory)))
      #+lispworks
      (hcl:with-output-to-fasl-file (out filename :overwrite t)
        (hcl:dump-form '(setf *loaded-lemmatizer* (make-lemmatizer)) out)
        (hcl:dump-form `(setf (lemmatizer-name *loaded-lemmatizer*) ,name) out)
        (hcl:dump-form `(setf (lemmatizer-dict *loaded-lemmatizer*) (plist-to-table ',(table-to-plist dict)))
                       out))
      #-lispworks
      (let ((src (make-pathname :name name :type "lisp" :defaults directory)))
        (with-open-file (out src
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (prin1 '(setf *loaded-lemmatizer* (make-lemmatizer)) out)
          (prin1 `(setf (lemmatizer-name *loaded-lemmatizer*) ,name) out)
          (prin1 `(setf (lemmatizer-dict *loaded-lemmatizer*) (plist-to-table ',(table-to-plist dict)))
                 out))
        (compile-file src :output-file filename)
        (delete-file src))
      (log-info "Dependency Parser saved, size: ~A" (print-size (file-size-in-octets filename))))))

(defun lemmatize (lemmatizer word)
  (let ((form (word-form word)))
    (setf (word-lemma word)
          (gethash form (lemmatizer-dict lemmatizer) form))))

(defmethod process ((lemmatizer lemmatizer) sentence)
  (for (word :across sentence)
       (lemmatize lemmatizer word)))

(defmethod train ((lemmatizer lemmatizer) sentences &key cycles save-dir)
  (declare (ignore cycles))
  (unless save-dir
    (setq save-dir (asdf/system:system-source-directory :aprnlp)))
  (log-info "Start training with ~D sentences. ~A"
            (length sentences)
            #+lispworks (lw:string-append "Heap size: " (print-size (getf (sys:room-values) :total-size)))
            #-lispworks "")
  (let ((start-time (get-internal-real-time)))
    (for* (sentence :across sentences)
          (word :across sentence)
          (setf (gethash (word-form word) (lemmatizer-dict lemmatizer))
                (word-lemma word)))
    (save-processor lemmatizer save-dir)
    (log-info "Training completed using ~,2Fs. ~A"
              (/ (- (get-internal-real-time) start-time) 1000)
              #+lispworks (lw:string-append "Heap size: " (print-size (getf (sys:room-values) :total-size)))
              #-lispworks "")))

(defmethod test ((lemmatizer lemmatizer) sentences)
  (let ((correct-count 0)
        (total-count   0)
        (start-time    (get-internal-real-time)))
    (for (sentence :across sentences)
         (new-sentence := (coerce (for (word :across sentence)
                                       (collect (copy-word word)))
                                  'vector))
         (process lemmatizer new-sentence)
         (for (truth :across sentence)
              (guess :across new-sentence)
              (when (= (word-head guess) (word-head truth))
                (incf correct-count))
              (incf total-count)))
    (log-info "Test ~D sentences using ~,2Fs, result: ~D/~D (~,2F%)"
              (length sentences)
              #+lispworks (/ (- (get-internal-real-time) start-time) 1000)
              #-lispworks (/ (- (get-internal-real-time) start-time) 1000000)
              correct-count total-count (* 100 (/ correct-count total-count)))
    (float (* 100 (/ correct-count total-count)))))

(defmethod test-training ((class (eql 'lemmatizer)))
  (let ((lemmatizer (make-instance 'lemmatizer))
        (ud-dir (merge-pathnames "ud-treebanks-v2.14/" (asdf:system-source-directory :aprnlp))))
    (train lemmatizer (read-conllu-files (merge-pathnames "UD_English-GUM/en_gum-ud-train.conllu" ud-dir)))
    (test lemmatizer (read-conllu-files (merge-pathnames "UD_English-GUM/en_gum-ud-test.conllu" ud-dir)))
    (setq *loaded-lemmatizer* lemmatizer)))
