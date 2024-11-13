;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package aprnlp)

(defclass dep-parser (perceptron-processor)
  ((name :initform "unnamed-dep-parser")))
(export 'dep-parser)

(defvar *loaded-dep-parser* nil)
(export '*loaded-dep-parser*)

(defvar *dep-parser-with-labeller* nil)

(defvar *root-word* (make-word :form :root :upos :root :head -1 :id 0))

(defmethod load-processor ((class (eql 'dep-parser)) file)
  #+lispworks (hcl:load-data-file file)
  #-lispworks (load file)
  *loaded-dep-parser*)

(defmethod save-processor ((processor dep-parser) directory)
  (with-slots (name weights) processor
    (let ((filename (make-pathname :name name :type "fasl" :defaults directory)))
      #+lispworks
      (hcl:with-output-to-fasl-file (out filename :overwrite t)
        (hcl:dump-form '(setf *loaded-dep-parser* (make-instance 'dep-parser)) out)
        (hcl:dump-form `(setf (slot-value *loaded-dep-parser* 'name) ,name) out)
        (hcl:dump-form `(setf (slot-value *loaded-dep-parser* 'weights)
                              (plist-to-table ',(table-to-plist weights)))
                       out))
      #-lispworks
      (let ((src (make-pathname :name name :type "lisp" :defaults directory)))
        (with-open-file (out src
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (prin1 '(setf *loaded-dep-parser* (make-instance 'dep-parser)) out)
          (prin1 `(setf (slot-value *loaded-dep-parser* 'name) ,name) out)
          (prin1 `(setf (slot-value *loaded-dep-parser* 'weights)
                        (plist-to-table ',(table-to-plist weights)))
                 out))
        (compile-file src :output-file filename)
        (delete-file src))
      (log-info "Tagger saved to ~A, size: ~A" (namestring filename) (print-size (file-size-in-octets filename)))
      *loaded-dep-parser*)))

(defun dep-features (left-word right-word stack sentence sentence-pointer)
  (let ((right-form   (general-form right-word))
        (left-form    (general-form left-word))
        (right-pos    (word-upos right-word))
        (left-pos     (word-upos left-word))
        (left-suffix  (word-suffix left-word))
        (right-suffix (word-suffix right-word))
        (distance     (if (= (word-id left-word) 0) 0
                        (- (word-id left-word) (word-id right-word)))))
    (vector (list :form     left-form   right-form)
            (list :form     left-form   t)
            (list :form     t           right-form)
            (list :pos      left-pos    right-pos)
            (list :pos      left-pos    t)
            (list :pos      t           right-pos)
            (list :word-pos left-form   right-pos)
            (list :pos-word left-pos    right-form)
            (list distance  left-pos    right-pos)
            (list :suffix   left-pos    right-suffix)
            (list :suffix   left-suffix right-pos)
            (list :suffix   left-suffix right-suffix)
            
            (list :stack left-form (when (second stack) (word-form (second stack))))
            (list :stack-pos left-pos (when (second stack) (word-upos (second stack))))
            (list :buffer right-form
                  (when (< (1+ sentence-pointer) (length sentence))
                    (word-form (aref sentence (1+ sentence-pointer)))))
            (list :buffer-pos right-pos
                  (when (< (1+ sentence-pointer) (length sentence))
                    (word-upos (aref sentence (1+ sentence-pointer)))))
            
            (list :left-head left-pos
                  (iter (for word :in-vector sentence :to (1- sentence-pointer))
                        (finding (general-form word) :such-that (= (word-id word) (word-head left-word)))))
            (list :left-children left-pos
                  (iter (for word :in-vector sentence :to (1- sentence-pointer))
                        (finding (general-form word) :such-that (= (word-head word) (word-id left-word)))))
            )))

(defmethod process ((parser dep-parser) sentence)
  (with-slots (weights) parser
    (let* ((sentence-len (length sentence))
           (sentence-pointer 0)
           (stack (list *root-word*))
           left right
           (actions (dict #'eq
                          :shift (lambda ()
                                   (push right stack)
                                   (incf sentence-pointer))
                          :left-arc (lambda ()
                                      (setf (word-head left) (word-id right)
                                            stack (cdr stack)))
                          :right-arc (lambda ()
                                       (setf (word-head right) (word-id left)
                                             stack (cons right stack))
                                       (incf sentence-pointer)))))
      (loop
         (when (= sentence-pointer sentence-len)
           (return))
         (setq left (first stack)
               right (aref sentence sentence-pointer))
         (let ((scores (make-hash-table :test #'eq))
               (features (dep-features left right stack sentence sentence-pointer)))
           (iter (for feature :in-vector features)
                 (awhen (apply #'href-default nil weights feature)
                   (iter (for (class weight) :in-hashtable it)
                         (incf (gethash class scores 0.0) weight))))
           (let ((action (if (= (length stack) 1)
                           :shift
                           (iter (for (class weight) :in-hashtable scores)
                                 (finding class :maximizing weight)))))
             (funcall (gethash action actions)))))
      sentence)))

(defun dep-parser-train-sentence (parser sentence)
  (let* ((sentence-len (if (array-has-fill-pointer-p sentence)
                         (fill-pointer sentence)
                         (length sentence)))
         (sentence-pointer 0)
         (correct-count 0)
         (total-count 0)
         (stack (list *root-word*))
         left right
         (actions (dict #'eq
                        :shift (lambda ()
                                 (push right stack)
                                 (incf sentence-pointer))
                        :left-arc (lambda ()
                                    (setf (word-head left) (word-id right)
                                          stack (cdr stack)))
                        :right-arc (lambda ()
                                     (setf (word-head right) (word-id left)
                                           stack (cons right stack))
                                     (incf sentence-pointer)))))
    (loop
       (when (= sentence-pointer sentence-len)
         (return))
       (setq left (first stack)
             right (aref sentence sentence-pointer))
       (let* ((features (dep-features left right stack sentence sentence-pointer))
              (guess (find-best parser features))
              (truth (cond ((= (word-head left) (word-id right))
                            :left-arc)
                           ((= (word-head right) (word-id left))
                            :right-arc)
                           (t :shift))))
         (when (eq guess truth) (incf correct-count))
         (incf total-count)
         (update parser truth guess features)
         (funcall (gethash truth actions))))
    (values correct-count total-count)))

(defmethod train ((parser dep-parser) sentences &key (cycles 5) (save-dir (asdf:system-source-directory :aprnlp)))
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (log-info "Start training with ~D sentences, ~D cycles. ~A"
            (length sentences) cycles
            #+lispworks (lw:string-append "Heap size: " (print-size (getf (sys:room-values) :total-size)))
            #-lispworks "")
  (iter (for cycle :range cycles)
        (let ((correct-count    0)
              (total-count      0)
              (cycle-start-time (get-internal-real-time)))
          (iter (for sentence :in-vector sentences)
                (multiple-value-bind (correct total)
                    (dep-parser-train-sentence parser sentence)
                  (incf correct-count correct)
                  (incf total-count total)))
          (log-info "Cycle ~D/~D completed using ~,2Fs with ~D/~D (~,2F%) correct. ~A"
                    (1+ cycle) cycles
                    #+lispworks (/ (- (get-internal-real-time) cycle-start-time) 1000)
                    #-lispworks (/ (- (get-internal-real-time) cycle-start-time) 1000000)
                    correct-count total-count (* 100.0 (/ correct-count total-count))
                    #+lispworks (lw:string-append "Heap size: " (print-size (getf (sys:room-values) :total-size)))
                    #-lispworks "")
          (setq sentences (shuffle sentences))))
  (unless *dep-parser-with-labeller*
    (average-weights parser)
    (save-processor parser save-dir)))

(defmethod test ((parser dep-parser) sentences)
  (let ((uas-correct-count 0)
        (las-correct-count 0)
        (total-count       0)
        (start-time        (get-internal-real-time)))
    (iter (for sentence :in-vector sentences)
          (iter (for truth :in-vector sentence)
                (for guess :in-vector (process parser (copy-sentence sentence)))
                (when (eql (word-head guess) (word-head truth))
                  (incf uas-correct-count))
                (when (and *dep-parser-with-labeller*
                           (eql (word-deprel guess) (word-deprel truth)))
                  (incf las-correct-count))
                (incf total-count)))
    (log-info "Test ~D sentences using ~,2Fs, UAS: ~D/~D (~,2F%) ~A"
              (length sentences)
              #+lispworks (/ (- (get-internal-real-time) start-time) 1000)
              #-lispworks (/ (- (get-internal-real-time) start-time) 1000000)
              uas-correct-count total-count (* 100 (/ uas-correct-count total-count))
              (if *dep-parser-with-labeller*
                (format nil "LAS: ~D/~D (~,2F%)"
                        las-correct-count total-count (* 100 (/ las-correct-count total-count)))
                ""))
    (values (* 100 (/ uas-correct-count total-count)) (* 100 (/ las-correct-count total-count)))))

(defmethod test-training ((class (eql 'dep-parser)))
  (let ((parser (make-instance 'dep-parser)))
    (train parser (read-conllu-files (treebank-file :english "GUM" :train)
                                     (treebank-file :english "EWT" :train)
                                     (treebank-file :english "Atis" :train))
           :cycles 5)
    (test parser (read-conllu-files (treebank-file :english "GUM" :test)))
    (setq *loaded-dep-parser* parser)
    nil))

;(test-training 'dep-parser)
