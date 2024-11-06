;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package aprnlp)

(defclass dep-graph-parser (dep-parser)
  ((name :initform "unnamed-dep-graph-parser")))

(defun dep-graph-features (head word)
  (let (;(head-id     (word-id head))
        ;(word-id     (word-id word))
        (head-form   (word-form head))
        (word-form   (word-form word))
        (head-pos    (word-upos head))
        (word-pos    (word-upos word))
        (head-shape  (word-shape head))
        (word-shape  (word-shape word))
        (head-suffix (word-suffix head))
        (word-suffix (word-suffix word))
        (distance    (if (= (word-id head) 0) 0
                       (- (word-id head) (word-id word)))))
    (vector ;(list :form     head-form   word-form)
            ;(list :form     head-form   t)
            ;(list :form     t           word-form)
            (list :pos      head-pos    word-pos)
            (list :pos      head-pos    t)
            (list :pos      t           word-pos)
            ;(list :shape    head-shape  t)
            ;(list :shape    t           word-shape)
            (list :shape    head-shape  word-shape)
            (list :word-pos head-form   word-pos)
            (list :pos-word head-pos    word-form)
            ;(list :head     head-form   head-pos)
            ;(list :head     head-shape  head-pos)
            ;(list :word     word-form   word-pos)
            ;(list :word     word-shape  word-pos)
            ;(list :id       head-id     word-id)
            
            (list :distance t           distance)
            (list head-pos  word-pos    distance)
            ;(list head-form word-form   distance)
            (list :suffix   head-pos    word-suffix)
            (list :suffix   head-suffix word-pos)
            (list :suffix   head-suffix word-suffix)
            )))

(defun generate-vertexes (parser sentence)
  "Generate vertexes and all their incoming edges from SENTENCE.

vertexes := ((vertex (income score) (income score) ...) (vertex ...) ...)"
  (for (vertex :across sentence)
       (:collect
        (cons vertex
              (cons (let ((features (dep-graph-features *root-word* vertex)))
                      (list *root-word* (for (feature :across features)
                                             (:sum (get-weight (slot-value parser 'weights) feature :uas)))))
                    (for (income :across sentence)
                         (features := (dep-graph-features income vertex))
                         (score := (for (feature :across features)
                                        (:sum (get-weight (slot-value parser 'weights) feature :uas))))
                         (unless (eq income vertex)
                           (:collect (list income score)))))))))

(defun adjust-weights (vertexes)
  "subtracting the score of the maximum edge entering each vertex from
the score of all the edges entering that vertex."
  (for (vertex-index :index)
       ((nil . income-score-list) :in vertexes)
       (max-score := (for ((nil score) :in income-score-list)
                          (:maximize score)))
       (for (income-index :from 1)
            ((nil score) :in income-score-list)
            (setf (nth 1 (nth income-index (nth vertex-index vertexes)))
                  (- score max-score))))
  vertexes)

(defun select-best-score (vertexes)
  "Return vertexes with their best-scored incoming edge in a form of ((vertex income score) ...)"
  (for ((vertex . income-score-list) :in vertexes)
       (collect (cons vertex (for (list :in income-score-list)
                                  (:find list :max (second list)))))))

(defun spanning-tree-p (vertexes)
  "Test if the vertexes consist a spanning tree, i.e. without cycle.

If there is a cycle, return the first cycle as the second value."
  (let (previous)
    (for ((vertex income) :in vertexes)
         (if (member (list income vertex) previous :test #'equal)
             (return (values nil (list vertex income)))
           (push (list vertex income) previous))
         (:final (return t)))))

(defun contract (vertexes cycled collapsed-vertex)
  (destructuring-bind (cycled-vertex-1 cycled-vertex-2) cycled
    (let ((cycled-income-scores-1 (cdr (find cycled-vertex-1 vertexes :key #'first)))
          (cycled-income-scores-2 (cdr (find cycled-vertex-2 vertexes :key #'first))))
      (setq vertexes (delete-if (op (member (first _) cycled)) vertexes))
      (push (cons collapsed-vertex
                  (nconc (mapcar (op (push-end cycled-vertex-1 _))
                                 (delete cycled-vertex-2 cycled-income-scores-1 :key #'first))
                         (mapcar (op (push-end cycled-vertex-2 _))
                                 (delete cycled-vertex-1 cycled-income-scores-2 :key #'first))))
            vertexes)
      vertexes)))

(defun expand (best-vertexes cycled collapsed-vertex)
  (destructuring-bind (income score real-vertex)
      (cdr (find collapsed-vertex best-vertexes :key #'first))
    (deletef best-vertexes collapsed-vertex :key #'first)
    (push (list real-vertex income score) best-vertexes)
    (push (list (first (delete real-vertex cycled)) real-vertex 0) best-vertexes)
    (for (vertex-index :index)
         ((nil income) :in best-vertexes)
         (when (eq income collapsed-vertex)
           (setf (nth 1 (nth vertex-index best-vertexes)) real-vertex)))
    best-vertexes))

(defun max-spanning-tree (vertexes)
  (let* ((adjusted (adjust-weights vertexes))
         (best (select-best-score adjusted)))
    (multiple-value-bind (spanningp cycled)
        (spanning-tree-p best)
      (if spanningp best
        (let ((collapsed (gensym "COLLAPSED-")))
          (expand (max-spanning-tree (contract adjusted cycled collapsed)) cycled collapsed))))))

(defmethod process ((parser dep-graph-parser) sentence)
  (for ((vertex head) :in (max-spanning-tree (generate-vertexes parser sentence)))
       (setf (word-head vertex) (word-id head))))

(defun dep-graph-parser-update (parser word head-truth head-guess)
  (declare (optimize (speed 3) (space 0) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (with-slots (weights living-weights last-updates update-count) parser
    (flet ((upd (feature val)
             (declare (inline upd))
             (let* ((last-update  (get-weight last-updates feature :uas))
                    (lived-cycle  (- update-count last-update)))
               (add-weight living-weights feature :uas
                           (* lived-cycle (get-weight weights feature :uas)))
               (set-weight last-updates feature :uas update-count)
               (add-weight weights feature :uas val))))
      (incf update-count)
      (unless (eq head-truth head-guess)
        (let ((truth-features (dep-graph-features head-truth word))
              (guess-features (when head-guess (dep-graph-features head-guess word))))
          (for (feature :across truth-features)
               (upd feature 1))
          (when head-guess
            (for (feature :across guess-features)
                 (upd feature -1))))))))

(defun dep-graph-parser-train-sentence (parser sentence)
  (let ((tree (max-spanning-tree (generate-vertexes parser sentence)))
        (correct-count 0)
        (total-count 0))
    (for ((vertex head-guess) :in tree)
         (head-truth := (if (= (word-head vertex) 0)
                            *root-word*
                          (find (word-head vertex) sentence :key #'word-id)))
         (when (eq head-truth head-guess)
           (incf correct-count))
         (dep-graph-parser-update parser vertex head-truth head-guess)
         (incf total-count))
    (values correct-count total-count)))

(defmethod test ((parser dep-graph-parser) sentences)
  (let ((correct-count 0)
        (total-count   0)
        (start-time    (get-internal-real-time)))
    (for* (sentence :across sentences)
          ((vertex head-guess) :in (max-spanning-tree (generate-vertexes parser sentence)))
          (when (= (word-head vertex) (word-id head-guess))
            (incf correct-count))
          (incf total-count))
    (log-info "Test ~D sentences using ~,2Fs, result: ~D/~D (~,2F%)"
              (length sentences)
              #+lispworks (/ (- (get-internal-real-time) start-time) 1000)
              #-lispworks (/ (- (get-internal-real-time) start-time) 1000000)
              correct-count total-count (* 100 (/ correct-count total-count)))
    (float (* 100 (/ correct-count total-count)))))

(defun train (parser sentences &key (cycles 5) save-dir)
  (unless save-dir
    (setq save-dir (asdf/system:system-source-directory :aprnlp)))
  (log-info "Start training with ~D sentences, ~D cycles. ~A"
            (length sentences) cycles
            #+lispworks (lw:string-append "Heap size: " (print-size (getf (sys:room-values) :total-size)))
            #-lispworks "")
  (for (cycle :range cycles)
       (let ((correct-count    0)
             (total-count      0)
             (cycle-start-time (get-internal-real-time)))
         (for (sentence :across sentences)
              (multiple-value-bind (correct total)
                  (dep-graph-parser-train-sentence parser sentence)
                (incf correct-count correct)
                (incf total-count total)))
         (log-info "Cycle ~D/~D completed using ~,2Fs with ~D/~D (~,2F%) correct. ~A"
                   (1+ cycle) cycles (/ (- (get-internal-real-time) cycle-start-time) 1000)
                   correct-count total-count (* 100.0 (/ correct-count total-count))
                   #+lispworks (lw:string-append "Heap size: " (print-size (getf (sys:room-values) :total-size)))
                   #-lispworks ""))
       (shuffle sentences))
  ;(dep-parser-average-weights parser)
  (save-processor parser save-dir))

(defmethod test-training ((class (eql 'dep-graph-parser)))
  (let ((parser (make-instance 'dep-graph-parser))
        (ud-dir (merge-pathnames "ud-treebanks-v2.14/" (asdf:system-source-directory :aprnlp))))
    (train parser (read-conllu-files (merge-pathnames "UD_English-GUM/en_gum-ud-train.conllu" ud-dir))
           :cycles 5)
    (test parser (read-conllu-files (merge-pathnames "UD_English-GUM/en_gum-ud-test.conllu" ud-dir)))
    (setq *loaded-dep-parser* parser)))

;(test-training 'dep-graph-parser)