;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package aprnlp)

;; Words

(defstruct word id form lemma upos xpos head deprel suffix prefix)

(defpackage pos/words)

(defun word-shape (word)
  (let ((form (word-form word)))
    (when (symbolp form) (setq form (symbol-name form)))
    (when (stringp (word-form word))
      (coerce (iter (for char :in-string form)
                    (collect (cond ((digit-char-p char) #\D)
                                   ((upper-case-p char) #\U)
                                   ((lower-case-p char) #\L)
                                   ((punct-char-p char) char))))
              'string))))

(defun general-form (word)
  (let ((str (symbol-name (word-form word))))
    (cond ((numericp str) :number)
          ((ordinalp str) :ordinal)
          ((and (= (length str) 1)
                (member str '(#\( #\[ #\{ #\<)))
           :left-pair)
          ((and (= (length str) 1)
                (member str '(#\) #\] #\} #\>)))
           :right-pair)
          ;((punctp str) :punct)
          ((and (find #\@ str) (find #\. str)) :email)
          (t str))))

(defun copy-sentence (sentence)
  (let ((result (make-array (length sentence) :element-type 'word)))
    (iter (for word :in-vector sentence :with-index i)
          (setf (aref result i) (copy-word word)))
    result))

;; Processor

(defclass perceptron-processor ()
  ((name             :initform "Unnamed Processor")
   (update-count     :initform 0)
   (weights          :initform (make-hash-table :test #'eq)
                     :accessor processor-weights)
   (last-updates     :initform (make-hash-table :test #'eq))
   (living-weights   :initform (make-hash-table :test #'eq))))

;; Helping functions

(defun get-weight (table feature class)
  (declare (optimize (safety 0) (speed 3) (space 0))
           (inline get-weight))
  (apply #'href-default 0 table (append feature (list class))))

(defun add-weight (table feature class val)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (destructuring-bind (type specializer value) feature
    (prog (type-table specializer-table value-table)
     (if-let (table (gethash type table))
         (progn
           (setq type-table table)
           (if-let (table (gethash specializer table))
               (progn
                 (setq specializer-table table)
                 (if-let (table (gethash value table))
                     (progn
                       (setq value-table table)
                       (go set-value))
                   (go create-value-table)))
             (go create-specializer-table)))
       (go create-type-table))
     create-type-table
     (setq type-table (setf (gethash type table) (make-hash-table :test #'eq)))
     create-specializer-table
     (setq specializer-table (setf (gethash specializer type-table) (make-hash-table :test #'eq)))
     create-value-table
     (setq value-table (setf (gethash value specializer-table) (make-hash-table :test #'eq)))
     set-value
     (incf (gethash class value-table 0) val))))

(defun set-weight (table feature class val)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (destructuring-bind (type specializer value) feature
    (prog (type-table specializer-table value-table)
     (if-let (table (gethash type table))
         (progn
           (setq type-table table)
           (if-let (table (gethash specializer table))
               (progn
                 (setq specializer-table table)
                 (if-let (table (gethash value table))
                     (progn
                       (setq value-table table)
                       (go set-value))
                   (go create-value-table)))
             (go create-specializer-table)))
       (go create-type-table))
     create-type-table
     (setq type-table (setf (gethash type table) (make-hash-table :test #'eq)))
     create-specializer-table
     (setq specializer-table (setf (gethash specializer type-table) (make-hash-table :test #'eq)))
     create-value-table
     (setq value-table (setf (gethash value specializer-table) (make-hash-table :test #'eq)))
     set-value
     (setf (gethash class value-table) val))))

(defun find-best (processor features)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((scores (make-hash-table :test #'eq)))
    (iter (for feature :in-vector features)
          (awhen (apply #'href-default nil (slot-value processor 'weights) feature)
            (iter (for (class weight) :in-hashtable it)
                  (incf (gethash class scores 0.0) weight))))
    (iter (for (class weight) :in-hashtable scores)
          (finding class :maximizing weight))))

;; Methods

(defgeneric update (processor truth guess features)
  (:method (processor truth guess features)
   (declare (optimize (speed 3) (space 0) (safety 0)))
   (with-slots (update-count weights last-updates living-weights) processor
     (flet ((upd (feature class val)
              (declare (inline upd))
              (let* ((last-update (get-weight last-updates feature class))
                     (lived-cycle (- update-count last-update)))
                (add-weight living-weights feature class
                            (* lived-cycle (get-weight weights feature class)))
                (set-weight last-updates feature class update-count)
                (add-weight weights feature class val))))
       (incf update-count)
       (unless (eq truth guess)
         (iter (for feature :in-vector features)
               (upd feature truth 1)
               (upd feature guess -1)))))))

(defgeneric average-weights (processor)
  (:method (processor)
   (declare (optimize (space 0) (speed 3) (safety 0) (float 0)))
   (with-slots (update-count weights last-updates living-weights) processor
     (iter (for (type type-table) :in-hashtable weights)
           (iter (for (specializer specializer-table) :in-hashtable type-table)
                 (iter (for (value value-table) :in-hashtable specializer-table)
                       (iter (for (class nil) :in-hashtable value-table)
                             (let* ((feature (list type specializer value))
                                    (last-update (get-weight last-updates feature class))
                                    (lived-cycle (- update-count last-update)))
                               (add-weight living-weights feature class
                                           (* lived-cycle (get-weight weights feature class)))
                               (let ((living-weight (get-weight living-weights feature class)))
                                 (set-weight weights feature class
                                             (float (/ living-weight update-count))))))))))))

(defgeneric process (processor sentence))
(defgeneric train (processor sentences &key cycles save-dir))
(defgeneric test (processor sentences))
(defgeneric test-training (processor-class))

(defgeneric save-processor (processor directory))
(defgeneric load-processor (class file))

(export '(process save-processor train test test-training))