(in-package pos)

;; Util

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

;; Tagger

(defstruct pos-tagger
  (name             "Unnamed")
  (update-count     0)
  (weights          (make-hash-table :test #'eq))
  (last-updates     (make-hash-table :test #'eq))
  (living-weights   (make-hash-table :test #'eq))
  (unique-tag-words (make-hash-table :test #'eq)))

(defun table-to-plist (table)
  (for (key val :in-table table)
       (if (hash-table-p val)
           (:nconc (list key (table-to-plist val)))
         (:nconc (list key val)))))

(defun plist-to-table (plist)
  (let ((table (make-hash-table :test #'eq)))
    (for (key val :in-plist plist)
         (if (consp val)
             (setf (gethash key table) (plist-to-table val))
           (setf (gethash key table) val)))
    table))

(defvar *loaded-tagger* nil)

(defun save-tagger (tagger directory)
  #+lispworks
  (hcl:with-output-to-fasl-file (out (make-pathname :name (pos-tagger-name tagger) :type "fasl"
                                                    :defaults directory)
                                 :overwrite t)
    (hcl:dump-form '(setf *loaded-tagger* (make-pos-tagger)) out)
    (hcl:dump-form `(setf (pos-tagger-name *loaded-tagger*) ,(pos-tagger-name tagger)) out)
    (hcl:dump-form `(setf (pos-tagger-weights *loaded-tagger*)
                      (plist-to-table ',(table-to-plist (pos-tagger-weights tagger))))
               out)
    (hcl:dump-form `(setf (pos-tagger-unique-tag-words *loaded-tagger*)
                      (plist-to-table ',(table-to-plist (pos-tagger-unique-tag-words tagger))))
               out))
  #-lispworks
  (let ((src (make-pathname :name (pos-tagger-name tagger) :type "lisp"
                                      :defaults directory)))
    (with-open-file (out src
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (prin1 '(setf *loaded-tagger* (make-pos-tagger)) out)
      (prin1 `(setf (pos-tagger-name *loaded-tagger*) ,(pos-tagger-name tagger)) out)
      (prin1 `(setf (pos-tagger-weights *loaded-tagger*)
                    (plist-to-table ',(table-to-plist (pos-tagger-weights tagger))))
             out)
      (prin1 `(setf (pos-tagger-unique-tag-words *loaded-tagger*)
                    (plist-to-table ',(table-to-plist (pos-tagger-unique-tag-words tagger))))
             out))
    (compile-file src :output-file (make-pathname :name (pos-tagger-name tagger) :type "fasl"
                                                  :defaults directory))
    (delete-file src))
  #+lispworks
  (log-info "Tagger saved, size: ~A"
            (print-size (sys:file-size (make-pathname :name (pos-tagger-name tagger) :type "fasl"
                                                      :defaults directory)))))

(defun load-tagger (name directory)
  #+lispworks
  (hcl:load-data-file (make-pathname :name name :type "fasl" :defaults directory))
  #-lispworks
  (load (make-pathname :name name :type "fasl" :defaults directory))
  *loaded-tagger*)

;; Features

(defun prev-word (i words)
  (declare (inline prev-word))
  (if (< i 1) 'start (word-form (aref words (1- i)))))
(defun prev-prev-word (i words)
  (declare (inline prev-prev-word))
  (if (< i 2) 'start (word-form (aref words (- i 2)))))
(defun next-word (i words)
  (declare (inline next-word))
  (if (> i (- (length words) 2)) 'end (word-form (aref words (1+ i)))))
(defun next-next-word (i words)
  (declare (inline next-next-word))
  (if (> i (- (length words) 3)) 'end (word-form (aref words (+ i 2)))))

(defun ordinalp (str)
  (declare (inline ordinalp))
  (if (symbolp str) (setq str (symbol-name str)))
  (let* ((len (length str)))
    (and (> len 2)
         (every #'digit-char-p (subseq str 0 (- len 2)))
         (member (subseq str (- len 2) len) '("st" "nd" "rd" "th")))))

(defun punctp (str)
  (declare (inline punctp))
  (if (symbolp str) (setq str (symbol-name str)))
  (and (every #'graphic-char-p str)
       (notany #'(lambda (c) (or #-lispworks (member c '(#\Space #\Newline #\Return))
                                 #+lispworks (lw:whitespace-char-p c)
                                 (alphanumericp c)))
               str)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *backward-features*
    '((:word           form     t)
      (:prev-word      form     (prev-word i words))
      (:prev-prev-word form     (prev-prev-word i words))
      (:next-word      form     (next-word i words))
      (:next-next-word form     (next-next-word i words))
      (:word           t        t)
      (:prev-word      t        (prev-word i words))
     ;(:prev-prev-word t        (prev-prev-word i words))
      (:next-word      t        (next-word i words))
     ;(:next-next-word t        (next-next-word i words))
     ;(:suffix         form     (suffix form))
     ;(:prefix         form     (prefix form))
      (:suffix         t        (word-suffix word))
      (:prefix         t        (word-prefix word))
     ;(:prev-suffix    form     (suffix (prev-word i words)))
     ;(:next-suffix    form     (suffix (next-word i words)))
      (:prev-suffix    t        (if (< i 1) 'start (word-suffix (aref words (1- i)))))
      (:next-suffix    t        (if (> i (- (length words) 2)) 'end (word-suffix (aref words (1+ i)))))
      (:prev-tag       form     prev-tag)
     ;(:prev-prev-tag  form     prev-prev-tag)
      (:prev-tag       t        prev-tag)
     ;(:prev-prev-tag  t        prev-prev-tag)
      (:prev-tags      prev-tag prev-prev-tag))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *forward-features*
    '((:next-tag      form     next-tag)
     ;(:next-next-tag form     next-next-tag)
      (:next-tag      t        next-tag)
     ;(:next-next-tag t        next-next-tag)
      (:next-tags next-tag next-next-tag))))

(macrolet ((expand (features)
             (let ((features (eval features)))
               `(let ((i (1- (word-id word)))
                      (form (let ((form (word-form word)))
                              (cond ((every #'digit-char-p (symbol-name form)) 'number)
                                    ((ordinalp form) 'ordinal)
                                    ((punctp form) 'punct)
                                    (t form))))
                      (vec (make-array ,(length features) :element-type 'vector)))
                  (declare (ignorable i))
                  (setf ,@(for (i :index)
                               (feature :in features)
                               (collect `(svref vec ,i))
                               (collect `(vector ,(first feature) ,(second feature) ,(third feature)))))
                  vec))))
  (defun backward-features (word words prev-tag prev-prev-tag)
    (declare (optimize (speed 3) (safety 0)))
    (expand *backward-features*))
  (defun forward-features (word words next-tag next-next-tag)
    (declare (optimize (speed 3) (safety 0))
             (ignore words))
    (expand *forward-features*))
  (defun all-features (word words prev-tag prev-prev-tag next-tag next-next-tag)
    (declare (optimize (speed 3) (safety 0)))
    (expand (append *backward-features* *forward-features*))))

;; Manipulate functions

(defun get-weight (table feature class)
  (declare (inline get-weight))
  (let ((type (svref feature 0))
        (specializer (svref feature 1))
        (value (svref feature 2)))
    (if-let (table (gethash type table))
        (if-let (table (gethash specializer table))
            (if-let (table (gethash value table))
                (gethash class table 0)
              0)
          0)
      0)))

(defun add-weight (table feature class val)
  (declare (optimize (safety 0) (speed 3) (space 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (let ((type (svref feature 0))
        (specializer (svref feature 1))
        (value (svref feature 2)))
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
     (setf (gethash class value-table) (+ val (gethash class value-table 0))))))

(defun set-weight (table feature class val)
  (declare (optimize (safety 0) (speed 3) (space 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (let ((type (svref feature 0))
        (specializer (svref feature 1))
        (value (svref feature 2)))
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

;; Algorithm

(defun predict-sentence (tagger sentence)
  (declare (optimize (speed 3) (space 0) (safety 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (let ((scores (make-array (length sentence) :element-type 'hash-table)))
    (for (prev-tag :previous tag 'start)
         (prev-prev-tag :previous prev-tag 'start)
         (i :index)
         (word :across sentence)
         (form := (word-form word))
         (unique-tag := (gethash form (pos-tagger-unique-tag-words tagger)))
         (tag := unique-tag)
         (setf (svref scores i) (make-hash-table :test #'eq))
         (if unique-tag
             (setf (gethash unique-tag (svref scores i)) most-positive-fixnum)
           (for (feature :across (backward-features word sentence prev-tag prev-prev-tag))
                (type = (svref feature 0))
                (specializer = (svref feature 1))
                (value = (svref feature 2))
                (type-table = (gethash type (pos-tagger-weights tagger)))
                (specializer-table = (when type-table (gethash specializer type-table)))
                (value-table = (when specializer-table (gethash value specializer-table)))
                (when value-table
                  (for (class weight :in-table value-table)
                       (setf (gethash class (svref scores i))
                             (+ weight (float (gethash class (svref scores i) 0.0))))))
                (:final (setq tag (for (class weight :in-table (svref scores i))
                                       (:find class :max weight)))))))
    (for (next-tag :previous tag 'end)
         (next-next-tag :previous next-tag 'end)
         (i :range (1- (length sentence)) 0)
         (word word := (aref sentence i))
         (form := (word-form word))
         (unique-tag := (gethash form (pos-tagger-unique-tag-words tagger)))
         (tag := unique-tag)
         (unless unique-tag
           (for (feature :across (forward-features word sentence next-tag next-next-tag))
                (type = (svref feature 0))
                (specializer = (svref feature 1))
                (value = (svref feature 2))
                (type-table = (gethash type (pos-tagger-weights tagger)))
                (specializer-table = (when type-table (gethash specializer type-table)))
                (value-table = (when specializer-table (gethash value specializer-table)))
                (when value-table
                  (for (class weight :in-table value-table)
                       (setf (gethash class (svref scores i))
                             (+ weight (float (gethash class (svref scores i) 0.0))))))
                (:final (setq tag (for (class weight :in-table (svref scores i))
                                       (:find class :max weight)))))))
    (for (table :across scores)
         (collect (for (class weight :in-table table)
                       (:find class :max weight))))))

(defun update (tagger truth guess features)
  (declare (optimize (speed 3) (space 0) (safety 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (flet ((upd (feature class val)
           (declare (inline upd))
           (let* ((update-count (pos-tagger-update-count tagger))
                  (last-update  (get-weight (pos-tagger-last-updates tagger) feature class))
                  (lived-cycle  (- update-count last-update)))
             (add-weight (pos-tagger-living-weights tagger) feature class
                         (* lived-cycle (get-weight (pos-tagger-weights tagger) feature class)))
             (set-weight (pos-tagger-last-updates tagger) feature class update-count)
             (add-weight (pos-tagger-weights tagger) feature class val))
           ))
    (incf (pos-tagger-update-count tagger))
    (unless (eq truth guess)
      (for (feature :across features)
           (upd feature truth 1)
           (upd feature guess -1)))))

(defun average-weights (tagger)
  (declare (optimize (space 0) (speed 3) (safety 0) (float 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (for* (type type-table :in-table (pos-tagger-weights tagger))
        (specializer specializer-table :in-table type-table)
        (value value-table :in-table specializer-table)
        (class :in-hash-keys value-table)
        (let* ((feature (vector type specializer value))
               (last-update (get-weight (pos-tagger-last-updates tagger) feature class))
               (lived-cycle (- (pos-tagger-update-count tagger) last-update)))
          (add-weight (pos-tagger-living-weights tagger) feature class
                      (* lived-cycle (get-weight (pos-tagger-weights tagger) feature class)))
          (let ((living-weight (get-weight (pos-tagger-living-weights tagger) feature class)))
            (set-weight (pos-tagger-weights tagger) feature class
                        (float (/ living-weight (pos-tagger-update-count tagger))))))))

(defun train (tagger save-dir sentences &optional (cycles 5))
  (declare (optimize (speed 3) (space 0) (safety 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (log-info "Start training with ~D sentences, ~D cycles. Current heap size: ~A"
            (length sentences) cycles #+lispworks (print-size (getf (sys:room-values) :total-size)) #-lispworks nil)
  (let ((table (make-hash-table :test #'eq)))
    (for* (sentence :across sentences)
          (word :across sentence)
          (incf (getf (gethash (word-form word) table) (word-upos word) 0)))
    (for (form plist :in-table table)
         (if (> (length plist) 2)
             (remhash form table)
           (setf (gethash form table) (first plist))))
    (setf (pos-tagger-unique-tag-words tagger) table)
    (log-info "Found ~A words has unique tag." (hash-table-count table)))
  (for (cycle :range cycles)
       (let ((correct-count    0)
             (total-count      0)
             (cycle-start-time (get-internal-real-time)))
         (for (sentence :across sentences)
              (guesses := (predict-sentence tagger sentence))
              (for (prev-tag      :prev   guess 'start)
                   (prev-prev-tag :prev   prev-tag 'start)
                   (i             :index)
                   (word          :across sentence)
                   (guess         :in     guesses)
                   (truth         :=      (word-upos word))
                   (features      :=      (all-features word sentence prev-tag prev-prev-tag
                                                        (nth (1+ i) guesses) (nth (+ 2 i) guesses)))
                   (update tagger truth guess features)
                   (when (eq guess truth) (incf correct-count))
                   (incf total-count)))
         (shuffle sentences)
         (log-info "Cycle ~D/~D completed using ~,2Fs with ~D/~D (~,2F%) correct. Heap size: ~A"
                   (1+ cycle) cycles (/ (- (get-internal-real-time) cycle-start-time) 1000)
                   correct-count total-count (* 100.0 (/ correct-count total-count))
                   #+lispworks (print-size (getf (sys:room-values) :total-size)) #-lispworks nil)))
  (average-weights tagger)
  (save-tagger tagger save-dir))

;; Test

(defun test-tagger (tagger sentences)
  (let ((correct-count       0)
        (total-count         0)
        (wrong-predict-table (make-hash-table :test #'eq))
        (start-time          (get-internal-real-time)))
    (for (sentence :across sentences)
         (for (word :across sentence)
              (guess :in (predict-sentence tagger sentence))
              (truth = (word-upos word))
              (if (eq guess truth)
                  #|(or (eq guess truth)
                      (and (member guess '(:noun :propn))
                           (member truth '(:noun :propn))))|#
                (incf correct-count)
                (incf (getf (gethash truth wrong-predict-table) guess 0)))
              (incf total-count)))
    (log-info "Test ~D sentences using ~,2Fs, result: ~D/~D (~,2F%)"
              (length sentences)
              #+lispworks (/ (- (get-internal-real-time) start-time) 1000)
              #-lispworks (/ (- (get-internal-real-time) start-time) 1000000)
              correct-count total-count (* 100 (/ correct-count total-count)))
    (values (float (* 100 (/ correct-count total-count)))
            wrong-predict-table)))

(defun test-training ()
  (let ((tagger (make-pos-tagger)))
    (train tagger "~/common-lisp/pos/"
           (read-conllu-files #P"~/Downloads/ud-treebanks-v2.14/UD_English-GUM/en_gum-ud-train.conllu"
                              ;#P"~/Downloads/ud-treebanks-v2.14/UD_English-EWT/en_ewt-ud-train.conllu"
                              ;#P"~/Downloads/ud-treebanks-v2.14/UD_English-Atis/en_atis-ud-train.conllu"
                              ;#P"~/Downloads/ud-treebanks-v2.14/UD_English-ParTUT/en_partut-ud-train.conllu"
                              )
           5)
    (test-tagger tagger (read-conllu-file #P"~/Downloads/ud-treebanks-v2.14/UD_English-GUM/en_gum-ud-test.conllu"))
    (setq *loaded-tagger* tagger)))

;(test-tagger *loaded-tagger* (read-conllu-file #P"~/Downloads/ud-treebanks-v2.14/UD_English-GUM/en_gum-ud-test.conllu"))

(export '(train test-tagger test-training predict-sentence))
