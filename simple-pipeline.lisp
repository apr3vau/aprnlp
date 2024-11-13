(in-package aprnlp)

(defclass simple-pipeline ()
  ((name   :initarg  :name
           :initform "unnamed-pipeline")
   (tagger :initarg  :tagger
           :initform *loaded-pos-tagger*)
   (parser :initarg  :parser
           :initform *loaded-dep-parser*)))

(defgeneric pipeline-process (pipeline result-type text)
  (:method ((pipeline simple-pipeline) (result-type (eql :dep)) text)
   (with-slots (tagger parser) pipeline
     (let ((sentences (simple-tokenize text)))
       (iter (for sentence :in-vector sentences)
             (process tagger sentence)
             (process parser sentence))
       sentences)))
  (:method ((pipeline simple-pipeline) (result-type (eql :pos)) text)
   (with-slots (tagger) pipeline
     (let ((sentences (simple-tokenize text)))
       (iter (for sentence :in-vector sentences)
             (process tagger sentence))
       sentences)))
  (:method ((pipeline simple-pipeline) (result-type (eql :token)) text)
   (simple-tokenize text))
  (:method ((pipeline simple-pipeline) result-type text)
   (pipeline-process pipeline :dep text)))

(defgeneric train-pipeline (class sentences &key name cycles save-dir)
  (:method ((class (eql 'simple-pipeline)) sentences &key (name "unnamed-pipeline") (cycles 5) (save-dir (asdf:system-source-directory :aprnlp)))
   (let* ((tagger (make-instance 'pos-tagger :name "tagger"))
          (parser (make-instance 'dep-parser :name "parser"))
          (pipeline-dir (ensure-directories-exist
                         (make-pathname :directory (append (pathname-directory save-dir) (list name))
                                        :name nil :type nil :defaults save-dir))))
     (train tagger sentences :cycles cycles :save-dir pipeline-dir)
     (let ((new-sentences (copy-sentences sentences)))
       (iter (for sentence :in-vector new-sentences)
             (process tagger sentence))
       (train parser new-sentences :cycles cycles :save-dir pipeline-dir))
     (values (make-instance 'simple-pipeline :name name :tagger tagger :parser parser)
             pipeline-dir))))

(defgeneric load-pipeline (class directory)
  (:method ((class (eql 'simple-pipeline)) directory)
   (let ((tagger (load-processor 'pos-tagger (merge-pathnames "tagger.fasl" directory)))
         (parser (load-processor 'dep-parser (merge-pathnames "parser.fasl" directory))))
     (make-instance 'simple-pipeline
                    :name (car (last (pathname-directory directory)))
                    :tagger tagger :parser parser))))

(defmethod test ((pipeline simple-pipeline) sentences)
  (with-slots (tagger parser) pipeline
    (test tagger sentences)
    (let ((new-sentences (copy-sentences sentences)))
      (iter (for sentence :in-vector new-sentences)
            (process tagger sentence))
      (test parser sentences))))

(defmethod test-training ((class (eql 'simple-pipeline)))
  (let ((pipeline (train-pipeline 'simple-pipeline (read-conllu-file (treebank-file :english "GUM" :train)))))
    (test pipeline (read-conllu-file (treebank-file :english "GUM" :test)))))

(export '(pipeline-process train-pipeline load-pipeline simple-pipeline name tagger parser))