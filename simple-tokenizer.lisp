(in-package aprnlp)

(defvar *sentence-ending-regex*
  (ppcre:create-scanner "(. )|(; )|(
)"))

(defun simple-tokenize (text)
  (let* ((sentences-list (ppcre:split *sentence-ending-regex* text))
         (sentences (make-array (length sentences-list) :element-type 'vector :fill-pointer 0)))
    (iter (for sentence-string :in sentences-list)
          (for words-list :next (split-sequence #\Space sentence-string :remove-empty-subseqs t))
          (for sentence :next (make-array (length words-list) :element-type 'word :fill-pointer 0))
          (iter (for id :from 0)
                (for form :in words-list)
                (vector-push
                 (make-word :id id
                            :form (intern form "POS/WORDS")
                            :suffix (intern (subseq form (- (length form) (min 3 (length form)))) "POS/WORDS")
                            :prefix (intern (subseq form 0 (min 3 (length form))) "POS/WORDS") )
                 sentence))
          (vector-push sentence sentences))
    sentences))
