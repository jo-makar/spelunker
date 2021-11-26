(load "config.lisp")

(require 'asdf)
(require 'cl-ppcre)
(require 'sqlite)

(defun tf-idf-score (entry database)
  (let* ((token-list       (tokenize (text entry)))
         (token-hash-table (let ((hash-table (make-hash-table :test #'equal)))
                             (loop for token in token-list
                                   do (multiple-value-bind (value exists) (gethash token hash-table)
                                        (setf (gethash token hash-table) (if exists (1+ value) 1))))
                             hash-table))
         (token-tf-idf     (make-hash-table :test #'equal))
         (matched-phrases  '())
         (score            0.0))
          
    (sqlite:execute-non-query database
      "insert into state (key, value) values (\"corpus_doc_count\", 1) \
       on conflict (key) do update set value=value+1")

    (loop for token being the hash-key using (hash-value entry-token-count) of token-hash-table
          do (sqlite:execute-non-query database
               "insert into word_count (word, count) values (?, ?) \
                on conflict (word) do update set count=count+?"
                token entry-token-count entry-token-count)
             (sqlite:execute-non-query database
               "insert into doc_count (word, count) values (?, 1) \
                on conflict (word) do update set count=count+1" token)

             (let* ((corpus-token-count (sqlite:execute-single database
                                          "select count from doc_count where word=?" token))
                    (corpus-doc-count   (sqlite:execute-single database
                                          "select value from state where key=\"corpus_doc_count\""))
                    (term-freq          (/ entry-token-count (length token-list)))
                    (inv-doc-freq       (1+ (log (/ corpus-doc-count corpus-token-count))))
                    (tf-idf             (* term-freq inv-doc-freq)))
              (setf (gethash token token-tf-idf) tf-idf)))
  
    (loop for score-weight in (cdr (assoc 'score-weights *config*))
          do (let* ((phrase-list   (car score-weight))
                    (phrase-weight (cdr score-weight))
                    (phrase-values (loop for word in phrase-list
                                         collect (gethash word token-tf-idf))))
               (when (every (lambda (x) x) phrase-values)
                 (incf score (* (apply #'+ phrase-values) phrase-weight))
                 (setq matched-phrases (cons phrase-list matched-phrases)))))

    (values
      score 
      matched-phrases
      (sort (loop for token being the hash-key using (hash-value tf-idf) of token-tf-idf
                  collect (cons token tf-idf))
            #'> :key #'cdr))))

(defun tokenize (text)
  (let ((word-char-p (lambda (c) (or (alpha-char-p c)
                                     (char= c #\')
                                     (char= c #\-)))))
    (remove-if
      (lambda (s) (position-if-not word-char-p s))
      (remove-if
        (lambda (s) (= (length s) 0))

        ; Strip off non-word chars from ends
        (mapcar (lambda (s)
                  (let* ((i (position-if word-char-p s))
                         (j (when i
                              (1+ (position-if word-char-p s :from-end t)))))
                    (if i (subseq s i j) "")))

          (mapcar #'string-downcase
            (cl-ppcre:split "\\s+" text)))))))
