(defun base64-encode (input)
  (let ((chunks (labels ((splitter (input split)
                           (cond ((>= (length input) 3) (splitter (subseq input 3)
                                                                  (cons (subseq input 0 3) split)))
                                 ((> (length input) 0)  (cons input split))
                                 ((= (length input) 0)  split))))
                  (reverse (splitter (coerce input 'string) '())))))

    (labels ((lookup (v)
             (cond ((< v 0)  (error "v < 0"))
                   ((< v 26) (code-char (+ (char-code #\A) v)))
                   ((< v 52) (code-char (+ (char-code #\a) (- v 26))))
                   ((< v 62) (code-char (+ (char-code #\0) (- v 52))))
                   ((= v 62) #\+)
                   ((= v 63) #\/)
                   (t        (error "v > 63"))))

             (convert (chunk)
               (case (length chunk)
                     (3 (let ((bits (logior (ash (char-code (aref chunk 0)) 16)
                                            (ash (char-code (aref chunk 1)) 8)
                                                 (char-code (aref chunk 2)))))
                          (list (lookup (logand (ash bits -18) #b111111))
                                (lookup (logand (ash bits -12) #b111111))
                                (lookup (logand (ash bits -6)  #b111111))
                                (lookup (logand      bits      #b111111)))))
                     (2 (let ((bits (logior (ash (char-code (aref chunk 0)) 10)
                                            (ash (char-code (aref chunk 1)) 2))))
                          (list (lookup (logand (ash bits -12) #b111111))
                                (lookup (logand (ash bits -6)  #b111111))
                                (lookup (logand      bits      #b111111))
                                #\=)))
                     (1 (let ((bits (ash (char-code (aref chunk 0)) 4)))
                          (list (lookup (logand (ash bits -6) #b111111))
                                (lookup (logand      bits     #b111111))
                                #\=
                                #\=))))))

      (concatenate 'string (mapcan #'convert chunks)))))

; TODO base64-decode
