(defvar *log-level* 'info)
    
(defun log-format (level control-string &rest args)
  (let* ((levels '(warn info debug trace))
         (p      (position level levels))
         (q      (position *log-level* levels)))
    (when (and p q (<= p q))
      (setq control-string
            (concatenate 'string (multiple-value-bind
                                   (sec min hour date month year)
                                   (decode-universal-time (get-universal-time))
                                   (format nil "~d-~2,'0d-~d ~2,'0d:~2,'0d:~2,'0d:"
                                     year month date hour min sec))
                                 (string-downcase (string level)) ":"
                                 control-string
                                 (unless (eq (search "~%" control-string)
                                             (- (length control-string) 2))
                                   "~%")))
      (apply #'format (append (list t control-string) args)))))

; TODO Emulate the flexible logging framework defined here:
;      https://gist.github.com/jo-makar/3f2b1dd6b4bb2f9d72208e980dea7210
;      How best to get calling function name, file name, and line number?
