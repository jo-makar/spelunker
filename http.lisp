(defun parse-url (url)
  (let ((scheme nil)
        (host   nil)
        (port   nil)
        (path   nil)
        (i      0))

    (loop for s in '("http" "https" "ws" "wss")
          for p = (concatenate 'string s "://")
          do (when (eql (search p url) 0)
               (setq scheme s)
               (incf i (length p))))
    (unless scheme
      (error "unexpected url scheme"))
    (when (= i (length url))
      (error "incomplete url"))

    (let ((c (char url i))
          (j nil))
      (cond ((alpha-char-p c); Hostname
              (let ((p (lambda (c) (or (alphanumericp c) (char= c #\-) (char= #\.)))))
                (setq j (position-if-not p url :start i))
                (setq host (subseq url i j))))
            ((digit-char-p c) ; IPv4 address
              (let ((p (lambda (c) (or (digit-char-p c) (char= c #\.)))))
                (setq j (position-if-not p url :start i))
                (setq host (subseq url i j))))
            ((char= c #\[); IPv6 address
              (let ((k (position #\] url :start i)))
                (unless k
                  (error "malformed ipv6 host"))
                (setq host (subseq url (1+ i) k))
                (setq j (1+ k))))
            (t (error "unexpected host format")))
      (when (or (null j) (= j (length url)))
        (return-from parse-url (list scheme host nil nil)))
      (setq i j))

    (when (char= (char url i) #\:)
      (let ((j (position-if-not #'digit-char-p url :start (1+ i))))
        (setq port (parse-integer (subseq url (1+ i) j)))
        (unless j
          (return-from parse-url (list scheme host port nil)))
        (setq i j)))

    (setq path (subseq url i))
    (list scheme host port path)))

(defun http-get (url)
  (let* ((u      (parse-url url))
         (scheme (car u))
         (host   (cadr u))
         (ip     (sb-bsd-sockets:host-ent-address
                   (sb-bsd-sockets:get-host-by-name host)))
         (port   (or (caddr u)
                     (if (string= scheme "https") 443 80)))
         (path   (or (cadddr u) "/"))
         (socket nil)
         (stream nil))

    ; TODO Add support for https / tls
    (unless (string= scheme "http")
      (error "unsupported scheme"))

    (setf socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
    (sb-bsd-sockets:socket-connect socket ip port)
    (setf stream (sb-bsd-sockets:socket-make-stream socket :input t :output t))

    (format stream "GET ~a HTTP/1.1~c~c" path #\return #\linefeed)
    (format stream "Host: ~a~c~c" host #\return #\linefeed)
    (format stream "~c~c" #\return #\linefeed)
    (finish-output stream)

    (sb-sys:with-deadline (:seconds 30)
      (let* ((lines   (loop for line = (string-right-trim '(#\return) (read-line stream))
                        while (string/= line "")
                        collect line))
             (headers (flet ((parser (line)
                               (let ((i (position #\: line)))
                                 (or (and i (> i 0) (< i (1- (length line))))
                                     (error "invalid header"))
                                 (let ((key (subseq line 0 i))
                                       (val (string-trim '(#\space #\tab) (subseq line (1+ i)))))
                                   (or (> (length val) 0)
                                       (error "invalid header"))
                                   (cons key val)))))
                        (mapcar #'parser (cdr lines)))))
        (unless (string= (car lines) "HTTP/1.1 200 OK")
          (error "unexpected response"))
        (let ((val (cdr (assoc "Content-Length" headers :test #'string=))))
          (unless val
            (error "missing Content-Length header"))
          (let* ((len  (parse-integer val))
                 (body (make-array len)))
            (read-sequence body stream)
            (coerce body 'string)))))))

; TODO Implement http-{post,put,delete,...}
;      Or generalize http-get for other methods
