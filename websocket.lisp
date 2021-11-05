(load "base64.lisp")

(require 'asdf)
(require 'ironclad)

(require 'sb-bsd-sockets)

(defclass websocket ()
  ((socket)
   (stream)))

(defun make-websocket (url)
  (let ((obj (make-instance 'websocket)))

    (with-slots (socket stream) obj
      (let* ((u      (parse-websocket-url url))
             (host   (car u))
             (ip     (sb-bsd-sockets:host-ent-address
                       (sb-bsd-sockets:get-host-by-name host)))
             (port   (or (cadr u) 80))
             (path   (or (caddr u) "/"))
             (key    (let ((state (make-random-state t)))
                       (base64-encode
                         (concatenate 'string (loop repeat 16
                                                collect (code-char (random 256 state)))))))
             (accept (flet ((byte-array-to-char-list (input)
                              (loop for b across input
                                collect (code-char b))))
                       (let ((postfix "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
                         (base64-encode
                           (byte-array-to-char-list
                             (ironclad:digest-sequence :sha1
                               (ironclad:ascii-string-to-byte-array
                                 (concatenate 'string key postfix)))))))))
        (setf socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (sb-bsd-sockets:socket-connect socket ip port)
        (setf stream (sb-bsd-sockets:socket-make-stream socket :input t :output t))

        (format stream "GET ~a HTTP/1.1~c~c" path #\return #\linefeed)
        (format stream "Host: ~a~c~c" host #\return #\linefeed)
        (format stream "Upgrade: websocket~c~c" #\return #\linefeed)
        (format stream "Connection: Upgrade~c~c" #\return #\linefeed)
        (format stream "Sec-WebSocket-Key: ~a~c~c" key #\return #\linefeed)
        (format stream "Sec-WebSocket-Version: 13~c~c" #\return #\linefeed)
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
            (unless (string= (car lines) "HTTP/1.1 101 WebSocket Protocol Handshake")
              (error "unexpected response"))
            (let ((val (cdr (assoc "Sec-WebSocket-Accept" headers :test #'string=))))
              (unless val
                (error "missing Sec-WebSocket-Accept header"))
              (unless (string= val accept)
                (error "invalid Sec-WebSocket-Accept header")))))))

    obj))

(defun parse-websocket-url (url)
  (let ((host nil)
        (port nil)
        (path nil)
        (i    0))
    (let ((prefix "ws://"))
      (unless (eql (search prefix url) 0)
        (error "unexpected url scheme"))
      (incf i (length prefix)))
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
        (return-from parse-websocket-url (list host nil nil)))
      (setq i j))

    (when (char= (char url i) #\:)
      (let ((j (position-if-not #'digit-char-p url :start (1+ i))))
        (setq port (parse-integer (subseq url (1+ i) j)))
        (unless j
          (return-from parse-websocket-url (list host port nil)))
        (setq i j)))

    (setq path (subseq url i))
    (list host port path)))

(defmethod websocket-close ((obj websocket))
  (with-slots (socket stream) obj
    (close stream)
    (sb-bsd-sockets:socket-close socket)))
