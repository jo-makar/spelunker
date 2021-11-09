(load "base64.lisp")
(load "http.lisp")

(require 'asdf)
(require 'ironclad)

(require 'sb-bsd-sockets)

(defclass websocket ()
  ((socket)
   (stream)))

(defun make-websocket (url)
  (let ((self (make-instance 'websocket)))

    (with-slots (socket stream) self
      (let* ((u      (parse-url url))
             (scheme (car u))
             (host   (cadr u))
             (ip     (sb-bsd-sockets:host-ent-address
                       (sb-bsd-sockets:get-host-by-name host)))
             (port   (or (caddr u)
                         (if (string= scheme "wss") 443 80)))
             (path   (or (cadddr u) "/"))
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

        ; TODO Add support for wss / tls
        (unless (string= scheme "ws")
          (error "unsupported scheme"))

        (setf socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (sb-bsd-sockets:socket-connect socket ip port)

        ; Use element-type of :default to support a bivalent stream (ie chars and bytes)
        (setf stream (sb-bsd-sockets:socket-make-stream socket :element-type :default
                                                               :input t :output t))

        (format stream "GET ~a HTTP/1.1~c~c" path #\return #\linefeed)
        (format stream "Host: ~a:~d~c~c" host port #\return #\linefeed)
        (format stream "Upgrade: websocket~c~c" #\return #\linefeed)
        (format stream "Connection: Upgrade~c~c" #\return #\linefeed)
        (format stream "Sec-WebSocket-Key: ~a~c~c" key #\return #\linefeed)
        (format stream "Sec-WebSocket-Version: 13~c~c" #\return #\linefeed)
        (format stream "Origin: http://~a~c~c" host #\return #\linefeed)
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

    self))

(defmethod websocket-close ((self websocket))
  (with-slots (socket stream) self
    (close stream)
    (sb-bsd-sockets:socket-close socket)))

(defmethod websocket-write ((self websocket) payload)
  (let ((frame    (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t))
        (binary   (let ((binary-char-p (lambda (c)
                                         (let ((d (char-code c)))
                                           (or (and (<= d 31) (/= d 9) (/= d 10) (/= d 13))
                                               (>= d 128))))))
                    (some binary-char-p payload)))
        (mask-key (let ((state (make-random-state t)))
                    (loop repeat 4 collect (random 256 state)))))
    (vector-push-extend (logior #x80 (if binary #x2 #x1)) frame)

    (let ((l (length payload)))
      (cond ((< l 126)        (vector-push-extend (logior #x80 l) frame))
            ((< l (ash 1 16)) (vector-push-extend (logior #x80 126) frame)
                              (loop for i from -8 to 0 by 8
                                    do (vector-push-extend (logand (ash l i) #xff) frame)))
            ((< l (ash 1 64)) (vector-push-extend (logior #x80 127) frame)
                              (loop for i from -56 to 0 by 8
                                    do (vector-push-extend (logand (ash l i) #xff) frame)))
            (t                (error "max frame size exceeded"))))

    (loop for b in mask-key do (vector-push-extend b frame))

    (loop for b across payload
          for k = 0 then (mod (1+ k) 4)
          do (vector-push-extend (logxor (char-code b) (nth k mask-key)) frame))

    (let ((stream (slot-value self 'stream)))
      (write-sequence frame stream)
      (finish-output stream))))

; FIXME STOPPED Will need to retrieve and parse websocket frames and process them into messages
;               Will need to maintain a buffer of frames and unconsumed messages
;               Likely will also need to handle control frames as well here (ping, pong, close)
;               Rely on caller's invocation of "read-message" to drive when frames are processed
;               Ie it potentially blocks and returns the next (potentially buffered) message
(defmethod websocket-read ((self websocket))
  (let ((stream (slot-value self 'stream))
        (frame (make-array 2 :adjustable t :fill-pointer 0)))
    (read-sequence frame stream)
    (print frame)
    ))

; FIXME Remove
(let ((websocket (make-websocket "ws://127.0.0.1:39285/devtools/page/596DC941BFCF753717BCF4FFC8A9461F")))
  (sleep 1)
  (websocket-write websocket "{\"id\":0,\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"navigator.userAgent\"}}")
  (sleep 1)
  (websocket-read websocket)
  (websocket-close websocket))
