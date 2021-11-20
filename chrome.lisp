(load "http.lisp")
(defvar *log-level* 'debug) ; FIXME Remove
(load "log-format.lisp")
(load "ring-buffer.lisp")
(load "websocket.lisp")

(require 'asdf)
(require 'yason)

(defclass chrome ()
  ((process)
   (websocket)
   (messages :initform (make-instance 'ring-buffer))
   (request-id :initform 0)))

(defun make-chrome (&key incognito)
  (let ((self (make-instance 'chrome)))
    
    (with-slots (process websocket messages) self
      (setf process
            (sb-ext:run-program
              "/opt/google/chrome/chrome"
              (append (list "--headless"
                            "--remote-debugging-port=0"
                            (format nil "--user-data-dir=~a/.config/google-chrome/"
                              (or (sb-ext:posix-getenv "HOME")
                                  (error "HOME environment variable not defined"))))
                      (when incognito '("--incognito")))
              :wait nil
              :input nil
              :output :stream
              :error :output))
      (log-format 'info "chrome process launched (pid ~d)" (sb-ext:process-pid process))
      
      (let ((devtools-url nil))
        (handler-case
          (sb-sys:with-deadline (:seconds 15)
            (let ((prefix "DevTools listening on ")
                  (output (sb-ext:process-output process)))
              (loop for line = (read-line output)
                do (log-format 'debug "chrome: ~a" line)
                   (when (eql (search prefix line) 0)
                     (setq devtools-url (subseq line (length prefix)))
                     (log-format 'info "using devtools url ~a" devtools-url)
                     (return)))))
          (sb-sys:deadline-timeout (c)
            (declare (ignore c))
            (error "deadline reached looking for devtools url")))

        (sb-thread:make-thread
          (lambda (output)
            (loop for line = (read-line output nil)
              while line
              do (log-format 'debug "chrome: ~a" line)))
          :arguments (list (sb-ext:process-output process))
          :name "chrome-stdout/err")

        (sleep 5)
        (let* ((port         (caddr (parse-url devtools-url)))
               (json-url     (format nil "http://127.0.0.1:~d/json" port))
               (id           (gethash "id" (car (yason:parse (http-get json-url)))))
               (debugger-url (format nil "ws://127.0.0.1:~d/devtools/page/~a" port id)))
          (log-format 'info "using debugger url ~a" debugger-url)
          (setf websocket (make-websocket debugger-url))))

      (sb-thread:make-thread
        (lambda (websocket messages)
          (loop for message = (websocket-read websocket)
            while message
            do (log-format 'debug ">>> ~a" message)
               (ring-buffer-insert messages (make-instance 'message :json (yason:parse message)))))
        :arguments (list websocket messages)
        :name "chrome-websocket")

      ; FIXME Review NewBrowserDevTools from ~/projects/scrapers/browser_devtools.go
      (chrome-execute self "Runtime.evaluate" '(("expression" . "navigator.userAgent")))
      ;(websocket-write websocket "{\"id\":0,\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"navigator.userAgent\"}}")
      ;(format t "~a~%" (websocket-read websocket))
      )

    self))

(defmethod chrome-close ((self chrome))
  (with-slots (process websocket) self
    (websocket-close websocket)

    (when (sb-ext:process-alive-p process)
      (flet ((process-wait-with-deadline (event &optional deadline)
               (handler-case
                 (sb-sys:with-deadline (:seconds (or deadline 15))
                   (sb-ext:process-wait process)
                   (return-from chrome-close))
                 (sb-sys:deadline-timeout (c)
                   (declare (ignore c))
                   (log-format 'warn "deadline reached waiting for process after ~a" event)))))

        ; FIXME Send a Browser.close command over the websocket
        ;(process-wait-with-deadline "Browser.close")

        (sb-ext:process-kill process 15)
        (process-wait-with-deadline "SIGTERM")

        (sb-ext:process-kill process 9)
        (process-wait-with-deadline "SIGKILL")

        (error "unable to kill chrome process (pid ~d)" (sb-ext:process-pid process))))))

(defmethod chrome-execute ((self chrome) method &optional params)
  (with-slots (websocket messages request-id) self
    (let ((request-hash-table (make-hash-table)))
      (setf (gethash "id" request-hash-table) (incf request-id))
      (setf (gethash "method" request-hash-table) method)
      (when params
        (let ((params-hash-table (make-hash-table)))
          (loop for pair in params
                do (setf (gethash (car pair) params-hash-table) (cdr pair)))
          (setf (gethash "params" request-hash-table) params-hash-table)))
      (let ((request-string (let ((stream (make-string-output-stream)))
                              (yason:encode request-hash-table stream)
                              (get-output-stream-string stream))))
        (log-format 'debug "<<< ~a" request-string)
        (websocket-write websocket request-string)))

    ; FIXME STOPPED Wait for response
    ;               Define a predicate to match the request-id for the preceding request
    ;(loop for response = (ring-buffer-find-if ring-buffer pred)
    ;      if response
    ;        do (format t "~a~%" response)
    ;           (return)
    ;      else
    ;        do (sleep 1))

    ; FIXME Implement a timeout mechanism (using sb-sys:with-deadline?)
    ;       It should be specified by an optional parameter to the method
    ))

(defclass message ()
  ((json      :initarg :json :reader json)
   (timestamp :initform (get-universal-time) :reader timestamp)))

; FIXME Remove
(let ((chrome (make-chrome)))
  (sleep 1)
  (chrome-execute chrome "foo" '(("a" . 1) ("b" . 2)))
  (sleep 1)
  (chrome-close chrome))

; TODO Add support for sessions (via devtools Target.attachToTarget?)
