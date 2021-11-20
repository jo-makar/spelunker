(load "http.lisp")
(load "log-format.lisp")
(load "ring-buffer.lisp")
(load "websocket.lisp")

(require 'asdf)
(require 'cl-ppcre)
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

      (let ((useragent (chrome-execute self "Runtime.evaluate"
                                            :params '(("expression" . "navigator.userAgent")))))
        (setq useragent (cl-ppcre:regex-replace "HeadlessChrome" useragent "Chrome"))
        (chrome-execute self "Network.setUserAgentOverride"
                             :params `(("userAgent" . ,useragent))))

      ; Enable the notification of Page domain events (used by chrome-goto)
      ; TODO Consider also enabling notification for DOM and Network events
      (chrome-execute self "Page.enable"))

    self))

(defmethod chrome-close ((self chrome))
  (with-slots (process websocket) self
    (handler-case
      (progn
        (chrome-execute self "Browser.close" :deadline 10)
        (return-from chrome-close))
      (sb-sys:deadline-timeout (c)
        (declare (ignore c))
        (log-format 'warn "deadline reached waiting for Browser.close")))
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
        (sb-ext:process-kill process 15)
        (process-wait-with-deadline "SIGTERM")
        (sb-ext:process-kill process 9)
        (process-wait-with-deadline "SIGKILL")
        (error "unable to kill chrome process (pid ~d)" (sb-ext:process-pid process))))))

(defmethod chrome-execute ((self chrome) method &key params deadline raw-response)
  (with-slots (websocket messages request-id) self
    (let ((request-hash-table (make-hash-table :test #'equal)))
      (setf (gethash "id" request-hash-table) (incf request-id))
      (setf (gethash "method" request-hash-table) method)
      (when params
        (let ((params-hash-table (make-hash-table :test #'equal)))
          (loop for pair in params
                do (setf (gethash (car pair) params-hash-table) (cdr pair)))
          (setf (gethash "params" request-hash-table) params-hash-table)))
      (let ((request-string (let ((stream (make-string-output-stream)))
                              (yason:encode request-hash-table stream)
                              (get-output-stream-string stream))))
        (log-format 'debug "<<< ~a" request-string)
        (websocket-write websocket request-string))

      (sb-sys:with-deadline (:seconds (or deadline 5))
        (flet ((equal-request-id-p (message)
                 (eql (gethash "id" (slot-value message 'json))
                      (gethash "id" request-hash-table))))
          (loop for response = (ring-buffer-find-if messages #'equal-request-id-p)
                if response
                  do (with-slots (json) response
                       (when raw-response
                         (return-from chrome-execute json))
                       ; FIXME STOPPED Write a helper function to simplify use of multiple-value-bind in this file
                       (multiple-value-bind (result result-exists) (gethash "result" json)
                         (unless result-exists
                           (error "result key not found"))
                         (multiple-value-bind (result2 result2-exists) (gethash "result" result)
                           (return-from chrome-execute
                             (when result2-exists
                               (return-from chrome-execute (gethash "value" result2)))))))
                else
                  do (sleep 0.1)))))))

(defmethod chrome-goto ((self chrome) url &key referrer deadline)
  (sb-sys:with-deadline (:seconds (or deadline 60))
    (with-slots (messages) self
      (let ((params `(("url" . ,url))))
        (when referrer
          (setf params (cons `("referrer" . ,referrer) params)))

        (let* ((response (chrome-execute self "Page.navigate" :params params :raw-response t))
               (frame-id  (multiple-value-bind (result result-exists) (gethash "result" response)
                            (unless result-exists
                              (error "result key not found"))
                            (multiple-value-bind (frame-id frame-id-exists) (gethash "frameId" result)
                              (unless frame-id-exists
                                (error "frameId key not found"))
                              frame-id))))
          (flet ((navigated-frame-id-p (message)
                   (with-slots (json) message
                     (unless (string= (gethash "method" json) "Page.frameNavigated")
                       (return-from navigated-frame-id-p))
                     (multiple-value-bind (params params-exists) (gethash "params" json)
                       (unless params-exists
                         (return-from navigated-frame-id-p))
                       (multiple-value-bind (frame frame-exists) (gethash "frame" params)
                         (unless frame-exists
                           (return-from navigated-frame-id-p))
                         (multiple-value-bind (id id-exists) (gethash "id" frame)
                           (unless id-exists
                             (return-from navigated-frame-id-p))
                           (string= id frame-id)))))))
            (loop for message = (ring-buffer-find-if messages #'navigated-frame-id-p)
                  if message
                    do (return-from chrome-goto)
                  else
                    do (sleep 0.1))))))))
  
; TODO Implement chrome-wait-for-event, chrome-input-text (cf the golang impl)

(defclass message ()
  ((json      :initarg :json :reader json)
   (timestamp :initform (get-universal-time) :reader timestamp)))

; FIXME Remove
(let ((chrome (make-chrome)))
  (chrome-goto chrome "https://www.google.com")
  (chrome-close chrome))

; TODO Add support for simultaneous sessions (via devtools Target.attachToTarget)
