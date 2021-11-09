(load "http.lisp")
(load "log.lisp")
(load "websocket.lisp")

(require 'asdf)
(require 'yason)

(defclass chrome ()
  ((process)
   (websocket)
   (request-id :initform 0)))

(defun make-chrome (&key incognito)
  (let ((self (make-instance 'chrome)))
    
    (with-slots (process websocket) self
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
          :arguments (list (sb-ext:process-output process)))

        (sleep 5)
        (let* ((port         (caddr (parse-url devtools-url)))
               (json-url     (format nil "http://127.0.0.1:~d/json" port))
               (id           (gethash "id" (car (yason:parse (http-get json-url)))))
               (debugger-url (format nil "ws://127.0.0.1:~d/devtools/page/~a" port id)))
          (log-format 'info "using debugger url ~a" debugger-url)
          (setf websocket (make-websocket debugger-url))))

      ; FIXME Need support for websocket framing impelementation first
      ; FIXME Make a thread that continuously reads from websocket
      ;       Perhaps this shouldn't read lines but chunks instead
      ;       Will need to process messages received
      ;(sb-thread:make-thread
      ;  (lambda (stream)
      ;    (loop for line = (read-line stream nil)
      ;      while line
      ;      ; FIXME Change to debug
      ;      do (log-format 'info ">>> ~a" line)))
      ;  :arguments (list (slot-value websocket 'stream)))

      ; FIXME Make this a user-friendly method
      ;       Consider writing a loop that accepts an assoc list
      ;(let ((hash-table (make-hash-table)))
      ;  (setf (gethash "id" hash-table) 0)
      ;  (setf (gethash "method" hash-table) "navigator.userAgent")
      ;  (format (slot-value websocket 'stream) "~a" (yason:encode hash-table))
      ;  )
      )

    self))

(defmethod chrome-close ((self chrome))
  (websocket-close (slot-value self 'websocket))

  (let ((process (slot-value self 'process)))
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

; FIXME Remove
(let ((chrome (make-chrome)))
  (sleep 15)
  (chrome-close chrome))
