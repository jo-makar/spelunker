(load "log.lisp")

(defclass chrome ()
  ((process)))

(defun make-chrome (&key incognito)
  (let ((obj (make-instance 'chrome)))
    
    (with-slots (process) obj
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
              (loop
                (let ((line (read-line output)))
                  (log-format 'debug "chrome: ~a" line)
                  (when (eql (search prefix line) 0)
                    (setq devtools-url (subseq line (length prefix)))
                    (log-format 'info "using devtools url ~a" devtools-url)
                    (return))))))
          (sb-sys:deadline-timeout (c)
            (declare (ignore c))
            (error "deadline reached looking for devtools url")))

        (sb-thread:make-thread
          (lambda (output)
            (loop
              (let ((line (read-line output)))
                (log-format 'debug "chrome: ~a" line))))
          :arguments (list (sb-ext:process-output process)))

        ; FIXME STOPPED Establish the websocket connection
      ))

    obj))

(defmethod chrome-close ((obj chrome))
  (let ((process (slot-value obj 'process)))
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
  (sleep 10)
  (chrome-close chrome))