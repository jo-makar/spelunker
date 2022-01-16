(load "chrome.lisp")
(load "log-format.lisp")

(require 'asdf)
(require 'lquery)

(defun linkedin-jobs (chrome &key remote)
  (let ((retval   (make-hash-table :test #'equal))
        (base-url (concatenate 'string
                    "https://linkedin.com/jobs/search/"
                    (when remote "?f_WT=2")))
        (start    0))

    (loop
      (let ((url (if (> start 0)
                   (concatenate 'string base-url
                                        (if (find #\? base-url) "&" "?")
                                        (format nil "start=~d" start))
                   base-url)))
        (log-format 'debug "goto ~a" url)
        (chrome-goto chrome url))

      (labels ((chrome-runtime-evaluate (expr)
                 (chrome-execute chrome "Runtime.evaluate" :params `(("expression" . ,expr))))
               (job-count ()
                 (chrome-runtime-evaluate "document.querySelectorAll('[data-job-id]').length")))

        ; Scroll through the page to populate the jobs
        (sb-sys:with-deadline (:seconds 100)
          (loop
            (let ((count (sb-sys:with-deadline (:seconds 30)
                           (loop
                             (let ((counts (loop repeat 3
                                                 do (sleep 3)
                                                 collect (job-count))))
                               (let ((first (first counts))
                                     (last  (car (last counts))))
                                 (when (and (> last 0) (= first last))
                                   (log-format 'debug "job count stable at ~d" last)
                                   (return last))))))))
              (when (>= count 25)
                (return))
              (chrome-runtime-evaluate "window.scrollTo(0, window.scrollY + window.innerHeight)"))))

        (let* ((html (chrome-runtime-evaluate "document.documentElement.outerHTML"))
               (jobs (lquery:$ (initialize html) "[data-job-id]" (each #'make-linkedin-job :replace t))))
          (loop for job across jobs
                do (when job
                     (setf (gethash (url job) retval) job))))

      (when (>= (hash-table-count retval) 100)
        (return))
      (incf start 25)))

    retval))

(defclass linkedin-job ()
  ((html            :initarg :html :reader html)
   (url             :initarg :url :reader url :reader id)
   (title           :initarg :title :reader title)
   (company         :initarg :company :reader company)
   (location        :initarg :location :reader location)
   (metadata        :initarg :metadata :reader metadata)
   (score           :accessor score)
   (matched-phrases :accessor matched-phrases)
   (token-tf-idf    :accessor token-tf-idf)))

(defun make-linkedin-job (job)
  (handler-case
    (progn
      (flet ((trim (s) (string-trim '(#\space #\tab #\newline) s)))
        (make-instance 'linkedin-job
          :html (lquery:$ job (html))

          :url (concatenate 'string "https://linkedin.com/jobs/view/"
                                    (lquery:$1 job (attr "data-job-id")))

          :title (trim (lquery:$1 job ".job-card-list__title" (text)))

          :company (trim (lquery:$1 job ".job-card-container__company-name" (text)))

          :location (trim (lquery:$1 job ".job-card-container__metadata-wrapper" (text)))

          :metadata (loop for wrapper across (subseq (lquery:$ job ".job-card-container__metadata-wrapper") 1)
                          nconcing (loop for item across (lquery:$ wrapper ".job-card-container__metadata-item")
                                         collect (trim (lquery:$1 item (text))))))))
    (error (c)
      (declare (ignore c))
      (log-format 'warn "error processing linkedin job")
      nil)))

(defmethod text ((self linkedin-job))
  (format nil "~{~a~^ ~}" (list (title self)
                                (company self)
                                (location self)
                                (format nil "~{~a~^ ~}" (metadata self)))))

(defmethod output ((self linkedin-job))
  (let ((stream (make-string-output-stream)))
    
    (format stream "<table><tr>
                      <td>~a</td>
                      <td>
                      <div><a href=\"~a\">~a</a></div>
                      <div>~a - ~a</div>
                      <div>~a</div>
                   "

      ; Svg images are not supported by Gmail
      ;"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"34\" height=\"34\" viewBox=\"0 0 34 34\">
      ;   <g><path d=\"M34,2.5v29A2.5,2.5,0,0,1,31.5,34H2.5A2.5,2.5,0,0,1,0,31.5V2.5A2.5,2.5,0,0,1,2.5,0h29A2.5,2.5,0,0,1,34,2.5ZM10,13H5V29h5Zm.45-5.5A2.88,2.88,0,0,0,7.59,4.6H7.5a2.9,2.9,0,0,0,0,5.8h0a2.88,2.88,0,0,0,2.95-2.81ZM29,19.28c0-4.81-3.06-6.68-6.1-6.68a5.7,5.7,0,0,0-5.06,2.58H17.7V13H13V29h5V20.49a3.32,3.32,0,0,1,3-3.58h.19c1.59,0,2.77,1,2.77,3.52V29h5Z\" fill=\"#0a66c2\"></path></g>
      ; </svg>
      ;"
      "<img src=\"https://content.linkedin.com/content/dam/me/about/LinkedIn_Icon.jpg.original.jpg\"/>"

      (url self)
      (title self)
      (company self)
      (location self)
      (format nil "~{~a~^, ~}" (metadata self)))

    (format stream "<div>score ~2$ " (score self))
    (loop for phrase in (matched-phrases self)
          do (format stream "~a " (format nil "~{~a~^+~}" phrase)))
    (let ((token-tf-idf (token-tf-idf self)))
      (loop for tf-idf in (subseq token-tf-idf 0 (min (length token-tf-idf) 5))
            do (format stream "~a/~2$ " (car tf-idf) (cdr tf-idf))))
    (format stream "</div>")

    (format stream "</td></tr></table>~%")

    (get-output-stream-string stream)))
