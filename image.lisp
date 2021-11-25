(load "base64.lisp")
(load "containers.lisp")

(require 'asdf)
(require 'drakma)

(defvar *image-url-cache* (make-instance 'fixed-hash-table))

(defun image-url-to-base64-image (image-url)
  (multiple-value-bind (value exists) (fixed-hash-table-get *image-url-cache* image-url)
    (if exists
      value
      (let* ((image-ext  (let ((n (position #\. image-url :from-end t)))
                           (unless n
                             (error (format nil "invalid image url: ~a" image-url)))
                           (subseq image-url (1+ n))))
             (media-type (cond ((string= image-ext "gif")      "image/gif")
                               ((or (string= image-ext "jpeg")
                                    (string= image-ext "jpg")) "image/jpeg")
                               ((string= image-ext "png")      "image/png")
                               ((string= image-ext "svg")      "image/svg+xml")
                               (t                              (error "unexpected media-type")))))
        (multiple-value-bind (image resp-code) (drakma:http-request image-url)
          (unless (= resp-code 200)
            (error (format nil "problem downloading image url: ~a" image-url)))
          (let ((value (format nil "<img src=\"data:~a;base64,~a\"/>"
                                   media-type
                                   (base64-encode (map 'string #'code-char image)))))
            (fixed-hash-table-set *image-url-cache* image-url value)
            value))))))
