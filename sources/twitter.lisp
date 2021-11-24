(load "chrome.lisp")
(load "http.lisp")
(load "image.lisp")
(load "log-format.lisp")

(require 'asdf)
(require 'lquery)

(defun twitter-tweets (chrome source &key source-type search-latest)
  (let ((url (case source-type
                   ((:handle nil) (concatenate 'string "https://twitter.com/" source))
                   (:hashtag      (concatenate 'string "https://twitter.com/hashtag/" (percent-encode source)))
                   (:search       (concatenate 'string "https://twitter.com/search?q=" (percent-encode source)
                                                       (when search-latest "&f=live")))
                   (otherwise (error "unsupported source-type")))))
    (chrome-goto chrome url))

  (labels ((chrome-runtime-evaluate (expr)
             (chrome-execute chrome "Runtime.evaluate" :params `(("expression" . ,expr))))  
           (tweet-count ()
             (chrome-runtime-evaluate "document.querySelectorAll('article').length")))

    ; TODO Polling until the tweet count is stable is not ideal
    ;      Look for alternate approaches, eg waiting for appropriate DOM, Network domain events
    (sb-sys:with-deadline (:seconds 60)
      (loop
        (let ((counts (loop repeat 3
                            do (sleep 5)
                            collect (tweet-count))))
          (let ((first (first counts))
                (last (car (last counts))))
            (when (and (> last 0) (= first last))
              (log-format 'debug "tweet count stable at ~d" last)
              (return))))))

    (let ((tweets (lquery:$ (initialize (chrome-runtime-evaluate "document.documentElement.outerHTML"))
                            "article"
                            (each #'make-tweet :replace t))))
      ; FIXME Make a hash of these tweets (keyed by url) and return to caller
      tweets)
    )
  )

(defclass tweet ()
  ((html          :initarg :html :reader html)
   (handle        :initarg :handle :reader handle)
   (profile-image :initarg :profile-image :reader profile-image)))

(defun make-tweet (article)
  (let* ((html    (lquery:$ article (html)))
         (first-a (lquery:$1 article "a")))
        
    ; FIXME Get Proper name, tweet url, timestamp, div with lang attribute as tweet
    ;       Make this as simple and robust as possible by sacrificing knowledge of retweet, etc

    (make-instance 'tweet
      :html html

      :handle (let ((href (lquery:$1 first-a (attr "href"))))
                (unless (char= (char href 0) #\/)
                  (error "unexpected handle href format"))
                (subseq href 1))

      :profile-image (let ((profile-image-url (lquery:$1 first-a "img" (attr "src"))))
                       (image-url-to-base64-image profile-image-url)))))

; FIXME Remove
;(let ((chrome (make-chrome)))
;  (twitter-handle-tweets chrome "lisperati")
;  (chrome-close chrome))
(with-open-file (stream "twitter.html" :direction :input)
  (format t "~a~%" (lquery:$ (initialize stream)
                             "article"
                             (each #'make-tweet :replace t))))
