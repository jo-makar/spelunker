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
      ; FIXME STOPPED Make a hash of these tweets (keyed by url) and return to caller
      tweets)
    )
  )

(defclass tweet ()
  ((html          :initarg :html :reader html)
   (url           :initarg :url :reader url)
   (handle        :initarg :handle :reader handle)
   (name          :initarg :name :reader name)
   (profile-image :initarg :profile-image :reader profile-image)
   (timestamp     :initarg :timestamp :reader timestamp)
   (tweet-text    :initarg :tweet-text :reader tweet-text)
   (has-media     :initarg :has-media :reader has-media)))

(defun make-tweet (article)
  (let ((tweet (lquery:$1 article "div" (filter #'(lambda (e) (lquery:$1 e (attr "lang")))))))
    (make-instance 'tweet
      :html (lquery:$ article (html))

      :handle (let ((href (lquery:$1 article "a" (attr "href"))))
                (unless (char= (char href 0) #\/)
                  (error "unexpected handle href format"))
                (concatenate 'string "@" (subseq href 1)))

      :profile-image (let ((profile-image-url (lquery:$1 article "a img" (attr "src"))))
                       (image-url-to-base64-image profile-image-url))

      :name (lquery:$1 article "a" #'(lambda (e) (aref e 1)) "span" (text))

      :url (concatenate 'string 
             "https://twiter.com"
             (lquery:$ article "a" #'(lambda (e) (aref e 2)) (attr "href")))

      :timestamp (lquery:$1 article "time" (attr "datetime"))

      ; TODO Handle emojis in the same manner as the profile-image?
      :tweet-text (lquery:$1 tweet (text))

      ; TODO Handle each type of associated media content
      :has-media (> (length (lquery:$1 tweet (parent) (siblings) (html))) 0))))

; FIXME Remove
;(let ((chrome (make-chrome)))
;  (twitter-handle-tweets chrome "lisperati")
;  (chrome-close chrome))

; FIXME Remove
(with-open-file (stream "twitter.html" :direction :input)
  (let ((tweets (lquery:$ (initialize stream) "article" (each #'make-tweet :replace t))))
    (loop for tweet across tweets
          do (format t "~a~%" (url tweet))
             (format t "~a ~a ~a~%" (name tweet) (handle tweet) (timestamp tweet)) 
             (format t "~a~%" (tweet-text tweet))
             (when (has-media tweet) (format t "(unhandled) media detected~%"))
             (format t "*****~%"))))
