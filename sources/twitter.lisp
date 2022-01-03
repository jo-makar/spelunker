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

  (let ((retval (make-hash-table :test #'equal)))
    (labels ((chrome-runtime-evaluate (expr)
               (chrome-execute chrome "Runtime.evaluate" :params `(("expression" . ,expr))))  
             (tweet-count ()
               (chrome-runtime-evaluate "document.querySelectorAll('article').length")))

      (loop
        ; TODO Polling until the tweet count is stable is not ideal
        ;      Look for alternate approaches, eg waiting for appropriate DOM, Network domain events
        (sb-sys:with-deadline (:seconds 60)
          (loop
            (let ((counts (loop repeat 3
                                do (sleep 3)
                                collect (tweet-count))))
              (let ((first (first counts))
                    (last (car (last counts))))
                (when (and (> last 0) (= first last))
                  (log-format 'debug "tweet count stable at ~d" last)
                  (return))))))

        (let* ((count  (hash-table-count retval))
               (html   (chrome-runtime-evaluate "document.documentElement.outerHTML"))
               (tweets (lquery:$ (initialize html) "article" (each #'make-tweet :replace t))))
          (loop for tweet across tweets
                do (when tweet
                     (setf (gethash (url tweet) retval) tweet)))

          (log-format 'debug "unique tweet count ~d" (hash-table-count retval))
          (when (or (= (hash-table-count retval) count)
                    (>= (hash-table-count retval) 100))
            (return)))

        (chrome-runtime-evaluate "window.scrollTo(0, document.body.scrollHeight)"))
      retval)))

(defclass tweet ()
  ((html            :initarg :html :reader html)
   (url             :initarg :url :reader url :reader id)
   (handle          :initarg :handle :reader handle)
   (name            :initarg :name :reader name)
   (profile-image   :initarg :profile-image :reader profile-image)
   (timestamp       :initarg :timestamp :reader timestamp)
   (text            :initarg :text :reader text)
   (has-media       :initarg :has-media :reader has-media)
   (score           :accessor score)
   (matched-phrases :accessor matched-phrases)
   (token-tf-idf    :accessor token-tf-idf)))

(defun make-tweet (article)
  (handler-case
    (progn
      (when (string= (lquery:$1 article (text)) "This Tweet is unavailable.")
        (return-from make-tweet nil))
    
      (let* ((tweet      (lquery:$1 article "div" (filter #'(lambda (e) (lquery:$1 e (attr "lang"))))))
             (is-retweet (if (search "Retweeted" (lquery:$1 article "a" (text))) t))
    
             (a-offset   (if is-retweet 1 0))
             (first-a    (lquery:$1 article "a" #'(lambda (e) (aref e (+ 0 a-offset)))))
             (second-a   (lquery:$1 article "a" #'(lambda (e) (aref e (+ 1 a-offset)))))
             (third-a    (lquery:$1 article "a" #'(lambda (e) (aref e (+ 2 a-offset))))))
    
        (make-instance 'tweet
          :html (lquery:$ article (html))
    
          :handle (let ((href (lquery:$1 first-a (attr "href"))))
                    (unless (char= (char href 0) #\/)
                      (error "unexpected handle href format"))
                    (concatenate 'string "@" (subseq href 1)))
    
          :profile-image (let ((profile-image-url (lquery:$1 first-a "img" (attr "src"))))
                           ; TODO Inline base64 images are not supported by Gmail, investigate further
                           ;(image-url-to-base64-image profile-image-url))
                           (format nil "<img src=\"~a\"/>" profile-image-url))
    
          :name (lquery:$1 second-a "span" (text))
    
          :url (concatenate 'string "https://twitter.com" (lquery:$1 third-a (attr "href")))
    
          :timestamp (lquery:$1 article "time" (attr "datetime"))
    
          ; TODO Handle emojis in the same manner as the profile-image?
          :text (lquery:$1 tweet (text))
    
          ; TODO Handle each type of associated media content
          ;      Most likely handle in the same manner as profile-image
          :has-media (or (> (length (lquery:$1 tweet (parent) (siblings) (html))) 0)
                         (null (lquery:$1 tweet (text)))))))
    (error (c)
      (declare (ignore c))
      ; TODO Ideally make an error "tweet" that includes the offending html
      (log-format 'warn "error processing tweet")
      nil)))
    
(defmethod output ((self tweet))
  (let ((stream (make-string-output-stream)))

    (format stream "<table><tr>
                      <td>~a</td>
                      <td>
                        <div>
                          ~a <a href=\"~a\">~a</a>
                          <a href=\"~a\">~a</a>
                        </div>
                        <div>~a</div>
                    "
      (profile-image self)
      (name self)
      (concatenate 'string "https://twitter.com/" (subseq (handle self) 1))
      (handle self)
      (url self)
      (timestamp self)
      (text self))

    (when (has-media self)
      (format stream "<div>*unhandled media detected*</div>"))

    (format stream "<div>score ~2$ " (score self))
    (loop for phrase in (matched-phrases self)
          do (format stream "~a " (format nil "~{~a~^+~}" phrase)))
    (let ((token-tf-idf (token-tf-idf self)))
      (loop for tf-idf in (subseq token-tf-idf 0 (min (length token-tf-idf) 5))
            do (format stream "~a/~2$ " (car tf-idf) (cdr tf-idf))))
    (format stream "</div>")

    (format stream "</td></tr></table>~%")

    (get-output-stream-string stream)))
