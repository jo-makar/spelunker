(load "chrome.lisp")
(load "config.lisp")
(load "sources/twitter.lisp")

(require 'asdf)
(require 'cl-smtp)
(require 'local-time)
(require 'sqlite)

(let ((chrome (make-chrome)))
  (unwind-protect

    ;
    ; Twitter
    ;

    (let ((tweets   (make-hash-table :test #'equal))
          (handles  (cdr (assoc 'twitter-handles *config*)))
          (hashtags (cdr (assoc 'twitter-hashtags *config*)))
          (searches (cdr (assoc 'twitter-searches *config*))))
      (loop for handle in handles
            do (loop for url being the hash-key using (hash-value tweet)
                     of (twitter-tweets chrome handle :source-type :handle)
                     do (setf (gethash url tweets) tweet)))
      (loop for hashtag in hashtags
            do (loop for url being the hash-key using (hash-value tweet)
                     of (twitter-tweets chrome hashtag :source-type :hashtag)
                     do (setf (gethash url tweets) tweet)))
      (loop for search in searches
            do (loop for url being the hash-key using (hash-value tweet)
                     of (twitter-tweets chrome search :source-type :search)
                     do (setf (gethash url tweets) tweet)))

      ; Ignore tweets older than 90 days
      (let ((remove '()))
        (let ((threshold (local-time:timestamp- (local-time:now) 90 :day)))
          (loop for url being the hash-key using (hash-value tweet) of tweets
                do (when (local-time:timestamp> threshold
                                                (local-time:parse-timestring (timestamp tweet)))
                     (setq remove (cons url remove)))))
        (loop for url in remove do (remhash url tweets)))

      ; Ignore media only tweets
      (let ((remove '()))
        (loop for url being the hash-key using (hash-value tweet) of tweets
              do (unless (text tweet)
                   (setq remove (cons url remove))))
        (loop for url in remove do (remhash url tweets)))

      ; Ignore previously seen tweets
      (sqlite:with-open-database (database "seen.db")
        (sqlite:execute-non-query database
          "create table if not exists seen (id text primary key, added text not null)")
        (sqlite:execute-non-query database
          "delete from seen where added < datetime('now', '-90 days')")

        (let ((remove '()))
          (loop for url being the hash-key using (hash-value tweet) of tweets
                do (if (= 1 (sqlite:execute-single database
                                "select count(*) from seen where id=?" url))
                     (setq remove (cons url remove))
                     (sqlite:execute-non-query database
                       "insert into seen (id, added) values (?, datetime('now'))" url)))
          (loop for url in remove do (remhash url tweets))))

      ; FIXME STOPPED Use data mining / text analysis to assign scores and sort the tweets

      (let ((stream (make-string-output-stream)))
        (format stream "<html><body>~%")
        (loop for tweet being the hash-value in tweets
              do (write-string (output tweet) stream))
        (format stream "</body></html>~%")

        (let ((smtp-server    (cdr (assoc 'smtp-server *config*)))
              (smtp-port      (cdr (assoc 'smtp-port *config*)))
              (smtp-username  (cdr (assoc 'smtp-username *config*)))
              (smtp-password  (cdr (assoc 'smtp-password *config*)))
              (smtp-from-addr (cdr (assoc 'smtp-from-addr *config*)))
              (smtp-to-addr   (cdr (assoc 'smtp-to-addr *config*))))
          (cl-smtp:send-email
            smtp-server
            smtp-from-addr
            smtp-to-addr
            (format nil "spelunker ~a"
              (local-time:format-timestring nil (local-time:today)
                :format '(:year #\- :month #\- :day)))
            (get-output-stream-string stream)
            :extra-headers '(("Content-Type" "text/html; charset=\"UTF-8\""))
            :port 587
            :ssl :starttls
            :authentication `(:login ,smtp-username ,smtp-password)))))

    (chrome-close chrome)))
