(load "chrome.lisp")
(load "config.lisp")
(load "sources/twitter.lisp")

(require 'asdf)
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
              do (unless (tweet-text tweet)
                   (setq remove (cons url remove))))
        (loop for url in remove do (remhash url tweets)))

      ; FIXME STOPPED use a local sqlite db and filter out seen tweets
      ;               delete entries older than 90 days
      (sqlite:with-open-database (database "seen.db")
        (sqlite:execute-non-query database
          "create table if not exists seen (id text primary key, added text not null)")
        (format t "~a~%" database)
        )

      ; FIXME use data mining / text analysis to assign scores to the tweets then sort
      ; FIXME write a tweet-specific method for outputing html
      ; FIXME write an email and send it out
      )

    (chrome-close chrome)))
