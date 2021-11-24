;
; Ring-buffer
;

(defclass ring-buffer ()
  ((buffer :initform (make-array 0 :adjustable t :fill-pointer t))
   (size   :initarg :size :initform 1000)
   (index  :initform 0)
   (mutex  :initform (sb-thread:make-mutex))))

(defmethod ring-buffer-insert ((self ring-buffer) item)
  (with-slots (buffer size index mutex) self
    (sb-thread:with-mutex (mutex)
      (when (< (length buffer) size)
        (vector-push-extend item buffer)
        (return-from ring-buffer-insert))
      (setf (aref buffer index) item)
      (setf index (mod (1+ index) size)))))

(defmethod ring-buffer-find-if ((self ring-buffer) predicate)
  (with-slots (buffer mutex) self
    (sb-thread:with-mutex (mutex)
      (find-if predicate buffer))))

;
; Fixed-size hash table
;

(defclass fixed-hash-table ()
  ((hash-table :initform (make-hash-table :test #'equal))
   (key-list   :initform '())
   (size       :initarg :size :initform 100)))

(defmethod fixed-hash-table-get ((self fixed-hash-table) key)
  (gethash key (slot-value self 'hash-table)))

(defmethod fixed-hash-table-set ((self fixed-hash-table) key val)
  (with-slots (hash-table key-list size) self
    (let ((n (find key key-list)))
      (cond
        ((and (null n) (< (length key-list) size))
           (progn
             (setq key-list (append key-list (list key)))
             (setf (gethash key hash-table) val)))
        ((and (null n) (>= (length key-list) size))
           (let ((old-key (car key-list)))
             (setq key-list (append (cdr key-list) (list key)))
             (remhash old-key hash-table)
             (setf (gethash key hash-table) val)))
        (n
           (progn (setq key-list (append (remove key key-list :test #'equal) (list key)))
                  (setf (gethash key hash-table) val)))))))
