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
