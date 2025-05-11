(require :usocket)

(defparameter statuses '((200 . "OK")
                         (404 . "Not Found")))

(defparameter content-types '((text (html css plain))
                              (image (png jpg jpeg svg))
                              (application (json xml))))


(defun get-mime (extension)
  (let ((item (find extension
                    content-types
                    :key #'cadr
                    :test (lambda (item list)
                            (member item list)))))
    (read-from-string (format nil "~a/~a" (car item) extension))))

(defun get-path (stream)
  (let ((line (read-line stream)))
    (subseq line
            (1+ (position #\slash line))
            (position #\space line :from-end t))))

(defun get-header (header)
  (let ((status (assoc (car header) statuses))
        (content-type (cdr header)))
    (format nil
            "HTTP/1.1 ~a ~a~%Content-Type: ~a~%~%"
            (car status)
            (cdr status)
            content-type)))

(defun send-data (header data)
  (princ (get-header header))
  (princ data))

(defun send-file (filename)
  (let* ((extension (read-from-string (pathname-type filename))) ;; pathname-type может вернуть nil, и тогда read-from-string - это не понравится
         (mime (get-mime extension)))
    (with-open-file (fstream filename :direction :input :element-type 'unsigned-byte :if-does-not-exist nil)
      (if fstream
          (progn
            (princ (get-header `(200 . ,mime)))
            (loop for byte = (read-byte fstream nil)
                  when (not byte) return 'eof
                    do (write-byte byte *standard-output*)))
          (send-data '(404 . text/html) "<html><h1>404 Not Found</h1></html>")))))

(defun serv (port req-handler)
  (let ((socket (usocket:socket-listen "localhost" port)))
    (unwind-protect (loop do
      (let* ((connection (usocket:socket-accept socket :element-type :default))
             (stream (usocket:socket-stream connection))
             (path (get-path stream))
             (*standard-output* stream))
        (funcall req-handler path)
        (force-output stream)
        (usocket:socket-close connection)))
      (usocket:socket-close socket))))
