(require :usocket)

(defparameter file-types '((html . text)
						   (css  . text)
						   (png  . image)
						   (jpg  . image)
						   (jpeg . image)))

(defun read-file-to-stream (file-stream stream)
  (let ((byte (read-byte file-stream nil)))
	(when byte
	  (write-byte byte stream)
	  (read-file-to-stream file-stream stream))))

(defun get-header-type (filename)
  (let* ((dot-pos (1+ (position #\. filename :from-end t)))
		 (str-file-type (subseq filename dot-pos))
		 (file-type (read-from-string str-file-type))
		 (mime-alist (assoc file-type file-types))
		 (mime (if mime-alist mime-alist '(plain . text))))
	(format nil "Content-Type: ~a/~a" (cdr mime) (car mime))))

(defun get-filename (stream)
  (let ((line (read-line stream)))
	(subseq line
			(1+ (position #\slash line))
			(position #\space line :from-end t))))

(defun send-file (filename stream)
  (format stream "HTTP/1.1 200 OK~%~a~%~%" (get-header-type filename))
  (with-open-file (fstream filename :direction :input :element-type 'unsigned-byte)
	(read-file-to-stream fstream stream)))

(defun req-handler (stream)
  (send-file (get-filename stream) stream)
  (force-output stream))

(defun serv (port)
  (let* ((socket (usocket:socket-listen "localhost" port)))
	(unwind-protect (loop do
						  (let* ((connection (usocket:socket-accept socket :element-type :default))
								 (stream (usocket:socket-stream connection)))
							(req-handler stream)
							(usocket:socket-close connection)))
			 (usocket:socket-close socket))))
