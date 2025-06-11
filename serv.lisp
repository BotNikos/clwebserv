(require :usocket)

;;;; Functional part

(defun get-mime (extension)
  (case (read-from-string extension)
    (html "text/html")
    (css "text/css")
    (js "text/javascript")
    (png "image/png")
    (jpg "image/jpg")
    (jpeg "image/jpeg")
    (svg "image/svg")
    (otherwise "text/plain")))

(defun get-url (line)
  (subseq line
          (position #\slash line)
          (position #\space line :from-end t)))

;; status needs to be first always
(defun get-header (header)
  (format nil "HTTP/1.1 ~a~%~{~a~^~%~}~%~%"
          (cdr (assoc 'status header))
          (loop for item in (cdr header)
                collect (format nil "~a: ~a" (car item) (cdr item)))))

(defun decode-char (lst)
  (let ((code (parse-integer (coerce lst 'string) :radix 16 :junk-allowed t)))
    (if code
        (code-char code)
        #\_)))

(defun decode-param (s)
  (labels ((p (lst)
           (when (car lst)
             (case (car lst)
               (#\+ (cons #\space (p (cdr lst))))
               (#\% (cons (decode-char (list (second lst) (third lst))) (p (cdddr lst))))
               (otherwise (cons (car lst) (p (cdr lst))))))))
    (coerce (p (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (if s
        (cons (cons (read-from-string (subseq s 0 i1))
                    (decode-param (subseq s (1+ i1) i2)))
              (and i2 (parse-params (subseq s (1+ i2))))))))

(defun template-string (s params)
  (declare (special params))
  (let* ((start (position #\| s)))
    (if start
        (let* ((end (position #\| s :start (1+ start)))
               (new-string (format nil "~a~a~a" (subseq s 0 start) (eval (read-from-string (subseq s (1+ start) end))) (subseq s (1+ end)))))
          (template-string new-string params))
        s)))

;;;; Imperative part

(defun send-data (data header)
  (princ (get-header header))
  (princ data))

(defun send-file (filename loop)
  (let* ((extension (pathname-type filename)))
    (with-open-file (fstream filename :direction :input :element-type :default :if-does-not-exist nil)
      (if fstream
          (progn
            (princ (get-header `((status . "200 OK") ("Content-Type" . ,(get-mime extension)))))
            (funcall loop fstream))
          (send-data "<html><h1 style=\"text-align: center\">404 Not Found</h1></html>" '((status . "404 Not Foud") ("Content-Type" . "text/html")))))))

(defun send (type data &key params header)
  (case type
    (data (send-data data (if header header '((status . "200 OK") ("Content-Type" . "text/plain")))))
    (file (send-file data (lambda (fstream)
            (loop for byte = (read-byte fstream nil)
                  when (not byte) return 'eof
                    do (write-byte byte *standard-output*)))))
    (temp (send-file data (lambda (fstream)
                            (loop for line = (read-line fstream nil)
                                  when (not line) return 'eof
                                    do (format t "~a~%" (template-string line params))))))))

(defun serv (port req-handler)
  (let ((socket (usocket:socket-listen "localhost" port)))
    (format t "Socket opened on ~d port~%" (usocket:get-local-port socket))
    (unwind-protect (loop do
      (let* ((connection (usocket:socket-accept socket :element-type :default))
             (stream (usocket:socket-stream connection))
             (url (get-url (read-line stream)))
             (path (subseq url 0 (position #\? url)))
             (params (parse-params (when (position #\? url) (subseq url (1+ (position #\? url))))))
             (*standard-output* stream))
        (funcall req-handler path params)
        (force-output stream)
        (usocket:socket-close connection)))
      (usocket:socket-close socket))))


