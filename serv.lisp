(require :usocket)

(defparameter statuses '((200 . "OK")
                         (404 . "Not Found")))

(defparameter content-types '((text (html css plain))
                              (image (png jpg jpeg svg))
                              (application (json xml))))

;;;; Functional part

(defun get-mime (extension)
  (let ((item (find extension
                    content-types
                    :key #'cadr
                    :test (lambda (item list)
                            (member item list)))))
    (read-from-string (format nil "~a/~a" (car item) extension))))

(defun get-url (line)
  (subseq line
          (position #\slash line)
          (position #\space line :from-end t)))

(defun get-header (header)
  (let ((status (assoc (car header) statuses))
        (content-type (cdr header)))
    (format nil
            "HTTP/1.1 ~a ~a~%Content-Type: ~a~%~%"
            (car status)
            (cdr status)
            content-type)))

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

;; (template-string "<div>Hello |(apply #'concatenate `(string . ,(loop repeat 5 collect (format nil \"<div>~a~%</div>\" (cdr (assoc 'name params))))))|</div>" '((name . "Help?")))
;; (template-string "|(format nil \"~{<div>~a<div/>~%~}\" (cdr (assoc 'list params)))|" '((list . ("1.png" "2.png" "3.png" "4.png" "5.png"))))
;; (template-string "<div class=\"|(if nil \"hello\" \"clsName\")|\">|(cdr (assoc 'name params))|</div>" '((name . "some test")))

;;;; Imperative part

(defun send-data (header data)
  (princ (get-header header))
  (princ data))

(defun send-file (filename)
  (let* ((extension (pathname-type filename))
         (mime (get-mime (if extension (read-from-string extension) 'html))))
    (with-open-file (fstream filename :direction :input :element-type 'unsigned-byte :if-does-not-exist nil)
      (if fstream
          (progn
            (princ (get-header `(200 . ,mime)))
            (loop for byte = (read-byte fstream nil)
                  when (not byte) return 'eof
                    do (write-byte byte *standard-output*)))
          (send-data '(404 . text/html) "<html><h1 style=\"text-align: center\">404 Not Found</h1></html>")))))

(defun send-template (filename params)
  (let* ((extension (pathname-type filename))
         (mime (get-mime (if extension (read-from-string extension) 'html))))
    (with-open-file (fstream filename :direction :input :if-does-not-exist nil)
      (if fstream
          (progn
            (princ (get-header `(200 . ,mime)))
            (loop for line = (read-line fstream nil)
                  when (not line) return 'eof
                    do (format t "~a~%" (template-string line params))))
          (send-data '(404 . text/html) "<html><h1 style=\"text-align:center\">404 Not Found</h1></html>")))))

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


