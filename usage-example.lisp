(load "serv.lisp")

(defun hello-form (params)
  (if params
      (send-data '(200 . text/html) (format nil "<html>Hello ~a</html>" (cdr (assoc 'fname params))))
      (send-file "resources/hello.html")))

(defun req-handler (path params)
  (cond ((equal path "/data") (send-data '(200 . application/json) "{\"name\": \"Nikita\", \"age\": 21}"))
        ((equal path "/hello") (hello-form params))
        (t (send-file (format nil "resources/~a" (subseq path 1))))))

(defun main ()
  (serv 0 #'req-handler))
