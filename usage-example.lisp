(load "serv.lisp")

(defun req-handler (path params)
  (cond ((equal path "data") (send-data '(200 . application/json) "{\"name\": \"Nikita\", \"age\": 21}"))
        (t (send-file "hello.html"))))

(defun main ()
  (serv 8081 #'req-handler))
