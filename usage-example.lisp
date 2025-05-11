(load "serv.lisp")

(defun req-handler (path)
  (cond ((equal path "data") (send-data '(200 . application/json) "{\"name\": \"Nikita\", \"age\": 21}"))
        (t (send-file path))))

(defun main ()
  (serv 8081 #'req-handler))
