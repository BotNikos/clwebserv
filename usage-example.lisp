(load "serv.lisp")

(defun req-handler (req stream)
  (send-file req stream))

(defun main ()
  (serv 8081 #'req-handler))
