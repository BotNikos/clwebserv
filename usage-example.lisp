(require "asdf")
(asdf:load-system 'clwebserv)

(defun req-handler (path params)
  (cond ((equal path "/template")
         (send 'temp
               "resources/template.html"
               :params '((name . "Every possible name ever!"))))
        ((equal path "/hello")
         (send 'data
               (format nil "{\"name\": \"~a\"}" (cdr (assoc 'fname params)))
               :header '((status . "200 OK")
                        ("Content-Type" . "application/json"))))
        (t (send 'file (format nil "resources/~a" (subseq path 1))))))

(defun main ()
  (serv 0 #'req-handler))
