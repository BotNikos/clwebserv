(require "asdf")
(asdf:load-system 'clwebserv)

(ql:quickload :sqlite)
(use-package :sqlite)

(setf *random-state* (make-random-state t))

(let ((images nil)
      (images-count nil))
  (defun random-poke (path)
    (if images-count
        (let ((poke (aref images (random images-count))))
          (format nil "<img class=\"sprite-name\" src=\"/sprites/~a\"/> <div class=\"sprite-name\">~a</div>"
                  (file-namestring poke)
                  (string-capitalize (substitute-if #\Space (lambda (char) (equal char #\-)) (pathname-name poke)))))
        (let ((images-list (uiop:directory-files path)))
          (setf images-count (list-length images-list))
          (setf images (make-array `(,images-count)
                                   :initial-contents images-list))
          (random-poke path)))))

(defun categ-items-unite (categories items)
  (labels ((select-items (category_id items)
             (remove-if-not (lambda (item) (eq category_id (cadddr item))) items)))
    (if (car categories)
        (cons (cons (select-items (caar categories) items)
                    (car categories))
              (categ-items-unite (cdr categories) items))
        nil)))

(defun put-data (params)
  (apply #'concatenate
         `(string .
           ,(loop for category in (cdr (assoc 'categories params))
                  collect "<div class=\"category container-shadow\">"
                  collect (format nil "<div class=\"category-name\" style=\"color: ~a\">~a</div>" (cadddr category) (caddr category))
                  collect "<hr/>"
                  collect "<div class=\"category-items-container\">"
                  collect (apply #'concatenate `(string . ,(loop for item in (car category)
                                collect (format nil "<a class=\"category-item\" href=\"~a\">~a</a>" (caddr item) (cadr item)))))
                  collect "</div></div>"))))

(defun send-poke (params)
  (let* ((db (connect "custom-tab.db"))
         (categories (execute-to-list db "select * from categories"))
         (items (execute-to-list db "select * from items"))
         (params (cons (cons 'categories
                             (categ-items-unite categories items))
                       params)))
    (disconnect db)
    (send-template "resources/index.html" params)))

(defun req-handler (path params)
  (cond ((or (equal path "/index.html") (equal path "/"))
         (send-poke params))
        (t (send-file (format nil "resources/~a" (subseq path 1))))))

(defun main ()
  (serv 0 #'req-handler))
