(in-package :lde)
  
#|
 | Configuration
 |#

;; Set auto escape
(setf djula:*auto-escape* nil)

;; Add template directory
(djula:add-template-directory
  (asdf:system-relative-pathname :lde "templates/"))


#|
 | Variables
 |#

;; Server instance
(defparameter *lde-server* nil)


#|
 | Server
 |#

;;; Start server
(defun start-lde-server (port)
  ; ;; Set session timeout
  ; (setf hunchentoot:*session-max-time* (* 60 60 4))

  (defparameter *port-web* port)
  (defparameter *port-ws* (+ port 1))

  ;; Create server instance
  (setf *lde-server* 
        (make-instance 'easy-routes:easy-routes-acceptor 
                       :port *port-web*
                       :document-root (asdf:system-relative-pathname :lde "document-root/")))
  
  ;; Start listen
  (hunchentoot:start *lde-server*)
  
  ;; Set initial basepath
  (defparameter *basepath* (namestring (truename "")))
  
  ;; Start websocket server
  (clack:clackup *session-ws-server* :server :hunchentoot :port *port-ws*)

  ;; Set session
  (defparameter *session* nil)
  (defparameter *session-output-stream* nil)
  (defparameter *session-input-stream* nil))

;;; Stop lde server
(defun stop-lde-server ()
  ;; Stop listen
  (hunchentoot:stop *lde-server*))


#|
 | Handler / Index
 |#

;;; Get view index
(defroute get-index ("/" :method :get) ()
  (let ((djula:*catch-template-errors-p* nil)
        (djula:*fancy-error-template-p* nil))
    (djula:render-template* 
      (djula:compile-template* "index.html") 
      nil
      :cur-path *basepath*
      :port-web *port-web*
      :port-ws *port-ws*)))
      
;;; Get list of file/directory
(defun get-file-list (path basepath)
  (let* ((escaped-basepath-str (ppcre:regex-replace "\\/" (namestring basepath) "\\/")))
    (mapcar
      (lambda (p)
        (ppcre:regex-replace escaped-basepath-str (namestring p) ""))
      (cl-fad:list-directory (merge-pathnames path basepath)))))

;;; Get list of file/directory
(defroute get-lis ("/lis" :method :post) ()
  (let* ((param (json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
         (path (cdr (assoc :path param))))
    (json:encode-json-to-string
      `((data . ,(mapcar #'namestring (get-file-list path *basepath*)))
        (msg . "foo")))))
        
;;; Change basepath
(defroute post-set-root ("/set-root" :method :post) ()
  (let* ((param (json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
         (path (cdr (assoc :path param))))
         
    (when (cl-fad:directory-exists-p path)
      (setf *basepath* path))
         
    (json:encode-json-to-string
      `((data . ,nil)
        (success . ,t)))))
        
;;; Open session
(defroute post-session-open ("/session/open" :method :post) ()
  (let* ((param (json:decode-json-from-string (hunchentoot:raw-post-data :force-text t))))
  
    (when *session*
      (sb-ext:process-kill *session* 15 :process-group)
      (sb-ext:process-wait *session*)
      (sb-ext:process-close *session*)
      (sb-ext:process-exit-code *session*)
      (setf *session* nil))

    (setf *session* (sb-ext:run-program 
      "sbcl" (list) 
      :output :stream
      :search t
      :if-output-exists :supersede 
      :input :stream :wait nil
      :directory *basepath*))

    (defparameter *session-output-stream* (sb-ext:process-output *session*))
    (defparameter *session-input-stream* (sb-ext:process-input *session*))
    (defparameter *session-clients* (list))
    
    ;; Prepare thread variable
    ; (format *session-input-stream* "(defparameter *lde-thread* nil)~%")
    ; (finish-output *session-input-stream*)

    (defparameter *session-output-string* "")

    (defparameter *session-thread* 
      (bordeaux-threads:make-thread 
        (lambda ()
          (sleep 1)
          (block watch-output
            (loop
              (when (not *session*)
                (return-from watch-output))

              (let ((str (read-char *session-output-stream*)))
                (setf *session-output-string* (format nil "~a~a" *session-output-string* str))))))
        :name "session-thread"))
        
    (defparameter *session-output-send-thread*
      (bordeaux-threads:make-thread
        (lambda ()
          (loop
            (progn
              (sleep 1)
              (mapc (lambda (c)
                (wsd:send c (format nil "~a" *session-output-string*)))
                *session-clients*)
              (setf *session-output-string* ""))))))

    (json:encode-json-to-string
      `((data . ,nil)
        (success . ,t)))))
        
;;; Close session
(defroute post-session-close ("/session/close" :method :post) ()
  (let* ((param (json:decode-json-from-string (hunchentoot:raw-post-data :force-text t))))
  
    (when *session*
      (sb-ext:process-kill *session* 15 :process-group)
      (sb-ext:process-wait *session*)
      (sb-ext:process-close *session*)
      (sb-ext:process-exit-code *session*)
      (setf *session* nil))
         
    (json:encode-json-to-string
      `((data . ,nil)
        (success . ,t)))))

;;; Get session status
(defroute get-session-status ("/session/status" :method :get) ()
  (let* ()
    (json:encode-json-to-string
      `((data . ,(if *session* t nil))
        (success . ,t)))))
        
;;; Delete file
(defroute post-delete-file ("/file/delete" :method :post) ()
  (let* ((param (json:decode-json-from-string 
                 (hunchentoot:raw-post-data :force-text t)))
         (path (cdr (assoc :path param)))
         (actual-path (merge-pathnames path *basepath*)))
    (hunchentoot:log-message* :INFO (namestring actual-path))
    (delete-file actual-path)
    (json:encode-json-to-string
      `((data . 
          ((:filepath . ,path)))
        (success . ,t)
        (msg . ,"")))))
    
    
#|
 | Handler / Editor
 |#

;;; Get view editor
(defroute view-editor ("/editor" :method :get) (target)
  (let ((djula:*catch-template-errors-p* nil)
        (djula:*fancy-error-template-p* nil))
    (hunchentoot:log-message* :INFO (format nil "~a" target))
    (djula:render-template* 
      (djula:compile-template* "editor.html") 
      nil
      :target target
      :feature-id "editor"
      :port-web *port-web*
      :port-ws *port-ws*)))

;;; Get source
(defroute get-editor ("/editor/file" :method :get) (filepath)
  (let ((actual-path (merge-pathnames filepath *basepath*)))
    (json:encode-json-to-string
      `((data . 
          ((:filepath . ,filepath)
           (:extension . ,(pathname-type actual-path))
           (:content . ,(if (probe-file actual-path) (lde.util:slurp actual-path) ""))))
        (success . ,t)
        (msg . ,"")))))

;;; Put (and eval when .lisp) source
(defroute post-editor-put ("/editor/file" :method :post) (filepath)
  (let* ((param (json:decode-json-from-string 
                 (hunchentoot:raw-post-data :force-text t)))
         (eval-on-save (cdr (assoc :eval-on-save param))))
    (lde.util:spit (merge-pathnames filepath *basepath*)
          (lde.util:trim (cdr (assoc :content param))))
    
    (when (and (equal (pathname-type (merge-pathnames filepath *basepath*)) "lisp")
               *session*
               eval-on-save)
      ; (format *session-input-stream* "(load \"~a\")~%" (merge-pathnames filepath *basepath*))
      ; (format *session-input-stream* "(sb-thread:make-thread (lambda () (load \"~a\")))~%" (merge-pathnames filepath *basepath*))
      ; (compile-file (merge-pathnames filepath *basepath*))
      (hunchentoot:log-message* :INFO "*** COMPILED: ~a > ~a ***~%" 
        (merge-pathnames filepath *basepath*)
        (merge-pathnames (format nil "~a.fasl" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*))))
        
  
      ; (format *session-input-stream* "(sb-thread:make-thread (lambda () (compile-file \"~a\") (load \"~a\")))~%" 
      ;   (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*)))
      ;   (merge-pathnames (format nil "~a.fasl" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*)))
      ;   )      
      ;(format *session-input-stream* "(sb-thread:make-thread (lambda () (load \"~a\")))~%" 
       ; (merge-pathnames (format nil "~a.fasl" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*))))
  
      ; (format *session-input-stream* "(sb-thread:make-thread (lambda () (load \"~a\")))~%" 
      ;   (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*))))
       

      ;; Eval in sub thread
      ; (format *session-input-stream* "(if (and *lde-thread* (sb-thread:thread-alive-p *lde-thread*)) (sb-thread:interrupt-thread *lde-thread* (lambda () (load \"~a\"))) (setf *lde-thread* (sb-thread:make-thread (lambda () (load \"~a\")))))~%"
      ;   (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*)))
      ;   (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*))))

      ;; Eval in main thread
      (format *session-input-stream* "(load \"~a\")~%"
        (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*))))
  
      ; (load (merge-pathnames (format nil "~a.fasl" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*))))
      (finish-output *session-input-stream*))
    
    (json:encode-json-to-string
      `((data . ())
        (success . ,t)
        (msg . ,"")))))
        
;;; Eval
(defroute post-editor-eval ("/editor/eval" :method :post) ()
  (let* ((param (json:decode-json-from-string 
                 (hunchentoot:raw-post-data :force-text t)))
         (src (cdr (assoc :src param))))
    (hunchentoot:log-message* :INFO src)
    
    ;; Eval in sub thread
    ; (format *session-input-stream* "(if (and *lde-thread* (sb-thread:thread-alive-p *lde-thread*)) (sb-thread:interrupt-thread *lde-thread* (lambda () ~a)) (setf *lde-thread* (sb-thread:make-thread (lambda () ~a))))~%" src src)
    ; (finish-output *session-input-stream*)

    ;; Eval in main thread
    (format *session-input-stream* "(progn ~a)~%" src src)
    (finish-output *session-input-stream*)
    
    (json:encode-json-to-string
      `((data . ,src)
        (success . ,t)))))

        
#|
 | Handler / Explorer
 |#

(defroute view-explorer ("/explorer" :method :get) ()
  (let ((djula:*catch-template-errors-p* nil)
        (djula:*fancy-error-template-p* nil))
    (djula:render-template* 
      (djula:compile-template* "explorer.html") 
      nil)))
      
      
#|
 | WebSocket
 |#

(defvar *session-ws-server*
  (lambda (env)
    (let ((ws (make-server env)))
      (on :message ws
          (lambda (message)
            (send ws message)))
      (on :open ws
          (lambda ()
            (setf *session-clients* (append *session-clients* (list ws)))))
      (lambda (responder)
        (declare (ignore responder))
        (start-connection ws)))))

(in-package :cl-user)
