(in-package :lde)
  
#|
 | Configuration
 |#

;; Configure djula
(setf djula:*auto-escape* nil)
(setf djula:*catch-template-errors-p* nil)
(setf djula:*fancy-error-template-p* nil)

;; Add template directory
(djula:add-template-directory "templates/")

;; Location of configuration file
(defparameter *config-rel-path* "./config")


#|
 | Variables
 |#

(defparameter *lde-server* nil)
(defparameter *time-to-shutdown* nil)
(defparameter *max-time-to-shutdown* (* 60 999))
(defparameter *os-type* :mswin) ; :linux | :mswin


#|
 | LDE Configuration
 |#
 
;;; LDE configuration
(defparameter *lde-config*
  `(
    :port 9000
    :auto-open t
  ))
  
;;; Load configuration file
(defun load-config ()
  (if (probe-file *config-rel-path*)
    (setf *lde-config* (read-from-string (lde.util:slurp *config-rel-path*)))
    (lde.util:spit *config-rel-path* (format nil "~s" *lde-config*))))


#|
 | Server
 |#
 
;;; Clear current session
(defun clear-session ()
  (defparameter *session* nil)
  (defparameter *session-output-stream* nil)
  (defparameter *session-input-stream* nil)
  (defparameter *session-thread-alive* nil)
  (defparameter *session-thread* nil)
  (defparameter *session-output-send-thread* nil))
  
;;; Open in browser
(defun open-in-browser ()
  (sb-ext:run-program 
    (cond ((equal *os-type* :linux) "/usr/bin/xdg-open")
          ((equal *os-type* :mswin) "C:\\Windows\\explorer.exe")) 
    (list (format nil "http://localhost:~a/" *port-web*)) :wait nil))

;;; Start server
(defun start-lde-server (config)
  ;; Set port
  (defparameter *port-web* (getf config :port))
  (defparameter *port-ws* (+ (getf config :port) 1))

  (format t "lde starts on port ~a ~a~%" *port-web* *port-ws*)

  ;; Create server instance
  (setf *lde-server* 
    (make-instance 'easy-routes:easy-routes-acceptor 
                   :port *port-web*
                   :document-root "document-root/"))
  
  ;; Start listen
  (hunchentoot:start *lde-server*)

  ;; Time to shutdown
  (keep-server-alive)

  ;; Open web socket
  (open-web-socket)
  
  ;; Set initial basepath
  (defparameter *basepath* (namestring (truename "")))

  ;; Start websocket server
  (clack:clackup *session-ws-server* :server :hunchentoot :port *port-ws*)

  ;; Set session
  (clear-session)

  ;; Auto open browser
  (when (getf config :auto-open)
    (open-in-browser))

  ;; Wait for shutdown
  (loop
    (decf *time-to-shutdown*)
    (when (< *time-to-shutdown* 0)

      (when *session-thread*
        (bordeaux-threads:destroy-thread *session-thread*))

      (when *session-output-send-thread*
        (bordeaux-threads:destroy-thread *session-output-send-thread*))

      (when *session*
        (sb-ext:process-kill *session* 15 :process-group)
        (sb-ext:process-wait *session*)
        (sb-ext:process-close *session*)
        (sb-ext:process-exit-code *session*)
        (setf *session* nil))

      ; (format t "Signal kill thread~%")
      ; (setf *session-thread-alive* nil)

      ; (when *session-thread*
      ;   (format t "Wait for join-thread *session-thread*~%")
      ;   (bordeaux-threads:join-thread *session-thread*))

      ; (when *session-output-send-thread*
      ;   (format t "Wait for join-thread *session-output-send-thread~%")
      ;   (bordeaux-threads:join-thread *session-output-send-thread*))

      (cl-user::exit))
    (sleep 1))
)

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
        
(defun do-session-thread ()
  (sleep 1)
  (loop
    (when (or (not *session*) (not *session-thread-alive*))
      (format t "return from watch-output~%")
      (return-from do-session-thread))
    (let ((str (read-char *session-output-stream*)))
      (setf *session-output-string* 
            (format nil "~a~a" *session-output-string* str)))))

(defun do-session-output-send-thread ()
  (loop
    (progn
      (sleep 1)
      (when (not *session-thread-alive*)
        (format t "return from send-output~%")
        (return-from do-session-output-send-thread))
      (mapc (lambda (c)
        (wsd:send c (format nil "~a" *session-output-string*)))
        *session-clients*)
      (setf *session-output-string* ""))))

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
    (defparameter *session-thread-alive* t)
    
    ;; Prepare thread variable
    (format *session-input-stream* "(defpackage lde.client (:use cl) (:export *lde-thread*))~%(defparameter lde.client:*lde-thread* nil)~%")
    (finish-output *session-input-stream*)

    (defparameter *session-output-string* "")

    (defparameter *session-thread* 
      (bordeaux-threads:make-thread 
        #'do-session-thread
        :name "session-thread"))
        
    (defparameter *session-output-send-thread*
      (bordeaux-threads:make-thread 
        #'do-session-output-send-thread
        :name "session-output-send-thread"))

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
         (eval-on-save (cdr (assoc :eval-on-save param)))
         (multithread (cdr (assoc :multithread param))))
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
       
      (if multithread
        ;; Eval in sub thread
        (format *session-input-stream* "(if (and lde.client:*lde-thread* (sb-thread:thread-alive-p lde.client:*lde-thread*)) (sb-thread:interrupt-thread lde.client:*lde-thread* (lambda () (load \"~a\"))) (setf lde.client:*lde-thread* (sb-thread:make-thread (lambda () (load \"~a\")))))~%"
          (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*)))
          (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*))))
  
        ;; Eval in main thread
        (format *session-input-stream* "(load \"~a\")~%"
          (merge-pathnames (format nil "~a.lisp" (namestring (pathname-name filepath))) (cl-fad:pathname-directory-pathname (merge-pathnames filepath *basepath*)))))

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
         (src (cdr (assoc :src param)))
         (multithread (cdr (assoc :multithread param))))
    (hunchentoot:log-message* :INFO src)
    
    (if multithread
      ;; Eval in sub thread
      (progn
        (format *session-input-stream* "(if (and lde.client:*lde-thread* (sb-thread:thread-alive-p lde.client:*lde-thread*)) (sb-thread:interrupt-thread lde.client:*lde-thread* (lambda () ~a)) (setf lde.client:*lde-thread* (sb-thread:make-thread (lambda () ~a))))~%" src src)
        (finish-output *session-input-stream*))
  
      ;; Eval in main thread
      (progn
        (format *session-input-stream* "~a~%" src src)
        (finish-output *session-input-stream*)))
    
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
 | Handler / Server
 |#

;;; Reset kill timer
(defun keep-server-alive ()
  (setf *time-to-shutdown* *max-time-to-shutdown*))

;;; GET /siki/keep-alive
(defroute get-keep-alive ("/siki/keep-alive" :method :get) ()
  (keep-server-alive)
  (json:encode-json-to-string 
    `(:success t :time ,*time-to-shutdown*)))

; ;;; GET /siki/server-state
; (defroute get-server-state ("/siki/server-state" :method :get) ()
;   (json:encode-json-to-string 
;     `((:success . t) 
;       (:siki-port . ,*siki-port*)
;       (:swank-port . ,*swank-port*))))
; 

;;; GET /siki/shutdown
(defroute get-siki-shutdown ("/siki/shutdown" :method :get) ()
  (cl-user::exit))
      
      
#|
 | WebSocket
 |#
(defun open-web-socket ()
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
          (start-connection ws))))))


#|
 | Program entry point
 |#
(defun main ()
  ;; Load configuration
  (load-config)
  
  ;; Start server
  (start-lde-server 
    ; (+ 40000 (random 1000 (make-random-state t)))
    ; (read-from-string (nth 1 (sb-ext:*posix-argv*)))
    *lde-config*
    ))


(in-package :cl-user)