(in-package :lde.util)

#|
 | Utilities
 |#

;; Trim
(defun trim (str)
  (string-trim '(#\Space #\Newline #\Tab) str))

;; Read file
(defun slurp (path)
  (alexandria:read-file-into-string path :external-format :utf-8))

;; Write file
(defun spit (path content)
  (alexandria:write-string-into-file content path 
    :external-format :utf-8 :if-exists :supersede))

;;; Ensure file exists
(defun ensure-file (path)
  (when (not (probe-file path))
    (error "File not found: ~A" path))
  path)

(in-package :cl-user)