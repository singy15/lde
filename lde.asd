(defsystem "lde"
  :version "0.1.0"
  :author "kedama"
  :license "MIT"
  :depends-on (
    "hunchentoot" 
    "djula" 
    "cl-json"
    "easy-routes"
    "alexandria"
    "cl-fad"
    "cl-ppcre"
    "websocket-driver-server"
    "websocket-driver-client"
    "clack"
    "bordeaux-threads")
  :components ((:module "src"
                :components
                ((:file "package-util")
                 (:file "package-lde")
                 (:file "util")
                 (:file "lde"))))
  :description ""
  :in-order-to ((test-op (test-op "lde/tests"))))

(defsystem "lde/tests"
  :author ""
  :license ""
  :depends-on ("lde"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lde"
  :perform (test-op (op c) (symbol-call :rove :run c)))

