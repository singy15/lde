;; Load systems
(ql:quickload :lde)

;; Start server
(sb-ext:save-lisp-and-die "lde.exe" :toplevel #'lde::main :executable t :application-type :gui)

