;;; _*_lisp_*_
;;; File     : init.lisp
;;; Created  : <2018-9-08 Sat 11:29:00 BST
;;; Modified : <2018-9-09 Sun 01:10:15 BST> Sharlatan
;;; Author   : Sharlatan
;;; Synopsis : <>

(in-package :stumpwm)


(load "~/quicklisp/setup.lisp")
(ql:quickload "clx-truetype")
(ql:quickload :xembed) ;; Required by stumptray

;; load some contrib modules
(set-module-dir "~/Data/sft/src/stumpwm-contrib")
(mapcar #'load-module '("cpu"
                        "mem"
                        "net"
                        "kbd-layouts"
                        "swm-emacs"
                        "disk"
                        "ttf-fonts"))

;; ------------------------------------------------------------------------------
;;; Functionality for local custom modules.

(defvar *stumpwm-config-dir* "~/.stumpwm.d/"
  "StumpWM configuration directory.")

(defun load-usr-module (name)
  "Load custom modules from *stumpwm-config-dir*."
  (load (make-pathname :defaults *stumpwm-config-dir*
                       :name name
                       :type "lisp")))

(mapcar #'load-usr-module '("swm-theme"
                            "swm-swank"
                            "swm-apps"
                            "swm-kbd"))
;;;; End of init.lisp
