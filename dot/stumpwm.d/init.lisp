;;; _*_lisp_*_
;;; File     : init.lisp
;;; Created  : <2018-9-08 Sat 11:29:00 BST
;;; Modified : <2021-07-03 Sat 11:42:58 BST>
;;; Author   : Sharlatan
;;; Synopsis : <Configuration file for StumpWM>

(in-package :stumpwm)

(defparameter *required-systems*
  '(truetype-clx slynk))
  ;'(truetype-clx cl-diskspace cl-mount-info slynk xembed))

#+quicklisp
(ql:quickload *required-systems*)

;; #+slynk
;; (slynk:create-server :dont-close t)
;; #+swank
;; (swank:create-server :port 4006)

;; ;; (set-module-dir "/mnt/library/code/stumpwm-contrib")
;; (mapcar #'load-module '("cpu"
;;                         "mem"
;;                         "net"
;;                         "kbd-layouts"
;;                         "disk"))

;;; Functionality for local custom modules.

;; (defparameter *stumpwm-config-dir* "~/.stumpwm.d/")
(set-prefix-key (kbd "s-t"))

;; ;;https://mmk2410.org/2018/02/15/scrolling-doesnt-work-in-gtk-3-apps-in-stumpwm/
;; ;; bugfix for scrolling doesn't work with an external mouse in GTK+3 apps.
;; (setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

;; focus follow mouse
(setf *mouse-focus-policy* :click)
;; (kbd-layouts::keyboard-layout-list "us" "ru")
(setf *caps-lock-behavior* :ctrl)

;;; UI
(set-fg-color "#61afef")
(set-bg-color "#21252b")
(set-border-color "#21252b")
(set-win-bg-color "#21252b")
(set-focus-color "#61afef")
(set-unfocus-color "#21252b")
(set-maxsize-gravity :center)
(set-transient-gravity :top)
(set-msg-border-width 5)

(setf *maxsize-border-width* 1
      *transient-border-width* 1
      *normal-border-width* 1
      *window-border-style* :thin
      *message-window-gravity* :top
      *message-window-padding* 20
      *input-window-gravity* :bottom
      *mouse-follows-focus* t)


;;;; mode-line

(setf *mode-line-background-color*  "#38394c"
      *mode-line-foreground-color*  "#61afef"
      *mode-line-border-color*      "#28394c")

(setf *screen-mode-line-format*
     (list ".:| "
           '(:eval (run-shell-command "date '+%R, %F %a'|tr -d [:cntrl:]" t))
           " | "
           '(:eval (run-shell-command "cut -d' ' -f 1,2,3  /proc/loadavg |tr -d [:cntrl:]" t))
           " | %l |:. [^B%n^b] %w"))

(toggle-mode-line (current-screen) (current-head))

;;;; End of init.lisp
