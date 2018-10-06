;;;; _*_lisp_*_
;;;; File     : swm-theme.lisp
;;;; Created  : <2018-9-08 Sat 11:29:00 BST>
;;;; Modified : <2018-9-20 Thu 22:06:07 BST> Sharlatan
;;;; Author   : Sharlatan
;;;; Synopsis : <Set up a general visual apperance of the StampWM.>

(in-package :stumpwm)

(set-fg-color "#61afef")
(set-bg-color "#21252b")
(set-border-color "#21252b")
(set-win-bg-color "#21252b")
(set-focus-color "#61afef")
(set-unfocus-color "#21252b")
(set-maxsize-gravity :center)
(set-transient-gravity :top)
(set-msg-border-width 5)

(setf  *maxsize-border-width* 1
       *transient-border-width* 1
       *normal-border-width* 1
       *window-border-style* :thin
       *message-window-gravity* :top
       *message-window-padding* 20
       *input-window-gravity* :bottom
       *mouse-follows-focus* t)

(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "Droid Sans"
                         :subfamily "Regular"
                         :size 11
                         :antialiased t))

;;; mode-line configuration

(setf *mode-line-background-color*  "#38394c"
      *mode-line-foreground-color*  "#61afef"
      *mode-line-border-color*      "#28394c")

(setf *screen-mode-line-format*
      (list ".:| "
            '(:eval (run-shell-command "date '+%R, %F %a'|tr -d [:cntrl:]" t))
            " | "
            '(:eval (run-shell-command "cut -d' ' -f 1,2,3  /proc/loadavg |tr -d [:cntrl:]" t))
            " | %l |:. [^B%n^b] %w"))


 ;; ".:| #(\| #S #I:#P \| %a %d-%m-%Y \| ⌚#[fg=colour34] %H:%M #[fg=colour244]|:."
;; End of swm-theme.lisp
