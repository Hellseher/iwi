;;;; _*_lisp_*_
;;;; File     : swm-kbd.lisp
;;;; Created  : <2018-9-08 Sat 14:28:15 BST>
;;;; Modified : <2018-9-08 Sat 20:30:54 BST> Sharlatan
;;;; Author   : Sharlatan
;;;; Synopsis : <Set or reset keyboard bindings for StumpWM.>

(in-package :stumpwm)


(set-prefix-key (kbd "C-t"))

;; focus follow mouse
(setf *mouse-focus-policy* :click)

(kbd-layouts::keyboard-layout-list "gb" "ru")

(setf *caps-lock-behavior* :ctrl)
;;;; End of swm-kbd.lisp
