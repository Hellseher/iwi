;;; File:           init.el
;;;
;;; Created       :  Tue 10 Mar 2015 11:39:46
;;; Last Modified :  Fri 19 Jun 2015 08:13:04
;;; Maintainer    :  sharlatan
;;;
;;;
;;-=:[ PLUGINS INSTAL ]:=----------------------------------------------------{{{
;; Package manager setup
(require 'package) ;; You might already have this line
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;;
(require 'powerline)(powerline-default-theme)
(load-file "~/.emacs.d/elpa/color-theme-solarized-20150110.936/color-theme-solarized.el")

(require 'evil)
(evil-mode 1)
;;(require 'color-theme-solarized)(color-theme-solarized)
;;< END OF PLUGINS INSTAL >--------------------------------------------------}}}

;;-=:[ PLUGINS SETTINGS ]:=--------------------------------------------------{{{
;;
;;---[ Slime
(load (expand-file-name "~/.cl/slime-helper.el")) 
;; Replace "sbcl" with the path to your implementation 
(setq inferior-lisp-program "sbcl") 
;; Add documentation and syntax hl to SLIME
(add-to-list 'load-path "~/.slime/")
(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-autodoc))
;;---[ evil-mode
;;< END OF PLUGINS SETTINGS >------------------------------------------------}}}

;;-=:[ EMACS SETTINGS ]:=----------------------------------------------------{{{
;;---[ Encoding
(progn
(setq default-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8))
;;----:[ Visual Settings ]:----
;; Color-themes
;(require 'color-theme)
;(color-theme-initialize)
;(setq color-theme-i-global t)
;(color-theme-classic)
;; Synatx hl
(require 'font-lock)

(require 'cedet)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) 

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;---:[ lines ]---
(require 'linum)
(line-number-mode   t)
(global-linum-mode  t)
(column-number-mode t)
(setq linum-format " %d ")
;;--[ window separators
(let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
      (setq standard-display-table display-table))
;;---[ Server mode
(require 'server)
  (unless (server-running-p)
            (server-start))
;; (set-face-attribute 'default nil: font "Terminus-12")
;;< END OF EMACS SETTINGS >--------------------------------------------------}}}

