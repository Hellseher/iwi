;;; File:           init.el
;;;
;;; Created       :  Tue 10 Mar 2015 11:39:46
;;; Last Modified :  Fri 16 Oct 2015 22:29:56
;;; Maintainer    :  sharlatan
;;; Credits       :  http://aaronbedra.com/emacs.d/
;;;               :  http://vinitkumar.me/articles/2014/05/04/Setting-Up-Emacs-For-Development/
;;;               :  https://github.com/purcell/emacs.d/blob/master/init.el
;;;
;;;
;;-=:[ PACKAGES ]:=----------------------------------------------------{{{
;; Package manager setup
(require 'package)
(add-to-list 'package-archives
                          '("melpa" . "https://melpa.org/packages/"))

(load (expand-file-name "~/.cl/slime-helper.el")) 
(add-to-list 'load-path "~/.cl/slime/")
; --[ list of required packages
(package-initialize)
(defvar abedra/packages '(ac-slime
                           auto-complete
                           autopair
                           clojure-mode
                           coffee-mode
                           csharp-mode
                           deft
                           erlang
                           evil
                           feature-mode
                           flycheck
                           go-mode
                           graphviz-dot-mode
                           haml-mode
                           haskell-mode
                           htmlize
                           idris-mode
                           markdown-mode
                           marmalade
                           nodejs-repl
                           o-blog
                           org
                           paredit
			   powerline
                           php-mode
                           puppet-mode
                           restclient
                           rvm
                           scala-mode
                           smex
                           sml-mode
                           solarized-theme
                           web-mode
                           writegood-mode
                           yaml-mode)
  "Default packages")

(defun abedra/packages-installed-p ()
    (loop for pkg in abedra/packages
                  when (not (package-installed-p pkg)) do  (return nil)
                          finally (return t)))

(unless (abedra/packages-installed-p)
    (message "%s" "Refreshing package database...")
      (package-refresh-contents)
        (dolist (pkg abedra/packages)
              (when (not (package-installed-p pkg))
                      (package-install pkg))))

;; ---[ load packages
(require 'linum)
(require 'server)
(require 'font-lock)
(require 'cedet)
(require 'powerline)        
(require 'auto-complete-config)
(require 'ansi-color)
(require 'evil)             ; Vim-mode for Emacs
(require 'autopair)         ; makes sure that brace structures (), [], {}
(require 'slime-autoloads)  
(require 'slime)
(require 'neotree)          ; NERDTree for Emacs
;;< END OF PLUGINS INSTAL >--------------------------------------------------}}}

;;-=:[ PACKAGES SETTINGS ]:=--------------------------------------------------{{{
;;---[ Slime
(eval-after-load "slime"
                 '(progn
                    (setq slime-lisp-implementations
                          '((sbcl ("/usr/bin/sbcl"))
                            (ecl ("/usr/bin/ecl"))
                            (clisp ("/usr/bin/clisp"))))
                    (slime-setup '(
                                  slime-asdf
                                  slime-autodoc
                                  slime-editing-commands
                                  slime-fancy-inspector
                                  slime-fontifying-fu
                                  slime-fuzzy
                                  slime-indentation
                                  slime-mdot-fu
                                  slime-package-fu
                                  slime-references
                                  slime-repl
                                  slime-sbcl-exts
                                  slime-scratch
                                  slime-xref-browser
                                   ))
                    (slime-autodoc-mode)
                          'slime-fuzzy-complete-symbol))
(evil-mode 1)
(ac-config-default) ; auto-complete
(global-set-key [f8] 'neotree-toggle)
;;< END OF PLUGINS SETTINGS >------------------------------------------------}}}

;;-=:[ EMACS SETTINGS ]:=----------------------------------------------------{{{
;;---[ user details
(setq column-number-mode t)
(setq user-full-name "Sharlatan")
(setq user-mail-address "sharlatanus@gmail.com")
(powerline-default-theme)
;;
;;
;; ----[ highlighting 
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


(if window-system
      (load-theme 'solarized-light t)
        (load-theme 'wombat t))

;; ----[ indenting
(setq tab-width 2)
;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t 
      ingibit-startup-message nil
      initial-major-mode 'org-mode)
; Scroll bar, Tool bar, Menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;---:[ lines ]---
(line-number-mode   t)
(global-linum-mode  t)
(column-number-mode t)
(setq linum-format " %d ")
;;--[ window separators
(let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
      (setq standard-display-table display-table))
;;;---[ Server mode
;  (unless (server-running-p)
;            (server-start))
;; (set-face-attribute 'default nil: font "Terminus-12")
;;< END OF EMACS SETTINGS >--------------------------------------------------}}}
