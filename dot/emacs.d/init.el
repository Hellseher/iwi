;;; package ---  init.el Emacs configuration
;;; Created    : <Tue 10 Mar 2015 11:39:46>
;;; Modified   : <2019-6-05 Wed 00:45:08 BST> Sharlatan
;;; Author     : sharlatan

;;; Commentary:
;;
;; This build is wraped around `use-package macro which helps to
;; control the consitance infrostructure of the Emacs system.
;;
;; If you add some changes reload init.el with `M-x eval-buffer' or
;; `M-x load-file ~/.emacs.d/init.el'
;;
;; - https://shrysr.github.io/docs/sr-config/
;; - http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html
;;
;;; Code:

;;; CORE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initial configurateion to make `use-package' main configuration
;; macro. Load dependancies libraries.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(put 'use-package 'lisp-indent-function 1)

(setq use-package-verbose t
      use-package-minimum-reported-time 0.01
      use-package-compute-statistics t)

;;; core-libs
(use-package f     :defer t) ; API for working with files and directories
(use-package dash  :defer t) ; List library. NO `cl' required.
(use-package s     :defer t) ; String manipulation library.

;; part-of-emacs: t
;; synopsis: C premitive functions.
(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :custom
  (scroll-step 1)
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (tab-width 4)
  (debug-on-quit nil))

;;; USE-PACKAGE-EXTENSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set of required addionals keywords and fucntions to extand
;; `use-package' functionality.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis: Functions to manage system packages.
;; URL: https://gitlab.com/jabranham/system-packages
;; purpose: `:ensure-system-package' allows to ensure system binaries exist.
(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t "Don't ask user confirmation to install package")
  (system-packages-use-sudo t))

;; part-of-emacs: nil
;; synopsis: Auto install system packages.
;; URL: https://github.com/waymondo/use-package-ensure-system-package
(use-package use-package-ensure-system-package :ensure t)

;; part-of-emacs: nil
;; synopsis: Diminished modes are minor modes with no modeline display.
;; URL: https://github.com/myrjola/diminish.el
;; purpose: to enable `:deminish' keyword in `use-package'
(use-package diminish :ensure t)

;; part-of-emacs: nil
;; synopsis: A simple way to manage personal keybindings.
;; purpose: to enable `:bind' keyworkd in `use-package'
(use-package bind-key :ensure t)

;; part-of-emacs: nil
;; sysnopsis: Emacs Lisp packages built directly from source.
;; URL: https://framagit.org/steckerhalter/quelpa
;; purpose: to enable `:quelpa' keyworkd in `use-package'
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

;; part-of-emacs: nil
;; synopsis: quelpa handler for use-package.
;; URL: https://framagit.org/steckerhalter/quelpa-use-packag
(use-package quelpa-use-package :ensure t)

;;; GLOGAL-CONFIRUATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode agnostic configuration, related to majority of functionality.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: nil
;; synopsis: A packages menu, colored, with package ratings, and customizable.
;; URL: https://github.com/Malabarba/paradox
(use-package paradox
  :ensure t
  :defer 1
  :config
  (paradox-enable))

;; part-of-emacs: t
;; synopsis: File input and output commands for Emacs.
(use-package files
  :ensure nil
  :hook
  ((before-save . delete-trailing-whitespace)
   (before-save . time-stamp))
  :custom
  (require-final-newline t)
  (backup-by-copying t)
  (backup-directory-alist
   `((".*" . ,(expand-file-name
               (concat user-emacs-directory "backups")))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

;; part-of-emacs: t
;; sysnopsis: Basic editing commands for Emacs.
(use-package simple
  :ensure nil
  :custom
  (kill-ring-max 300)
  :diminish
  ((visual-line-mode . " ↩")
   (auto-fill-function . " ↵"))
  :config
  (column-number-mode t)
  (toggle-truncate-lines 1))

;; part-of-emacs: t
;; synopsis: Dinamicaly update time stamp of the file.
(use-package time-stamp
  :defer t
  :custom
  (time-stamp-pattern
   "8/Modified[ \t]*:\\\\?[ \t]*<%04Y-%:m-%02d %03a %02H:%02M:%02S %Z> %u\\\\?$"))

;; part-of-emacs: t
;; synopsis: Revert buffers when files on disk change.
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

;; part-of-emacs: nil
;; synopsis: Init file(and directory) Quick Acces
;; URL: https://github.com/emacsmirror/iqa
(use-package iqa
  :ensure t
  :custom
  (iqa-user-init-file (concat user-emacs-directory "init.el"))
  :config
  (iqa-setup-default))

;; part-of-emacs: t
;; synopsis: Tools for customizing Emacs and Lisp packages.
(use-package cus-edit
  :defer t
  :custom
  (custom-file null-device))

;; part-of-emacs: nil
;; synopsis: View large files in Emacs.
;; URL: https://github.com/m00natic/vlfi
(use-package vlf
  :ensure t
  :after (ivy counsel)
  :config
  (ivy-add-actions  'counsel-find-file '(("l" vlf "view large file"))))

;; part-of-eamcs: t
;; synopsis: EasyPG assistant.
(use-package epa
  :defer t
  :ensure nil
  :custom
  (epg-gpg-program "gpg")
  (epa-pinentry-mode nil))

;; part-of-emacs: t
;; synopsis: Unique buffer names dependent on file name.
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

;; part-of-emacs: t
;; synopsis: Save minibuffer history.
(use-package savehist
  :unless noninteractive
  :config
  (savehist-mode 1))

;; part-of-emacs: t
;; synopsis: Automatically save place of cursor in files.
(use-package saveplace
  :unless noninteractive
  :config
  (save-place-mode 1))

;;; TRAMP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; It is for transparently accessing remote files from within
;; Emacs. TRAMP enables an easy, convenient, and consistent interface
;; to remote files as if they are local files. TRAMP's
;; transparency extends to editing, version control, and dired.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis: Transparent Remote Access, Multiple Protocol.
(use-package tramp
  :defer 5
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil)
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp")))

;; part-of-emacs: nil
;; synopsis: Open files as another user.
;; URL: https://github.com/nflath/sudo-edit
(use-package sudo-edit
  :ensure t
  :bind
  (:map ctl-x-map ("M-s" . sudo-edit)))

;; part-of-emacs: nil
;; synopsis: Get environment variables such as $PATH from the shell.
;; URL: https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :defer 0.1
  :config
  (exec-path-from-shell-initialize))


;;; ESHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; It is a shell-like command interpreter implemented in Emacs
;; Lisp. It invokes no external processes except for those requested
;; by the user. It is intended to be an alternative to the IELM (see
;; Emacs Lisp Interaction) REPL for Emacs and with an interface
;; similar to command shells such as bash, zsh, rc, or 4dos.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis: The Emacs command shell.
(use-package eshell
  :defer t
  :ensure nil)

;; part-of-emacs: t
;; synopsis:h
(use-package em-smart
  :defer t
  :ensure nil
  :config
  (eshell-smart-initialize)
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))

;; part-of-emacs: nil
;; synopsis:
;; link:
(use-package esh-help
  :ensure t
  :config
  (setup-esh-help-eldoc))

;; part-of-emacs: nil
;; synopsis: fish-like history autosuggestions in eshell
;; link: https://github.com/dieggsy/esh-autosuggest
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)


;;; DIRED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; It makes an Emacs buffer containing a listing of a directory,
;; and optionally some of its subdirectories as well. You can use the
;; normal Emacs commands to move around in this buffer, and special
;; Dired commands to operate on the listed files. Dired works with
;; both local and remote directories.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis:
(use-package dired
  :ensure nil
  :custom (dired-dwim-target t "guess a target directory")
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-toggle
  :ensure t
  :defer t)

(use-package dired-hide-dotfiles
  :ensure t
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

;; part-of-emacs: t
;; synopsis: operate on buffers like dired
(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

;; part-of-emacs: nil
;; synopsis: asynchronous processing in emacs
;; URL: https://github.com/jwiegley/emacs-async
(use-package async
  :ensure t
  :init
  (dired-async-mode t))

(use-package dired-rsync
  :ensure t
  :bind
  (:map dired-mode-map ("r" . dired-rsync)))

(use-package dired-launch :ensure t)


;;; NATURAL-LANGUAGES-SUPPORT
;;

;; Spell checkers

(use-package mule
  :ensure nil
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment "UTF-8"))

;; part-of-emacs: t
;; synopsis: interface to spell checkers
(use-package ispell
  :defer t
  :ensure nil
  :custom
  (ispell-local-dictionary-alist
   '(("russian"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[-']"  nil ("-d" "en_GB") nil utf-8)))
  (ispell-program-name "hunspell")
  (ispell-dictionary "russian")
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-silently-savep t))

(use-package flyspell
  :defer t
  :ensure nil
  :custom
  (flyspell-delay 1))

;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything wich concerns visual repsentation of Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; additional-themes
(use-package doom-themes                          :ensure t :defer t)
(use-package gruvbox-theme                        :ensure t :defer t)
(use-package color-theme-sanityinc-solarized-dark :ensure t :defer t)

;; part-of-emacs: nil
;; synopsis: A minimal and morden modeline.
;; URL: https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon t)
  (doom-modeline-height 10)
  (doom-modeline-bar-width 3))

;; part-of-emacs: t
;; synopsis:
(use-package faces
  :ensure nil
  :defer 0.1
  :custom
  (face-font-family-alternatives '(("Hack"
                                    "Consolas"
                                    "Monaco"
                                    "Monospace")))
  :config
  (set-face-attribute 'default
                      nil
                      :family (caar face-font-family-alternatives)
                      :weight 'regular
                      :height 120))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :defer t
  :ensure nil
  :config
  (scroll-bar-mode -1))

;; part-of-emacs:
;; synopsis:
;; URL:
(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1)
  :bind
  ([S-f10] . menu-bar-mode))

(use-package tooltip
  :defer t
  :ensure nil
  :custom
  (tooltip-mode -1))

(use-package time
  :defer t
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))

(use-package font-lock+
  :ensure nil
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

;; part-of-emacs: nil
;; synopsis: A library for inserting Developer icons.
;; URL: https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (all-the-icons-install-fonts)
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0))))

;; part-of-emacs: nil
;; synopsis:
(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; part-of-emacs: nil
;; synopsis:
(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :config
  (all-the-icons-ivy-setup))

;; part-of-emacs: nil
;; synopsis: A startup screen extracted from Spacemacs.
;; URL: https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice '(lambda () (get-buffer "*dashboard*")))
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t))

;; part-of-emacs: t
;; synopsis: Restore old window configurations.
(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package hl-line
  :ensure nil
  :hook
  (prog-mode . hl-line-mode))

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

;; part-of-emacs: nil
;; sysnopsis:  Display ^L page breaks as tidy horizontal lines
;; URL: https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; part-of-emacs: nil
;; synopsis: Highlight brackets according to their depth
;; URL: https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; part-of-emacs: nil
;; synopsis:  Highlight identifiers according to their names.
;; URL: https://github.com/Fanael/rainbow-identifiers
(use-package rainbow-identifiers
  :ensure t
  :custom
  (rainbow-identifiers-cie-l*a*b*-lightness 80)
  (rainbow-identifiers-cie-l*a*b*-saturation 50)
  (rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face)
  :hook
  (emacs-lisp-mode . rainbow-identifiers-mode)
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook prog-mode)


;;; NAVIGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Native navigation inside active buffer, jump to line, jump to
;; symbol, jump to word, jump to error. Move around menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :custom
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :config
  (ivy-mode t))

(use-package ivy-yasnippet
  :ensure t)

(use-package ivy-xref
  :ensure t
  :defer t
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; part-of-emacs: t
;; synopsis: Various completion functions using Ivy.
;; URL: https://github.com/abo-abo/swiper
(use-package counsel
  :ensure t
  :bind
  (([remap menu-bar-open] . counsel-tmm)
   ([remap insert-char] . counsel-unicode-char)
   ([remap isearch-forward] . counsel-grep-or-swiper)
   :map mode-specific-map
   :prefix-map counsel-prefix-map
   :prefix "c"
   ("a" . counsel-apropos)
   ("b" . counsel-bookmark)
   ("B" . counsel-bookmarked-directory)
   ("c w" . counsel-colors-web)
   ("c e" . counsel-colors-emacs)
   ("d" . counsel-dired-jump)
   ("f" . counsel-file-jump)
   ("F" . counsel-faces)
   ("g" . counsel-org-goto)
   ("h" . counsel-command-history)
   ("H" . counsel-minibuffer-history)
   ("i" . counsel-imenu)
   ("j" . counsel-find-symbol)
   ("l" . counsel-locate)
   ("L" . counsel-find-library)
   ("m" . counsel-mark-ring)
   ("o" . counsel-outline)
   ("O" . counsel-find-file-extern)
   ("p" . counsel-package)
   ("r" . counsel-recentf)
   ("s g" . counsel-grep)
   ("s r" . counsel-rg)
   ("s s" . counsel-ag)
   ("t" . counsel-org-tag)
   ("v" . counsel-set-variable)
   ("w" . counsel-wmctrl)
   :map help-map
   ("F" . counsel-describe-face))
  :init
  (counsel-mode))

(use-package ivy-rich
  :ensure t
  :custom
  (ivy-rich-switch-buffer-name-max-length 60 "Increase max length of buffer name.")
  :config
  (ivy-rich-mode 1))

(use-package recentf
  :custom
  (recentf-auto-cleanup 30)
  :config
  (run-with-idle-timer 30 t 'recentf-save-list))

;; counsel-M-x can use this one
(use-package smex :ensure t)

(use-package swiper :ensure t)

(use-package mb-depth
  :ensure nil
  :config
  (minibuffer-depth-indicate-mode 1))

;; part-of-emacs: t
;; synopsis: Jump to arbitrary positions in visible text and select text quickly.
;; URL: https://github.com/abo-abo/avy
(use-package avy
  :defer t
  :config
  (avy-setup-default)
  :bind
  (("C-:" .   avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line)
   ("M-s M-s" . avy-goto-word-1)))

(use-package avy-zap
  :ensure t
  :bind
  ([remap zap-to-char] . avy-zap-to-char))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame.")
  :bind
  (("M-o" . ace-window)))

(use-package link-hint
  :ensure t
  :bind
  (("<XF86Search>" . link-hint-open-link)
   ("S-<XF86Search>" . link-hint-copy-link)
   :map mode-specific-map
   :prefix-map link-hint-keymap
   :prefix "l"
   ("o" . link-hint-open-link)
   ("c" . link-hint-copy-link)))

(use-package ace-link
  :ensure t
  :after link-hint ; to use prefix keymap
  :bind
  (:map link-hint-keymap
        ("l" . counsel-ace-link))
  :config
  (ace-link-setup-default))

(use-package select
  :ensure nil
  :custom
  (selection-coding-system 'utf-8)
  (select-enable-clipboard t "Use the clipboard"))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package edit-indirect
  :ensure t
  :bind
  (:map mode-specific-map
        ("r" . edit-indirect-region)))

(use-package clipmon
  :ensure t
  :config
  (clipmon-mode))

(use-package man
  :ensure nil
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t)))))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; part-of-emacs: t
;; synopsis: Display available keybindings in popup.
;; URL: https://github.com/justbur/emacs-which-key
(use-package which-key
  :defer nil
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package free-keys
  :commands free-keys)

(use-package helpful
  :defer t)

(use-package multitran
  :defer t)

(use-package imgbb
  :ensure t)

(use-package calendar
  :defer t
  :ensure nil
  :custom
  (calendar-week-start-day 1))

;;; DOCUMENTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Document editing/vewing, formatting and organazings configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis: Outline-based notes management and organizer.
;; URL: https://orgmode.org
(use-package org
  :mode (("\\.org$" . org-mode))
  :defer t
  :ensure org-plus-contrib
  :custom
  (org-babel-load-languages
   '((R . t)
     (shell . t)
     (restclient . t))))

;; part-of-emacs: nil
;; synopsis: Org exporter for pandoc.
;; URL: https://github.com/kawabata/ox-pandoc
(use-package ox-pandoc
  :after org
  :defer 5)

;; part-of-emacs: nil
;; synopsis: Show bullets in org-mode as UTF-8 characters.
;; URL: https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :custom
  (org-ellipsis "…")
  :hook
  (org-mode . org-bullets-mode))

;; part-of-emacs: nil
;; synopsis: Create an aggregated Org table from another one.
;; URL: https://github.com/tbanel/orgaggregate
(use-package orgtbl-aggregate
  :ensure t)

;; part-of-emacs: nil
;; synopsis: An org-mode extension to restclient.el
;; URL: https://github.com/alf/ob-restclient.el
(use-package ob-restclient
  :after org restclient)

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;;; LANGUAGE-SUPPORT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Most of the language support should include at least -
;; autocompletion, syntax higlighting, code navigation and
;; documentation search
;;
;; Supported programming and markup languages:
;; + Rust
;; + Perl
;; + Python
;; + Shell/Bash
;; + YAML
;; + JSON
;; + TOML
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: nil
;; synopsis: A major emacs mode for editing Rust source code
;; URL: https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  :mode
  ("\\.[Rr][Ss]\\'" . rust-mode)
  :custom
  (rust-format-on-save t))

;; part-of-emacs: nil
;; synopsis: code completion, goto-definition and docs browsing for Rust via racer
;; URL: https://github.com/racer-rust/emacs-racer
(use-package racer
  :ensure t
  :after rust-mode
  :diminish (racer-mode . "\u24e1")
  :hook
  ((rust-mode . racer-mode)
   (racer-mode . eldoc-mode))
  :custom
  (racer-rust-src-path ""))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package cargo
  :ensure t
  :after rust-mode
  :hook
  (rust-mode . cargo-minor-mode))

;; part-of-emacs: t
;; synopsis: Lisp editing commands for Emacs
(use-package lisp
  :ensure nil
  :hook
  (after-save . check-parens))

(use-package csv-mode
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package markdown-mode
  :ensure-system-package markdown
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init (setq markdown-command "markdown"))

;; part-of-emacs: nil
;; synopsis: it adds interaction R, S-Plus, SAS, Stata and OpenBUGS/JAGS.
;; URL: https://ess.r-project.org/
(use-package ess
  :ensure t)

;; part-of-emacs: t
;; synopsis: perl code editing commands for emacs
(use-package cperl-mode
  :mode
  ("\\.pl\\'" . cperl-mode))

(use-package ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  :mode
  (("/\\.ssh/config\\'"     . ssh-config-mode)
   ("/sshd?_config\\'"      . ssh-config-mode)
   ("/known_hosts\\'"       . ssh-known-hosts-mode)
   ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :hook
  (ssh-config-mode . turn-on-font-lock))

(use-package plantuml-mode
  :ensure t
  :mode
  (("\\.plantuml\\'" . plantuml-mode)))

(use-package lua-mode
  :defer t)

(use-package toml-mode
  :ensure t
  :mode
  (("\\.toml\\'" . toml-mode)
   ("\\.tml\\'" . toml-mode)))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode)))

;; (use-package htmlize
;;   :defer t
;;   :custom
;;   (org-html-htmlize-output-type 'css)
;;   (org-html-htmlize-font-prefix "org-"))

;; (use-package ibuffer-vc
;;   :config
;;   (define-ibuffer-column icon
;;     (:name "Icon" :inline t)
;;     (all-the-icons-ivy--icon-for-mode major-mode))
;;   :custom
;;   (ibuffer-formats
;;    '((mark modified read-only vc-status-mini " "
;;            (name 18 18 :left :elide)
;;            " "
;;            (size 9 -1 :right)
;;            " "
;;            (mode 16 16 :left :elide)
;;            " "
;;            filename-and-process)) "include vc status info")
;;   :hook
;;   (ibuffer . (lambda ()
;;                (ibuffer-vc-set-filter-groups-by-vc-root)
;;                (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                  (ibuffer-do-sort-by-alphabetic)))))

;;;; PROJECTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set of packages for better productivities with all kind of
;; projects. Collection of snippets, autocmplite, git etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: nil
;; synopsis: Manage and navigate projects in Emacs easily
;; URL: https://github.com/bbatsov/projectile
(use-package projectile
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :custom
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  (projectile-completion-system 'ivy))

;; part-of-emacs: nil
;; synopsis: a git porcelain inside emacs
;; URL: https://github.com/magit/magit
(use-package magit
  :bind (("C-x g" . magit-status)))

;; part-of-emacs: nil
;; synopsis: Project management for Emacs package development.
;; URL: http://github.com/cask/cask
(use-package cask
  :ensure t)

;; part-of-emacs:
;; synopsis:
;; URL:
(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

;; part-of-emacs: nil
;; synopsis: Browse target page on github/bitbucket from Emacs buffers.
;; URL: https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . browse-at-remote)
        ("k" . browse-at-remote-kill)))

;; part-of-emacs: t
;; synopsis: minor mode to resolve diff3 conflicts
(use-package smerge-mode
  :defer t
  :ensure nil
  :diminish smerge-mode)

;; part-of-emacs: nil
;; synopsis: emacs package for highlighting uncommitted changes
;; URL: https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

;; part-of-emacs: nil
;; synopsis: smarter commenting for Emacs.
;; URL: https://github.com/paldepind/smart-comment
(use-package smart-comment
  :bind ("M-;" . smart-comment))

(use-package counsel-projectile
  :after counsel projectile
  :config
  (counsel-projectile-mode))

;; (use-package ag
;;   :defer t
;;   :ensure-system-package (ag . silversearcher-ag)
;;   :custom
;;   (ag-highlight-search t "Highlight the current search term."))

;; (use-package dumb-jump
;;   :defer t
;;   :custom
;;   (dumb-jump-selector 'ivy)
;;   (dumb-jump-prefer-searcher 'ag))

;; part-of-emacs: t
;; synopsis: Modular text completion framework.
;; URL: http://company-mode.github.io/
(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :hook
  (after-init . global-company-mode))

;; part-of-emacs: nil
;; synopsis: Popup documentation for completion candidates.
;; URL: https://www.github.com/expez/company-quickhelp
(use-package company-quickhelp
  :defer t
  :custom
  (company-quickhelp-delay 3)
  :config
  (company-quickhelp-mode 1))

;; (use-package company-shell
;;   :defer t
;;   :config
;;   (add-to-list 'company-backends 'company-shell))

(use-package autoinsert
  :ensure nil
  :hook
  (find-file . auto-insert))

;; part-of-emacs: nil
;; synopsis: Yet another snippet extension for Emacs.
;; URL: https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  :hook
  (prog-mode  . yas-minor-mode))

;; part-of-emacs: nil
;; synopsis: Yasnippet official snippet collections.
;; URL: https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

;; part-of-emacs: nil
;; synopsis: Jump to and fix syntax errors using `flycheck' with `avy'.
;; URL: https://github.com/magicdirac/avy-flycheck
(use-package avy-flycheck
  :defer t
  :config
  (avy-flycheck-setup))

;; (use-package highlight-defined
;;   :ensure t
;;   :custom
;;   (highlight-defined-face-use-itself t)
;;   :hook
;;   (emacs-lisp-mode . highlight-defined-mode))

;; (use-package highlight-quoted
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . highlight-quoted-mode))

;; (use-package eros
;;   :hook
;;   (emacs-lisp-mode . eros-mode))

;; (use-package suggest
;;   :defer t)

;; (use-package ipretty
;;   :config
;;   (ipretty-mode 1))

(use-package nameless
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))

;; ;; bind-key can't bind to keymaps
;; (use-package erefactor
;;   :defer t)

(use-package flycheck-package
  :defer t
  :after flycheck
  (flycheck-package-setup))

;; (use-package kibit-helper
;;   :defer t)

;; (use-package conkeror-minor-mode
;;   :defer t
;;   :hook
;;   (js-mode . (lambda ()
;;                (when (string-match "conkeror" (or (buffer-file-name) ""))
;;                  (conkeror-minor-mode 1)))))

;; (use-package graphql-mode
;;   :mode "\\.graphql\\'"
;;   :custom
;;   (graphql-url "http://localhost:8000/api/graphql/query"))

;; (use-package executable
;;   :ensure nil
;;   :hook
;;   (after-save . executable-make-buffer-file-executable-if-script-p))

;; (use-package restclient
;;   :mode
;;   ("\\.http\\'" . restclient-mode))

;; (use-package restclient-test
;;   :hook
;;   (restclient-mode-hook . restclient-test-mode))

;; (use-package company-restclient
;;   :after (company restclient)
;;   :config
;;   (add-to-list 'company-backends 'company-restclient))

;; (use-package emamux
;;   :defer t)

;;; CUSTOM-FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Additional functio which do not belong to any of the packages and
;; could be added on the fly but `eval-defun' to the main functionality.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exzellenz/hiorisontal-line ()
  "Insert dashed horisotnal line till 80th column.
It's commented out by current default comment symbol."
  (interactive)
  (progn
    (insert-char #x002D 78)
    (comment-region (line-beginning-position) (line-end-position))))

(defun exzellenz/timestamp ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "%y%m%d%H%M%S")))

(provide 'init.el)
;;; init.el ends here
