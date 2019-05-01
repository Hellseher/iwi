;;; package ---  init.el Emacs configuration
;;; Created    : <Tue 10 Mar 2015 11:39:46>
;;; Modified   : <2019-5-02 Thu 00:01:41 BST> Sharlatan
;;; Author     : sharlatan

;;; Commentary:
;;
;; This build is wraped around `use-package macro which helps to
;; control the consitance infrostructure of the Emacs system.

;;; Code:


;;; CORE
;;

(require 'package)

(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(put 'use-package 'lisp-indent-function 1)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)
(setq use-package-compute-statistics t)

;; part-of-emacs: t
;; info: C premitive functions.
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

;; part-of-emacs: t
;; info: functions to manage system packages.
(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

;; part-of-emacs:
;; info:
(use-package use-package-ensure-system-package :ensure t)

;; Keyword
(use-package diminish :ensure t)

(use-package bind-key :ensure t)

(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package :ensure t)


;;; GLOGAL-CONFIRUATIONS
;;

;; part-of-emacs: nil
;; info: A modern Packages Menu. Colored, with package ratings, and customizable.
;; git: https://github.com/Malabarba/paradox
;; melpa: https://melpa.org/#/paradox
(use-package paradox
  :ensure t
  :defer 1
  :config
  (paradox-enable))

;; part-of-emacs: t
(use-package files
  :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
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
;; info: Dinamicaly update ts of the file.
(use-package time-stamp
  :ensure nil
  :hook
  (before-save . time-stamp)
  :custom
  (time-stamp-pattern "8/Modified[ \t]*:\\\\?[ \t]*<%04Y-%:m-%02d %03a %02H:%02M:%02S %Z> %u\\\\?$"))

;; part-of-emacs: t
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

;; part-of-emacs: nil
;; info: Init file(and directory) Quick Acces
;; git: https://github.com/emacsmirror/iqa
;; melpa:
(use-package iqa
  :ensure t
  :custom
  (iqa-user-init-file (concat user-emacs-directory "init.el"))
  :config
  (iqa-setup-default))

;; part-of-emacs: t
;; info: tools for customizing Emacs and Lisp packages
(use-package cus-edit
  :ensure t
  :custom
  (custom-file (concat user-emacs-directory "_customize.el")))

;; part-of-emacs: nil
;; info: View Large Files in Emacs
;; git:
;; melpa:
(use-package vlf
  :ensure t
  :after (ivy counsel)
  :config
  (ivy-add-actions  'counsel-find-file '(("l" vlf "view large file"))))

;; part-of-eamcs: t
;; info: EasyPG Assistant
(use-package epa
  :defer t
  :ensure nil
  :custom
  (epg-gpg-program "gpg")
  (epa-pinentry-mode nil))

;; part-of-emacs: t
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))


;;; TRAMP
;; It is for transparently accessing remote files from within
;; Emacs. TRAMP enables an easy, convenient, and consistent interface
;; to remote files as if they are local files. TRAMP's
;; transparency extends to editing, version control, and dired.

(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package sudo-edit
  :ensure t
  :bind
  (:map ctl-x-map ("M-s" . sudo-edit)))

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

(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

(use-package exec-path-from-shell
  :ensure t
  :defer 0.1
  :config
  (exec-path-from-shell-initialize))


;;; ESHELL
;; It is a shell-like command interpreter implemented in Emacs
;; Lisp. It invokes no external processes except for those requested
;; by the user. It is intended to be an alternative to the IELM (see
;; Emacs Lisp Interaction) REPL for Emacs and with an interface
;; similar to command shells such as bash, zsh, rc, or 4dos.

;; part-of-emacs: t
;; info:
(use-package eshell
  :defer t
  :ensure nil)

;; part-of-emacs: t
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
;; info:
;; link:
(use-package esh-help
  :ensure t
  :config
  (setup-esh-help-eldoc))

;; part-of-emacs: nil
;; info: fish-like history autosuggestions in eshell
;; link: https://github.com/dieggsy/esh-autosuggest
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)


;;; DIRED
;; It makes an Emacs buffer containing a listing of a directory,
;; and optionally some of its subdirectories as well. You can use the
;; normal Emacs commands to move around in this buffer, and special
;; Dired commands to operate on the listed files. Dired works with
;; both local and remote directories.

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
;; info: interface to spell checkers
(use-package ispell
  :defer t
  :ensure nil
  :custom
  (ispell-local-dictionary-alist
   '(("russian"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[-']"  nil ("-d" "ru_RU,en_US") nil utf-8)))
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


;;; UI
;;

(use-package faces
  :ensure nil
  :defer 0.1
  :custom
  (face-font-family-alternatives '(("Hack" "Consolas" "Monaco" "Monospace")))
  :config
  (set-face-attribute 'default
                      nil
                      :family (caar face-font-family-alternatives)
                      :weight 'regular
                      :height 120))

(use-package custom
  :ensure nil
  :custom
  (custom-safe-themes t "Treat all themes as safe"))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :defer t
  :ensure nil
  :config
  (scroll-bar-mode -1))

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

(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0))))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :config
  (all-the-icons-ivy-setup))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-init)
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon t)
  (doom-modeline-height 25))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice '(lambda ()
                            (setq initial-buffer-choice nil)
                            (get-buffer "*dashboard*")))
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5))))

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

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

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


;;; NAVIGATION-AND-AUTOCOMPLETE

;; counsel-M-x can use this one
(use-package smex :ensure t)

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

(use-package ivy-xref
  :ensure t
  :defer t
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

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

(use-package swiper :ensure t)

(use-package ivy-rich
  :ensure t
  :custom
  (ivy-rich-switch-buffer-name-max-length 60 "Increase max length of buffer name.")
  :config
  (ivy-rich-mode 1))

(use-package mb-depth
  :ensure nil
  :config
  (minibuffer-depth-indicate-mode 1))

(use-package avy
  :ensure t
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

(use-package ace-jump-buffer
  :ensure t
  :bind
  (("M-g b" . ace-jump-buffer)))

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

(use-package which-key
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

;;; ORG-MODE

;; part-of-emacs: t
;; info:
;; git:
;; melpa:
(use-package org
  :mode (("\\.org$" . org-mode))
  :defer t
  :ensure org-plus-contrib
  :custom
  (org-babel-load-languages
   '((R . t)
     (restclient . t))))

;; part-of-emacs: t
;; info:
;; git:
(use-package org-bullets
  :custom
  (org-ellipsis "…")
  :hook
  (org-mode . org-bullets-mode))

;; part-of-emacs: nil
;; info: An org-mode extension to restclient.el
;; git: https://github.com/alf/ob-restclient.el
;; melpa:
(use-package ob-restclient
  :after org restclient)

(use-package org-password-manager
  :hook
  (org-mode . org-password-manager-key-bindings))


;;; LANGUAGE-SUPPORT
;;

(use-package lsp-rust
  :ensure t
  :disabled t
  :after lsp-mode
  :init
  (add-hook 'rust-mode-hook #'lsp-rust-enable))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package racer
  :ensure t
  :after rust-mode
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook (lambda () (setq eldoc-documentation-function nil))))

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
;; info: it adds interaction R, S-Plus, SAS, Stata and OpenBUGS/JAGS.
;; homepage: https://ess.r-project.org/
(use-package ess
  :defer t)

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


;;;; PROJECTS
;;

(use-package magit
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

;; part-of-emacs: nil
;; info: Browse target page on github/bitbucket from emacs buffers
;; git: https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . browse-at-remote)
        ("k" . browse-at-remote-kill)))

;; part-of-emacs: t
;; info:
(use-package smerge-mode
  :defer t
  :ensure nil
  :diminish smerge-mode)

(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

;; (use-package smart-comment
;;   :bind ("M-;" . smart-comment))

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

(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :hook
  (after-init . global-company-mode))

;; (use-package company-quickhelp
;;   :defer t
;;   :custom
;;   (company-quickhelp-delay 3)
;;   :config
;;   (company-quickhelp-mode 1))

;; (use-package company-shell
;;   :defer t
;;   :config
;;   (add-to-list 'company-backends 'company-shell))

;; (use-package autoinsert
;;   :ensure nil
;;   :hook
;;   (find-file . auto-insert))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-prompt-functions '(yas-completing-prompt yas-ido-prompt))
  :config
  (yas-reload-all)
  :hook
  (prog-mode  . yas-minor-mode))

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

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

(provide 'init)
;;;
