;;; package ---  init.el Emacs configuration
;;; Created    : <Tue 10 Mar 2015 11:39:46>
;;; Modified   : <2020-12-15 Tue 20:26:06 GMT> Sharlatan
;;; Author     : sharlatan <sharlatanus@gmail.com>

;;; Commentary:
;;
;; This build is wrapped around `use-package' macro which helps to
;; control the consistence infrastructure of the Emacs system.
;;
;; If you add some changes reload init.el with `M-x eval-buffer' or
;; `M-x load-file ~/.emacs.d/init.el'
;;
;; Some of the Emacs packages depends on system ones.
;;
;;  jedi      | elpy
;;  flake8    | elpy
;;  autopep80 | elpy
;;  yapf1     | elpy
;;  black2    | elpy
;;
;; Main functionality:
;; + *-mode              -- Syntax highlight and indention
;; + `company'           -- Code auto completion
;; + `company-quickhelp' -- Inline documentation
;; + `flyspell'          -- Sintax check
;; + `flycheck'          -- Error checking
;; + `M-.'               -- Jump to definition
;;
;;; Debugging:
;;
;; 1. Run Emacs in clean mode from your shell, without any
;; configurations.
;;
;;    emacs --init-debug -q
;;
;; 2. Then evaluate each S-exp one by pressing `C-M-x' (eval-defun),
;;    to navigate by S-exp use default keybindings `C-M-n'
;;    (forward-list) and `C-M-p'(backward-list) Previous.
;;
;;; References:
;;
;; - https://shrysr.github.io/docs/sr-config/
;; - http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html
;; - https://github.com/syl20bnr/spacemacs
;; - https://github.com/hlissner/doom-emacs

;;; Code:

;;; CORE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initial configuration to make `use-package' main configuration
;; macro. Load dependencies libraries.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;;:FIXME: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(setq package-enable-at-startup nil
      ;file-name-handler-alist t
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;(put 'use-package 'lisp-indent-function 1)

(setq use-package-verbose t
      use-package-minimum-reported-time 0.01
      use-package-compute-statistics t)

;;;; core-libs
(use-package f     :ensure t) ; API for working with files and directories
(use-package s     :ensure t) ; String manipulation library.
(use-package dash  :ensure t) ; List library. NO `cl' required.

;; part-of-emacs: t
;; synopsis: C primitive functions.
(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :custom
  (scroll-step 1)
  (source-directory (concat user-emacs-directory "src"))
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (tab-width 4)
  (debug-on-quit nil)
  (make-pointer-invisible t)
  :config
  (cond
   ((string-equal system-type "darwin")
    (setq user-full-name "Oleg Bocharov"
          user-mail-address "oleg.bocharov@mirrorweb.com"))
   ((string-equal system-type "gnu/linux")
    (setq user-full-name "#Rλatan"
          user-mail-address "abc@incerto.xyz"))
   (t (setq user-full-name "#Rλatan"
            user-mail-address "abc@incerto.xyz"))))

;;; USE-PACKAGE-EXTENSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set of required additional keywords and functions to extend
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
;; synopsis: A simple way to manage personal key-bindings.
;; purpose: to enable `:bind' keyword in `use-package'
(use-package bind-key :ensure t)

;; part-of-emacs: nil
;; synopsis: Emacs Lisp packages built directly from source.
;; URL: https://framagit.org/steckerhalter/quelpa
;; purpose: to enable `:quelpa' keyword in `use-package'
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

;; part-of-emacs: nil
;; synopsis: Quelpa handler for use-package.
;; URL: https://framagit.org/steckerhalter/quelpa-use-packag
(use-package quelpa-use-package :ensure t)

;;; GLOBAL-CONFIRUATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode agnostic configuration, related to majority of functionality.
;; + paradox
;; + files
;; + smartparens
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: nil
;; synopsis: A packages menu, coloured, with package ratings, and customizable.
;; URL: https://github.com/Malabarba/paradox
(use-package paradox
  :ensure t
  :config
  (paradox-enable)
  :custom
  (paradox-github-token nil))

;; part-of-emacs: nil
;; synopsis: Miror mode for Emacs that deals with parens pairs.
;; URL: https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode 1)
  (show-smartparens-mode 1)
  :config
  (use-package smartparens-config)
  :bind
  ;;; Naviagation
  (("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ;;; Manipulation
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-<delete>" . sp-unwrap-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)))

;; part-of-emacs: t
;; synopsis: File input and output commands for Emacs.
(use-package files
  :ensure nil
  :hook
  ((before-save . whitespace-cleanup)
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
;; synopsis: Basic editing commands for Emacs.
(use-package simple
  :ensure nil
  :custom
  (kill-ring-max 300)
  (kill-whole-line t "if NIL, kill whole line and move the next line up")
  :diminish
  ((visual-line-mode . " ↩")
   (auto-fill-function . " ↵"))
  :config
  (column-number-mode t)
  (toggle-truncate-lines 1))

;; part-of-emacs: t
;; synopsis: Dynamically update time stamp of the file.
(use-package time-stamp
  :defer t
  :custom
  (time-stamp-pattern
   "8/Modified[ \t]*:\\\\?[ \t]*<%04Y-%02m-%02d %03a %02H:%02M:%02S %Z\\\\?[\">]"))

;; part-of-emacs: t
;; synopsis: Revert buffers when files on disk change.
(use-package autorevert
  :defer t
  :diminish auto-revert-mode
  :custom
  (auto-revert-interval 1)
  :config
  (global-autorevert-mode))

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
  (ivy-add-actions 'counsel-find-file '(("l" vlf "view large file"))))

;; part-of-eamcs: t
;; synopsis: EasyPG assistant.
(use-package epa
  :defer t
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
;; synopsis: Save mini buffer history.
(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode))

;; part-of-emacs: t
;; synopsis: Automatically save place of cursor in files.
(use-package saveplace
  :ensure t
  :unless noninteractive
  :config
  (save-place-mode 1))

;; part-of-emacs: t
;; synopsis: Callendar functions.
(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-week-start-day 1))

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
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-remote-path (append tramp-remote-path
                                  '("~/.guix-profile/bin"
                                    "~/.guix-profile/sbin"
                                    "/run/current-system/profile/bin"
                                    "/run/current-system/profile/sbin"))))

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
  :ensure nil)

;; part-of-emacs: t
;; synopsis: Smart display of output.
(use-package em-smart
  :ensure nil
  :after eshell
  :config
  (eshell-smart-initialize)
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))

;; part-of-emacs: nil
;; synopsis: Add some help functions and support for Eshell.
;; URL: https://github.com/tom-tan/esh-help
(use-package esh-help
  :ensure t
  :config
  (setup-esh-help-eldoc))

;; part-of-emacs: nil
;; synopsis: Fish-like history auto suggestions in eshell
;; URL: https://github.com/dieggsy/esh-autosuggest
(use-package esh-autosuggest
  :ensure t
  :hook
  (eshell-mode . esh-autosuggest-mode))

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
;; synopsis: Directory-browsing commands.
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t "guess a target directory"))

;; part-of-emacs: nil
;; synopsis: Show dired as sidebar.
;; URL: https://github.com/fasheng/dired-toggle
(use-package dired-toggle
  :ensure t
  :defer t)

;; part-of-emacs: nil
;; synopsis: Hide dotfiles in dired.
;; URL: https://github.com/mattiasb/dired-hide-dotfiles
(use-package dired-hide-dotfiles
  :ensure t
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

;; part-of-emacs: nil
;; synopsis: Extra font lock rules for a more colourful dired.
;; URL: https://github.com/purcell/diredfl
(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

;; part-of-emacs: t
;; synopsis: Operate on buffers like `dired'.
(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

;; part-of-emacs: nil
;; synopsis: Asynchronous processing in Emacs.
;; URL: https://github.com/jwiegley/emacs-async
(use-package async
  :ensure t
  :init
  (dired-async-mode t))

;; part-of-emacs: nil
;; synopsis: Allow rsync from dired buffers.
;; URL: https://github.com/stsquad/dired-rsync
(use-package dired-rsync
  :ensure t
  :ensure-system-package rsync
  :bind
  (:map dired-mode-map ("r" . dired-rsync)))

;; part-of-emacs: nil
;; synopsis: Use dired as a launcher.
;; URL: https://github.com/thomp/dired-launch
(use-package dired-launch
  :ensure t)

;;; NATURAL-LANGUAGES-SUPPORT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Human languages supports for keyboard leyaout and regions
;; adoptation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis: Basic commands for multilingual environment.
(use-package mule
  :ensure nil
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment "UTF-8"))

;; part-of-emacs: t
;; synopsis: Interface to spell checkers.
(use-package ispell
  :ensure nil
  :defer t
  :custom
  (ispell-local-dictionary-alist
   '(("russian"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[-']"  nil ("-d" "en_GB") nil utf-8)))
  (ispell-program-name "hunspell")
;  (ispell-dictionary "russian")
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-silently-savep t))

;; part-of-emacs: t
;; synopsis: On-the-fly spell checker.
(use-package flyspell
  :ensure nil
  :defer t
  :custom
  (flyspell-delay 1)
  :hook
  (prog-mode-hook . flyspell-prog-mode))

;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything wich concerns visual repsentation of Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; additional-themes
(use-package zenburn-theme :ensure t :defer t)
(use-package doom-themes   :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)

;; current theme
(use-package custom
  :ensure nil
  :config
  (load-theme 'doom-nord t))

;; part-of-emacs: nil
;; synopsis: A minimal and modern modeline.
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
;; synopsis: Control how Emacs displays text in buffers.
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
                      :height 100
                      :box nil))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-column 80))

;; part-of-emacs: t
;; synopsis: Setting up the tool bar.
(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

;; part-of-emacs: t
;; synopsis: Window system-independent scroll bar support.
(use-package scroll-bar
  :defer t
  :ensure nil
  :config
  (scroll-bar-mode -1))

;; part-of-emacs: t
;; synopsis: define a default menu bar.
(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1)
  :bind
  ([S-f10] . menu-bar-mode))

;; part-of-emacs: t
;; synopsis: Shows frames that display text information at the mouse position.
(use-package tooltip
  :defer t
  :ensure nil
  :custom
  (tooltip-mode -1))

;; part-of-emacs: t
;; synopsis: Display time, load and mail indicator in mode line of Emacs.
(use-package time
  :defer t
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))

;; part-of-emacs: nil
;; synopsis: A library for inserting Developer icons.
;; URL: https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :ensure t
  :config
  ;(all-the-icons-install-fonts)
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0))))

;; part-of-emacs: nil
;; synopsis: Shows icons for each file in dired mode.
;; URL: https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; part-of-emacs: nil
;; synopsis: Shows icons while using ivy and counsel.
;; URL: https://github.com/asok/all-the-icons-ivy
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
  (setq  dahsboard-startup-banner 1)
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

;; part-of-emacs: t
;; synopsis: Highlight matching paren.
(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

;; part-of-emacs: t
;; synopsis: Highlight current line.
(use-package hl-line
  :ensure nil
  :hook
  (prog-mode . hl-line-mode))

;; part-of-emacs: nil
;; synopsis: Highlight numbers in source code.
;; URL: https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

;; part-of-emacs: nil
;; sysnopsis: Display ^L page breaks as tidy horizontal lines
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
;; synopsis: Highlight identifiers according to their names.
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
  (lisp-mode . rainbow-identifiers-mode)
  (scheme-mode . rainbow-identifiers-mode))

;; part-of-emacs: nil
;; synopsis: Colorize color names in buffers.
;; URL:
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook
  ((org-mode . rainbow-mode)
   (emacs-lisp-mode . rainbow-mode)))


;;; NAVIGATION-COMPLETION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Native navigation inside active buffer, jump to line, jump to
;; symbol, jump to word, jump to error. Move around menu.
;;
;; General completion is built round `ivy' and `comapny' frameworks.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis: Modular text completion framework.
;; URL: http://company-mode.github.io/
(use-package company
  :ensure nil
  :diminish company-mode
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.6)
  (company-minimum-prefix-length 1))

;; part-of-emacs: nil
;; synopsis: Popup documentation for completion candidates.
;; URL: https://www.github.com/expez/company-quickhelp
(use-package company-quickhelp
  :ensure t
  :hook
  (global-company-mode . company-quickhelp-mode))

;; part-of-emacs: t
;; synopsis: Incremental Vertical completYon.
;; URL: https://github.com/abo-abo/swiper
(use-package ivy
  :ensure nil
  :diminish ivy-mode
  :custom
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :config
  (ivy-mode t))

;; part-of-emacs: nil
;; synopsis: Preview yasnippets with `ivy'.
;; URL: https://github.com/mkcms/ivy-yasnippet
(use-package ivy-yasnippet
  :ensure t
  :defer 10
  :bind
  (("C-c y y" . ivy-yasnippet)))

;; part-of-emacs: nil
;; synopsis: Ivy interface for xref results.
;; URL: https://github.com/alexmurray/ivy-xref
(use-package ivy-xref
  :ensure t
  :defer t
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; part-of-emacs: nil
;; synopsis: More friendly display transformer for ivy.
;; URL: https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :ensure t
  :defer 10
  :after (ivy counsel)
  :custom
  (ivy-rich-switch-buffer-name-max-length 60 "Increase max length of buffer name.")
  :config
  (ivy-rich-mode 1))

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
   ("t" . counsel-org-tag)
   ("v" . counsel-set-variable)
   ("w" . counsel-wmctrl)
   :map help-map
   ("F" . counsel-describe-face))
  :init
  (counsel-mode)
  :config
  (with-eval-after-load 'helpful
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)))

;; part-of-emacs: t
;; synopsis: Setup a menu of recently opened files.
(use-package recentf
  :ensure nil
  :custom
  (recentf-auto-cleanup 'never)
  :config
  (run-with-idle-timer 30 t 'recentf-save-list))

;; part-of-emacs: t
;; synopsis: Indicate minibuffer-depth in prompt
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
  (:map mode-specific-map
        :prefix-map avy-prefix-map
        :prefix "j"
        ("j" .   avy-goto-char-timer)
        ("l" . avy-goto-line)
        ("w" . avy-goto-word-1)))

;; part-of-emacs: nil
;; synopsis: Zap to char using
;; URL: https://github.com/cute-jumper/avy-zap
(use-package avy-zap
  :ensure t
  :bind
  ([remap zap-to-char] . avy-zap-to-char))

;; part-of-emacs: t
;; synopsis: Quickly switch windows.
;; URL: https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame.")
  :bind
  (("M-o" . ace-window)))

;; part-of-emacs: nil
;; synopsis: Use avy to open, copy, etc. visible links.
;; URL: https://github.com/noctuid/link-hint.el
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

;; part-of-emacs: nil
;; synopsis: Quickly follow links.
;; URL: https://github.com/abo-abo/ace-link
(use-package ace-link
  :ensure t
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("l" . counsel-ace-link))
  :config
  (ace-link-setup-default))

;; part-of-emacs: t
;; synopsis: Lisp portion of standard selection support.
(use-package select
  :ensure nil
  :custom
  (selection-coding-system 'utf-8)
  (select-enable-clipboard t "Use the clipboard"))

;; part-of-emacs: nil
;; synopsis: Jump to definition for multiple languages.
;; URL: https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'grep))

;; part-of-emacs: nil
;; synopsis: Increase selected region by semantic units
;; URL: https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; part-of-emacs: nil
;; synopsis: Edit regions in separate buffers.
;; URL: https://github.com/Fanael/edit-indirect
(use-package edit-indirect
  :ensure t
  :bind
  (:map mode-specific-map
        ("r" . edit-indirect-region)))

;; part-of-emacs: nil
;; synopsis: Watch system clipboard, add changes to kill ring/autoinsert.
;; URL: https://github.com/bburns/clipmon
(use-package clipmon
  :ensure t
  :config
  (clipmon-mode))

;; part-of-emacs: nil
;; synopsis: Track command frequencies.
;; URL: https://github.com/dacap/keyfreq
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; part-of-emacs: nil
;; synopsis: Display available keybindings in popup.
;; URL: https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :defer 10
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; part-of-emacs: nil
;; synopsis: Show free keybindings for modkeys or prefixes.
;; URL: https://github.com/Fuco1/free-keys
(use-package free-keys
  :ensure t
  :commands free-keys)

;;; DOCUMENTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Document editing/vewing, formatting and organazings configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-of-emacs: t
;; synopsis: Outline-based notes management and organizer.
;; URL: https://orgmode.org
(use-package org
  ;:ensure org-plus-contrib
  :defer t
  :mode (("\\.org$" . org-mode))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure . t)
                                 (C . t)
                                 (R . t)
                                 (perl . t)
                                 (python . t)
                                 (shell . t)
                                 (restclient . t))))

;; part-of-emacs: nil
;; synopsis: Org exporter for pandoc.
;; URL: https://github.com/kawabata/ox-pandoc
(use-package ox-pandoc
  :ensure t
  :defer 5
  :after org)

;; part-of-emacs: nil
;; synopsis: Show bullets in org-mode as UTF-8 characters.
;; URL: https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :ensure t
  :custom
  (org-ellipsis "…")
  :hook
  (org-mode . org-bullets-mode))

;; part-of-emacs: nil
;; synopsis: Create an aggregated Org table from another one.
;; URL: https://github.com/tbanel/orgaggregate
(use-package orgtbl-aggregate
  :ensure t
  :after org-mode)

;; part-of-emacs: nil
;; synopsis: An org-mode extension to `restclient'.
;; URL: https://github.com/alf/ob-restclient.el
(use-package ob-restclient
  :ensure nil
  :after org-mode )

;; part-of-emacs: nil
;; synopsis: Support library for PDF documents.
;; URL: https://github.com/politza/pdf-tools
(use-package pdf-tools
  :pin manual
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :defer t
  :config
  (setq-default pdf-view-display-size 'fit-page))

;; part-of-emacs: nil
;; synopsis: Better *help* buffer interface.
;; URL: https://github.com/Wilfred/helpful
(use-package helpful
  :ensure t)

;; part-of-emacs: t
;; synopsis: Browse UNIX manual pages.
(use-package man
  :ensure nil
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t)))))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package org-roam
  :ensure t
  :custom
  (org-roam-db-location "~/roam/org-roam.db")
  (org-roam-directory "~/roam/")
  (org-roam-dailies-directory "daily/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph)
               ("C-c n d" . org-roam-dailies-capture-today)))
  :config
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n\n")
          ("w" "work" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n\n"
           :olp ("Work")))))


;;; LANGUAGE-SUPPORT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Most of the language support should include at least -
;; autocompletion, syntax higlighting, code navigation and
;; documentation search
;;
;; Supported programming and markup languages:
;; + Rust
;; + C/C++
;; + Lisp: emacs-lis, scheme, common-lisp, clojure
;; + Perl
;; + Python
;; + Shell/Bash
;; + Data-siralization: YAML, JSON, CSV, TOML
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; part-of-emacs: t
;; ;; synopsis: Semantic buffer evaluator.
;; (use-package semantic
;;   :ensure nil
;;   :custom
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode 1)
;;   :config
;;   (semantic-mode 1))

;; ;; part-of-emacs: t
;; ;; synopsis: Company-mode completion backend using Semantic.
;; ;; URL:
;; (use-package company-semantic
;;   :ensure nil
;;   :defer t)


;;;; Rust: https://www.rust-lang.org/
;; Rust is a multi-paradigm system programming language focused on
;; safety, especially safe concurrency. Rust is syntactically similar
;; to C++, but is designed to provide better memory safety while
;; maintaining high performance.
;;
;; Requirements:
;;
;; $ rustup toolchain add nightly
;; $ rustup component add rust-src
;; $ cargo +nightly install racer
;; $ rustup component add rustfmt-preview
;; $ cargo install cargo-check cargo-edit

;; part-of-emacs: nil
;; synopsis: A major emacs mode for editing Rust source code.
;; URL: https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  :mode
  ("\\.[Rr][Ss]\\'" . rust-mode)
  :custom
  (rust-format-on-save t))

;; part-of-emacs: nil
;; synopsis: Code completion, goto-definition and docs browsing for Rust.
;; URL: https://github.com/racer-rust/emacs-racer
(use-package racer
  :ensure t
  :after rust-mode
  :diminish (racer-mode . "\u24e1")
  :hook
  ((rust-mode . racer-mode)
   (racer-mode . eldoc-mode)))

;; part-of-emacs: nil
;; synopsis: Emacs Minor Mode for Cargo, Rust's Package Manager.
;; URL: https://github.com/kwrooijen/cargo.el
(use-package cargo
  :ensure nil
  :hook
  (rust-mode . cargo-minor-mode))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package flycheck-rust
  :ensure t)

;;;; C/C++
;; C is a general-purpose, imperative computer programming language
;; supporting structured programming, lexical variable scope, and
;; recursion, while a static type system prevents unintended
;; operations. © Wikipedia
;;
;; C++ is a general-purpose programming language created by Bjarne
;; Stroustrup as an extension of the C programming language, or "C
;; with Classes" © Wikipedia

;; part-of-emacs: t
;; synopsis:
(use-package cc-mode
  :ensure nil
  :custom
  (c-basic-offset 4))

;; part-of-emacs: nil
;; synopsis: Company mode backend for C/C++ header files.
;; URL: https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  :hook
  (c-mode-common-hook . hs-minor-mode))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package ggtags
  :ensure t
  :defer t)

;;;; Lisp
;; elisp - Emacs Lisp is a dialect of the Lisp programming language
;; used as a scripting language by Emacs. It is used for implementing
;; most of the editing functionality built into Emacs, the remainder
;; being written in C, as is the Lisp interpreter. © Wikipedia
;;
;; common-lisp - is a dialect of the Lisp programming language,
;; published in ANSI standard document ANSI INCITS 226-1994. The
;; Common Lisp HyperSpec, a hyperlinked HTML version, has been derived
;; from the ANSI Common Lisp standard. The Common Lisp language was
;; developed as a standardized and improved successor of Maclisp. ©
;; Wikipedia
;;
;; - https://lispcookbook.github.io
;;
;; scheme - is a programming language that supports multiple
;; paradigms, including functional and imperative programming. It is
;; one of the three main dialects of Lisp, alongside Common Lisp and
;; Clojure.

;; part-of-emacs: t
;; synopsis: Lisp editing commands for Emacs
(use-package lisp
  :ensure nil
  :defer t
  :mode
  ("sbclrc" . lisp-mode)
  :hook
  (after-save . check-parens))

;; part-of-emacs: nil
;; synopsis: Syntax highlighting of known Elisp symbols.
;; URL: https://github.com/Fanael/highlight-defined
(use-package highlight-defined
  :ensure t
  :custom
  (highlight-defined-face-use-itself t))

;; part-of-emacs: nil
;; synopsis: Hide package namespace in your emacs-lisp code.
;; URL: https://github.com/Malabarba/nameless
(use-package nameless
  :ensure t
  :hook
  (emacs-lisp-mode . nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))

;; part-of-emacs: nil
;; synopsis: Interactive Emacs Lisp pretty-printing.
;; URL: https://framagit.org/steckerhalter/ipretty

(use-package ipretty
  :ensure t
  :config
  (ipretty-mode 1))

;; part-of-emacs: nil
;; synopsis: Suggest elisp functions that give the output requested.
;; URL: https://github.com/Wilfred/suggest.el
(use-package suggest
  :ensure t
  :defer t)

;; part-of-emacs: nil
;; synopsis: GNU Emacs and Scheme talk to each other.
;; URL: https://www.nongnu.org/geiser/
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(guile)))

;; part-of-emacs: nil
;; synopsis: It is the Emacs interface for GNU Guix package manager.
;; URl: https://emacs-guix.gitlab.io/website/index.html
(use-package guix
  :ensure t)

;; part-of-emacs: nil
;; synopsis: Superior Lisp Interaction Mode for Emacs.
;; URL: https://github.com/slime/slime
(use-package slime
  :ensure t
  :defer t
  :init
  (setq slime-contribs '(slime-fancy
                         slime-quicklisp
                         slime-asdf
                         slime-sbcl-exts
                         slime-scratch))
  :config
  (slime-setup)
  (setq inferior-lisp-program (executable-find "sbcl")))

;; part-of-emacs: nil
;; synopsis: Slime commpletion backend for company mode.
;; URL: https://github.com/anwyn/slime-company
(use-package slime-company
  :ensure t
  :after (slime))


;;;; R
;; R - is a programming language and free software environment for
;; statistical computing and graphics supported by the R Foundation
;; for Statistical Computing. The R language is widely used among
;; statisticians and data miners for developing statistical software
;; and data analysis. © Wikipedia

;; part-of-emacs: nil
;; synopsis: It adds interaction R, S-Plus, SAS, Stata and OpenBUGS/JAGS.
;; URL: https://ess.r-project.org/
(use-package ess
  :ensure t)

;;;; Perl 5
;; Perl - is a family of two high-level, general-purpose, interpreted,
;; dynamic programming languages. "Perl" usually refers to Perl 5. ©
;; Wikipedia

;; part-of-emacs: t
;; synopsis: Perl code editing commands for Emacs.
(use-package cperl-mode
  :mode
  ("\\.pl\\'" . cperl-mode))

;;;; Python
;;

;; part-of-emacs: t
;; synopsis: Python's flying circus support for Emacs.
(use-package python
  :ensure t
  :defer t
  :mode
  ("\\.py\\'" . python-mode))

;; part-of-emacs: nil
;; synopsis: Emacs Python Development Environment.
;; URL: https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :after python
  :config
  (elpy-enable))

;;;; Shell

;; part-of-emacs: t
;; synopsis: shell-script editing commands for Emacs.
(use-package sh-script
  :ensure nil
  :hook
  (sh-mode . flycheck-mode))

;; part-of-emacs: nil
;; synopsis: Company mode backend for shell functions.
;; URL: https://github.com/Alexander-Miller/company-shell
(use-package company-shell
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-shell))

;;;; Data-serialization formats

;; part-of-emacs: t
;; synopsis:
;; URL:
(use-package csv-mode
  :ensure nil
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package toml-mode
  :ensure t
  :mode
  (("\\.[Tt][Oo][Mm][Ll]\\'" . toml-mode)
   ("\\.[Tt][Mm][Ll]\\'" . toml-mode)))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode)))


;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package ssh-config-mode
  :ensure t
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  :mode
  (("/\\.ssh/config\\'"     . ssh-config-mode)
   ("/sshd?_config\\'"      . ssh-config-mode)
   ("/known_hosts\\'"       . ssh-known-hosts-mode)
   ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :hook
  (ssh-config-mode . turn-on-font-lock))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package plantuml-mode
  :ensure t
  :mode
  (("\\.plantuml\\'" . plantuml-mode)))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package lua-mode
  :ensure t
  :defer t)

;;;; Markup

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'"          . markdown-mode)
   ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-command "markdown"))

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
  :ensure t
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
;; synopsis: Ivy integration for Projectile.
;; URL: https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :config
  (counsel-projectile-mode))

;; part-of-emacs: nil
;; synopsis: A git porcelain inside Emacs.
;; URL: https://github.com/magit/magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package gitconfig-mode
  :ensure t
  :defer t)

;; part-of-emacs: nil
;; synopsis:
;; URL:
(use-package gitignore-mode
  :ensure t
  :defer t)

;; part-of-emacs: nil
;; synopsis: Browse target page on github/bitbucket from Emacs buffers.
;; URL: https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :ensure t
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . browse-at-remote)
        ("k" . browse-at-remote-kill)))

;; part-of-emacs: t
;; synopsis: Minor mode to resolve diff3 conflicts.
(use-package smerge-mode
  :ensure nil
  :defer t
  :diminish smerge-mode)

;; part-of-emacs: nil
;; synopsis: Emacs package for highlighting uncommitted changes.
;; URL: https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

;; part-of-emacs: nil
;; synopsis: Smarter commenting for Emacs.
;; URL: https://github.com/paldepind/smart-comment
(use-package smart-comment
  :ensure t
  :defer t
  :bind ("M-;" . smart-comment))

;; part-of-emacs: nil
;; synopsis: Pop-up documentation for completion candidates.
;; URL: https://www.github.com/expez/company-quickhelp
(use-package company-quickhelp
  :ensure t
  :defer t
  :custom
  (company-quickhelp-delay 3)
  :config
  (company-quickhelp-mode 1))

;; part-of-emacs: nil
;; synopsis: Yet another snippet extension for Emacs.
;; URL: https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas-global-mode)
         ("C-c y m" . yas-minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :config
  (yas-reload-all)

  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-global-mode 1))

;; part-of-emacs: nil
;; synopsis: Yasnippet official snippet collections.
;; URL: https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)

;; part-of-emacs: nil
;; synopsis: On-the-fly syntax checking.
;; URL: http://www.flycheck.org
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

;; part-of-emacs: nil
;; synopsis: Jump to and fix syntax errors using `flycheck' with `avy'.
;; URL: https://github.com/magicdirac/avy-flycheck
(use-package avy-flycheck
  :ensure t
  :defer t
  :config
  (avy-flycheck-setup))

;; part-of-emacs: nil
;; synopsis: An interactive HTTP client for Emacs.
;; URL: https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t
  :mode
  ("\\.http\\'" . restclient-mode))

;; part-of-emacs: nil
;; synopsis: Run tests with restclient.el
;; URL: https://github.com/simenheg/restclient-test.ely
(use-package restclient-test
  :ensure t
  :hook
  (restclient-mode-hook . restclient-test-mode))

;; part-of-emacs: nil
;; synopsis: company-mode completion back-end for restclient-mode.
;; URL: https://github.com/iquiw/company-restclient
(use-package company-restclient
  :ensure t
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

;; part-of-emacs: nil
;; synopsis: RSS feed reader
;; URL:
(use-package elfeed-org
  :ensure t)

;;; CUSTOM-FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Additional functions which do not belong to any of the packages and
;; could be added on the fly but `eval-defun' to the main functionality.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exzellenz/horisontal-line ()
  "Insert dashed horizontal line till 80th column.
It's commented out by current default comment symbol."
  (interactive)
  (progn
    (insert-char #x002D 78)
    (comment-region (line-beginning-position) (line-end-position))))

(defun exzellenz/timestamp ()
  "Insert time stamp YmdHMS."
  (interactive)
  (insert (format-time-string "%y%m%d%H%M%S")))

(defun exzellenz/insert-timed-buffer-sha256 ()
  "Insert sha256sum of the current file into the current buffer
    prefixed with org-time-stamp."
  (interactive)
  (progn
    (beginning-of-line)
    (insert "- ")
    (org-time-stamp '(16) t)
    (insert (concat ":" (secure-hash 'sha256 (buffer-string)) ":"))))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'.
https://www.emacswiki.org/emacs/SortWords"
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\_<.*?\\_>" "\\&" beg end))

(provide 'init.el)

;;; init.el ends here
