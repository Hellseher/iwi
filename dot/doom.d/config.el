;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Modified : <2020-07-22 Wed 13:52:43 BST>

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(cond
 ((string-equal system-type "darwin")
  (setq user-full-name "Oleg Bocharov"
        user-mail-address "oleg.bocharov@mirrorweb.com"))
 ((string-equal system-type "gnu/Linux")
  (setq user-full-name "#Rλatan"
        user-mail-address "abc@incerto.xyz")))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-nord)

;; If you intend to use org, it is recommended you change this!
(cond
 ((string-equal system-type "darwin")
  (setq org-directory "~/MIRRORWEB/wkfl/projects/"))
 ((string-equal system-type "gnu/Linux")
  (setq org-directory "~/org/")))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq time-stamp-pattern "8/Modified[ \t]*:\\\\?[ \t]*<%04Y-%02m-%02d %03a %02H:%02M:%02S %Z>$")
(setq blink-cursor-mode t)

;; part-of-emacs: nil
;; sysnopsis: Display ^L page breaks as tidy horizontal lines
;; URL: https://github.com/purcell/page-break-lines
(use-package! page-break-lines
  :config
  (global-page-break-lines-mode))

(after! geiser
  (setq geiser-active-implementations '(guile))
  (setq geiser-default-implementation 'guile)
  (setq geiser-guile-binary (executable-find "guile")))

(after! org
  (setq org-log-done 'time)
  (setq org-log-into-drawer t))
