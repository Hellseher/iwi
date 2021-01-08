;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Modified : <2021-01-06 Wed 21:38:23 GMT>

;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `use-package-hook!'
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys

(cond
 ((string-equal system-type "darwin")
  (setq user-full-name "Oleg Bocharov"
        user-mail-address "oleg.bocharov@mirrorweb.com"))
 ((string-equal system-type "gnu/linux")
  (setq user-full-name "#Rλatan"
        user-mail-address "abc@incerto.xyz"))
 (t (setq user-full-name "#Rλatan"
          user-mail-address "abc@incerto.xyz")))

(setq doom-font (font-spec :family "Hack" :size 14)
      doom-theme 'doom-nord
      display-line-numbers-type nil)

(setq time-stamp-pattern
      "8/[\+ ][Mm]odified[ \t]*:\\\\?[ \t]*<%04Y-%02m-%02d %03a %02H:%02M:%02S %Z>$")
(add-hook 'before-save-hook 'time-stamp)

(blink-cursor-mode nil)
(setq cursor-type 'hbar)

(use-package! page-break-lines
  :config
  (global-page-break-lines-mode))

;;; :lang Scheme

(after! geiser
  (setq
   geiser-active-implementations '(guile)
   geiser-default-implementation 'guile
   geiser-guile-binary (executable-find "guile")))

;;; Org-mode org-roam

(use-package! org
  :config
  (setq org-log-into-drawer "LOGBOOK"
        org-log-done 'time
        org-log-reschedule 'time
        org-log-redeadline 'note
        org-log-note-clock-out t))

(use-package! org-roam
  :custom
  (org-roam-db-location "~/zettelkasten/org-roam.db")
  (org-roam-directory "~/zettelkasten")
  (org-roam-dailies-directory "daily/")
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph)
          ("C-c n d" . org-roam-dailies-capture-today)))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+created: %U
#+modified: <>
#+roam_alias:
#+roam_tags:

- tags :: "
           :unnarrowed t)
          ("b" "book" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "books/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
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
           :olp ("Work"))))
  (setq org-roam-graph-node-extra-config '(("color" . "skyblue")))
  (setq org-roam-graph-edge-extra-config '(("dir" . "back")
                                           ("arrowsize" . "0.5")))
  (setq org-roam-graph-exclude-matcher '("private" "daily"))
  (setq org-roam-graph-edge-cites-extra-config '(("color" . "red"))))

(add-hook 'org-mode-hook (lambda () (set-fill-column 100)))
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'js-mode-hook (lambda () (set-fill-column 100)))


;;; Tramp
;;; https://lists.gnu.org/archive/html/help-guix/2016-10/msg00049.html

(after! tramp
  (setq tramp-own-remote-path
        '("~/.guix-profile/bin"
          "~/.guix-profile/sbin"
          "/run/current-system/profile/bin"
          "/run/current-system/profile/sbin"))
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path)))
  (setq tramp-copy-size-limit nil))



(defun exzellenz/hiorisontal-line ()
  "Insert dashed horisotnal line."
  (interactive)
  (progn
    (insert-char #x002D 78)
    (comment-region (line-beginning-position) (line-end-position))))

(defun exzellenz/timestamp ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "%y%m%d%H%M%S")))

(defun exzellenz/timestamp-iso ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S%z")))

(defun exzellenz/insert-local-time-utc ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "<%H:%M:%S%z>")))

(defun exzellenz/epoch ()
  "Insert timestamp seconds from epoch."
  (interactive)
  (insert (format-time-string "%s")))

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


(defun sort-lines-by-length (reverse beg end)
  "ref: https://stackoverflow.com/questions/30697523
Sort lines in region by their length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))
