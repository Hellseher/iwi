;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Modified : <2023-12-22 Fri 03:05:24 GMT>

;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `use-package-hook!'
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys


(setq emacsql-sqlite-executable (executable-find "emacsql-sqlite"))

(cond
 ((string-equal system-type "darwin")
  (setq user-full-name "Oleg Bocharov"
        user-mail-address "oleg.bocharov@mirrorweb.com"))
 ((string-equal system-type "gnu/linux")
  (setq user-full-name "#Rλatan"
        user-nick-name "Sharlatan"
        user-mail-address "abc@incerto.xyz"))
 (t (setq user-full-name "#Rλatan"
          user-mail-address "abc@incerto.xyz")))

(setq doom-font (font-spec :family "Hack" :size 12 :weight 'medium)
      doom-theme 'doom-nord
      display-line-numbers-type nil)

(setq time-stamp-pattern
      "8/[\+ ][Mm]odified[ \t]*:\\\\?[ \t]*<%04Y-%02m-%02d %03a %02H:%02M:%02S %Z>$")
(add-hook 'before-save-hook 'time-stamp)

(blink-cursor-mode nil)
;(set-default 'cursor-type 'hbar)

(use-package! page-break-lines
  :config
  (global-page-break-lines-mode))

;;; :lang Scheme
;;;

(use-package! geiser
  :init
  (setq
   geiser-active-implementations '(guile)
   geiser-default-implementation 'guile
   geiser-guile-binary (executable-find "guile")))

(after! geiser-guile
  (add-to-list 'geiser-guile-load-path "~/code/guix"))

(use-package! rainbow-identifiers
  :custom
  (rainbow-identifiers-cie-l*a*b*-lightness 80)
  (rainbow-identifiers-cie-l*a*b*-saturation 50)
  (rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face)
  :hook
  (emacs-lisp-mode . rainbow-identifiers-mode)
  (lisp-mode . rainbow-identifiers-mode)
  (scheme-mode . rainbow-identifiers-mode))

;;; Org-mode org-roam

(use-package! org
  :init
  (setq org-log-into-drawer "LOGBOOK"
        org-log-done 'time
        org-log-reschedule 'time
        org-log-redeadline 'note
        org-log-note-clock-out t))

;; ;; :source https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2
;; (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
;;   (let* ((count (caar (org-roam-db-query
;;                        [:select (funcall count source)
;;                                 :from links
;;                                 :where (= dest $s1)
;;                                 :and (= type "id")]
;;                        (org-roam-node-id node)))))
;;     (format "[%d]" count)))

(use-package! org-roam
  :after org
  :custom
  (org-roam-db-location "~/zettelkasten/org-roam.db")
  (org-roam-directory "~/zettelkasten")
  (org-roam-dailies-directory "daily/")
  ;; :bind (:map org-roam-mode-map ("[mouse-1]" org-roam-visit-thing))
  :config
  ;; (setq org-roam-node-display-template
  ;;       "${doom-hierarchy:*} ${backlinkscount:6} ${doom-tags:45}")
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              ;;#'org-roam-reflinks-section
              ;; #'org-roam-unlinked-references-section
              ))
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}
#+created: %U
#+modified: <>
#+filetags: :note:

- tags ::
- keywords ::
- source ::

* References")
           :immediate-finish t
           :unnarrowed t)
          ("w" "wiki" plain "%?"
           :if-new (file+head "wiki/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}
#+created: %U
#+modified: <>
#+filetags: :wiki:%^G:

- tags :: ")
           :unnarrowed t)
          ("d" "drafts" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}
#+created: %U
#+modified: <>
#+filetags: :note:draft:

- tags :: ")
           :unnarrowed t)
          ("b" "bibliography" plain "%?"
           :if-new (file+head "bib/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}
#+created: %U
#+modified: <>
#+filetags: :bib:

- tags ::
- author ::
- illistrator ::
- publisher/journal/platform ::
- volume ::
- number ::
- pages ::
- year ::
- DOI ::
- ISSN ::
- ISBN-13 ::
- ISBN-10 ::
- URL ::

* Abstract ")
           :unnarrowed t))))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(add-hook 'org-mode-hook (lambda () (set-fill-column 100)))
(add-hook 'js-mode-hook (lambda () (set-fill-column 100)))
(add-hook 'terraform-mode-hook (lambda () (setq comment-start "//")))

;;; Tramp and Shells
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

;; (use-package! racer
;;   :config
;;   (setq racer-rust-src-path "~/code/rust/library"))

(after! rustic
  (setq rustic-lsp-server 'rls))

(add-hook 'eshell-preoutput-filter-functions
           'ansi-color-filter-apply)


(defun exzellenz/hiorisontal-line ()
  "Insert dashed horisotnal line."
  (interactive)
  (progn
    (insert-char #x002D 78)
    (comment-region (line-beginning-position) (line-end-position))))

(defun exzellenz/timestamp ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))

(defun exzellenz/timestamp-iso ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "%Y%m%dT%H%M%S%z")))

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

(defun exzellenz/user-note ()
  "Insert NOTE with iso timestamp"
  (interactive)
  (progn
    (insert (format "NOTE: (%s-%s): "
                    user-nick-name
                    (format-time-string "%Y%m%dT%H%M%S%z")))
    (comment-region (line-beginning-position) (line-end-position))))

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

(defun exzellenz/smarge-all-lower ()
  "Keep all lower part of conflict by applying `smarge-keep-lower' for
entire buffer."
  (interactive)
  (save-restriction
    (goto-char (point-min))
    (while (re-search-forward smerge-begin-re nil t)
      (smerge-keep-lower)
      (save-buffer))))

;; Allow cyrillic inputs
;; source :: https://github.com/rynffoll/.doom.d/blob/master/config.el
;; (def-package! reverse-im
;;   :config
;;   (reverse-im-activate "russian-computer")
;;   (after! evil
;;     ;; cyrillic tweaks
;;     (define-key evil-normal-state-map (kbd "C-х") #'evil-force-normal-state)
;;     (define-key evil-insert-state-map (kbd "C-х") #'evil-normal-state)
;;     (define-key evil-visual-state-map (kbd "C-х") #'evil-exit-visual-state)))

;; Email

;; https://github.com/kensanata/ggg
(setq user-mail-address "sharlatanus@gmail.com"
      user-full-name    "Sharlatan Hellseher"
      mml-secure-openpgp-signers '("984781DE689C21C26418086776D727BFF62CD2B5")
      mml2015-signers '("984781DE689C21C26418086776D727BFF62CD2B5")
      ;; gnus-select-method
      ;; '(nnimap "gmail"
      ;;     (nnimap-address "imap.gmail.com")
      ;;     (nnimap-server-port 993)
      ;;     (nnimap-stream ssl)
      ;;     (nnir-search-engine imap)
      ;;     (nnimap-authinfo-file "~/.authinfo.gpg"))
      gnus-agent nil
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      gnus-message-archive-group nil
      message-send-mail-function 'smtpmail-send-it
      mml-secure-openpgp-encrypt-to-self t
      nntp-authinfo-file "~/.authinfo.gpg"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "sharlatanus@gmail.com"))
      starttls-extra-arguments nil
      starttls-gnutls-program ".guix-profile/bin/gnutls-cli"
      starttls-use-gnutls t)

(add-hook 'message-setup-hook 'mml-secure-message-encrypt)
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

(defun my-gnus-summary-keys ()
  (local-set-key "y" 'gmail-archive)
  (local-set-key "$" 'gmail-report-spam))

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))
