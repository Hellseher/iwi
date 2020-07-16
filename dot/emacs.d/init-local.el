;;; init-local.el -- Custom local variables and functios.
;;; Created       : Thu 11 Aug 2016 22:32:01
;;; Modified      : <2017-8-02 Wed 21:47:49 BST> #Rλatan
;;; Author        : #Rλatan <#Rλatanus@gmail.com>
;;; Maintainer(s) : #Rλatan
;;; Commentary:
;;;
;;; Combined with Purcell "A reasonable Emacs config" https://github.com/purcell/emacs.d
;;;
;;; Code:

(defvar exzellenz/required-packages '(org-bullets
                                      ggtags
                                      yasnippet
                                      multi-term
                                      )
  "Required custom packages for exzellenz set up.")
(dolist (pkg exzellenz/required-packages)
  (require-package pkg))


;;; Time stemtp in the header when save the file
;; <Yar-month-day weekday Time Zone> username
;; TODO: how to insert WEEK number?
(setq time-stamp-pattern
      "8/Modified[ \t]*:\\\\?[ \t]*<%04Y-%:m-%02d %03a %02H:%02M:%02S %Z> %u\\\\?$" )

(add-hook 'before-save-hook 'time-stamp)


;;; tramp
;; https://www.gnu.org/software/tramp/
(defvar tramp-default-method)
(after-load 'tramp
  (setq tramp-default-method "ssh"))


;;; multi-term
;; https://www.emacswiki.org/emacs/download/multi-term.el

(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode -1)
            (add-to-list 'term-bind-key-alist '("C-c C-n" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-c C-p" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
            (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
            ))
(global-set-key (kbd "C-c T") 'multi-term)

;;; Custom functios
;;

;; https://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun exzellenz/hl-insert ()
  "Insert dashed horisotnal line."
  (interactive)
  (progn
    (insert-char #x002D 78)
    (comment-region (line-beginning-position) (line-end-position) )))

(defun exzellenz/timestamp ()
  "Insert timestamp YmdHMS."
  (interactive)
  (insert (format-time-string "%y%m%d%H%M%S")))

(defun exzellenz/cix--find-cmd-file ()
  "Find a file name of the command/word under ther cursor."
  (shell-command-to-string
   (concat "find ./ -type f -name \"*org\" -exec grep -lP \"^\\*\\* "
           (thing-at-point 'word)
           "\\s\" {} \\;")))

(defun exzellenz/cix-create-link-to-cmd ()
  "Insert a link to command under cursor."
  (interactive)
  (progn
    (beginning-of-line)
    (insert "[[file:"(exzellenz/cix--find-cmd-file))
    (delete-char -1)
    (kill-word 1)
    (insert "::*")
    (yank)
    (insert "][")
    (yank)
    (insert "]]")))

(defun exzellenz/org-todo-move-to-top ()
  "Move TODO entery to the top ot the file when it is DONE."
  (interactive)
  (save-excursion
    (progn
      (org-cut-special)
      (goto-char (point-min))
      (if (search-forward "* COMPLETED" nil 't)
          (progn
            (beginning-of-line)
            (org-yank))
        (goto-char (point-min))
        (beginning-of-line)
        (forward-line 10)
        (org-yank)))))

(after-load 'org
  (define-key org-mode-map (kbd "C-c C-x t") 'exzellenz/org-todo-move-to-top))


;;; Fixes

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://ergoemacs.org/misc/emacs_bug_cant_paste_2015.html

(provide 'init-local)
;;; init-local.el ends here
