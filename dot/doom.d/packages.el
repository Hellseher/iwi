(package! page-break-lines)
(package! restclient)
(package! json-mode)
;; Org Roam
;; https://github.com/org-roam/org-roam-ui#doom
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui
  :recipe
  (:host github
   :repo "org-roam/org-roam-ui"
   :files ("*.el" "out")))
;;
(package! rainbow-identifiers)
(package! modus-themes)
(package! debbugs)
