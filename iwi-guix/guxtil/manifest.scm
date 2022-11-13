;; Modified : <2022-11-03 Thu 22:35:10 GMT>
;;
;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define package-group-font
  (list "font-gnu-unifont"
        "font-google-noto"
        "font-hack"
        "font-liberation"))

(define package-group-graphic
  (list "gphoto2"
        "gimp"
        "shotwell"
        "blender"
        "darktable"
        "rawtherapee"))

(define package-group-stumpwm
  (list "sbcl-clx-truetype"
        "sbcl-slynk"))

(define package-group-code-craft
  (list "git"
        "git:send-email"
        "go"
        "python"
        "licensecheck"
        "python-ipython"
        "rust"
        "sbcl"))

(define package-group-emacs
  ;; This vaiable provides a list of Emacs itself and it's related package names
  (list "emacs"
        "emacs-emacsql-sqlite3"))

(define package-group-misc
  (list "alacritty"
        "aspell-dict-ru"
        "calibre"
        "cryptsetup"
        "curl"
        "fd"
        "adwaita-icon-theme"
        "gnome-tweaks"
        "gnupg"
        "graphviz"
        "htop"
        "ispell"
        "jq"
        "moka-icon-theme"
        "nmap"
        "pandoc"
        "remmina"
        "rhythmbox"
        "ripgrep"
        "rsync"
        "rtorrent"
        "setxkbmap"
        "sqlite"
        "stackistry"
        "telegram-desktop"
        "tmux"
        "tree"
        "ungoogled-chromium"
        "vim"
        "vlc"
        "xdg-utils"))

(specifications->manifest
 (append
  package-group-code-craft
  package-group-emacs
  package-group-font
  package-group-graphic
  package-group-stumpwm
  package-group-misc))
