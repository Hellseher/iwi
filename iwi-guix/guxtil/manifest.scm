;; Modified : <2021-11-22 Mon 20:38:52 GMT>
;;
;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define package-group-font
  (list
   "font-gnu-unifont"
   "font-google-noto"
   "font-hack"
   "font-liberation"))

(define package-group-graphic
  (list
   "gphoto2"
   "gimp"
   "blender"
   "darktable"
   "rawtherapee"))

(define package-group-stumpwm
  (list
   "sbcl-clx-truetype"
   "sbcl-slynk"))

(define package-group-code-craft
  (list
   "git"
   "go"
   "python-ipython"
   "rust-cargo"
   "rust-analyzer"
   "rust"
   "sbcl"))

(define package-group-emacs
  (list
   "emacs"
   "emacs-emacsql-sqlite3"))

(define package-group-misc
  (list
   "alacritty"
   "curl"
   "pandoc"
   "fd"
   "git"
   "gnome-icon-theme"
   "gnome-tweaks"
   "gnupg"
   "graphviz"
   "htop"
   "ispell"
   "aspell-dict-ru"
   "jq"
   "moka-icon-theme"
   "nmap"
   "remmina"
   "setxkbmap"
   "rhythmbox"
   "ripgrep"
   "rsync"
   "rtorrent"
   "sqlite"
   "stackistry"
   "ungoogled-chromium"
   "telegram-desktop"
   "tmux"
   "tree"
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
