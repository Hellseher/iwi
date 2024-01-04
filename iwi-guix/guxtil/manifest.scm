;; Modified : <2023-12-27 Wed 23:08:37 GMT>
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
        "make"
        "perl"
        "licensecheck"
        "python-ipython"
        "rust"
        "sbcl"))

(define package-group-emacs
  ;; This vaiable provides a list of Emacs itself and it's related package names
  (list "emacs"
        "emacs-sqlite"))

(define package-group-misc
  (list "alacritty"
        "adwaita-icon-theme"
        "aspell-dict-ru"
        "calibre"
        "cryptsetup"
        "curl"
        "fd"
        "filezilla"
        "glibc-locales"
        "gnome-tweaks"
        "gnupg"
        "gnutls"
        "graphviz"
        "htop"
        "ispell"
        "jq"
        "le-certs"
        "moka-icon-theme"
        "nmap"
        "nss-certs"
        "nyxt"
        "pandoc"
        "pinentry"
        "remmina"
        "rhythmbox"
        "ripgrep"
        "rsync"
        "rtorrent"
        "setxkbmap"
        "sqlite"
        "stackistry"
        "stellarium"
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
