;; config.scm -- Global configuration for Guix
;; Path      : /etc/config.scm
;; Author    : Sharlatan <sharlatanus@gmail.com>
;; Created   : <2019-6-02 Sun 11:08:17 BST>
;; Modified  : <2020-1-16 Thu 23:06:51 GMT> Sharlatan

;; URL: https://github.com/Hellseher/iwi

;;; Cometary
;;
;;; Configuration pre-setup
;;  http://guix.gnu.org/manual/en/html_node/Keyboard-Layout-and-Networking-and-Partitioning.html
;;
;; 3 partitions need to be added just before using this system declartion file.
;; EFI systems. Asume you want to deploy your system on drive /dev/sda:
;;
;; #+BEGIN
;; GUIX>: passwd # reset root password
;; GUIX>: herd start ssh-daemon
;;
;; GUIX>: parted -a opt /dev/sda mkpart primary 0 1024M
;; GUIX>: parted /dev/sda set 1 esp on
;; GUIX>: mkfs.fat -F32 /dev/sda1
;;
;; GUIX>: parted -a opt /dev/sda mkpart primary 1024M 95%
;; GUIX>: mkfs.ext4 -L system /dev/sda2
;;
;; GUIX>: parted -a opt /dev/sda mkpart primary 95% 100%
;; GUIX>: mkswap -L SWAP /dev/sda3
;; GUIX>: swapon /dev/sda3
;;
;; GUIX>: mkdir -p /boot/efi
;; GUIX>: mount /dev/sda1 /boot/efi
;;
;; GUIX>: mkdir -p /mnt
;; GUIX>: mount /dev/sda2 /mnt
;; GUIX>: herd start cow-ctore /mnt
;;
;; GUIX>: mkdir /mnt/etc
;; HOST>: scp config.scm root@guix:/mnt/etc/
;; GUIX>: guix systemd init /mnt/etc/config.scm /mnt
;; #-END
;;
;; When all partition are aligned and mounted copy this systemd declaration file
;; over to the fresh Guix installation. You may start ssh-daemon on Guix system
;; or use USB media drive to trasfere it.
;;
;; After running `guix system init` you'll get:
;; + a system with GNOME desktop manager and optional StumpWM window manager.
;; + 2 available browsers installed globally - IceCat and Next


(use-modules (gnu)
             (gnu packages)
             (gnu system)
             (gnu system nss))

(use-service-modules desktop
                     xorg
                     networking
                     ssh)

(use-package-modules emacs
                     emacs-xyz
                     web-browsers
                     gnuzilla
                     certs
                     lisp)

(operating-system
 (locale "en_GB.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "gb" "extd"))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (swap-devices (list "/dev/sda3"))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device "/dev/sda1")
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device "/dev/sda2")
          (type "ext4"))
         %base-file-systems))

 (host-name "guix-t420")

 (users (cons* (user-account
                (name "hellseher")
                (comment "Too late to think.")
                (group "users")
                (home-directory "/home/hellseher")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 (packages (cons* nss-certs
                  emacs
                  emacs-exwm
                  emacs-guix
                  stumpwm
                  sbcl-next
                  icecat
                  %base-packages))

 (services
  (append
   (list (service gnome-desktop-service-type)
         (service openssh-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services)))

;; config.scm ends here
