;; config.scm -- Global configuration for Guix
;; Path      : /etc/config.scm
;; Author    : Sharlatan <sharlatanus@gmail.com>
;; Created   : <2019-6-02 Sun 11:08:17 BST>
;; Modified  : <2019-8-01 Thu 23:07:36 BST> Sharlatan

;; URL: https://github.com/Hellseher/iwi

;;; Cometary
;;
;; After running 'guix system reconfigure /etc/config.scm you'll get
;; + systems with GNOME desktop manger and optional StumpWM window manager.
;; + 2 available browser install globally - IceCat and Next


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
 (swap-devices (list "/dev/sda2"))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "E689-0CB3" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "9c5b7718-ac49-4f1f-a3c4-5296d91d4c70"
                 'ext4))
          (type "ext4"))
         %base-file-systems))

 (host-name "lpt-t420")

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
