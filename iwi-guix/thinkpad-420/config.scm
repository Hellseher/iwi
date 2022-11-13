;; File : config.scm
;; Created : <2022-09-24 Sat 16:33:34 BST>
;; Modified : <2022-09-24 Sat 17:16:39 BST>
;;
;; Commentary:
;;
;; This Guix configuration file is deployed for Lenovo ThinkPad T420:
;; - CPU ::
;; - VGA ::
;; - MEM ::
;; - MB ::
;; - SYSTEM drive :: KINGSTON SH103S3120G~
;;
;; It requires noGuix channel:
;;
;; ~# guix describe
;; Generation 19   Sep 20 2022 20:18:49    (current)
;;   guix 6abdcef
;;     repository URL: https://git.savannah.gnu.org/git/guix.git
;;     branch: master
;;     commit: 6abdcef4a68e98f538ab69fde096adc5f5ca4ff4
;;   nonguix 415aeda
;;     repository URL: https://gitlab.com/nonguix/nonguix
;;     branch: master
;;     commit: 415aeda016ca174764a700d8493cc93285ebe3dd

(use-modules (gnu)
             (gnu packages xorg)
             (gnu packages wm)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)

(operating-system
  (locale "en_GB.utf8")
  (timezone "Europe/London")
  (keyboard-layout (keyboard-layout "gb"))
  (host-name "t420")
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (kernel-arguments '("nomodset"))
  (users (cons* (user-account
                 (name "sacha")
                 (comment "Sacha")
                 (group "users")
                 (home-directory "/home/sacha")
                 (supplementary-groups '("netdev" "audio" "video")))
                (user-account
                 (name "oleg")
                 (comment "Oleg")
                 (group "users")
                 (home-directory "/home/oleg")
                 (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages (append (map specification->package
                         '("nss-certs"))
                    %base-packages))
  (services
   (append (list (service gnome-desktop-service-type)
                 (service xfce-desktop-service-type)
                 (service openssh-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (modules (list xf86-video-amdgpu
                                                     xf86-video-vesa
                                                     xf86-input-libinput))
                                      (keyboard-layout keyboard-layout))))
           %desktop-services))
  ;; https://guix.gnu.org/manual/en/html_node/Bootloader-Configuration.html
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sdd"))
               (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                       (target (uuid "26b11234-d944-485a-b633-c3e2252cdce3")))))
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid "024f1aa6-1185-4ad1-a07d-115cdfde7448"))
                         (type "ext4"))
                       %base-file-systems)))

;; config.scm ends here
