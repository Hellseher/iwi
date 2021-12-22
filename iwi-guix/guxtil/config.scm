;; Modified : <2021-12-22 Wed 21:02:03 GMT>
;;
;; Commentary:
;;
;; This Guix configuration file is deployed and tested on hardware:
;; - CPU :: AMD Ryzen 7 3800X 8-Core Processor
;; - VGA :: [AMD/ATI] Baffin [Radeon Pro WX 4100]
;; - MEM :: CMK64GX4M2D3600C18
;; - MB :: ASRock X570 Steel Legend
;; - SYSTEM drive :: Samsung SSD 980 PRO 500GB
;;
;; It requires noGuix channel:
;;
;; ~# guix describe
;; Generation 15   Dec 16 2021 10:16:21    (current)
;;   guix 08218fd
;;     repository URL: https://git.savannah.gnu.org/git/guix.git
;;     branch: master
;;     commit: 08218fd7a4e941c83d069d7f4fd89f0a9dee60e2
;;   nonguix 21d41c8
;;     repository URL: https://gitlab.com/nonguix/nonguix
;;     branch: master
;;     commit: 21d41c8dd4ad3099c5440a2413d0045a3de8f21a

(use-modules (gnu)
             (gnu packages xorg)
             (gnu packages wm)
             (gnu packages databases)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg nfs databases)

(operating-system
 (locale "en_GB.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "guxtil")
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (kernel-arguments '("nomodset"))
 (users
  (cons*
   (user-account
    (name "sharlatan")
    (comment "Sharlatan")
    (group "users")
    (home-directory "/home/sharlatan")
    (supplementary-groups
     '("wheel" "netdev" "audio" "video")))
   %base-user-accounts))
 (packages
  (append
   (map specification->package
        '("nss-certs"
          "sbcl-clx-truetype"
          "sbcl-stumpwm-cpu"
          "sbcl-stumpwm-disk"
          "sbcl-stumpwm-kbd-layouts"
          "sbcl-stumpwm-mem"
          "sbcl-stumpwm-net"
          "stumpwm-with-slynk"))
   %base-packages))
 (services
  (append
   (list
    (service gnome-desktop-service-type)
    (service openssh-service-type)
    (service nfs-service-type
             (nfs-configuration
              (exports
               '(("/mnt/library/pve" "192.168.1.4/24(rw,sync,no_root_squash)")))))
    (service postgresql-service-type
             (postgresql-configuration
              (postgresql postgresql-13)
              (data-directory "/var/lib/postgresql/13/data")
              (config-file
               (postgresql-config-file
                (socket-directory #f)))))
    (set-xorg-configuration
     (xorg-configuration
      (modules
       (list
        xf86-video-amdgpu
        xf86-video-vesa
        xf86-input-libinput))
      (keyboard-layout keyboard-layout))))
   %desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/nvme0n1")
   (keyboard-layout keyboard-layout)))
 (mapped-devices (list
                  (mapped-device
                   (source (list "/dev/sda1" "/dev/sdb1"))
                   (target "/dev/md0")
                   (type raid-device-mapping))))
 (swap-devices
  (list (uuid "2ce1d166-3280-44b3-91eb-8db1d69b2846")))
 (file-systems
  (cons*
   (file-system
    (mount-point "/")
    (device (uuid "defa5f0b-5d2f-4095-b299-39e5819cc54e" 'ext4))
    (type "ext4"))
   (file-system
    (mount-point "/mnt/library")
    (device "/dev/md0")
    (type "xfs"))
   %base-file-systems)))

;; config.scm ends here
