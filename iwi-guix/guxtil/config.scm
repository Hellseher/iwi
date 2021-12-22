;; Modified : <2021-11-02 Tue 19:18:30 GMT>
;;
;; Commentary:
;;
;; This Guix configuration file is deployed and tested on hardware:
;; - CPU :: AMD Ryzen 7 3800X 8-Core Processor
;; - VGA :: [AMD/ATI] Baffin [Radeon Pro WX 4100]
;;
;; It requires noGuix channel:
;;
;; ~# guix describe
;; Generation 9    Jun 22 2021 09:49:37    (current)
;;   guix 61a9ba3
;;     repository URL: https://git.savannah.gnu.org/git/guix.git
;;     branch: master
;;     commit: 61a9ba398f39258e603ba7eaf107ecf051d4f462
;;   nonguix c5fed11
;;     repository URL: https://gitlab.com/nonguix/nonguix
;;     branch: master
;;     commit: c5fed11e7df221cad1045fc193b4136b34e23451

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
