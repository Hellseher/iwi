;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules 
  (gnu)
  (gnu system locale)
  (gnu packages)
  (gnu packages base)
  (gnu packages ci))

(use-service-modules desktop networking ssh xorg cuirass)

(use-package-modules xorg bash lisp emacs admin)

(use-package-modules base)

(define %cuirass-specs
  #~(list
     '((#:name . "my-manifest")
       (#:load-path-inputs . ("guix"))
       (#:package-path-inputs . ("custom-packages"))
       (#:proc-input . "guix")
       (#:proc-file . "build-aux/cuirass/gnu-system.scm")
       (#:proc . cuirass-jobs)
       (#:proc-args . ((subset . "manifests")
                       (systems . ("x86_64-linux"))
                       (manifests . (("config" . "guix/manifest.scm")))))
       (#:inputs . (((#:name . "guix")
                     (#:url . "git://git.savannah.gnu.org/guix.git")
                     (#:load-path . ".")
                     (#:branch . "master")
                     (#:no-compile? . #t)))))))

(operating-system
  (locale "en_GB.utf8")
  (locale-definitions
    (list (locale-definition (source "en_GB")
			     (name "en_GB.utf8"))))
  (locale-libcs (list glibc-2.23 (canonical-package glibc)))
  (timezone "Europe/London")
  (keyboard-layout (keyboard-layout "gb" "extd"))
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/sda")
      (keyboard-layout keyboard-layout)))
  (swap-devices (list "/dev/sda1"))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "86b53a14-f99e-4b5f-8f88-985303a24f52"
                     'ext4))
             (type "ext4"))
           %base-file-systems))
  (host-name "guix-t510i")
  (users (cons* (user-account
                  (name "phaser")
                  (comment "phaser")
                  (group "users")
                  (home-directory "/home/phaser")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (map specification->package
	  '("emacs" "nmap" "stumpwm" "glibc-locales" "nss-certs"))
      %base-packages))

  (services
   (append
    (list
        (service cuirass-service-type
		(cuirass-configuration
			(specifications %cuirass-specs)))
      	(service gnome-desktop-service-type)
        (service openssh-service-type)
        (set-xorg-configuration
	  (xorg-configuration 
	   (keyboard-layout keyboard-layout)))
	
	(extra-special-file "/usr/bin/env"
			    (file-append coreutils "/bin/env"))
	
	(extra-special-file "/usr/bin/bash"
			    (file-append bash "/bin/bash")))

      	%desktop-services)))
