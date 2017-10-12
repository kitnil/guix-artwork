;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base data)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (contact-media
	    screenshots))


;;;
;;; Data.
;;;


(define contact-media
  (list
   ;; The first three will be featured in the home page.
   (contact
    #:name "IRC Channel"
    #:description
    '(p
      "Join the #guix channel on the Freenode IRC network to chat
      with the community about GuixSD or GNU Guix or to get help in
      real-time.")
    #:url (guix-url "contact/irc/")
    #:log "https://gnunet.org/bot/log/guix/")

   (contact
    #:name "Help Mailing List"
    #:description
    '(p
      "Subscribe to the Help mailing list to get support from the
      GuixSD and GNU Guix community via email. "
      (a (@ (href "https://lists.gnu.org/archive/html/guix-devel/2015-12/msg00584.html"))
	 "Until December 2015")
      ", the Guix-devel mailing list filled that role.")
    #:url "https://lists.gnu.org/mailman/listinfo/help-guix"
    #:log "https://lists.gnu.org/archive/html/help-guix")

   (contact
    #:name "Bug Reporting"
    #:description
    '(p
      "If you found a bug in GuixSD or Guix, check whether the bug is
      already in the "
      (a (@ (href "https://debbugs.gnu.org/cgi/pkgreport.cgi?package=guix;max-bugs=100"))
	 "bug database")
      ". If it is not, please report it.")
    #:url "mailto:bug-guix@gnu.org"
    #:log "https://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=guix")

   (contact
    #:name "Development Mailing List"
    #:description
    '(p
      "Discussion about the development of GNU Guix and the Guix
      System Distribution (GuixSD). "
      (a (@ (href "https://lists.gnu.org/archive/html/bug-guix/2013-07/msg00039.html"))
	 " Until July 2013")
      ", the bug-Guix mailing list filled that role. ")
    #:url "https://lists.gnu.org/mailman/listinfo/guix-devel"
    #:log "https://lists.gnu.org/archive/html/guix-devel")

   (contact
    #:name "Patches Mailing List"
    #:description
    '(p
      "Submission of patches.  Every message sent to this mailing list
      leads to a new entry in our "
      (a (@ (href "https://bugs.gnu.org/guix-patches"))
	 "patch tracking tool")
      ".  See "
      (a (@ (href "https://debbugs.gnu.org/Advanced.html")) "this page")
      " for more information on how to use it.  "
      (a (@ (href "https://lists.gnu.org/archive/html/guix-devel/2017-02/msg00627.html"))
	 "Until February 2017")
      ", the guix-devel mailing list filled that role.")
    #:url "https://lists.gnu.org/mailman/listinfo/guix-patches"
    #:log "https://lists.gnu.org/archive/html/guix-patches")

   (contact
    #:name "Commits Mailing List"
    #:description
    `(p
      "Notifications of commits made to the "
      (a (@ (href ,(guix-url "contribute/"))) "Git repositories")
      ".")
    #:url "https://lists.gnu.org/mailman/listinfo/guix-commits"
    #:log "https://lists.gnu.org/archive/html/guix-commits")

   (contact
    #:name "Security Mailing List"
    #:description
    `(p
      "This is a private mailing list that anyone can post to to "
      (a (@ (href ,(guix-url "security/"))) "report security issues")
      " in Guix itself or in "
      "the " (a (@ (href ,(guix-url "packages/"))) "packages")
      " it provides.  Posting here allows Guix developers to address
      the problem before it is widely publicized.")
    #:url "https://lists.gnu.org/mailman/listinfo/guix-security"
    #:log "")

   (contact
    #:name "Sysadmin Mailing List"
    #:description
    '(p
      "Private mailing list for the "
      (a (@ (href "https://hydra.gnu.org/")) "build farm")
      " system administration.")
    #:url "https://lists.gnu.org/mailman/listinfo/guix-sysadmin"
    #:log "")


   ;; Non-Guix lists.

   (contact
    #:name "GNU System Discuss Mailing List"
    #:description
    '(p "Discussion about the development of the broader GNU system.")
    #:url "https://lists.gnu.org/mailman/listinfo/gnu-system-discuss"
    #:log "http://lists.gnu.org/archive/html/gnu-system-discuss/")

   (contact
    #:name "GNU/Linux Libre Mailing List"
    #:description
    '(p "Workgroup for fully free GNU/Linux distributions.")
    #:url "https://lists.nongnu.org/mailman/listinfo/gnu-linux-libre"
    #:log "https://lists.nongnu.org/archive/html/gnu-linux-libre/")

   (contact
    #:name "GNU Info Mailing List"
    #:description
    '(p "GNU software announcements.")
    #:url "https://lists.gnu.org/mailman/listinfo/info-gnu"
    #:log "https://lists.gnu.org/archive/html/info-gnu/")))



(define screenshots
  (list
   (screenshot
    #:title "Graphical log-in"
    #:slug "slim"
    #:image (guix-url "static/media/img/slim.png")
    #:preview (guix-url "static/media/img/guixsd-slim.mini.png")
    #:caption "Graphical log-in screen")

   (screenshot
    #:title "GNOME"
    #:slug "gnome"
    #:image (guix-url "static/media/img/gnome-totem-epiphany.png")
    #:preview (guix-url "static/media/img/gnome-totem-epiphany.mini.png")
    #:caption "Control your computer with the GNOME desktop environment")

   (screenshot
    #:title "Xfce"
    #:slug "xfce"
    #:image (guix-url "static/media/img/guixsd-xfce-icecat-emacs.png")
    #:preview (guix-url "static/media/img/guixsd-xfce-icecat-emacs.mini.png")
    #:caption "The Xfce desktop environment with GNU Emacs and IceCat")

   (screenshot
    #:title "Virtual machine"
    #:slug "virtual-machine"
    #:image (guix-url "static/media/img/guix-system-vm.png")
    #:preview (guix-url "static/media/img/guix-system-vm.mini.png")
    #:caption "Virtual machine started with 'guix system vm'")

   (screenshot
    #:title "Enlightenment"
    #:slug "enlightenment"
    #:image (guix-url "static/media/img/enlightenment-inkscape.png")
    #:preview (guix-url "static/media/img/enlightenment-inkscape.mini.png")
    #:caption "Enlightenment, Inkscape, and Serbian text")))
