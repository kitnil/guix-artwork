;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates security)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (security-t))

(define ludovics-key
  "3CE4 6455 8A84 FDC6 9DB4 0CFB 090B 1199 3D9A EBB5")

(define (security-t)
  "Return the Security page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Security"))
   #:description
   (G_ "Important information about getting security updates
   for your GNU Guix installation, and instructions on how
   to report security issues.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
      (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Security updates") #\|)
   #:active-menu-item (C_ "website menu" "About")
   #:css (list
	  (guix-url "static/base/css/page.css"))
   #:crumbs (list (crumb (C_ "website menu" "Security") "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      ,(G_ `(h2 "Security"))

      ,(G_ `(h3 "How to report security issues"))
      ,(G_
        `(p
          "To report sensitive security issues in Guix itself or the
           packages it provides, you can write to the private mailing list "
          (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-security"))
             ("guix-security@gnu.org")) ".  This list is monitored by a
           small team of Guix developers."))
      ,(G_
        `(p
          "If you prefer to send your report using OpenPGP encrypted email,
           please send it to one of the following Guix developers using their
           respective OpenPGP key:"))
      (ul
        (li "Leo Famulari"
          (ul (@ (class "mono"))
            (li "4F71 6F9A 8FA2 C80E F1B5 E1BA 5E35 F231 DE1A C5E0")))
        (li "Ludovic Courtès"
          (ul (@ (class "mono"))
            (li ,ludovics-key)))
        (li "Mark H Weaver"
          (ul (@ (class "mono"))
            (li "D919 0965 CE03 199E AF28 B3BE 7CEF 2984 7562 C516")))
        (li "Ricardo Wurmus"
          (ul (@ (class "mono"))
            (li "BCA6 89B6 3655 3801 C3C6 2150 197A 5888 235F ACAC"))))

      ,(G_ `(h3 "Release signatures"))
      ,(G_
        `(p
          "Releases of Guix are signed using the OpenPGP "
          "key with the fingerprint "
          (span (@ (class "mono")) ,ludovics-key)
          ".  "
          "Users should "
          ,(G_ (manual-href "verify"
                            (G_ "en")
                            (G_ "Binary-Installation.html")))
          " their downloads before extracting or running them."))

      ,(G_ `(h3 "Security updates"))
      ,(G_
        `(p
          "When security vulnerabilities are found in Guix or the "
          "packages provided by Guix, we will provide "
          ,(G_ (manual-href "security updates"
                            (G_ "en")
                            (G_ "Security-Updates.html")))
          " quickly and with minimal disruption for users."))
      ,(G_
        `(p
          "Guix uses a “rolling release” model.  All security "
          "bug-fixes are pushed directly to the master branch.  There"
          " is no “stable” branch that only receives security fixes."))))))
