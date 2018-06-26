;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates security)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (security-t))

(define ludovics-key
  "3CE4 6455 8A84 FDC6 9DB4 0CFB 090B 1199 3D9A EBB5")

(define (security-t)
  "Return the Security page in SHTML."
  (theme
   #:title '("Security")
   #:description
   "Important information about geting security updates for your
   GuixSD or GNU Guix installation, and instructions on how to report
   security issues."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Security updates")
   #:active-menu-item "About"
   #:css (list
	  (guix-url "static/base/css/page.css"))
   #:crumbs (list (crumb "Security" "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      (h2 "Security")

      (h3 "How to report security issues")
      (p
       "To report sensitive security issues in Guix itself or the
        packages it provides, you can write to the private mailing list "
       (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-security"))
	  ("guix-security@gnu.org")) ".  This list is monitored by a
        small team of Guix developers.")
      (p
       "If you prefer to send your report using OpenPGP encrypted email,
        please send it to one of the following Guix developers using their
        respective OpenPGP key:")
      (ul
        (li "Leo Famulari"
          (ul
            (li "4F71 6F9A 8FA2 C80E F1B5 E1BA 5E35 F231 DE1A C5E0")))
        (li "Ludovic Courtès"
          (ul
            (li ,ludovics-key)))
        (li "Mark Weaver"
          (ul
            (li "D919 0965 CE03 199E AF28 B3BE 7CEF 2984 7562 C516")))
        (li "Ricardo Wurmus"
          (ul
            (li "BCA6 89B6 3655 3801 C3C6 2150 197A 5888 235F ACAC"))))

      (h3 "Release signatures")
      (p
       "Releases of Guix and GuixSD are signed using the OpenPGP "
       "key with the fingerprint "
       ,ludovics-key
       ".  "
       "Users should "
       (a (@ (href ,(manual-url "Binary-Installation.html"))) "verify")
       " their downloads before extracting or running them.")

      (h3 "Security updates")
      (p
       "When security vulnerabilities are found in Guix or the "
       "packages provided by Guix, we will provide "
       (a (@ (href ,(manual-url "Security-Updates.html"))) "security updates")
       " quickly and with minimal disruption for users.")
      (p
       "Guix uses a \"rolling release\" model.  All security "
       "bug-fixes are pushed directly to the master branch.  There"
       " is no \"stable\" branch that only receives security fixes.")))))
