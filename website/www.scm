;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Initially written by Luis Felipe López Acevedo <felipe.lopez@openmailbox.org>
;;; who waives all copyright interest on this file.
;;;
;;; This file is part of GuixSD website.
;;;
;;; GuixSD website is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GuixSD website is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with GuixSD website.  If not, see <http://www.gnu.org/licenses/>.

(define-module (www)
  #:use-module (www utils)
  #:use-module (www shared)
  #:use-module (www download)
  #:use-module (www donate)
  #:use-module (www about)
  #:use-module (www contribute)
  #:use-module (www graphics)
  #:use-module (www help)
  #:use-module (www packages)
  #:use-module (www security)
  #:use-module (www news)
  #:use-module (haunt post)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (main-page

            %web-pages
            export-web-page
            export-web-site))

(define %video-url
  ;; Note: No "http:" so that people viewing the parent page via HTTPS get
  ;; the video via HTTPS as well (otherwise some browsers complain.)
  "//audio-video.gnu.org/video/misc/2016-07__GNU_Guix_Demo_2.webm")

(define (sxml->string* tree)
  "Flatten tree by dismissing tags and attributes, and return the resulting
string."
  (define (sxml->strings tree)
    (match tree
      (((? symbol?) ('@ _ ...) body ...)
       (append-map sxml->strings body))
      (((? symbol?) body ...)
       (append-map sxml->strings body))
      ((? string?)
       (list tree))
      ((lst ...)
       (sxml->strings `(div ,@lst)))))

  (string-concatenate (sxml->strings tree)))

(define (summarize-string str n)
  "Truncate STR at the first space encountered starting from the Nth
character."
  (if (<= (string-length str) n)
      str
      (let ((space (string-index str #\space n)))
        (string-take str (or space n)))))

(define (post->summary-sxml post url)
  "Return the an SXML tree representing POST, a Haunt blog post, with a link
to URL."
  `(a (@ (href ,url)
         (class "news-entry"))
      (h4 ,(post-ref post 'title))
      (p (@ (class "news-date"))
         ,(date->string (post-date post) "~B ~e, ~Y"))
      (p (@ (class "news-summary"))
         ,(summarize-string (sxml->string* (post-sxml post))
                            170)
         "…")))

(define* (screenshot image
                     #:key
                     (directory (latest-guix-version))
                     (alt ""))
  `(a (@ (href ,(screenshot-url directory image)))
      (img (@ (src ,(thumb-url image))
              (class "screenshot-thumb img-responsive")
              (alt ,alt)))))

(define* (main-page #:optional site (posts '()))
  "Produce the main page showing a subset of POSTS, a list of Haunt blog
posts."
  `(html (@ (lang "en"))
	 ,(html-page-header
           "GNU's advanced distro and transactional package manager"
           #:css "index.css")

	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (div (@ (id "featured-box"))
		    (div (@ (class "featured-content"))
			 (h1 (@ (class "featured-heading"))
			     "The Guix System Distribution")
			 (ul (li (b "Liberating.")
				 " GuixSD is an advanced distribution of the "
				 (a (@ (href ,(gnu-url "gnu/about-gnu.html"))
				       (class "hlink-yellow"))
				    "GNU operating system")
				 " developed by the "
				 (a (@ (href ,(gnu-url ""))
				       (class "hlink-yellow"))
				    "GNU Project ")
				 "—which respects the "
				 (a (@ (href ,(gnu-url
                                               "distros/free-system-distribution-guidelines.html"))
				       (class "hlink-yellow"))
				    "freedom of computer users")
				 ". ")
			     (li (b "Dependable.")
				 " The "
				 (a (@ (href ,(guix-url "manual/html_node/Package-Management.html"))
				       (class "hlink-yellow"))
				    "GNU Guix package manager")
                                 ", in addition to standard
package management features, supports transactional upgrades and roll-backs,
unprivileged package management, per-user profiles, "
                                 (a (@ (href ,(guix-url
                                               "manual/html_node/Features.html"))
                                       (class "hlink-yellow"))
                                    "and more") ".")
			     (li (b "Hackable.")
				 " It provides "
				 (a (@ (href ,(gnu-url "software/guile/"))
				       (class "hlink-yellow"))
				    "Guile Scheme")
				 " APIs, including high-level embedded
domain-specific languages (EDSLs) to "
                                 (a (@ (href ,(guix-url
                                               "manual/html_node/Defining-Packages.html"))
                                       (class "hlink-yellow"))
                                    "define packages")
                                 " and "
                                 (a (@ (href ,(guix-url
                                               "manual/html_node/System-Configuration.html"))
                                       (class "hlink-yellow"))
                                    "whole-system configurations")
                                 "."))
			 (div (@ (class "featured-actions"))
			      (a (@ (href ,(base-url "download"))
				    (class "action download"))
				 "DOWNLOAD v" ,(latest-guix-version))
			      (a (@ (href ,(base-url "contribute"))
				    (class "action contribute"))
				 "CONTRIBUTE"))))
	       (div (@ (id "discovery-box"))
		    (h2 "Discover GuixSD")
		    (div (@ (class "info-box text-center"))
			 (video (@ (src ,%video-url)
				   (poster
				    ,(image-url "guix-demo.png"))
				   (controls "controls")
				   (class "video-preview"))
				(p "Video: "
				   (a (@ (href ,%video-url)
					 (class "hlink-more-light"))
				      "Guix in action (1m30)"))))
		    (div (@ (class "info-box text-left"))
			 (p (a (@ (href ,(guix-url "manual"))
				  (class "hlink-yellow"))
			       "GNU Guix Documentation")
			    (br)
			    "You may also find more information about GNU Guix
by running info guix.")
			 (p (a (@ (href "http://arxiv.org/abs/1305.4584")
				  (class "hlink-yellow"))
			       "Functional Package Management with Guix")
			    (br)
			    "A paper presented at the 2013 European Lisp
Symposium (ELS), describes the rationale, design, and implementation of Guix's
packaging API. ")
			 (p (a (@ (href ,(base-url "help"))
				  (class "hlink-more-light"))
			       "Find more documentation")))
		    (img (@ (src ,(image-url "h-separator-darker.png"))
			    (class "h-separator")
			    (alt "")))
		    (div (@ (id "screens-box"))
                         ,@(map (match-lambda*
                                  (((version file) alt)
                                   (screenshot file
                                               #:alt alt
                                               #:directory version)))
                                '(("0.8.2" "guixsd-slim.png")
                                  ("0.9.1" "gnome-totem-epiphany.png")
                                  ("0.8.2" "guixsd-xfce-icecat-emacs.png")
                                  ("0.8.3" "guix-system-vm.png")
                                  ("0.8.3" "enlightenment-inkscape.png"))
                                '("Graphical log-in screen"
                                  "GNOME with Totem and Ephiphany"
                                  "Xfce, IceCat, and Emacs"
                                  "VM started with 'guix system vm'"
                                  "Enlightenment, Inkscape, and Serbian text")))
		    (p (a (@ (href ,(base-url "contribute") )
			     (class "hlink-yellow-boxed"))
			  "Help us improve Guix →")))

	       (div (@ (id "news-box"))
		    (h2 "News")
		    ,@(map (lambda (post)
                             (post->summary-sxml post
                                                 (post-url post site)))
                           (take (posts/reverse-chronological posts)
                                 (min 3 (length posts))))
		    (p (a (@ (href ,(base-url "news"))
			     (class "hlink-more-dark"))
			  "More news")))

	       (div (@ (id "contact-box"))
		    (h2 "Contact")
		    (div (@ (class "info-box text-justify"))
			 (h3 "IRC Channel")
			 (p "Some Guix users and developers hang out on the
#guix channel of the Freenode IRC network. "
			    (small "(See "
				   (a (@ (href "https://gnunet.org/bot/log/guix/"))
				      "channel logs")
				   ")")
			    ".")

                         ;; XXX: Doesn't feel right to (1) suggest a JS
                         ;; client, and (2) make it too easy to join the
                         ;; channel.
			 ;; (p (@ (class "text-right"))
			 ;;    (a (@ (href "http://webchat.freenode.net/?channels=%23guix")
			 ;;          (class "button btn-blue"))
			 ;;       "Connect"))

			 (h3 "Report Bugs")
			 (p "Use the bugs mailing list to report bugs. Please
check whether the bug is already in the "
			    (a (@ (href "http://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=guix"))
			       "bug database")
			    ".")
			 (p (@ (class "text-right"))
			    (a (@ (href "mailto:bug-guix@gnu.org")
				  (class "button btn-red"))
			       "Report")))

		    (div (@ (class "info-box text-left"))
			 (h3 "Mailing Lists")
			 (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/help-guix"))
			       (b "help-guix"))
			    (small " ("
				   (a (@ (href "https://lists.gnu.org/archive/html/help-guix"))
				      "archive")
				   ")")
			    (br)
			    "Support for users of GNU Guix and
the Guix System Distribution (GuixSD).")
			 (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix"))
			       (b "bug-guix"))
			    (small " ("
				   (a (@ (href "https://lists.gnu.org/archive/html/bug-guix"))
				      "archive")
				   ")")
			    (br)
			    "Bug reports for GNU Guix and the Guix System
Distribution.")

			 (p (a (@ (href ,(base-url "about#contact"))
				  (class "hlink-more-dark"))
			       "Find all the available lists")))))
	  ,(html-page-footer))))


;;;
;;; HTML export.
;;;

(define %web-pages
  ;; Mapping of web pages to HTML file names.  Note: in the CVS repo at
  ;; sv.gnu.org, the main page has to be called 'guix.html'.
  `(("guix.html" ,main-page)
    ("about/index.html" ,about-page)
    ("contribute/index.html" ,contribute-page)
    ("donate/index.html" ,donate-page)
    ("download/index.html" ,download-page)
    ("graphics/index.html" ,graphics-page)
    ("help/index.html" ,help-page)
    ("security/index.html" ,security-page)
    ;; ,@(map (lambda (group)
    ;;          `(,(string-append "packages/" group ".html")
    ;;            ,(paginated-packages-page group)))
    ;;        %groups)
    ;; ("packages/index.html" ,(paginated-packages-page "0-9"))
    ;; ("packages/all.html" ,packages-page)
    ;; ("packages/issues.html" ,issues-page)
    ))

(define (mkdir* directory)
  "Make DIRECTORY unless it already exists."
  (catch 'system-error
    (lambda ()
      (mkdir directory))
    (lambda args
      (unless (= EEXIST (system-error-errno args))
        (apply throw args)))))

(define (export-web-page page file)
  "Export PAGE, an SXML tree, to FILE."
  (mkdir* (dirname file))
  (call-with-output-file file
    (lambda (port)
      (display "<!DOCTYPE html>\n" port)
      (sxml->xml page port))
    #:encoding "UTF-8"))

(define* (export-web-site #:optional (directory "."))
  "Export the whole web site as HTML files created in DIRECTORY."
  (for-each (match-lambda
              ((filename page)
               (export-web-page (page)
                                (string-append directory
                                               file-name-separator-string
                                               filename))))
            %web-pages))
