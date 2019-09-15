;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates irc)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (irc-t))


(define (irc-t)
  "Return the Kiwi IRC widget page in SHTML."
  (theme
   #:title
   (list (C_ "webpage title" "IRC")
         (C_ "webpage title" "Contact"))
   #:description
   (G_ "Internet relay chat.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|IRC|chat") #\|)
   #:active-menu-item "About"
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/base/css/irc.css"))
   #:crumbs (list (crumb (C_ "webpage title" "Contact") (guix-url "contact/"))
		  (crumb (C_ "webpage title" "IRC") "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      ,(G_ `(h2 "IRC"))

      ,(G_
        `(p
          (@ (class "centered-block limit-width"))
          "Join the " (code "#guix") " channel on the "
          ,(G_ `(a (@ (href "https://en.wikipedia.org/wiki/Freenode"))
                   "Freenode IRC network"))
          " to chat with the GNU Guix community or to get help
          in real-time. You can use the chat widget below, or just use
          the "
          ,(G_ `(a (@ (href "https://en.wikipedia.org/wiki/Comparison_of_Internet_Relay_Chat_clients"))
                   "IRC client"))
          " of your preference. Note that the conversations that happen
          on the " (code "#guix") " channel are logged ("
          ,(G_ `(a (@ (href ,guix-irc-log-url)) "browse the log"))
          ")."))

      (iframe
       (@ (class "chat-widget centered-block")
	  (src "https://kiwiirc.com/client/irc.freenode.net/?nick=PotentialUser-?#guix")))))))
