;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base data)
  #:use-module (apps base templates components)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (srfi srfi-1)
  #:use-module (sexp-xgettext)
  #:export (contact-media
	    screenshots))


;;;
;;; Data.
;;;

(define contact-media
  (list
   ;; The first three will be featured in the home page.
   (contact
    #:name (G_ "IRC Channel")
    #:description
    (G_
     `(p
       "Join the " (code "#guix") " channel on the Freenode IRC network to chat
       with the community about GNU Guix or to get help in
       real-time."))
    #:url (guix-url "contact/irc/")
    #:log guix-irc-log-url)

   (contact
    #:name (G_ "Info Mailing List")
    #:description
    (G_
     `(p
       "Subscribe to the " (code "info-guix") " low-traffic mailing
list to receive important announcements sent by the project maintainers (in
English)."))
    #:url "https://lists.gnu.org/mailman/listinfo/info-guix"
    #:log "https://lists.gnu.org/archive/html/info-guix")

   (contact
    #:name (G_ "Help Mailing List")
    #:description
    ;; Compute an association list from language code to blurb.
    ;; If possible, look up translated blurbs from the PO file.
    ;; Fall back to old hard-coded translations.
    (let ((original '(G_
                      "Subscribe to the Help mailing list to get support
from the GNU Guix community via email.  You can post messages in English
though we also accept other languages."))
          (lang-code '(C_ "unique lingua code like en or zh-cn" "en")))
      (sort
       (delete-duplicates
        (append
         (delete ;delete untranslated blurbs other than "en"
          (cons original lang-code)
          (map-in-order
           (lambda (lingua)
             (begin
               (setlocale LC_ALL (string-append lingua ".utf8"))
               (let ((out (list (gettext (string-append
                                          (cadr lang-code) ;msgctxt
                                          (string #\eot) ;separates msgctxt
                                          (caddr lang-code))) ;msgid
                                (gettext (cadr original)))))
                 (setlocale LC_ALL "")
                 (if (string-index (car out) #\eot) ;if untranslated
                     (list (caddr lang-code) (cadr original)) ;use original
                     out)))) ;else use what has been looked up via gettext
           %linguas)
          (lambda (to-delete b) (and ;delete where text is equal to original
                                 (string=? (cadar to-delete) (cadr b))
                                 ;; but language code is different
                                 (not (string=? (cadddr to-delete) (car b))))))
         `(("de"
            "Melden Sie sich bei der „Help“-Mailingliste an, um per E-Mail
gemeinschaftlichen Rat zu GuixSD und Guix zu bekommen.  Sie können
Nachrichten auch auf deutsch verfassen.")
           ("eo"
            "Subskribu al la retmesaĝolisto \"Help\" por demandi helpon pri
GNU Guix al la grupo.  Vi povas skribi esperantlingve.")
           ("es"
            "Suscríbete a la lista de correo electrónico \"Help\" por pedir
ayuda con Guix.  Puedes escribir mensajes en Español.")
           ("fr"
             "Abonnez-vous à la liste de diffusion « Help » pour obtenir l'aide
de la communauté sur GNU Guix par courrier électronique.  Vous
pouvez envoyer des messages en français.")
           ("hu"
            "Iratkozzon fel a „Help“ levelezőlistára, hogy segítséget kaphasson
e-mailben a GuixSD és a GNU Guix közösségtől. Magyarul is küldhet
üzeneteket.")
           ("it"
            "Iscrivetevi alla mailing list 'Help' per essere aiutati da altri
utenti di Guix e GuixSD.  Potete scrivere sulla mailing list anche in
italiano.")
           ("ja"
            "メールでGNU GuixとGuixSDのコミュニティからサポートを受けるには、
「Help」のメーリングリストに登録してください。
メッセージ内容は日本語でも問題ございませんが、多言語でも受け付けております。")
           ("nb"
            "Meld deg på diskusjonslisten «Help» for å få råd og tips fra
andre GuixSD- og GNU Guix-brukere via e-post.  Du kan legge inn
meldinger på norsk.")
           ("nl"
            "Abonneer je op de discussielijst \"Help\" om hulp te vragen
van de GuixSD- en GNU Guix-gemeenschap.  Je kunt berichten sturen in
het Nederlands.")
           ("ru"
            "Подпишитесь на список рассылки «Help», чтобы получить помощь от
сообщества GuixSD и GNU Guix по электронной почте.  Вы можете писать на русском
языке.")
           ("zh-Hant"
            "訂閱「Help」郵件群組以電郵從GuixSD及GNU Guix社群取得支援。你可以使用
正體、繁體中文發送訊息。")))
        (lambda (a b) (string=? (car a) (car b))))
       (lambda (a b) (string<? (car a) (car b)))))
    #:url "https://lists.gnu.org/mailman/listinfo/help-guix"
    #:log "https://lists.gnu.org/archive/html/help-guix")

   (contact
    #:name (G_ "Bug Reporting")
    #:description
    (G_
     `(p
       "If you found a bug in Guix, check whether the bug is
       already in the "
       ,(G_ `(a (@ (href "https://issues.guix.gnu.org"))
                "bug database"))
       ". If it is not, please "
       ,(G_ `(a (@ (href "mailto:bug-guix@gnu.org")) "report it."))))
    #:url "https://lists.gnu.org/mailman/listinfo/bug-guix"
    #:log "https://issues.guix.gnu.org/")

   (contact
    #:name (G_ "Development Mailing List")
    #:description
    (G_
     `(p
       "Discussion about the development of GNU Guix. "
       ,(G_ `(a (@ (href "https://lists.gnu.org/archive/html/bug-guix/2013-07/msg00039.html"))
                " Until July 2013"))
       ", the bug-Guix mailing list filled that role. "))
    #:url "https://lists.gnu.org/mailman/listinfo/guix-devel"
    #:log "https://lists.gnu.org/archive/html/guix-devel")

   (contact
    #:name (G_ "Patches Mailing List")
    #:description
    (G_
     `(p
       "Submission of patches.  Every message sent to this mailing list
       leads to a new entry in our "
       ,(G_ `(a (@ (href "https://issues.guix.gnu.org"))
                "patch tracking tool"))
       ".  See "
       ,(G_ `(a (@ (href "https://debbugs.gnu.org/Advanced.html")) "this page"))
       " for more information on how to use it; see "
       ,(G_ (manual-href "the manual" (G_ "en") (G_ "Submitting-Patches.html")))
       " for more information on how to submit a patch.  "
       ,(G_
         `(a (@ (href "https://lists.gnu.org/archive/html/guix-devel/2017-02/msg00627.html"))
             "Until February 2017"))
       ", the guix-devel mailing list filled that role."))
    #:url "https://lists.gnu.org/mailman/listinfo/guix-patches"
    #:log "https://issues.guix.gnu.org")

   (contact
    #:name (G_ "Commits Mailing List")
    #:description
    (G_
     `(p
       "Notifications of commits made to the "
       ,(G_ `(a (@ (href ,(guix-url "contribute/"))) "Git repositories"))
       "."))
    #:url "https://lists.gnu.org/mailman/listinfo/guix-commits"
    #:log "https://lists.gnu.org/archive/html/guix-commits")

   (contact
    #:name (G_ "Security Mailing List")
    #:description
    (G_
     `(p
       "This is a private mailing list that anyone can post to to "
       ,(G_ `(a (@ (href ,(guix-url "security/"))) "report security issues"))
       " in Guix itself or in "
       "the " ,(G_ `(a (@ (href ,(guix-url "packages/"))) "packages"))
       " it provides.  Posting here allows Guix developers to address
       the problem before it is widely publicized."))
    #:url "https://lists.gnu.org/mailman/listinfo/guix-security"
    #:log "")

   (contact
    #:name (G_ "Sysadmin Mailing List")
    #:description
    (G_
     `(p
       "Private mailing list for the "
       ,(G_ `(a (@ (href "https://hydra.gnu.org/")) "build farm"))
       " system administration."))
    #:url "https://lists.gnu.org/mailman/listinfo/guix-sysadmin"
    #:log "")


   ;; Non-Guix lists.

   (contact
    #:name (G_ "GNU System Discuss Mailing List")
    #:description
    (G_ '(p "Discussion about the development of the broader GNU system."))
    #:url "https://lists.gnu.org/mailman/listinfo/gnu-system-discuss"
    #:log "https://lists.gnu.org/archive/html/gnu-system-discuss/")

   (contact
    #:name (G_ "GNU/Linux-libre Mailing List")
    #:description
    (G_ '(p "Workgroup for fully free GNU/Linux distributions."))
    #:url "https://lists.nongnu.org/mailman/listinfo/gnu-linux-libre"
    #:log "https://lists.nongnu.org/archive/html/gnu-linux-libre/")

   (contact
    #:name (G_ "GNU Info Mailing List")
    #:description
    (G_ '(p "GNU software announcements."))
    #:url "https://lists.gnu.org/mailman/listinfo/info-gnu"
    #:log "https://lists.gnu.org/archive/html/info-gnu/")))



(define screenshots
  (list
   (screenshot
    #:title (C_ "screenshot title" "Graphical log-in")
    #:slug "slim"
    #:image (guix-url "static/media/img/gdm-sessions.png")
    #:preview (guix-url "static/media/img/gdm-sessions.mini.png")
    #:caption (G_ "Graphical log-in screen"))

   (screenshot
    #:title (C_ "screenshot title" "GNOME")
    #:slug "gnome"
    #:image (guix-url "static/media/img/gnome-totem-epiphany.png")
    #:preview (guix-url "static/media/img/gnome-totem-epiphany.mini.png")
    #:caption (G_ "Control your computer with the GNOME desktop environment"))

   (screenshot
    #:title (C_ "screenshot title" "Xfce")
    #:slug "xfce"
    #:image (guix-url "static/media/img/guixsd-xfce-icecat-emacs.png")
    #:preview (guix-url "static/media/img/guixsd-xfce-icecat-emacs.mini.png")
    #:caption (G_ "The Xfce desktop environment with GNU Emacs and IceCat"))

   (screenshot
    #:title (C_ "screenshot title" "Virtual machine")
    #:slug "virtual-machine"
    #:image (guix-url "static/media/img/guix-system-vm.png")
    #:preview (guix-url "static/media/img/guix-system-vm.mini.png")
    #:caption (G_ "Virtual machine started with 'guix system vm'"))

   (screenshot
    #:title (C_ "screenshot title" "Enlightenment")
    #:slug "enlightenment"
    #:image (guix-url "static/media/img/enlightenment-inkscape.png")
    #:preview (guix-url "static/media/img/enlightenment-inkscape.mini.png")
    #:caption (G_ "Enlightenment, Inkscape, and Serbian text"))))
