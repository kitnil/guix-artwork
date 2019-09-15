;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

;;; This module defines HTML parts like header, breadcrumbs, footer,
;;; buttons, etc., which are used website-wide.

(define-module (apps base templates components)
  #:use-module (apps aux lists)
  #:use-module (apps aux strings)
  #:use-module (apps aux sxml)
  #:use-module (apps aux web)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (breadcrumbs
	    button-big
	    button-little
	    contact-preview
	    contact->shtml
            horizontal-line
	    horizontal-separator
            horizontal-skip
	    link-more
	    link-subtle
	    link-yellow
            manual-href
            manual-link-yellow
	    navbar
	    page-indicator
	    page-selector
	    screenshot->shtml))


;;;
;;; Components.
;;;

(define (breadcrumbs crumbs)
  "Return an SHTML nav element representing the breadcrumbs.

   CRUMBS (list)
     A non-empty list of <crumb> objects as defined in
     (apps base types)."
  `(nav
    (@ (class "breadcrumbs"))
    ,(G_ `(h2 (@ (class "a11y-offset")) "Your location:"))

    ,(G_ `(a (@ (class "crumb") (href ,(guix-url))) "Home")) (span " → ")
    ,@(separate (crumbs->shtml crumbs) '(span " → "))))


(define (crumbs->shtml crumbs)
  "Return the list of CRUMBS as list of SHTML a elements.

   CRUMBS (list)
     A non-empty list of <crumb> objects as defined in
     (apps base types)."
  (cond ((= (length crumbs) 1)
	 (cons
	  `(a
	    (@ (class "crumb crumb-active")
	       (href ,(crumb-url (first crumbs))))
	    ,(crumb-label (first crumbs)))
	  '()))
	(else
	 (cons
	  `(a
	    (@ (class "crumb")
	       (href ,(crumb-url (first crumbs))))
	    ,(crumb-label (first crumbs)))
	  (crumbs->shtml (rest crumbs))))))


(define* (button-big #:key (label "Button") (url "#") (light #false))
  "Return an SHTML a element that looks like a big button.

   LABEL (string)
     The text for the button. For example: 'Download!'.

   URL (string)
     A URL to use for the href attribute of the a element. If not
     specified, the value defaults to #.

   LIGHT (boolean)
     True if the button is going to be used on a dark background; false
     otherwise (this is the default)."
  `(a
    (@ (class ,(string-append "button-big" (if light " button-light" "")))
       (href ,url))
    ,label))


(define* (button-little #:key (label "Button") (url "#") (active #false))
  "Return an SHTML a element that looks like a little button.

   LABEL (string)
     The text for the button. For example: 'Next'.

   URL (string)
     A URL to use for the href attribute of the a element. If not
     specified, the value defaults to #.

   ACTIVE (boolean)
     True if the button should be highlighted as active (on)."
  `(a
    (@ (class ,(string-append "button-little"
			      (if active " button-little-active" "")))
       (href ,url))
    ,label))


(define (contact-preview contact)
  "Return an SHTML preview of the given contact object.

   CONTACT (<contact>)
     A contact object as defined in (apps base types)."
  `(a
    (@ (class "item-preview")
       (href ,(contact-url contact)))
    (h3 ,(contact-name contact))
    (p
     ,(string-summarize
       (sxml->string*
        (match (contact-description contact)
          ((and multilingual (((? string?) (? string?)) ...))
           (let ((code %current-lang))
             (match (assoc code multilingual)
               ((code blurb) blurb))))
          (blurb
           blurb)))
       30)
     "…")))


(define (language-tag lang)
  `(span (@ (class "button-little button-little-active")
            (style "text-align: center; width: 20px; vertical-align: middle"))
         ,lang))

(define (contact->shtml contact)
  "Return an SHTML representation of the given contact object.

   CONTACT (<contact>)
     A contact object as defined in (apps base types)."
  `(div
    (@ (class "contact-medium"))
    (a (@ (href ,(contact-url contact))) (b ,(contact-name contact)))
    ,(if (string=? (contact-log contact) "")
	 ""
	 `(small
	   " (" ,(G_ `(a (@ (href ,(contact-log contact))) "archive")) ") "))

    ;; The description can be a list of language/blurb pairs.
    ,(match (contact-description contact)
       ((((? string? languages) blurbs) ...)
        `(div (@ (id "help-guix-mailing-list-description"))
	  ,@(map (lambda (language blurb)
                     `(div (@ (style "display: flex; align-items: center; margin: 0 10px 10px 0"))
                           ,(language-tag language)
                           (div (@ (lang ,language) (style "flex: 1")) ,blurb)))
                   languages
                   blurbs)))
       (blurb
        blurb))))


(define* (horizontal-separator #:key (light #false))
  "Return an SHTML img element that works as a separator.

   LIGHT (boolean)
     True if the separator is going to be used on a dark background;
     false otherwise (this is the default)."
  `(img
    (@ (class "h-separator")
       ,(if light
	    `(src ,(guix-url "static/base/img/h-separator.png"))
	    `(src ,(guix-url "static/base/img/h-separator-dark.png")))
       (alt ""))))

(define (horizontal-skip)
  "Return SHTML for a small horizontal space."
  `(span (@ (class "hskip"))))

(define (horizontal-line)
  "Return SHTML for a visible separator to be used in a dropdown menu
like a menu item."
  `(img (@ (class "hline")
           (src ,(guix-url "static/base/img/h-separator.png"))
           (alt ""))))


(define* (link-more #:key (label "More") (url "#") (light #false))
  "Return an SHTML a element that looks like a 'more →' link.

   LABEL (string)
     The text for the link. For example: 'Read the manual'.

   URL (string)
     A URL to use for the href attribute of the a element. If not
     specified, the value defaults to #.

   LIGHT (boolean)
     True if the link is going to be used on a dark background; false
     otherwise (this is the default)."
  `(a
    (@ (class ,(string-append "link-more" (if light " link-more-light" "")))
       (href ,url))
    ,label))


(define* (link-subtle #:key (label "link") (url "#"))
  "Return an SHTML a element that does not stand too much on white backgrounds.

   LABEL (string)
     The text for the link. For example: 'Additional notes'.

   URL (string)
     The URL of the link. If not specified, the value defaults to #."
  `(a (@ (class "link-subtle") (href ,url)) ,label))


(define* (link-yellow #:key (label "link") (url "#"))
  "Return a yellow SHTML a element to use on dark backgrounds.

   LABEL (string)
     The text for the link. For example: 'read the manual'.

   URL (string)
     The URL of the link. If not specified, the value defaults to #."
  `(a (@ (class "link-yellow") (href ,url)) ,label))




(define (manual-href label manual-lang _1 subpath _2)
  "Return an HTML a element with its href attribute pointing to the
manual.  It can be marked for translation as:

  (G_ (manual-href \"some-text\" (G_ \"en\") (G_ \"Some-section.html\")))

   LABEL (string)
     The content of the a element.

   MANUAL-LANG (string)
     The normalized language for the Guix manual as produced by
'doc/build.scm' in the Guix source tree, i.e. \"en\" for the English
manual.

   SUBPATH (string)
     The same as in the manual-url procedure."
  ;; The _ arguments are placeholders for args added by G_, cf. i18n-howto.txt.
  `(a (@ (href ,(manual-url subpath #:language manual-lang))) label))

(define* (manual-link-yellow label manual-lang _1 #:optional (subpath "") _2)
  "Return a link-yellow component pointing to the manual.  It can be
used like this:

  (manual-link-yellow \"some-text\" (G_ \"en\") \"Package-Management.html\")

   LABEL (string)
     The label of the link-yellow.

   MANUAL-LANG (string)
     The normalized language for the Guix manual as produced by
'doc/build.scm' in the Guix source tree, i.e. \"en\" for the English
manual.

   SUBPATH (string)
     The same as in the manual-url procedure."
  ;; The _ arguments are placeholders for args added by G_, cf. i18n-howto.txt.
  (link-yellow
   #:label label
   #:url (manual-url subpath #:language manual-lang)))




(define* (menu-dropdown #:key (label "Item") (active-item "") (url "#") (items '()))
  "Return an SHTML li element representing a dropdown for the navbar.

   LABEL (string)
     The text for the dropdown. For example: 'About'.

   ACTIVE-ITEM (string)
     A string representing the label of the current active item in the
     navigation bar. If the values of LABEL and ACTIVE-ITEM are the
     same, the dropdown is highlighted.

   URL (string)
     The URL of the web resource referenced by the dropdown. Any
     value used for an HTML a element is valid. If not specified, the
     value defaults to #.

   ITEMS (list of menu items)
     A list of menu items as returned by the menu-item procedure in this
     same module. If not provided, the value defaults to an empty list."
  (let ((label-hash (number->string (string-hash label))))
    `(li
      (@ (class ,(if (string=? (string-downcase label)
                               (string-downcase active-item))
                     "menu-item menu-item-active dropdown dropdown-btn"
                     "menu-item dropdown dropdown-btn")))
      ,@(let ((id (string-append "visible-dropdown-" label-hash)))
          `(;; show dropdown when button is checked:
            (style ,(string-append "#" id ":checked ~ #submenu-" label-hash "
{
    width: initial;
    height: initial;
    min-width: 150px;
    overflow: initial;
}"))
            ;; show uncheck version of button iff button is checked
            (style ,(string-append "#" id ":checked \
~ label[for=all-dropdowns-hidden]
{
    display: initial;
}"))
            (style "label[for=all-dropdowns-hidden]
{
    display: none;
}")
            ;; show check version of button iff button is unchecked
            (style ,(string-append "#" id ":checked ~ label[for=" id "]
{
    display: none;
}"))
            (input (@ (class "menu-hidden-input")
                      (type "radio")
                      (name "dropdown")
                      (id ,id)))
            (label
             (@ (for ,id))
             ,label)
            (label
             (@ (for "all-dropdowns-hidden"))
             ,label)))
      (div
       (@ (class "submenu")
          (id ,(string-append "submenu-" label-hash)))
       (div (@ (class "submenu-triangle"))
            " ")
       (ul ,@items)))))


(define* (menu-item #:key (label "Item") (active-item "") (url "#"))
  "Return an SHTML li element representing an item for the navbar.

   LABEL (string)
     The text for the item. For example: 'About'.

   ACTIVE-ITEM (string)
     A string representing the label of the current active item in the
     navigation bar. If the values of LABEL and ACTIVE-ITEM are the
     same, the menu item is highlighted.

   URL (string)
     The URL of the web resource referenced by the menu item. Any
     value used for an HTML a element is valid. If not specified, the
     value defaults to #."
  `(li
    (a
     (@ (class
	 ,(if (string=? (string-downcase label) (string-downcase active-item))
	      "menu-item menu-item-active"
	      "menu-item"))
	(href ,url))
     ,label)))


(define* (navbar #:key (active-item "About"))
  "Return an SHTML header element with the given ACTIVE ITEM highlighted."
  `(header
    (@ (class "navbar"))

    ;; Branding.
    (h1
     (a
      (@ (class "branding") (href ,(guix-url)))
      ,(C_ "website menu" `(span (@ (class "a11y-offset")) "Guix"))))

    ;; Menu.
    (nav (@ (class "menu"))
     ,(G_ `(h2 (@ (class "a11y-offset")) "website menu:"))
     (input (@ (class "menu-hidden-input")
               (type "radio")
               (name "dropdown")
               (id "all-dropdowns-hidden")))
     (ul
      ,(C_ "website menu" (menu-item #:label "Overview" #:active-item active-item #:url (guix-url)))
      ,(C_ "website menu" (menu-item #:label "Download" #:active-item active-item #:url (guix-url "download/")))
      ,(C_ "website menu" (menu-item #:label "Packages" #:active-item active-item #:url (guix-url "packages/")))
      ,(C_ "website menu" (menu-item #:label "Blog" #:active-item active-item #:url (guix-url "blog/")))
      ,(C_ "website menu" (menu-item #:label "Help" #:active-item active-item #:url (guix-url "help/")))
      ,(C_ "website menu" (menu-item #:label "Donate" #:active-item active-item #:url (guix-url "donate/")))

      ,(menu-dropdown #:label (C_ "website menu" "About") #:active-item active-item
	#:items
        (list
         (C_ "website menu" (menu-item #:label "About" #:active-item active-item #:url (guix-url "about/")))
         (horizontal-line)
         (C_ "website menu" (menu-item #:label "Contact" #:active-item active-item #:url (guix-url "contact/")))
         (C_ "website menu" (menu-item #:label "Contribute" #:active-item active-item #:url (guix-url "contribute/")))
         (C_ "website menu" (menu-item #:label "Security" #:active-item active-item #:url (guix-url "security/")))
         (C_ "website menu" (menu-item #:label "Graphics" #:active-item active-item #:url (guix-url "graphics/")))))
      ,(horizontal-skip)
      ;; Languages dropdown.
      ,(menu-dropdown #:label (locale-display-name) #:active-item active-item
        #:items
        (map-in-order
         (lambda (ietf-info)
           (let ((lingua (car ietf-info))
                 (code (cdr ietf-info)))
             (setlocale LC_ALL (string-append lingua ".utf8"))
             (let ((out (menu-item #:label (locale-display-name)
                                   #:active-item active-item
                                   #:url (guix-url (string-append code "/")
                                                   #:localize #f))))
               (setlocale LC_ALL "")
               out)))
         (sort (delete %current-lingua
                       ietf-tags-file-contents
                       (lambda (a b) (string=? a (car b))))
               (lambda (a b) string<?))))))


    ;; Menu button.
    (a
     (@ (class "menu-btn")
        (href ,(guix-url "menu/"))) "")))

(define (page-indicator page-number total-pages)
  "Return an SHTML span element in the form 'page X of Y' if there is
   more than one page. Otherwise, return an empty string.

   PAGE-NUMBER (number)
     The number of the page that the user is seeing.

   TOTAL-PAGES (number)
     The total number of pages that should be displayed."
  (if (> total-pages 1)
      (G_ `(span
            (@ (class "page-number-indicator"))
            " (Page " ,(number->string page-number)
            " of " ,(number->string total-pages) ")"))
      ""))


(define (page-selector pages active-page base-url)
  "Return an SHTML nav element representing a page selection widget.

   PAGES (number)
     The total number of pages that should be displayed.

   ACTIVE-PAGE (number)
     The number of the page that should be displayed as active.

   BASE-URL (string)
     Absolute URL path to prepend to page numbers. For example:
     '/en/blog'. This would result in URLs like: '/en/blog/page/N',
     where N is the number of the page."
  `(nav
    (@ (class "page-selector"))
    (h3
     (@ (class "a11y-offset"))
     ,(G_ (string-append "Page " (number->string active-page) " of "
                         (number->string pages) ". Go to another page: ")))
    ,(if (> pages 1)
	 (map
	  (lambda (page-number)
	    (list
	     (button-little
	      #:label page-number
	      #:url (url-path-join base-url "page"
				   (number->string page-number) "")
	      #:active (= page-number active-page))
	     " ")) ; NOTE: Force space for readability in non-CSS browsers.
	  (iota pages 1))
	 "")))


(define (screenshot->shtml shot)
  "Return an SHTML representation of the given screenshot object.

   SHOT (<screenshot>)
     A screenshot object as defined in (apps base types)."
  `(div
    (@ (class "screenshot-preview"))
    (a
     (@ (href ,(guix-url (url-path-join "screenshots"
					(screenshot-slug shot) ""))))
     (img
      (@ (class "responsive-image")
	 (src ,(screenshot-preview shot))
	 (alt "")))
     (span (@ (class "screenshot-inset-shadow")) ""))
    (p ,(screenshot-caption shot) (span (@ (class "hidden")) "."))))
