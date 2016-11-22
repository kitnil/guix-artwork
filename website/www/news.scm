;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (www news)
  #:use-module (www utils)
  #:use-module (www shared)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt builder blog)
  #:use-module (srfi srfi-19)
  #:export (post-url
            %news-haunt-theme))

(define (post-url post site)
  "Return the URL of POST, a Haunt blog post, for SITE."
  (base-url (string-append "news/" (site-post-slug site post) ".html")))

(define* (post->sxml post #:key post-uri)
  "Return the SXML for POST."
  `(div (h2 (@ (class "title"))
            ,(if post-uri
                 `(a (@ (href ,post-uri))
                     ,(post-ref post 'title))
                 (post-ref post 'title)))
        (div (@ (class "post-about"))
             ,(post-ref post 'author)
             " — " ,(date->string (post-date post) "~B ~e, ~Y"))
        (div (@ (class "post-body"))
             ,(post-sxml post))))

(define (news-page-sxml site title posts prefix)
  "Return the SXML for the news page of SITE, containing POSTS."
  `((div (@ (class "news-header"))
         (h1 "Recent News "
             (a (@ (href ,(base-url "news/feed.xml")))
                (img (@ (alt "Atom feed")
                        (src ,(image-url "feed.png")))))))
    (div (@ (class "post-list"))
         ,@(map (lambda (post)
                  (post->sxml post #:post-uri (post-url post site)))
                posts))))

(define (base-layout body)
  `((doctype "html")
    (html (@ (lang "en"))
	  ,(html-page-header "News" #:css "news.css")

	  (body
	   ,(html-page-description)
	   ,(html-page-links)

	   (div (@ (id "content-box"))
		(article ,body))

	   ,(html-page-footer)))))

(define %news-haunt-theme
  ;; Theme for the rendering of the news pages.
  (theme #:name "GuixSD"
         #:layout (lambda (site title body)
                    (base-layout body))
         #:post-template post->sxml
         #:collection-template news-page-sxml))
