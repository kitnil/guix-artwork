title: A packaging tutorial for Guix
date: 2018-10-10 16:00
author: Pierre Neidhardt
tags: Software development, Programming interfaces, Scheme API
---


# Introduction

GNU Guix stands out as the *hackable* package manager, mostly because it uses
[GNU Guile](https://www.gnu.org/software/guile/), a powerful high-level programming language, one of the [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
dialects from the [Lisp family](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

Package definitions are also written in Scheme, which empowers Guix in some very
unique ways, unlike most other package managers that use shell scripts or
simple languages.

-   Use functions, structures, macros and all of Scheme expressiveness for your
    package definitions.

-   Inheritance makes it easy to customize a package by inheriting from it and
    modifying only what is needed.

-   Batch processing: the whole package collection can be parsed, filtered and
    processed.  Building a headless server with all graphical interfaces stripped
    out?  It's possible.  Want to rebuild everything from source using specific
    compiler optimization flags?  Pass the `#:make-flags "..."` argument to the
    list of packages.  It wouldn't be a stretch to think [Gentoo USE flags](https://wiki.gentoo.org/wiki/USE_flag) here,
    but this goes even further: the changes don't have to be thought out
    beforehand by the packager, they can be *programmed* by the user!

The following tutorial covers all the basics around package creation with Guix.
It does not assume much knowledge of the Guix system nor of the Lisp language.
The reader is only expected to be familiar with the command line and to have some
basic programming knowledge.


# A "Hello World" package

The [“Defining Packages” section of the manual](https://www.gnu.org/software/guix/manual/en/html_node/Defining-Packages.html) introduces the basics of Guix
packaging.  In the following section, we will partly go over those basics again.

`GNU hello` is a dummy project that serves as an idiomatic example for
packaging.  It uses the GNU build system (`./configure && make && make install`).
Guix already provides a package definition which is a perfect example to start
with.  You can look up its declaration with `guix edit hello` from the
command line.  Let's see how it looks:

```scheme
    (define-public hello
      (package
        (name "hello")
        (version "2.10")
        (source (origin
                  (method url-fetch)
                  (uri (string-append "mirror://gnu/hello/hello-" version
                                      ".tar.gz"))
                  (sha256
                   (base32
                    "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
        (build-system gnu-build-system)
        (synopsis "Hello, GNU world: An example GNU package")
        (description
         "GNU Hello prints the message \"Hello, world!\" and then exits.  It
    serves as an example of standard GNU coding practices.  As such, it supports
    command-line arguments, multiple languages, and so on.")
        (home-page "https://www.gnu.org/software/hello/")
        (license gpl3+)))
```

As you can see, most of it is rather straightforward.  But let's review the
fields together:

-   **name:** The project name.  Using Scheme conventions, we prefer to keep it
    lower case, without underscore and using dash-separated words.
-   **source:** This field contains a description of the source code origin.  The
    `origin` record contains these fields:
    1.  The method, here `url-fetch` to download via HTTP/FTP, but other methods
        exist, such as `git-fetch` for Git repositories.
    2.  The URI, which is typically some `https://` location for `url-fetch`.  Here
        the special `mirror://gnu` refers to a set of well known locations, all of
        which can be used by Guix to fetch the source, should some of them fail.
    3.  The `sha256` checksum of the requested file.  This is essential to ensure
        the source is not corrupted.  Note that Guix works with base32 strings,
        hence the call to the `base32` function.
-   **build-system:** This is where the power of abstraction provided by the Scheme
    language really shines: in this case, the `gnu-build-system`
    abstracts away the famous `./configure && make && make
                      install` shell invocations.  Other build systems include the
    `trivial-build-system` which does not do anything and requires
    from the packager to program all the build steps, the
    `python-build-system`, the `emacs-build-system`, [and many
    more](https://www.gnu.org/software/guix/manual/en/html_node/Build-Systems.html).
-   **synopsis:** It should be a concise summary of what the package
    does.  For many packages a tagline from the
    project's home page can be used as the synopsis.
-   **description:** Same as for the synopsis, it's fine to re-use the project
    description from the homepage.  Note that Guix uses Texinfo
    syntax.
-   **home-page:** Use HTTPS if available.
-   **license:** See `guix/licenses.scm` in the project source for a full list.

Time to build our first package!  Nothing fancy here for now: we will stick to a
dummy "my-hello", a copy of the above declaration.

As with the ritualistic "Hello World" taught with most programming languages,
this will possibly be the most "manual" approach.  We will work out an ideal
setup later; for now we will go the simplest route.

Save the following to a file `my-hello.scm`.

```scheme
    (use-modules (guix packages)
                 (guix download)
                 (guix build-system gnu)
                 (guix licenses))

    (package
      (name "my-hello")
      (version "2.10")
      (source (origin
                (method url-fetch)
                (uri (string-append "mirror://gnu/hello/hello-" version
                                    ".tar.gz"))
                (sha256
                 (base32
                  "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
      (build-system gnu-build-system)
      (synopsis "Hello, Guix world: An example custom Guix package")
      (description
       "GNU Hello prints the message \"Hello, world!\" and then exits.  It
    serves as an example of standard GNU coding practices.  As such, it supports
    command-line arguments, multiple languages, and so on.")
      (home-page "https://www.gnu.org/software/hello/")
      (license gpl3+))
```

We will explain the extra code in a moment.

Feel free to play with the different values of the various fields.  If you
change the source, you'll need to update the checksum.  Indeed, Guix refuses to
build anything if the given checksum does not match the computed checksum of the
source code.  To obtain the correct checksum of the package declaration, we
need to download the source, compute the sha256 checksum and convert it to
base32.

Thankfully, Guix can automate this task for us; all we need is to provide the
URI:

```sh
    $ guix download mirror://gnu/hello/hello-2.10.tar.gz

    Starting download of /tmp/guix-file.JLYgL7
    From https://ftpmirror.gnu.org/gnu/hello/hello-2.10.tar.gz...
    following redirection to `https://mirror.ibcp.fr/pub/gnu/hello/hello-2.10.tar.gz'...
     …10.tar.gz  709KiB                                 2.5MiB/s 00:00 [##################] 100.0%
    /gnu/store/hbdalsf5lpf01x4dcknwx6xbn6n5km6k-hello-2.10.tar.gz
    0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i
```

In this specific case that the output tells us which mirror was chosen.
If the result of the above command is not the same as in the above snippet,
update your `my-hello` declaration accordingly.

Note that GNU package tarballs come with an OpenPGP signature, so you
should definitely check the signature of this tarball with `gpg` to
authenticate it before going further:

```sh
	$ guix download mirror://gnu/hello/hello-2.10.tar.gz.sig

	Starting download of /tmp/guix-file.03tFfb
	From https://ftpmirror.gnu.org/gnu/hello/hello-2.10.tar.gz.sig...
	following redirection to `https://ftp.igh.cnrs.fr/pub/gnu/hello/hello-2.10.tar.gz.sig'...
	 ….tar.gz.sig  819B                                                                                                                       1.2MiB/s 00:00 [##################] 100.0%
	/gnu/store/rzs8wba9ka7grrmgcpfyxvs58mly0sx6-hello-2.10.tar.gz.sig
	0q0v86n3y38z17rl146gdakw9xc4mcscpk8dscs412j22glrv9jf
	$ gpg --verify /gnu/store/rzs8wba9ka7grrmgcpfyxvs58mly0sx6-hello-2.10.tar.gz.sig /gnu/store/hbdalsf5lpf01x4dcknwx6xbn6n5km6k-hello-2.10.tar.gz
	gpg: Signature made Sun 16 Nov 2014 01:08:37 PM CET
	gpg:                using RSA key A9553245FDE9B739
	gpg: Good signature from "Sami Kerola <kerolasa@iki.fi>" [unknown]
	gpg:                 aka "Sami Kerola (http://www.iki.fi/kerolasa/) <kerolasa@iki.fi>" [unknown]
	gpg: WARNING: This key is not certified with a trusted signature!
	gpg:          There is no indication that the signature belongs to the owner.
	Primary key fingerprint: 8ED3 96E3 7E38 D471 A005  30D3 A955 3245 FDE9 B739
```

Now you can happily run

```sh
    $ guix package --install-from-file=my-hello.scm
```

You should now have `my-hello` in your profile!

```sh
    $ guix package --list-installed=my-hello
    my-hello	2.10	out
    /gnu/store/f1db2mfm8syb8qvc357c53slbvf1g9m9-my-hello-2.10
```

We've gone as far as we could without any knowledge of Scheme.  Now is the right
time to introduce the minimum we need from the language before we can proceed.


# A Scheme crash-course

As we've seen above, basic packages don't require much Scheme knowledge, if none
at all.  But as you progress and your desire to write more and more complex
packages grows, it will become both necessary and empowering to hone your Lisper
skills.

Since an extensive Lisp course is very much out of the scope of this tutorial,
we will only cover some basics here.

Guix uses the Guile implementation of Scheme.  To start playing with the
language, install it with `guix package --install guile` and start a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) by
running `guile` from the command line.

Alternatively you can also run `guix environment --ad-hoc guile -- guile` if
you'd rather not have Guile installed in your user profile.

In the following examples we use the `>` symbol to denote the REPL prompt, that
is, the line reserved for user input.  See [the Guile manual](https://www.gnu.org/software/guile/manual/html_node/Using-Guile-Interactively.html) for more details on
the REPL.

-   Scheme syntax boils down to a tree of expressions (or *s-expression* in Lisp
    lingo).  An expression can be a literal such numbers and strings, or a
    compound which is a parenthesized list of compounds and literals.  `#t` and
    `#f` stand for the booleans "true" and "false", respectively.

    Examples of valid expressions:

        > "Hello World!"
        "Hello World!"
        > 17
        17
        > (display (string-append "Hello " "Guix" "\n"))
        "Hello Guix!"

-   This last example is a function call embedded in another function call.  When
    a parenthesized expression is evaluated, the first term is the function and
    the rest are the arguments passed to the function.  Every function returns the
    last evaluated expression as value.

-   Anonymous functions are declared with the `lambda` term:

        > (lambda (x) (* x x))
        #<procedure 120e348 at <unknown port>:24:0 (x)>

    The above lambda returns the square of its argument.  Since everything is an
    expression, the `lambda` expression returns an anonymous function, which can
    in turn be applied to an argument:

        > ((lambda (x) (* x x)) 3)
        9

-   Anything can be assigned a global name with `define`:

        > (define a 3)
        > (define square (lambda (x) (* x x)))
        > (square a)
        9

-   Procedures can be defined more concisely with the following syntax:

        (define (square x) (* x x))

-   A list structure can be created with the `list` procedure:

        > (list 2 a 5 7)
        (2 3 5 7)

-   The *quote* disables evaluation of a parenthesized expression: the first term
    is not called over the other terms.  Thus it effectively returns a list of
    terms.

        > '(display (string-append "Hello " "Guix" "\n"))
        (display (string-append "Hello " "Guix" "\n"))
        > '(2 a 5 7)
        (2 a 5 7)

-   The *quasiquote* disables evaluation of a parenthesized expression until a
    colon re-enables it.  Thus it provides us with fine-grained control over what
    is evaluated and what is not.

        > `(2 a 5 7 (2 ,a 5 ,(+ a 4)))
        (2 a 5 7 (2 3 5 7))

    Note that the above result is a list of mixed elements: numbers, symbols (here
    `a`) and the last element is a list itself.

-   Multiple variables can be named locally with `let`:

        > (define x 10)
        > (let ((x 2)
                (y 3))
            (list x y))
        (2 3)
        > x
        10
        > y
        ERROR: In procedure module-lookup: Unbound variable: y

    Use `let*` to allow later variable declarations to refer to earlier
    definitions.

        > (let* ((x 2)
                 (y (* x 3)))
            (list x y))
        (2 6)

-   The keyword syntax is `#:`, it is used to create unique identifiers.  See also
    the [Keywords section in the Guile manual](https://www.gnu.org/software/guile/manual/html_node/Keywords.html).

-   The percentage `%` is typically used for read-only global variables in the
    build stage.  Note that it is merely a convention, like `_` in C.  Scheme Lisp
    treats `%` exactly the same as any other letter.

-   Modules are created with `define-module`.  For instance

        (define-module (guix build-system ruby)
          #:use-module (guix store)
          #:export (ruby-build
                    ruby-build-system))

    defines the module `ruby` which must be located in
    `guix/build-system/ruby.scm` somewhere in `GUILE_LOAD_PATH`.  It depends on
    the `(guix store)` module and it exports two symbols, `ruby-build` and
    `ruby-build-system`.

For a more detailed introduction, check out [Scheme at a Glance](http://www.troubleshooters.com/codecorn/scheme_guile/hello.htm), by Steve Litt.

One of the reference Scheme books is the seminal *Structure and Interpretation
of Computer Programs*, by Harold Abelson and Gerald Jay Sussman, with Julie
Sussman.  You'll find a free copy [online](https://mitpress.mit.edu/sites/default/files/sicp/index.html), together with [videos of the lectures
by the authors](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/).  The book is available in Texinfo format as the `sicp` Guix
package.  Go ahead, run `guix package --install sicp` and start reading with
`info sicp` (or with the Emacs Info reader).  An unofficial ebook [is also
available](https://sarabander.github.io/sicp/).

You'll find more books, tutorials and other resources at <https://schemers.org/>.


# Setup

Now that we know some Scheme basics we can detail the different possible setups
for working on Guix packages.

There are several ways to set up a Guix packaging environment.

We recommend you work directly on the Guix source checkout since it makes it
easier for everyone to contribute to the project.

But first, let's look at other possibilities.


### Local file

This is what we previously did with `my-hello`.  Now that we know more Scheme,
let's explain the leading chunks.  As stated in `guix package --help`:

```scheme
    -f, --install-from-file=FILE
                           install the package that the code within FILE
                           evaluates to
```

Thus the last expression *must* return a package, which is the case in our
earlier example.

The `use-modules` expression tells which of the modules we need in the file.
Modules are a collection of values and procedures.  They are commonly called
"libraries" or "packages" in other programming languages.


### GUIX_PACKAGE_PATH

*Note: Starting from Guix 0.16, the more flexible Guix "channels" are the
preferred way and supersede `GUIX_PACKAGE_PATH`.  See below.*

It can be tedious to specify the file from the command line instead of simply
calling `guix package --install my-hello` as you would do with the official
packages.

Guix makes it possible to streamline the process by adding as many "package
declaration paths" as you want.

Create a directory, say `~./guix-packages` and add it to the `GUIX_PACKAGE_PATH`
environment variable:

```sh
    $ mkdir ~/guix-packages
    $ export GUIX_PACKAGE_PATH=~/guix-packages
```

To add several directories, separate them with a colon (`:`).

Our previous `my-hello` needs some adjustments though:

```scheme
    (define-module (my-hello)
      #:use-module (guix licenses)
      #:use-module (guix packages)
      #:use-module (guix build-system gnu)
      #:use-module (guix download))

    (define-public my-hello
      (package
        (name "my-hello")
        (version "2.10")
        (source (origin
                  (method url-fetch)
                  (uri (string-append "mirror://gnu/hello/hello-" version
                                      ".tar.gz"))
                  (sha256
                   (base32
                    "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
        (build-system gnu-build-system)
        (synopsis "Hello, Guix world: An example custom Guix package")
        (description
         "GNU Hello prints the message \"Hello, world!\" and then exits.  It
    serves as an example of standard GNU coding practices.  As such, it supports
    command-line arguments, multiple languages, and so on.")
        (home-page "https://www.gnu.org/software/hello/")
        (license gpl3+)))
```

Note that we have assigned the package value to an exported variable name with
`define-public`.  This is effectively assigning the package to the `my-hello`
variable so that it can be referenced, among other as dependency of other
packages.

If you use `guix package --install-from-file=my-hello.scm` on the above file, it
will fail because the last expression, `define-public`, does not return a
package.  If you want to use `define-public` in this use-case nonetheless, make
sure the file ends with an evaluation of `my-hello`:

```scheme
    ; ...
    (define-public my-hello
      ; ...
      )

    my-hello
```

This last example is not very typical.

Now `my-hello` should be part of the package collection like all other official
packages.  You can verify this with:

```sh
    $ guix package --show=my-hello
```


### Guix channels

Guix 0.16 features channels, which is very similar to `GUIX_PACKAGE_PATH` but
provides better integration and provenance tracking.  Channels are not
necessarily local, they can be maintained as a public Git repository for
instance.  Of course, several channels can be used at the same time.

See the [“Channels” section in the manual](http://guix.info/manual/en/Channels.html) for setup details.


### Direct checkout hacking

Working directly on the Guix project is recommended: it reduces the friction
when the time comes to submit your changes upstream to let the community benefit
from your hard work!

Unlike most software distributions, the Guix repository holds in one place both
the tooling (including the package manager) and the package definitions.  This
choice was made so that it would give developers the flexibility to modify the
API without breakage by updating all packages at the same time.  This reduces
development inertia.

Check out the official [Git](https://git-scm.com/) repository:

```sh
    $ git clone https://git.savannah.gnu.org/git/guix.git
```

In the rest of this article, we use `$GUIX_CHECKOUT` to refer to the location of
the checkout.

Follow the instruction from the ["Contributing" chapter](https://www.gnu.org/software/guix/manual/en/html_node/Contributing.html) in the manual to set up the
repository environment.

Once ready, you should be able to use the package definitions from the
repository environment.

Feel free to edit package definitions found in `$GUIX_CHECKOUT/gnu/packages`.

The `$GUIX_CHECKOUT/pre-inst-env` script lets you use `guix` over the package
collection of the repository.

-   Search packages, such as Ruby:

        $ cd $GUIX_CHECKOUT
        $ ./pre-inst-env guix package --list-available=ruby
            ruby    1.8.7-p374      out     gnu/packages/ruby.scm:119:2
            ruby    2.1.6   out     gnu/packages/ruby.scm:91:2
            ruby    2.2.2   out     gnu/packages/ruby.scm:39:2

-   Build a package, here Ruby version 2.1:

        $ ./pre-inst-env guix build --keep-failed ruby@2.1
        /gnu/store/c13v73jxmj2nir2xjqaz5259zywsa9zi-ruby-2.1.6

-   Install it to your user profile:

        $ ./pre-inst-env guix package --install ruby@2.1

-   Check for common mistakes:

        $ ./pre-inst-env guix lint ruby@2.1

Guix strives at maintaining a high packaging standard; when contributing to the
Guix project, remember to

-   follow the [coding style](https://www.gnu.org/software/guix/manual/en/html_node/Coding-Style.html),
-   and review the [check list](https://www.gnu.org/software/guix/manual/en/html_node/Submitting-Patches.html) from the manual.

Once you are happy with the result, you are welcome to send your contribution to
make it part of Guix.  This process is also detailed in the [manual](https://www.gnu.org/software/guix/manual/en/html_node/Contributing.html).

It's a community effort so the more join in, the better Guix becomes!


# Extended example

The above "Hello World" example is as simple as it goes.  Packages can be more
complex than that and Guix can handle more advanced scenarios.  Let's look at
another, more sophisticated package (slightly modified from the source):

```scheme
    (define-module (gnu packages version-control)
      #:use-module ((guix licenses) #:prefix license:)
      #:use-module (guix utils)
      #:use-module (guix packages)
      #:use-module (guix git-download)
      #:use-module (guix build-system cmake)
      #:use-module (gnu packages ssh)
      #:use-module (gnu packages web)
      #:use-module (gnu packages pkg-config)
      #:use-module (gnu packages python)
      #:use-module (gnu packages compression)
      #:use-module (gnu packages tls))

    (define-public my-libgit2
      (let ((commit "e98d0a37c93574d2c6107bf7f31140b548c6a7bf")
            (revision "1"))
        (package
          (name "my-libgit2")
          (version (git-version "0.26.6" revision commit))
          (source (origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/libgit2/libgit2/")
                          (commit commit)))
                    (file-name (git-file-name name version))
                    (sha256
                     (base32
                      "17pjvprmdrx4h6bb1hhc98w9qi6ki7yl57f090n9kbhswxqfs7s3"))
                    (patches (search-patches "libgit2-mtime-0.patch"))
                    (modules '((guix build utils)))
                    (snippet '(begin
                                ;; Remove bundled software.
                                (delete-file-recursively "deps")
                                #t))))
          (build-system cmake-build-system)
          (outputs '("out" "debug"))
          (arguments
           `(#:tests? #t                            ; Run the test suite (this is the default)
             #:configure-flags '("-DUSE_SHA1DC=ON") ; SHA-1 collision detection
             #:phases
             (modify-phases %standard-phases
               (add-after 'unpack 'fix-hardcoded-paths
                 (lambda _
                   (substitute* "tests/repo/init.c"
                     (("#!/bin/sh") (string-append "#!" (which "sh"))))
                   (substitute* "tests/clar/fs.h"
                     (("/bin/cp") (which "cp"))
                     (("/bin/rm") (which "rm")))
                   #t))
               ;; Run checks more verbosely.
               (replace 'check
                 (lambda _ (invoke "./libgit2_clar" "-v" "-Q")))
               (add-after 'unpack 'make-files-writable-for-tests
                   (lambda _ (for-each make-file-writable (find-files "." ".*")))))))
          (inputs
           `(("libssh2" ,libssh2)
             ("http-parser" ,http-parser)
             ("python" ,python-wrapper)))
          (native-inputs
           `(("pkg-config" ,pkg-config)))
          (propagated-inputs
           ;; These two libraries are in 'Requires.private' in libgit2.pc.
           `(("openssl" ,openssl)
             ("zlib" ,zlib)))
          (home-page "https://libgit2.github.com/")
          (synopsis "Library providing Git core methods")
          (description
           "Libgit2 is a portable, pure C implementation of the Git core methods
    provided as a re-entrant linkable library with a solid API, allowing you to
    write native speed custom Git applications in any language with bindings.")
          ;; GPLv2 with linking exception
          (license license:gpl2))))
```

(In those cases were you only want to tweak a few fields from a package
definition, you should rely on inheritance instead of copy-pasting everything.
See below.)

Let's discuss those fields in depth.


## `git-fetch` method

Unlike the `url-fetch` method, `git-fetch` expects a `git-reference` which takes
a Git repository and a commit.  The commit can be any Git reference such as
tags, so if the `version` is tagged, then it can be used directly.  Sometimes
the tag is prefixed with a `v`, in which case you'd use `(commit (string-append
"v" version))`.

To ensure that the source code from the Git repository is stored in a unique
directory with a readable name we use `(file-name (git-file-name name
version))`.

Note that there is also a `git-version` procedure that can be used to derive the
version when packaging programs for a specific commit.


## Snippets

Snippets are quoted (i.e. non-evaluated) Scheme code that are a means of patching
the source.  They are a Guix-y alternative to the traditional `.patch` files.
Because of the quote, the code in only evaluated when passed to the Guix daemon
for building.

There can be as many snippet as needed.

Snippets might need additional Guile modules which can be imported from the
`modules` field.


## Inputs

First, a syntactic comment: See the quasi-quote / comma syntax?

```scheme
    (native-inputs
     `(("pkg-config" ,pkg-config)))
```

is equivalent to

```scheme
    (native-inputs
     (list (list "pkg-config" pkg-config)))
```

You'll mostly see the former because it's shorter.

There are 3 different input types.  In short:

-   **native-inputs:** Required for building but not runtime &#x2013; installing a package
    through a substitute won't install these inputs.
-   **inputs:** Installed in the store but not in the profile, as well as being
    present at build time.
-   **propagated-inputs:** Installed in the store and in the profile, as well as
    being present at build time.

See [the package reference in the manual](https://www.gnu.org/software/guix/manual/en/html_node/package-Reference.html) for more details.

The distinction between the various inputs is important: if a dependency can be
handled as an *input* instead of a *propagated input*, it should be done so, or
else it "pollutes" the user profile for no good reason.

For instance, a user installing a graphical program that depends on a
command line tool might only be interested in the graphical part, so there is no
need to force the command line tool into the user profile.  The dependency is a
concern to the package, not to the user.  *Inputs* make it possible to handle
dependencies without bugging the user by adding undesired executable files (or
libraries) to their profile.

Same goes for *native-inputs*: once the program is installed, build-time
dependencies can be safely garbage-collected.
It also matters when a substitute is available, in which case only the *inputs*
and *propagated inputs* will be fetched: the *native inputs* are not required to
install a package from a substitute.


## Outputs

Just like how a package can have multiple inputs, it can also produce multiple
outputs.

Each output corresponds to a separate directory in the store.

The user can choose which output to install; this is useful to save space or
to avoid polluting the user profile with unwanted executables or libraries.

Output separation is optional.  When the `outputs` field is left out, the
default and only output (the complete package) is referred to as `"out"`.

Typical separate output names include `debug` and `doc`.

It's advised to separate outputs only when you've shown it's worth it: if the
output size is significant (compare with `guix size`) or in case the package is
modular.


## Build system arguments

The `arguments` is a keyword-value list used to configure the build process.

The simplest argument `#:tests?` can be used to disable the test suite when
building the package.  This is mostly useful when the package does not feature
any test suite.  It's strongly recommended to keep the test suite on if there is
one.

Another  common argument is `:make-flags`, which specifies a list of flags to
append when running make, as you would from the command line.  For instance, the
following flags

```scheme
    #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out"))
                       "CC=gcc")
```

translate into

```sh
    $ make CC=gcc prefix=/gnu/store/...-<out>
```

This sets the C compiler to `gcc` and the `prefix` variable (the installation
directory in Make parlance) to `(assoc-ref %outputs "out")`, which is a build-stage
global variable pointing to the destination directory in the store (something like
`/gnu/store/...-my-libgit2-20180408`).

Similarly, it's possible to set the "configure" flags.

```scheme
    #:configure-flags '("-DUSE_SHA1DC=ON")
```

The `%build-inputs` variable is also generated in scope.  It's an association
table that maps the input names to their store directories.

The `phases` keyword lists the sequential steps of the build system.  Typically
phases include `unpack`, `configure`, `build`, `install` and `check`.  To know
more about those phases, you need to work out the appropriate build system
definition in `$GUIX_CHECKOUT/guix/build/gnu-build-system.scm`:

```scheme
    (define %standard-phases
      ;; Standard build phases, as a list of symbol/procedure pairs.
      (let-syntax ((phases (syntax-rules ()
                             ((_ p ...) `((p . ,p) ...)))))
        (phases set-SOURCE-DATE-EPOCH set-paths install-locale unpack
                bootstrap
                patch-usr-bin-file
                patch-source-shebangs configure patch-generated-file-shebangs
                build check install
                patch-shebangs strip
                validate-runpath
                validate-documentation-location
                delete-info-dir-file
                patch-dot-desktop-files
                install-license-files
                reset-gzip-timestamps
                compress-documentation)))
```

Or from the REPL:

```scheme
    > (add-to-load-path "/path/to/guix/checkout")
    > ,module (guix build gnu-build-system)
    > (map first %standard-phases)
    (set-SOURCE-DATE-EPOCH set-paths install-locale unpack bootstrap
	patch-usr-bin-file patch-source-shebangs configure
	patch-generated-file-shebangs build check install patch-shebangs strip
	validate-runpath validate-documentation-location delete-info-dir-file
	patch-dot-desktop-files install-license-files reset-gzip-timestamps
	compress-documentation)
```

If you want to know more about what happens during those phases, consult the
associated procedures.

For instance, as of this writing the definition of `unpack` for the GNU build
system is

```scheme
    (define* (unpack #:key source #:allow-other-keys)
      "Unpack SOURCE in the working directory, and change directory within the
    source.  When SOURCE is a directory, copy it in a sub-directory of the current
    working directory."
      (if (file-is-directory? source)
          (begin
            (mkdir "source")
            (chdir "source")

            ;; Preserve timestamps (set to the Epoch) on the copied tree so that
            ;; things work deterministically.
            (copy-recursively source "."
                              #:keep-mtime? #t))
          (begin
            (if (string-suffix? ".zip" source)
                (invoke "unzip" source)
                (invoke "tar" "xvf" source))
            (chdir (first-subdirectory "."))))
      #t)
```

Note the `chdir` call: it changes the working directory to where the source was
unpacked.
Thus every phase following the `unpack` will use the source as a working
directory, which is why we can directly work on the source files.
That is to say, unless a later phase changes the working directory to something
else.

We modify the list of `%standard-phases` of the build system with the
`modify-phases` macro as per the list of specified modifications, which may have
the following forms:

-   `(add-before PHASE NEW-PHASE PROCEDURE)`: Run `PROCEDURE` named `NEW-PHASE` before `PHASE`.
-   `(add-after PHASE NEW-PHASE PROCEDURE)`: Same, but afterwards.
-   `(replace PHASE PROCEDURE)`.
-   `(delete PHASE)`.

The `PROCEDURE` supports the keyword arguments `inputs` and `outputs`.  Each
input (whether *native*, *propagated* or not) and output directory is referenced
by their name in those variables.  Thus `(assoc-ref outputs "out")` is the store
directory of the main output of the package.  A phase procedure may look like
this:

```scheme
    (lambda* (#:key inputs outputs #:allow-other-keys)
      (let (((bash-directory (assoc-ref inputs "bash"))
             (output-directory (assoc-ref outputs "out"))
             (doc-directory (assoc-ref outputs "doc"))
      ; ...
      #t)
```

The procedure must return `#t` on success.  It's brittle to rely on the return
value of the last expression used to tweak the phase because there is no
guarantee it would be a `#t`.  Hence the trailing `#t` to ensure the right value
is returned on success.


## Code staging

The astute reader may have noticed the quasi-quote and comma syntax in the
argument field.  Indeed, the build code in the package declaration should not be
evaluated on the client side, but only when passed to the Guix daemon.  This
mechanism of passing code around two running processes is called [code staging](https://arxiv.org/abs/1709.00833).


## "Utils" functions

When customizing `phases`, we often need to write code that mimics the
equivalent system invocations (`make`, `mkdir`, `cp`, etc.) commonly used during
regular "Unix-style" installations.

Some like `chmod` are native to Guile.  See the [Guile reference manual](https://www.gnu.org/software/guile/manual/guile.html) for a
complete list.

Guix provides additional helper functions which prove especially handy in the
context of package management.

Some of those functions can be found in
`$GUIX_CHECKOUT/guix/guix/build/utils.scm`.  Most of them mirror the behaviour
of the traditional Unix system commands:

-   **which:** Like the `which` system command.
-   **find-files:** Akin to the `find` system command.
-   **mkdir-p:** Like `mkdir -p`, which creates all parents as needed.
-   **install-file:** Similar to `install` when installing a file to a (possibly
    non-existing) directory.  Guile has `copy-file` which works
    like `cp`.
-   **copy-recursively:** Like `cp -r`.
-   **delete-file-recursively:** Like `rm -rf`.
-   **invoke:** Run an executable.  This should be used instead of `system*`.
-   **with-directory-excursion:** Run the body in a different working directory,
    then restore the previous working directory.
-   **substitute\*:** A "sed-like" function.


## Module prefix

The license in our last example needs a prefix: this is because of how the
`license` module was imported in the package, as `#:use-module ((guix licenses)
#:prefix license:)`.  The [Guile module import mechanism](https://www.gnu.org/software/guile/manual/html_node/Using-Guile-Modules.html) gives the user full
control over namespacing: this is needed to avoid clashes between, say, the
`zlib` variable from `licenses.scm` (a *license* value) and the `zlib` variable
from `compression.scm` (a *package* value).


# Other build systems

What we've seen so far covers the majority of packages using a build system
other than the `trivial-build-system`.  The latter does not automate anything
and leaves you to build everything manually.  This can be more demanding and we
won't cover it here for now, but thankfully it is rarely necessary to fall back
on this system.

For the other build systems, such as ASDF, Emacs, Perl, Ruby and many more, the
process is very similar to the GNU build system except for a few specialized
arguments.

Learn more about build systems in

-   [the manual, section 4.2 Build systems](https://www.gnu.org/software/guix/manual/en/html_node/Build-Systems.html#Build-Systems),
-   the source code in the `$GUIX_CHECKOUT/guix/build` and
    `$GUIX_CHECKOUT/guix/build-system` directories.


# Programmable and automated package definition

We can't repeat it enough: having a full-fledged programming language at hand
empowers us in ways that reach far beyond traditional package management.

Let's illustrate this with some awesome features of Guix!


## Recursive importers

You might find some build systems good enough that there is little to do at all
to write a package, to the point that it becomes repetitive and tedious after a
while.  A *raison d'être* of computers is to replace human beings at those
boring tasks.  So let's tell Guix to do this for us and create the package
definition of an R package from CRAN (the output is trimmed for conciseness):

```sh
    $ guix import cran --recursive walrus

    (define-public r-mc2d
        ; ...
        (license gpl2+)))

    (define-public r-jmvcore
        ; ...
        (license gpl2+)))

    (define-public r-wrs2
        ; ...
        (license gpl3)))

    (define-public r-walrus
      (package
        (name "r-walrus")
        (version "1.0.3")
        (source
          (origin
            (method url-fetch)
            (uri (cran-uri "walrus" version))
            (sha256
              (base32
                "1nk2glcvy4hyksl5ipq2mz8jy4fss90hx6cq98m3w96kzjni6jjj"))))
        (build-system r-build-system)
        (propagated-inputs
          `(("r-ggplot2" ,r-ggplot2)
            ("r-jmvcore" ,r-jmvcore)
            ("r-r6" ,r-r6)
            ("r-wrs2" ,r-wrs2)))
        (home-page "https://github.com/jamovi/walrus")
        (synopsis "Robust Statistical Methods")
        (description
          "This package provides a toolbox of common robust statistical tests, including robust descriptives, robust t-tests, and robust ANOVA.  It is also available as a module for 'jamovi' (see <https://www.jamovi.org> for more information).  Walrus is based on the WRS2 package by Patrick Mair, which is in turn based on the scripts and work of Rand Wilcox.  These analyses are described in depth in the book 'Introduction to Robust Estimation & Hypothesis Testing'.")
        (license gpl3)))
```

The recursive importer won't import packages for which Guix already has package
definitions, except for the very first.

Not all applications can be packaged this way, only those relying on a select
number of supported systems.  Read about the full list of importers in the [guix
import section](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-import.html) of the manual.


## Automatic update

Guix can be smart enough to check for updates on systems it knows.  It can
report outdated package definitions with

```sh
    $ guix refresh hello
```

In most cases, updating a package to a newer version requires little more than
changing the version number and the checksum.  Guix can do that automatically as
well:

```sh
    $ guix refresh hello --update
```


## Inheritance

If you've started browsing the existing package definitions, you might have
noticed that a significant number of them have a `inherit` field:

```scheme
    (define-public adwaita-icon-theme
      (package (inherit gnome-icon-theme)
        (name "adwaita-icon-theme")
        (version "3.26.1")
        (source (origin
                  (method url-fetch)
                  (uri (string-append "mirror://gnome/sources/" name "/"
                                      (version-major+minor version) "/"
                                      name "-" version ".tar.xz"))
                  (sha256
                   (base32
                    "17fpahgh5dyckgz7rwqvzgnhx53cx9kr2xw0szprc6bnqy977fi8"))))
        (native-inputs
         `(("gtk-encode-symbolic-svg" ,gtk+ "bin")))))
```

All unspecified fields are inherited from the parent package.  This is very
convenient to create alternative packages, for instance with different source,
version or compilation options.


# Getting help

Sadly, some applications can be tough to package.  Sometimes they need a patch to
work with the non-standard filesystem hierarchy enforced by the store.
Sometimes the tests won't run properly.  (They can be skipped but this is not
recommended.)  Other times the resulting package won't be reproducible.

Should you be stuck, unable to figure out how to fix any sort of packaging
issue, don't hesitate to ask the community for help.

See the [Guix homepage](https://www.gnu.org/software/guix/contact/) for information on the mailing lists, IRC, etc.


# Conclusion

This tutorial was an showcase of the sophisticated package management that Guix
boasts.  At this point we have mostly restricted this introduction to the
`gnu-build-system` which is a core abstraction layer on which more advanced
abstractions are based.

Now where do we go from here?  Next we ought to dissect the innards of the build
system by removing all abstractions, using the `trivial-build-system`: this
should give us a thorough understanding of the process before investigating some
more advanced packaging techniques and edge cases.

Other features worth exploring are the interactive editing and debugging
capabilities of Guix provided by the Guile REPL.

Those fancy features are no strict requirements and this is a good point to stop
for now.  With what we've introduced here you should be well armed to package
lots of programs.  You can get started right away and hopefully we will see your
contributions soon!


# References

-   The [package reference in the manual](https://www.gnu.org/software/guix/manual/en/html_node/Defining-Packages.html)

-   [Pjotr’s hacking guide to GNU Guix](https://gitlab.com/pjotrp/guix-notes/blob/master/HACKING.org)

-   ["GNU Guix: Package without a
    scheme!"](https://www.gnu.org/software/guix/guix-ghm-andreas-20130823.pdf),
    by Andreas Enge


# About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
manager for the GNU system.  The Guix System Distribution or GuixSD is
an advanced distribution of the GNU system that relies on GNU Guix and
[respects the user's
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  Guix uses low-level
mechanisms from the Nix package manager, except that packages are
defined as native [Guile](https://www.gnu.org/software/guile) modules,
using extensions to the [Scheme](http://schemers.org) language.  GuixSD
offers a declarative approach to operating system configuration
management, and is highly customizable and hackable.

GuixSD can be used on an i686, x86_64, ARMv7, and AArch64 machines.  It
is also possible to use Guix on top of an already installed GNU/Linux
system, including on mips64el and aarch64.
