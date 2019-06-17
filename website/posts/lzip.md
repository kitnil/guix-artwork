title: Substitutes are now available as lzip
date: 2019-06-17 14:30
author: Ludovic Courtès
tags: Scheme API
---

For a long time, our build farm at ci.guix.gnu.org has been delivering
[substitutes](https://www.gnu.org/software/guix/manual/en/html_node/Substitutes.html)
(pre-built binaries) compressed with gzip.  Gzip was never the best
choice in terms of compression ratio, but it was a reasonable and
convenient choice: it’s rock-solid, and zlib made it easy for us to have
[Guile
bindings](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/zlib.scm)
to perform in-process compression in our multi-threaded [`guix
publish`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-publish.html)
server.

With the exception of building software from source, downloads take the
most time of Guix package upgrades.  If users can download less,
upgrades become faster, and happiness ensues.  Time has come to improve
on this, and starting from early June, Guix can publish and fetch
[lzip](https://nongnu.org/lzip/)-compressed substitutes, in addition to
gzip.

# Lzip

[Lzip](https://nongnu.org/lzip/) is a relatively little-known
compression format, initially developed by Antonio Diaz Diaz ca. 2013.
It has several C and C++ implementations with surprisingly few lines of
code, which is always reassuring.  One of its distinguishing features is
a very good compression ratio with reasonable CPU and memory
requirements, [according to benchmarks published by the
authors](https://nongnu.org/lzip/lzip_benchmark.html).

[Lzlib](https://nongnu.org/lzip/lzlib.html) provides a well-documented C
interface and Pierre Neidhardt set out to write bindings for that
library, which eventually landed as the [`(guix lzlib)`
module](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/lzlib.scm).

With this in place we were ready to start migrating our tools, and then
our build farm, to lzip compression, so we can all enjoy smaller
downloads.  Well, easier said than done!

# Migrating

The compression format used for substitutes is not a core component like
it can be in “traditional” binary package formats [such as
`.deb`](https://lwn.net/Articles/789449/) since Guix is conceptually a
“source-based” distro.  However, deployed Guix installations did not
support lzip, so we couldn’t just switch our build farm to lzip
overnight; we needed to devise a transition strategy.

Guix asks for the availability of substitutes over HTTP.  For example, a
question such as:

> “Dear server, do you happen to have a binary of
> `/gnu/store/6yc4ngrsig781bpayax2cg6pncyhkjpq-emacs-26.2` that I could download?”

translates into prose to an HTTP GET of
[https://ci.guix.gnu.org/6yc4ngrsig781bpayax2cg6pncyhkjpq.narinfo](https://ci.guix.gnu.org/6yc4ngrsig781bpayax2cg6pncyhkjpq.narinfo),
which returns something like:

```
StorePath: /gnu/store/6yc4ngrsig781bpayax2cg6pncyhkjpq-emacs-26.2
URL: nar/gzip/6yc4ngrsig781bpayax2cg6pncyhkjpq-emacs-26.2
Compression: gzip
NarHash: sha256:0h2ibqpqyi3z0h16pf7ii6l4v7i2wmvbrxj4ilig0v9m469f6pm9
NarSize: 134407424
References: 2dk55i5wdhcbh2z8hhn3r55x4873iyp1-libxext-1.3.3 …
FileSize: 48501141
System: x86_64-linux
Deriver: 6xqibvc4v8cfppa28pgxh0acw9j8xzhz-emacs-26.2.drv
Signature: 1;berlin.guixsd.org;KHNpZ25hdHV…
```

(This narinfo format is inherited from [Nix](https://nixos.org/nix/) and
implemented
[here](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/scripts/substitute.scm?id=121d9d1a7a2406a9b1cbe22c34343775f5955b34#n283)
and
[here](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/scripts/publish.scm?id=121d9d1a7a2406a9b1cbe22c34343775f5955b34#n265).)
This tells us we can download the actual binary from
`/nar/gzip/…-emacs-26.2`, and that it will be about 46 MiB (the
`FileSize` field.)  This is what `guix publish` serves.

The trick we came up with was to allow `guix publish` to advertise
several URLs, one per compression format.  Thus, for recently-built
substitutes, we get something [like
this](https://ci.guix.gnu.org/mvhaar2iflscidl0a66x5009r44fss15.narinfo):

```
StorePath: /gnu/store/mvhaar2iflscidl0a66x5009r44fss15-gimp-2.10.12
URL: nar/gzip/mvhaar2iflscidl0a66x5009r44fss15-gimp-2.10.12
Compression: gzip
FileSize: 30872887
URL: nar/lzip/mvhaar2iflscidl0a66x5009r44fss15-gimp-2.10.12
Compression: lzip
FileSize: 18829088
NarHash: sha256:10n3nv3clxr00c9cnpv6x7y2c66034y45c788syjl8m6ga0hbkwy
NarSize: 94372664
References: 05zlxc7ckwflz56i6hmlngr86pmccam2-pcre-8.42 …
System: x86_64-linux
Deriver: vi2jkpm9fd043hm0839ibbb42qrv5xyr-gimp-2.10.12.drv
Signature: 1;berlin.guixsd.org;KHNpZ25hdHV…
```

Notice that there are two occurrences of the `URL`, `Compression`, and
`FileSize` fields: one for gzip, and one for lzip.  Old Guix instances
will just pick the first one, gzip; newer Guix will pick whichever
supported method provides the smallest `FileSize`, usually lzip.  This
will make migration trivial in the future, should we add support for
other compression methods.

Users need to upgrade their Guix daemon to benefit from lzip.  On a
“foreign distro”, simply run `guix pull` as root.  On standalone Guix
systems, run `guix pull && sudo guix system reconfigure
/etc/config.scm`.  In both cases, the daemon has to be restarted, be it
with `systemctl restart guix-daemon.service` or with `herd restart
guix-daemon`.

# First impressions

This new gzip+lzip scheme has been deployed on ci.guix.gnu.org for a
week.  Specifically, we run `guix publish -C gzip:9 -C lzip:9`, meaning
that we use the highest compression ratio for both compression methods.

Currently, only a small subset of the package substitutes are available
as both lzip and gzip; those that were already available as gzip have
not been recompressed.  The following Guile program that taps into the
API of [`guix
weather`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-weather.html)
allows us to get some insight:

```scheme
(use-modules (gnu) (guix)
             (guix monads)
             (guix scripts substitute)
             (srfi srfi-1)
             (ice-9 match))

(define all-packages
  (@@ (guix scripts weather) all-packages))

(define package-outputs
  (@@ (guix scripts weather) package-outputs))

(define (fetch-lzip-narinfos)
  (mlet %store-monad ((items (package-outputs (all-packages))))
    (return
     (filter (lambda (narinfo)
               (member "lzip" (narinfo-compressions narinfo)))
             (lookup-narinfos "https://ci.guix.gnu.org" items)))))

(define (lzip/gzip-ratio narinfo)
  (match (narinfo-file-sizes narinfo)
    ((gzip lzip)
     (/ lzip gzip))))

(define (average lst)
  (/ (reduce + 0 lst)
     (length lst) 1.))
```

Let’s explore this at the
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop):

```scheme
scheme@(guile-user)> (define lst
                       (with-store s
                         (run-with-store s (fetch-lzip-narinfos))))
computing 9,897 package derivations for x86_64-linux...
updating substitutes from 'https://ci.guix.gnu.org'... 100.0%
scheme@(guile-user)> (length lst)
$4 = 2275
scheme@(guile-user)> (average (map lzip/gzip-ratio lst))
$5 = 0.7398994395478715
```

As of this writing, around 20% of the package substitutes are
available as lzip, so take the following stats with a grain of salt.
Among those, the lzip-compressed substitute is on average 26% smaller
than the gzip-compressed one.  What if we consider only packages bigger
than 5 MiB uncompressed?

```scheme
scheme@(guile-user)> (define biggest
                       (filter (lambda (narinfo)
                                 (> (narinfo-size narinfo)
                                    (* 5 (expt 2 20))))
                               lst))
scheme@(guile-user)> (average (map lzip/gzip-ratio biggest))
$6 = 0.5974238562384483
scheme@(guile-user)> (length biggest)
$7 = 440
```

For those packages, lzip yields substitutes that are 40% smaller on
average.  Pretty nice!  Lzip decompression is slightly more
CPU-intensive than gzip decompression, but downloads are
bandwidth-bound, so the benefits clearly outweigh the costs.

# Going forward

The switch from gzip to lzip has the potential to make upgrades “feel”
faster, and that is great in itself.

Fundamentally though, we’ve always been looking in this project at
peer-to-peer solutions with envy.  Of course, the main motivation is to
have a community-supported and resilient infrastructure, rather than a
centralized one, and that vision goes [hand-in-hand with reproducible
builds](https://www.gnu.org/software/guix/blog/2017/reproducible-builds-a-status-update/).

We started working on [an extension to publish and fetch
substitutes](https://issues.guix.gnu.org/issue/33899) over
[IPFS](https://ipfs.io/).  Thanks to its content-addressed nature, IPFS
has the potential to further reduce the amount of data that needs to be
downloaded on an upgrade.

The good news is that IPFS developers are also [interested in working
with package manager
developers](https://github.com/ipfs/package-managers), and I bet
there’ll be interesting discussions at [IPFS
Camp](https://camp.ipfs.io/) in just a few days.  We’re eager to pursue
our IPFS integration work, and if you’d like to join us and hack the
good hack, [let’s get in
touch!](https://www.gnu.org/software/guix/contact/)


#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
manager and an advanced distribution of the GNU system that [respects
user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the kernel Linux, or it
can be used as a standalone operating system distribution for i686,
x86_64, ARMv7, and AArch64 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](http://schemers.org) language.
