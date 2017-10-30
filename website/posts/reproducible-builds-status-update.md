title: Reproducible builds: a status update
date: 2017-10-31 11:30
author: Ludovic Courtès
tags: Reproducibility
---

With the yearly [Reproducible Build
Summit](https://reproducible-builds.org/events/berlin2017/) starting
today, now’s a good time for an update on what has happened in Guix land
in that area!

[Isolated build
environments](https://www.gnu.org/software/guix/manual/html_node/Features.html)
are very helpful to achieve [reproducible
builds](https://reproducible-builds.org/docs/definition/), but they are
not sufficient: timestamps and non-determinism can still make a package
build non-reproducible.  Developers can rely on [`guix build
--check`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-build.html)
and [`guix
challenge`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-challenge.html)
to identify non-reproducible builds.

This article provides an overview of the progress made to fix
non-reproducibility issues in packages over the year, and then goes on
to show a very concrete way for Guix to take advantage of reproducible
builds.

#### Building reproducibly

Tools that produce build artifacts occupy a key role: if their output is
non-reproducible, then lots of packages that use them will be
non-reproducible as a result.  Among those packages, we fixed:

  - [GNU R](https://bugs.gnu.org/25598) (timestamps in `.rds` files and
    man pages; random temporary file names recorded in generated files);
  - [GNU Guile](https://bugs.gnu.org/20272) (order-sensitive symbol
    generation during macro expansion);
  - [Ghostscript](https://bugs.gnu.org/27563) (timestamps and UUIDs in
    generated PDF files);
  - [GNU groff](https://bugs.gnu.org/27593) (timestamps in generated
    files);
  - [gdk-pixbuf](https://bugs.gnu.org/25414) (unsorted directory entries
    ending up in generated cache files);
  - [Perl build
    system](https://lists.gnu.org/archive/html/guix-devel/2016-11/msg00874.html)
    (`perllocal.pod` files were produced in a non-deterministic way).

Sometimes we think that an issue is rare and we embark on a trip to fix
individual packages that are affected… until we realize that it’s common
enough to deserve a global, once-and-for-all fix.  This is what happened
with timestamps in gzip headers: after blissfully assuming that “almost
everyone” uses the [`-n` flag of
gzip](https://www.gnu.org/software/gzip/manual/gzip.html#Invoking-gzip),
we finally
[introduced](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=1d636d63193b66f67fbd0f10315cd61818f132c1)
a build phase to automatically strip timestamps for gzip headers—this is
a subset of what Debian’s
[`strip-nondeterminism`](https://packages.debian.org/sid/strip-nondeterminism)
achieves, but hey, Scheme integration matters to us!

There’s a number of well-identified issues left to be addressed: [Python
bytecode](https://bugs.gnu.org/22533), [GTK+ icon them
caches](https://bugs.gnu.org/29076), [TeX
Live](https://bugs.gnu.org/28173), and more.  Often, the [issue
database](https://anonscm.debian.org/cgit/reproducible/notes.git/tree/)
initiated by Debian is a great resource to find about issues and fixes.

#### And the result is…

We [recently gained a new build farm called
`berlin.guixsd.org`](https://lists.gnu.org/archive/html/guix-devel/2017-08/msg00128.html),
which is slated to replace our existing build farm at
`mirror.hydra.gnu.org`.  Having set it up as an independent build
farm—`berlin` does not download binaries from `hydra`—we can *challenge*
build reproducibility by comparing the binaries produced on each of
these build farms.  Comparing the results of two independent build
farms, with different hardware and kernel versions, maximizes the
chances to catch all sorts of non-reproducibility issues.  The result
with today’s master is… *drum rolls*

```
$ guix challenge $(guix package -A | cut -f1) \
    --substitute-urls="https://mirror.hydra.gnu.org https://berlin.guixsd.org"

…

6,501 store items were analyzed:
  - 5,048 (77.6%) were identical
  - 533 (8.2%) differed
  - 920 (14.2%) were inconclusive
```

We’re somewhere between 78% and 91%—[not as good as Debian
yet](https://isdebianreproducibleyet.com/), but we know what to do next!
The inconclusive comparisons here can be due to a package that failed to
build on one machine, for instance because its test suite fails in a
non-deterministic way, or simply because one of the build farms is
lagging behind.  `guix challenge` lists all the problematic packages,
which makes it easy to retrieve the faulty binaries and investigate.

#### Reproducible builds = faster downloads!

There’s a very practical advantage to reproducible builds: anyone who
[publishes](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-publish.html)
binaries is in essence a *mirror* of our build farm.

Until now, Guix’s public key infrastructure (PKI) was used pretty
rigidly: you could download binaries from a server *if and only if* you
had previously [authorized its public
key](https://www.gnu.org/software/guix/manual/html_node/Substitutes.html).
So to download binaries from the person next to you, you would first
need to retrieve their public key and authorize it.  In addition to
being inconvenient, it has the drawback of being an all-or-nothing
decision: you would now accept *any* binary coming from that person.
Can’t we do better?

We realized there’s an easy way to exploit the mirroring property
mentioned above: assuming I trust binaries from `mirror.hydra.gnu.org`,
then I can download from anyone *who publishes the exact same binaries*.
Put this way, it seems obvious, but it required some adjustments to the
substitute code.

To understand what’s going on, let’s look at the metadata `guix publish`
produces, in a format inherited from [Hydra](https://nixos.org/hydra/):

```
$ wget -q -O - https://berlin.guixsd.org/8kib1cirdv0qbmn9hdkjzjfx3n5nw1yw.narinfo
StorePath: /gnu/store/8kib1cirdv0qbmn9hdkjzjfx3n5nw1yw-sed-4.4
URL: nar/gzip/8kib1cirdv0qbmn9hdkjzjfx3n5nw1yw-sed-4.4
Compression: gzip
NarHash: sha256:18v7dgny1xna7f53mbkj8bk4y2f00l5rjk2k6hj166kjv964lz7r
NarSize: 637360
References: 3x53yv4v144c9xp02rs64z7j597kkqax-gcc-5.4.0-lib 8kib1cirdv0qbmn9hdkjzjfx3n5nw1yw-sed-4.4 n6nvxlk2j8ysffjh3jphn1k5silnakh6-glibc-2.25
FileSize: 218663
System: x86_64-linux
Deriver: pi8686q63rwr4md90vm3qxwhk2g2fvqa-sed-4.4.drv
Signature: 1;berlin.guixsd.org;KHNpZ25hdHVyZSAKIChkYXRhIAogIChmbGFncyByZmM2OTc5KQogIChoYXNoIHNoYTI1NiAjQTRDRjUyMTVGNzlBOEUxRkFBNjIyOEQwQjk0QjMyMTZCRkY1RjA1NkQxMzZENUEzNTFGM0I2OTYzQzc1MzQzMiMpCiAgKQogKHNpZy12YWwgCiAgKGVjZHNhIAogICAociAjMDFDM0NGMEIzRUMwNkIwRUNGMTJEMTU4MkNCMzA2RjkzMEU2Njc1NDNFOEQ2NkZCRjhDRUY4QkQwMkMzOTg1NCMpCiAgIChzICMwRTg2MUEyRjI3MDg2MjVBRDkzMDg5RjFFRjE4NzUwQjIzQjM0RTA5MkFFRkQ3RTlFNkZCMjlCMkMwMURFNjI5IykKICAgKQogICkKIChwdWJsaWMta2V5IAogIChlY2MgCiAgIChjdXJ2ZSBFZDI1NTE5KQogICAocSAjOEQxNTZGMjk1RDI0QjBEOUE4NkZBNTc0MUE4NDBGRjJEMjRGNjBGN0I2QzQxMzQ4MTRBRDU1NjI1OTcxQjM5NCMpCiAgICkKICApCiApCg==
```

This “narinfo” gives us, among other things, the hash of the sed binary
that `berlin.guixsd.org` obtained, the URL where it can be downloaded,
and a signature on this metadata (a [base64-encoded canonical
s-expression](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/pki.scm#n134)).

Guix has supported the ability to specify several *substitute servers*
for a while, with `--substitute-urls`, but it would filter out narinfos
signed by an unauthorized key.  The main change was thus to keep
narinfos *with a hash identical to that advertised by one of the
authorized narinfos*.  Thus, if I run:

```
$ guix build sed \
  --substitute-urls="https://somebody.example.org https://mirror.hydra.gnu.org"
```

Guix will fetch a narinfo from both URLs.  If `somebody`’s narinfo
claims the same hash as `hydra`, then Guix will download the actual
binary from `somebody`—which, hopefully, may be faster than downloading
from `hydra`.  Of course, when the download completes, `guix-daemon`
verifies that the hash is really as advertised in the narinfo, such that
`somebody.example.org` cannot tweak me into downloading a different
binary.

#### The future

This feature
[landed](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=a9468b422b6df2349a3f4d1451c9302c3d77011b)
in September, and will be in the forthcoming Guix release.

Among the ideas [floating around](https://bugs.gnu.org/28324), one is to
have `guix publish` advertise itself on the local network *via* Avahi.
Guix could, optionally, discover neighboring `guix publish` instances
and add them to its list of substitute servers.  Binaries could
sometimes be downloaded from the local network, which should be faster.

More generally, the role of our build farm shifts from providing
binaries to providing *meta-data about binaries*.  We can entirely
decouple the choice of a meta-data server from the choice of a binary
provider.

Longer-term, binaries could very well be downloaded from a
content-addressed store such as [IPFS](https://ipfs.io/) or
[GNUnet](https://gnunet.org/) without having to forego our existing
infrastructure.  Peer-to-peer distribution of binaries has been on our
mind [for a
while](https://www.gnu.org/software/guix/news/gsoc-update.html), but we
hadn’t quite realized this decoupling and how it would allow us to
support a smooth transition.

These are all exciting perspectives, and a nice practical consequence of
reproducible builds!
