title: Coming events
date: 2017-10-17 12:00
author: Ludovic Courtès
tags: Talks, Reproducible builds, Research, Papers
---

Guix will be present on a few venues in the coming weeks:

  1. On October 23rd, I (Ludovic Courtès) will be at
     [GPCE](https://conf.researchr.org/track/gpce-2017/gpce-2017-GPCE-2017),
     an academic conference co-located with SPLASH in Vancouver, Canada.
     I will present the paper [_Code Staging in
     GNU Guix_](https://hal.inria.fr/hal-01580582/en), which discusses
     the motivation for and genesis of
     [G-expressions](https://www.gnu.org/software/guix/manual/html_node/G_002dExpressions.html),
     as well as recent improvements.  It’s an honor to be presenting
     before an audience of experts in the field!
  2. Christopher Baines will be at [freenode
     #live](https://freenode.live/) in Bristol, UK, among well-known
     free software activists from a variety of organizations and
     projects.  Christopher will give a talk on October 29th to give an
     overview of Guix and GuixSD.
  3. On October 31st, Ricardo Wurmus, Jan Nieuwenhuizen, and possibly
     more Guix hackers will join a dozen free software projects at the
     [third Reproducible Build
     Summit](https://reproducible-builds.org/events/berlin2017/) in
     Berlin, Germany.  As in
     [previous](/software/guix/news/reproducible-build-summit-2nd-edition.html)
     [years](https://lists.gnu.org/archive/html/guix-devel/2015-12/msg00107.html),
     we expect it to be a good time to share tips & tricks as well as a
     longer-term vision with our fellow hackers!

If you’re around in Vancouver, Bristol, or Berlin, let’s get in touch!
:-)

#### About GNU Guix

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

GuixSD can be used on an i686 or x86_64 machine.  It is also possible to
use Guix on top of an already installed GNU/Linux system, including on
mips64el, armv7, and aarch64.
