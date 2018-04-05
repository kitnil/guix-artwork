title: Guix & reproducible builds at LibrePlanet 2018
date: 2018-04-05 14:00
author: Ludovic Courtès
tags: Talks, Reproducible builds
---

[LibrePlanet](https://libreplanet.org/2018/), the yearly free software
conference organized by the Free Software Foundation, took place a week
ago.  Among the many great talks and workshops, David Thompson, a core
Guix developer also working as a DevOps, presented many aspects of Guix
and GuixSD in his talk, _Practical, verifiable software freedom with
GuixSD_
([video](https://media.libreplanet.org/u/libreplanet/m/practical-verifiable-software-freedom-with-guixsd/),
[slides](https://www.gnu.org/software/guix/guix-libreplanet-practical-freedom-20180325.pdf)).

In a similar domain, Chris Lamb, current [Debian](https://debian.org)
Project Leader and a driving force behind the [Reproducible Builds
effort](https://reproducible-builds.org), gave a talk entitled _You
think you're not a target? A tale of three developers..._
([video](https://media.libreplanet.org/u/libreplanet/m/you-think-you-re-not-a-target-a-tale-of-three-developers/),
[slides](https://salsa.debian.org/reproducible-builds/reproducible-presentations/tree/master/2018-03-24-libreplanet-2018)).

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

GuixSD can be used on an i686, x86_64 and armv7 machines.  It is also
possible to use Guix on top of an already installed GNU/Linux system,
including on mips64el and aarch64.
