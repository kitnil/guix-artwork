title: aarch64 build machines donated
date: 2018-01-25 22:00
author: Ludovic Courtès
tags: ARM, Build farm
---

Good news!  We got a present for our build farm in the form of two
SoftIron OverDrive 1000 aarch64 machines donated by ARM Holdings.  One
of them is already running behind our new build farm, which distributes
binaries from `https://berlin.guixsd.org`, and the other one should be
operational soon.

The OverDrive has 4 cores and 8 GiB of RAM.  It comes in a fancy
VCR-style case, which looks even more fancy with the obligatory
stickers:

![An OverDrive 1000 with its fancy Guix stickers.](https://www.gnu.org/software/guix/static/blog/img/overdrive.jpg)

A few months ago we reported on the [status of the aarch64
port](https://www.gnu.org/software/guix/blog/2017/state-of-aarch64-on-guix/),
which was already looking good.  The latest
[releases](https://www.gnu.org/software/guix/blog/2017/gnu-guix-and-guixsd-0.14.0-released/)
include a pre-built binary tarball of Guix for aarch64.

Until now though, the project’s official build farms were not building
aarch64 binaries.  Consequently, Guix on aarch64 would build everything
from source.  We are glad that this is about to be fixed.  We will need
to expand our build capacity for this architecture and for ARMv7 as
well, and [you too can help](https://www.gnu.org/software/guix/donate/)!

Thanks to ARM Holdings and in particular to Richard Henwood for
contributing to our build infrastructure!

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
