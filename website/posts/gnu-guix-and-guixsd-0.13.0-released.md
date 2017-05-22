title: GNU Guix and GuixSD 0.13.0 released
date: 2017-05-22 15:30
author: Ludovic Courtès
slug: gnu-guix-and-guixsd-0.13.0-released
tags: release
---
We are pleased to announce the new release of GNU Guix and GuixSD,
version 0.13.0!

The release comes with
[GuixSD USB installation images](https://www.gnu.org/software/guix/manual/html_node/System-Installation.html),
a
[virtual machine image of GuixSD](https://www.gnu.org/software/guix/manual/html_node/Running-GuixSD-in-a-VM.html),
and with tarballs to install the package manager on top of your
GNU/Linux distro, either
[from source](https://www.gnu.org/software/guix/manual/html_node/Requirements.html)
or
[from binaries](https://www.gnu.org/software/guix/manual/html_node/Binary-Installation.html).

It’s been 5 months since the previous release, during which 83 people
contribute code and packages.  The highlights include:

  - Guix now supports aarch64 (64-bit ARM processors).  This release
    does not include a binary installation tarball though, and our build
    farm does not provide aarch64
    [substitutes](https://www.gnu.org/software/guix/manual/html_node/Substitutes.html)
    yet.  We are looking for aarch64 hardware to address this.  Please
    [get in touch with us](https://www.gnu.org/software/guix/donate/) if
    you can help!
  - Likewise, this release no longer includes a mips64el tarball, though
    Guix still supports that platform.  We do not know whether we will
    continue to support mips64el in the long run; if you’d like to weigh
    in, please email us on `guix-devel@gnu.org`!
  - The GuixSD installation image now supports UEFI.  GuixSD can also be
    installed on Btrfs now.
  - GuixSD has support to run system services (daemons) in isolated
    containers as a way to mitigate the harm that can be done by
    vulnerabilities in those daemons.  See
    [this article from April](https://www.gnu.org/software/guix/news/running-system-services-in-containers.html).
  - A new
    [`guix pack`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-pack.html)
    command to create standalone binary bundles is available.  We
    [presented it in March](https://www.gnu.org/software/guix/news/creating-bundles-with-guix-pack.html).
  - Guix now runs on the
    [brand-new 2.2 series of GNU Guile](https://www.gnu.org/software/guile/news/gnu-guile-220-released.html).
    The transition led to hiccups that we have been addressing, in particular
    [for users of `guix pull`](https://lists.gnu.org/archive/html/guix-devel/2017-05/msg00123.html).
    Among other things though, the
    [noticeable performance improvement](https://lists.gnu.org/archive/html/guix-devel/2017-04/msg00427.html)
    that comes for free is welcome!
  - [`guix publish`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-publish.html),
    which is what we use to distribute binaries, has a new `--cache`
    operation mode that improves performance when distributing binaries
    to a large number of users, as is the case of our build farm.
  - Many reproducibility issues found in packages have been
    addressed—more on that in a future post.
  - 840 new packages, leading to
    [a total of 5,400+](https://www.gnu.org/software/guix/packages/),
    and many updates, including glibc 2.25, Linux-libre 4.11, and GCC 7.
  - New
    [system services](https://www.gnu.org/software/guix/manual/html_node/Services.html)
    for
    [Redis](https://www.gnu.org/software/guix/manual/html_node/Database-Services.html#index-redis_002dservice_002dtype),
    [Exim](https://www.gnu.org/software/guix/manual/html_node/Mail-Services.html#index-exim_002dservice_002dtype),
    [Open vSwitch](https://www.gnu.org/software/guix/manual/html_node/Networking-Services.html#index-openvswitch_002dservice_002dtype), and more.  The interface of existing
    services, notably that of the
    [NGINX service](https://www.gnu.org/software/guix/manual/html_node/Web-Services.html),
    has been greatly improved.
  - Many [bug fixes](https://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=guix#_4_2_5)!

See the release announcement for details.

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

