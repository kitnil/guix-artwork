title: GNU Guix and GuixSD 0.15.0 released
date: 2018-07-06 14:00
author: Ludovic Courtès
slug: gnu-guix-and-guixsd-0.15.0-released
tags: Releases
---
We are pleased to announce the new release of GNU Guix and GuixSD,
version 0.15.0!  This release brings us close to what we wanted to have
for 1.0, so it’s probably one of the last zero-dot-something releases.

The release comes with [GuixSD ISO-9660 installation
images](https://www.gnu.org/software/guix/manual/en/html_node/System-Installation.html),
a [virtual machine image of
GuixSD](https://www.gnu.org/software/guix/manual/en/html_node/Running-GuixSD-in-a-VM.html),
and with tarballs to install the package manager on top of your
GNU/Linux distro, either [from
source](https://www.gnu.org/software/guix/manual/en/html_node/Requirements.html)
or [from
binaries](https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html).

It’s been 7 months (much too long!) since the previous release, during
which 100 people contributed code and packages.  The highlights include:

  - The unloved [`guix pull`
    command](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-pull.html),
    which allows users to upgrade Guix and its package collection, has
    been overhauled and we hope you will like it.  We’ll discuss these
    enhancements in another post soon but suffice to say that the new
    `guix pull` now supports rollbacks (just like `guix package`) and
    that the new `--list-generations` option allows you to visualize
    past upgrades.  It’s also faster, not as fast as we’d like though,
    so we plan to optimize it further in the near future.
  - [`guix
    pack`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-pack.html)
    can now produce [relocatable
    binaries](https://www.gnu.org/software/guix/blog/2018/tarballs-the-ultimate-container-image-format/).
    With `-f squashfs` it can now produce images stored as SquashFS file
    systems.  These images can then be executed by
    [Singularity](http://singularity.lbl.gov), a “container engine”
    deployed on some high-performance computing clusters.
  - GuixSD now runs on ARMv7 and AArch64 boxes!  We do not provide an
    installation image though because the details depend on the board
    you’re targeting, so you’ll have to build the image yourself
    [following the
    instructions](https://www.gnu.org/software/guix/manual/en/html_node/Building-the-Installation-Image.html).
    On ARMv7 it typically uses U-Boot, while AArch64 boxes such as the
    [OverDrive](https://www.gnu.org/software/guix/blog/2018/aarch64-build-machines-donated/)
    rely on the EFI-enabled GRUB.  Bootloader definitions are available
    [for many
    boards](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/bootloader/u-boot.scm#n100)—Novena,
    A20 OLinuXino, BeagleBone, and _even
    [NES](https://en.wikipedia.org/wiki/NES_Classic_Edition)_.
  - We further improved error-reporting and hints provided by [`guix
    system`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-system.html).
    For instance, it will now suggest upfront kernel modules that should
    be added to the initrd—previously, you could install a system that
    would fail to boot simply because the initrd lacked drivers for your
    hard disk.
  - OS configuration has been simplified with the introduction of things
    like the [`initrd-modules`
    field](https://www.gnu.org/software/guix/manual/en/html_node/operating_002dsystem-Reference.html)
    and the [`file-system-label` construct](https://www.gnu.org/software/guix/manual/en/html_node/File-Systems.html#File-Systems).
  - There’s a new [`guix system
    docker-image`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-system.html)
    command that does exactly what you’d expect.  :-)
  - There’s a dozen new [GuixSD
    services](https://www.gnu.org/software/guix/manual/en/html_node/Services.html):
    the [Enlightenment and MATE
    desktops](https://www.gnu.org/software/guix/manual/en/html_node/Desktop-Services.html),
    [Apache httpd](https://www.gnu.org/software/guix/manual/en/html_node/Web-Services.html),
    support for transparent emulation with QEMU through the
    [`qemu-binfmt`
    service](https://www.gnu.org/software/guix/manual/en/html_node/Virtualization-Services.html#Transparent-Emulation-with-QEMU),
    [OpenNTPD](https://www.gnu.org/software/guix/manual/en/html_node/Networking-Services.html#index-OpenNTPD),
    and more.
  - There were 1,200 new packages, so we’re now [close to 8,000
    packages](https://www.gnu.org/software/guix/packages/).
  - Many [bug
    fixes](https://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=guix#_4_2_5)!
  - The manual is now partially [translated into
    French](https://www.gnu.org/software/guix/manual/fr/html_node/) and
    you can help [translate it into your native
    language](https://translationproject.org/domain/guix-manual.html) by
    [joining the Translation
    Project](https://translationproject.org/html/translators.html).

See the [release
announcement](https://lists.gnu.org/archive/html/guix-devel/2018-07/msg00082.html)
for details.


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

GuixSD can be used on an i686, x86_64, ARMv7, and AArch64 machines.  It
is also possible to use Guix on top of an already installed GNU/Linux
system, including on mips64el and aarch64.
