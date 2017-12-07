title: GNU Guix and GuixSD 0.14.0 released
date: 2017-12-07 14:00
author: Ludovic Courtès
slug: gnu-guix-and-guixsd-0.14.0-released
tags: Releases
---
We are pleased to announce the new release of GNU Guix and GuixSD,
version 0.14.0!

The release comes with [GuixSD ISO-9660 installation
images](https://www.gnu.org/software/guix/manual/html_node/System-Installation.html),
a [virtual machine image of
GuixSD](https://www.gnu.org/software/guix/manual/html_node/Running-GuixSD-in-a-VM.html),
and with tarballs to install the package manager on top of your
GNU/Linux distro, either [from
source](https://www.gnu.org/software/guix/manual/html_node/Requirements.html)
or [from
binaries](https://www.gnu.org/software/guix/manual/html_node/Binary-Installation.html).

It’s been 6 months since the previous release, during which 88 people
contributed code and packages.  The highlights include:

  - The GuixSD installation image is now [available as an ISO-9660
    image](https://www.gnu.org/software/guix/manual/html_node/USB-Stick-and-DVD-Installation.html),
    which can either be written to a USB stick or burnt on a DVD.
    Previously we would only provide a USB stick image.  This was long
    overdue!  Such images can now be generated with [`guix system disk-image
    --file-system-type=iso9660`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-system.html).
  - Several user interface improvements, notably: [`guix
    package`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-package.html)
    reports how much is going to be downloaded, warns if the user has
    insufficient disk space, [reports about package
    collisions](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27271)
    early on; `guix package --search` sorts results by relevance, and
    there’s a new [`guix system
    search`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-system.html)
    command to search for GuixSD system services; `guix system` reports
    [incorrect file system labels and
    UUIDs](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28706) and
    provides
    [hints](https://lists.gnu.org/archive/html/guix-devel/2017-11/msg00139.html)
    for unbound variables.
  - GuixSD has a new [bootloader
    API](https://www.gnu.org/software/guix/manual/html_node/Bootloader-Configuration.html#Bootloader-Configuration),
    which has allowed GuixSD to gain support not just for GRUB (UEFI and
    BIOS) but also U-Boot and Extlinux.  This paves the way to a GuixSD
    port to ARM-based devices, which fearless hackers are soon going to
    [make a
    reality](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29409)!
  - To make it easier for newcomers to get started defining packages,
    there’s a new [`guix import
    json`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-import.html)
    command that takes JSON-formatted package metadata as input and
    produces the usual [package
    definition](https://www.gnu.org/software/guix/manual/html_node/Defining-Packages.html).
  - [`guix-daemon`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix_002ddaemon.html) 
	has a new `--listen` option, which is particularly useful when
	[installing Guix on a
	cluster](https://guix-hpc.bordeaux.inria.fr/blog/2017/11/installing-guix-on-a-cluster/).
  - There are [1,211 new
    packages](https://www.gnu.org/software/guix/packages/)—oh, and the
    package list as well as the whole web site have been revamped too,
    thanks to our intrepid web designer
    [sirgazil](https://sirgazil.bitbucket.io/).
  - There’s a dozen new [GuixSD
    services](https://www.gnu.org/software/guix/manual/html_node/Services.html)
    as well: [git-http and
    cgit](https://www.gnu.org/software/guix/manual/html_node/Version-Control-Services.html),
    [libvirt](https://www.gnu.org/software/guix/manual/html_node/Virtualization-Services.html),
    [Memcached](https://www.gnu.org/software/guix/manual/html_node/Database-Services.html),
    and
    [Murmur](https://www.gnu.org/software/guix/manual/html_node/Telephony-Services.html)
    to name a few.
  - Many [bug fixes](https://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=guix#_4_2_5)!

See the [release
announcement](https://lists.gnu.org/archive/html/guix-devel/2017-12/msg00100.html)
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

GuixSD can be used on an i686 or x86_64 machine.  It is also possible to
use Guix on top of an already installed GNU/Linux system, including on
mips64el, armv7, and aarch64.

