title: Upcoming Talk: "Everyday Use of GNU Guix"
date: 2018-09-29 17:00
author: Chris Marusich
tags: Talks
---

At [SeaGL](https://seagl.org/) 2018, Chris Marusich will present a
talk introducing GNU Guix to people of all skill levels and
backgrounds.  SeaGL is an annual GNU/Linux conference in
[Seattle](https://en.wikipedia.org/wiki/Seattle).  Attendance is
gratis.

If you're in the Seattle area, please consider coming!  Even if you
can't make it in person, the talk will be recorded and later made
available on the SeaGL website, so you can watch it at your
convenience after it's been uploaded.

# Abstract

[Everyday Use of GNU
Guix](https://osem.seagl.org/conferences/seagl2018/program/proposals/526)

In this talk, I will introduce GNU Guix: a liberating, dependable, and
hackable package manager that follows the "purely functional software
deployment model" pioneered by Nix.

I will demonstrate some common use cases of Guix and show you how I
use it in my everyday life. In addition, I will briefly explain the
basic idea behind the functional model and how it enables Guix to
provide useful features like the following:

* Transactional upgrades and roll-back of installed software.
* Unprivileged users can simultaneously install multiple versions of
  software.
* Transparently build from source or download pre-built binaries.
* Installed software is bootstrappable, trustable, and auditable all
  the way down to your compiler's compiler.
* Eliminates an entire class of "works on my system" type problems.

No prior knowledge of Guix, Nix, or the functional model is
required. When you leave this talk, I hope you will have a basic
understanding of what Guix is, how to use it, and why it will help
make your life brighter.

# Schedule

The talk will take place at the following time and location:

* Date: November 10th (Saturday), 2018
* Time: 13:30 local time
* Duration: 50 minutes
* Room Number: 5102
* Conference: [Seattle GNU/Linux Conference 2018](https://seagl.org)

For details, please refer to [the official SeaGL page for the
talk](https://osem.seagl.org/conferences/seagl2018/program/proposals/526).

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

GuixSD can be used on an i686, x86_64 and armv7 machines.  It is also
possible to use Guix on top of an already installed GNU/Linux system,
including on mips64el and aarch64.
