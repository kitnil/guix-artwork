title: Meet Guix at FOSDEM
slug: meet-guix-at-fosdem-2018
date: 2018-01-29 16:00:00
author: Ludovic Courtès
tags: FOSDEM
---

GNU Guix will be present at [FOSDEM](https://fosdem.org/2018/) in the
coming days with a couple of talks:

  - [_The many ways of using Guix
    packages_](https://fosdem.org/2018/schedule/event/usingguix/) on
    Saturday afternoon, in the “package management” track, will be _a
    guide to which ways might suit you_, by Christopher Baines.
  - On Sunday morning, in the “high-performance computing” track, I will
    give a talk entitled [_Tying software deployment to scientific
    workflows_](https://fosdem.org/2018/schedule/event/guix_workflows/).
    I will explain how Guix can be thought of as a programming language
    extension that can be used to _make software deployment a
    first-class citizen_, as illustrated by the [Guix Workflow
    Language](http://guixwl.org), and how this fits with [our vision for
    HPC](https://guix-hpc.bordeaux.inria.fr).
	
We are also organizing a *one-day Guix workshop* where contributors and
enthusiasts will meet, thanks to the efforts of Manolis Ragkousis and
Pjotr Prins.  The workshop takes place on Friday Feb. 2nd at the
Institute of Cultural Affairs (ICAB) in Brussels.  The morning will be
dedicated to talks—among other things, we are happy to welcome Eelco
Dolstra, the founder of [Nix](https://nixos.org/nix/), without which
Guix would not exist today.  The afternoon will be a more informal
discussion and hacking session.

Attendance to the workshop is free and open to everyone, though you are
invited to register.  Check out [the workshop’s wiki
page](https://libreplanet.org/wiki/Group:Guix/FOSDEM2018) for the
program, registration, and practical info.  Hope to see you in Brussels!

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
