title: Guix welcomes Outreachy, GSoC, and Guix-HPC interns
date: 2018-04-26 17:00
author: Ludovic Courtès
tags: GSoC, Outreachy, High-performance computing
---

We are thrilled to announce that five people will join Guix as interns
over the next few months!  As part of Google’s Summer of Code (GSoC),
under the umbrella of the GNU Project, three people are joining us:

  - Tatiana Sholokhova will work on a Web interface for the Guix
    continuous integration (CI) tool,
    [Cuirass](https://git.savannah.gnu.org/cgit/guix/guix-cuirass.git),
    similar in spirit to [that of Hydra](https://hydra.gnu.org).
    Cuirass was started as part of [GSoC
    2016](https://www.gnu.org/software/guix/blog/2016/gnu-guix-welcomes-four-students-for-gsoc/).
  - uniq10 will take over the build daemon rewrite in Scheme, a project
    started as part of last year's GSoC by reepca.  The existing code
    lives in the [`guile-daemon`
    branch](https://git.savannah.gnu.org/cgit/guix.git/log/?h=guile-daemon).
    [Results from last
    year](https://lists.gnu.org/archive/html/guix-devel/2017-08/msg00267.html)
    already got us a long way towards a drop-in replacement of the
    current C++ code base.
  - Ioannis P. Koutsidis will work on implementing semantics similar to
    that of systemd unit files in [the
    Shepherd](https://www.gnu.org/software/shepherd), the “init system”
    (PID 1) used on GuixSD.

Through [Outreachy](https://www.outreachy.org), the inclusion program
for groups underrepresented in free software and tech, one person will
join:

  - Sahithi Yarlagadda will work [improving the user experience for the
    `guix package` command-line
    tool](https://www.outreachy.org/communities/cfp/gnu-guix/project/improve-the-user-experience-for-the-guix-package-c/).

Finally, we are welcoming one intern as part of the
[Guix-HPC](https://guix-hpc.bordeaux.inria.fr) effort:

  - Pierre-Antoine Rouby arrived a couple of weeks ago at
    [Inria](https://www.inria.fr/en) for a four-month internship on
    improving the user experience of Guix in high-performance computing
    (HPC) and reproducible scientific workflows.  Pierre-Antoine has
    already contributed a couple of HPC package definitions and will
    next look at tools such as
    [hpcguix-web](https://github.com/UMCUGenetics/hpcguix-web), [`guix
    pack`](https://guix-hpc.bordeaux.inria.fr/blog/2017/10/using-guix-without-being-root/),
    and more.

Gábor Boskovits, Ricardo Wurmus, and Ludovic Courtès will be their
primary mentors, and the whole Guix crowd will undoubtedly help and
provide guidance as it has always done.  Welcome to all of you!

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
