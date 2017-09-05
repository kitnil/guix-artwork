title: Announcing Guix-HPC
date: 2017-09-05 12:00
author: Ludovic Courtès, Roel Janssen, Pjotr Prins, Ricardo Wurmus
tags: HPC Research Reproducibility Containers
---

Today, Inria, the Max Delbrück Center for Molecular Medicine (MDC), and
the Utrecht Bioinformatics Center (UBC)
are
[announcing a joint effort](https://www.inria.fr/en/centre/bordeaux/news/towards-reproducible-software-environments-in-hpc-with-guix) to
consolidate GNU Guix for reproducible scientific workflows in
high-performance computing (HPC).  The three research institutes have
been using Guix and contributing to it.  The new effort,
dubbed [Guix-HPC](https://guix-hpc.bordeaux.inria.fr/), hopes to extend
Guix functionality to better address the needs of HPC users, as well as
augmenting its package collection.

Guix was not initially designed with HPC in mind.  However, we believe
it has many good properties both for flexible software deployment on
clusters, and as a foundation for reproducible scientific workflows.
The [Guix-HPC blog](https://guix-hpc.bordeaux.inria.fr/) will regularly
feature articles with HPC “howtos” and stories about our achievements.
We are thrilled by the opportunities this new effort offers!

To learn more, visit
the [Guix-HPC Web site](https://guix-hpc.bordeaux.inria.fr/about.html).

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
