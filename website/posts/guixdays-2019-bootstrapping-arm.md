title: Guix Days: Bootstrapping ARM
date: 2019-02-22 00:00
author: Chris Marusich
tags: ARM, Guix Days, FOSDEM, Bootstrapping
---

During the [Guix
Days](https://libreplanet.org/wiki/Group:Guix/FOSDEM2019) before
[FOSDEM](https://fosdem.org/2019/), some of us discussed bootstrapping
on ARM architectures.  We focused on how to port Mes to ARM.  This
post consists of notes from that discussion.

#### Recap: i686/x86_64 Reduced Binary Seed

We started our discussion by reviewing the current status for
i686-linux and x86_64-linux.  Jan (janneke) Nieuwenhuizen gave [a
similar summary](https://fosdem.org/2019/schedule/event/gnumes/) a few
days later at FOSDEM, and you can [read the slides
online](https://fosdem.org/2019/schedule/event/gnumes/attachments/slides/2848/export/events/attachments/gnumes/slides/2848/gnu_mes_fosdem19_v2.pdf).

Previously, the size of Guix's binary seed totaled about 250 MB.  Now,
on the
[core-updates](http://git.savannah.gnu.org/cgit/guix.git/log/?h=core-updates)
branch, it's been reduced to about 130 MB.  This is nearly a 50%
reduction in size, which is great progress!  Using this 130 MB reduced
binary seed, it's currently possible to bootstrap Guix for both
i686-linux and x86_64-linux.

To bootstrap x86_64-linux, we actually "cheat" and bootstrap from the
i686-linux bootstrap binaries.  This is possible because an
x86_64-linux system can natively run i686-linux executables, and also
because Guix automatically [cross-compiles for the host
platform](http://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/commencement.scm?id=63d4ef52ebad4157817d56ccbe974da8fff81929#n1562)
during the bootstrap process.  In other words, on an x86_64-linux
system, Guix uses the i686-linux bootstrap binaries to build a
cross-compilation toolchain, which it then uses to build a final,
normal x86_64-linux toolchain.  Guix then uses this final toolchain to
build everything else for the x86_64-linux system.

That's great news for owners of i686 and x86_64 machines!  But what
about ARM?  Although we could cross-compile the bootstrap binaries for
ARM from an x86_64 machine, this isn't great because it would increase
the number of things a person or organization would have to verify in
order to audit the system.  Perhaps more importantly, it would force
owners of ARM machines to implicitly trust an x86_64 machine.  The
dominant vendors of CPUs implementing the x86_64 architecture, Intel
and AMD, both include a management engine in many of their products,
which represents a [serious risk to user
freedom](https://www.fsf.org/blogs/sysadmin/the-management-engine-an-attack-on-computer-users-freedom).

In the short term, cross-compilation is better than nothing, but in
the long term, we'd prefer to bootstrap ARM without cross-compiling
from another architecture.  Concretely, we'll need to complete at
least the following tasks.

#### TODO: Implement a Mes backend for ARM

We need to implement a new Mes backend for an ARM architecture.  We
should choose an ARM instruction set that can work on a variety of ARM
platforms with minimal fuss.  The following candidates were suggested:

- ARMv4.  We would need to avoid unaligned memory accesses because
  they can behave in different ways depending on the CPU.  This is the
  latest version of ARM that GCC 2.95 supports, which matters because
  [GCC 2.95 is the latest version that the Mes C Library
  supports](http://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/commencement.scm?id=63d4ef52ebad4157817d56ccbe974da8fff81929#n510).

- ARMv7.  If we avoid extensions and unaligned memory accesses, it
  might still work for our needs.  It was mentioned in the session
  that TinyCC will probably work with either ARMv4 or ARMv7.  In
  TinyCC, as a first step, it's probably fine to depend on ARMv7 since
  it's the most common recent architecture version (and it is more
  forgiving).  Later, if we remove unaligned accesses, it will also
  work on ARMv4 and thus on basically all ARM CPUs.

After the session concluded, Danny Milosavljevic committed some
changes to [the wip-arm branch of
mes](http://git.savannah.gnu.org/cgit/mes.git/log/?h=wip-arm) which
enabled many of the tests to pass - but some tests still fail, and you
can help finish the work!

#### TODO: Port mescc-tools to ARM, also

The mes project depends upon the
[mescc-tools](https://savannah.nongnu.org/projects/mescc-tools)
project, which also must be ported.  The mescc-tools project contains
an M1 macro assembler, which would need to be extended to support ARM
branches.  Currently, ARM branches are very broken.

#### TODO: Improve Guix integration and merge core-updates

Even if we had a new Mes backend and mescc-tools for ARM, there would
still be more to do.  The Guix integration is not quite complete - the
[core-updates](http://git.savannah.gnu.org/cgit/guix.git/log/?h=core-updates)
branch still needs to be merged with
[master](http://git.savannah.gnu.org/cgit/guix.git/log/?h=master), and
we'll need to fix any problems that arise.  Even on i686-linux, the
bottom of the bootstrap path is incomplete.  Preliminary Guix code
exists on the
[wip-bootstrap](http://git.savannah.gnu.org/cgit/guix.git/log/?h=wip-bootstrap)
branch to achieve a scheme-only bootstrap, but help would be welcome!

#### You can help!

In summary, you can help the Mes project by doing any of the following
things:

- Help implement an ARMv7 (or ARMv4) backend for Mes!  This entails
  machine code and assembly.  It should be fun for anyone who wants to
  play around close to the metal.

- Help port mescc-tools to ARM.  This entails [writing an assembler in
  M1 macro
  code](http://git.savannah.nongnu.org/cgit/mescc-tools.git/tree/HACKING?id=0a930aa47389ec842bcbe8a270ab2921a36f62d9#n33)
  and probably goes hand-in-hand with work on Mes itself.

- Help firm up
  [core-updates](http://git.savannah.gnu.org/cgit/guix.git/log/?h=core-updates)
  and merge it to
  [master](http://git.savannah.gnu.org/cgit/guix.git/log/?h=master)!
  This involves Guix package definitions and troubleshooting build
  failures.  It should be fun for anyone who wants to learn more about
  the bigger picture of how Guix bootstraps all of its software from
  the new reduced binary seed.

- Help complete the i686-linux and x86_64-linux bootstrap.  You can
  hack on the bleeding edge scheme code in the
  [wip-bootstrap](http://git.savannah.gnu.org/cgit/guix.git/log/?h=wip-bootstrap)
  branch, or maybe you can help extend the bootstrap path all the way
  down to [approximately 500 bytes of auditable assembly
  code](https://github.com/oriansj/stage0)!

There's still plenty of meaty work left to be done!  If you're
interested, [get in touch](http://bootstrappable.org/who.html) and
we'll help you get started.

#### Think Big: Bootstrapping without an OS

In addition to the immediate tasks necessary for porting Mes to ARM,
we also took some time to think about the long term hopes and dreams
of the bootstrappable project.

We discussed how in the long term, in parallel with the aforementioned
tasks, it should be possible to investigate how to bootstrap an entire
system without relying on a OS or even a kernel running on the
machine.  For example, one can imagine loading the transitive closure
of source (including a tiny, human-readable machine code program to
kick off the entire process) into a computer as a kind of "firmware
image".  When the computer runs, it would execute this "firmware
image" and eventually produce a fully bootstrapped system.

#### Think Bigger: Bootstrapping Hardware

We also briefly talked about how even after we achieve full source
software bootstrap, we will still need to tackle the problem of
"hardware bootstrap".  It isn't clear what form this will eventually
take, but surely [free hardware
design](https://www.gnu.org/philosophy/free-hardware-designs.en.html)
will play an important role in ensuring that we can trust our
hardware, too.

#### About Bootstrappable Builds and Mes

Software is bootstrappable when it does not depend on a binary seed
that cannot be built from source.  Software that is not
bootstrappable - even if it is free software - is a serious security
risk
[for](https://www.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf)
[a](https://manishearth.github.io/blog/2016/12/02/reflections-on-rusting-trust/)
[variety](https://www.quora.com/What-is-a-coders-worst-nightmare/answer/Mick-Stute)
[of](http://blog.regehr.org/archives/1241)
[reasons](https://www.alchemistowl.org/pocorgtfo/pocorgtfo08.pdf).
The [Bootstrappable Builds](https://bootstrappable.org/) project aims
to reduce the number and size of binary seeds to a bare minimum.

[GNU Mes](https://www.gnu.org/software/mes/) is closely related to the
Bootstrappable Builds project.  Mes aims to create an entirely
source-based bootstrapping path for the Guix System and other
interested GNU/Linux distributions.  The goal is to start from a
minimal, easily inspectable binary (which should be readable as
source) and bootstrap into something close to R6RS Scheme.

Currently, Mes consists of a mutual self-hosting scheme interpreter
and C compiler.  It also implements a C library.  Mes, the scheme
interpreter, is written in about 5,000 lines of code of simple C.
MesCC, the C compiler, is written in scheme.  Together, Mes and MesCC
can compile [a lightly patched
TinyCC](http://gitlab.com/janneke/tinycc) that is self-hosting.  Using
this TinyCC and the Mes C library, it is possible to bootstrap the
entire Guix System for i686-linux and x86_64-linux.

#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
manager and an advanced distribution of the GNU system that [respects
user
freedom](https://www.gnu.org/distros/free-system-distribution-guidelines.html).
Guix can be used on top of any system running the kernel Linux, or it
can be used as a standalone operating system distribution for i686,
x86_64, ARMv7, and AArch64 machines.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.  When used as a standalone
GNU/Linux distribution, Guix offers a declarative, stateless approach to
operating system configuration management.  Guix is highly customizable
and hackable through [Guile](https://www.gnu.org/software/guile)
programming interfaces and extensions to the
[Scheme](http://schemers.org) language.
