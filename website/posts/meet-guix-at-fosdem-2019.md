title: Meet Guix at FOSDEM
slug: meet-guix-at-fosdem-2019
date: 2019-01-28 12:00:00
author: Ludovic Courtès
tags: FOSDEM
---

As usual, GNU Guix will be present at [FOSDEM](https://fosdem.org/2019/)
in the coming days with a couple of talks:

  - Saturday afternoon, [_Building a whole distro on top of a
    minimalistic
    language_](https://fosdem.org/2019/schedule/event/gnuguixminimalism/)
    will present to the [minimalist languages
    track](https://fosdem.org/2019/schedule/track/minimalistic_languages/)
    audience how Scheme, rather than “piling feature on top of feature”,
    provides the fundamental tools that have allowed us to build a wide
    range of operating system features on top of it.
  - In the same track, [_GNU Mes—Reduced binary seed bootstrap for
    Guix_](https://fosdem.org/2019/schedule/event/gnumes/) will report
    on two years of hard work tackling one of the most pressing security
    issues of operating systems—the [“trusting trust”
    attack](https://en.wikipedia.org/wiki/Backdoor_(computing)#Compiler_backdoors).
    Janneke will present exciting [bootstrapping
    achievements](https://bootstrappable.org) and their integration in
    Guix.
  - Ricardo [will present the GNU Guix Workflow
    Language](https://fosdem.org/2019/schedule/event/guixinfra/) in the
    same track—on the benefits of having reproducible software
    deployment built into a scientific workflow execution engine.
  - On Sunday afternoon (don’t rush to the train station yet!),
    [_GNU Guix’s take on a new approach to software
    distribution_](https://fosdem.org/2019/schedule/event/gnu_guix_new_approach_to_software_distribution/)
    will present to [fellow distro
    hackers](https://fosdem.org/2019/schedule/track/distributions/) how
    Guix differs from “traditional distros” and how it tries to make
    popular ad-hoc software deployment tools (pip, VirtualEnv, Cabal,
    but also Flatplak, Docker—you name it) less appealing than its
    integrated solutions.

The [minimalist languages
track](https://fosdem.org/2019/schedule/event/gnuguixminimalism/) will
also feature talks about [GNU Guile](https://gnu.org/s/guile) and about
[Racket](https://racket-lang.org) that you should not miss under any
circumstances!

![Guix Days logo.](https://www.gnu.org/software/guix/static/blog/img/Guix-Days-2019.png)

For the second time, we are also organizing the Guix Days as a [FOSDEM
fringe event](https://fosdem.org/2019/fringe/), a two-day Guix workshop
where contributors and enthusiasts will meet.  The workshop takes place
on Thursday Jan. 31st and Friday Feb. 1st at the Institute of Cultural
Affairs (ICAB) in Brussels.

This year there will be few talks; instead, the event will consist
primarily of
“[unconference-style](https://en.wikipedia.org/wiki/Unconference)”
sessions focused on specific hot topics about Guix, the Shepherd,
continuous integration, and related tools and workflows.  We are also
happy to welcome fellow [Nix](https://nixos.org/nix/) hackers, which
should allow us to develop cross-distro cooperation.

Attendance to the workshop is free and open to everyone, though you are
invited to register (there are only a few seats left!).  Check out [the
workshop’s wiki
page](https://libreplanet.org/wiki/Group:Guix/FOSDEM2019) for
registration and practical info.  Hope to see you in Brussels!

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
