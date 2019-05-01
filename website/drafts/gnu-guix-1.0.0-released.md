title: GNU Guix 1.0.0 released
date: 2019-05-02 16:00
author: Ludovic Courtès
slug: gnu-guix-1.0.0-released
tags: Releases
---
We are excited to announce the release of GNU Guix version 1.0.0!

The release comes with [ISO-9660 installation
images](https://www.gnu.org/software/guix/manual/en/html_node/System-Installation.html),
a [virtual machine
image](https://www.gnu.org/software/guix/manual/en/html_node/Running-Guix-in-a-VM.html),
and with tarballs to install the package manager on top of your
GNU/Linux distro, either [from
source](https://www.gnu.org/software/guix/manual/en/html_node/Requirements.html)
or [from
binaries](https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html).
Guix users can update by running `guix pull`.

One-point-oh always means a lot for free software releases.  For Guix,
1.0 is the result of seven years of development, with code, packaging,
and documentation contributions made by 260 people, translation work
carried out by a dozen of people, and artwork and web site development
by a couple of individuals, to name some of the activities that have
been happening.  During those years we published no less than [19 “0.x”
releases](https://www.gnu.org/software/guix/blog/tags/releases/).

# The journey to 1.0

We took our time to get there, which is quite unusual in an era where
free software moves so fast.  Why did we take this much time?  First, it
takes time to build a community around a GNU/Linux distribution, and a
distribution wouldn’t really exist without it.  Second, we feel like
we’re contributing an important piece to [the GNU operating
system](https://www.gnu.org/gnu/about-gnu.html), and that is surely
intimidating and humbling.

Last, we’ve been building something new.  Of course we stand on the
shoulders of giants, and in particular [Nix](https://nixos.org/nix/),
which brought the functional software deployment paradigm that Guix
implements.  But developing Guix has been—and still is!—a challenge in
many ways: it’s a [programming](https://arxiv.org/abs/1305.4584)
[language](https://www.gnu.org/software/guix/blog/2017/back-from-gpce/)
design challenge, an
[operating](https://www.gnu.org/software/guix/blog/2015/service-composition-in-guixsd/)
[system](https://www.gnu.org/software/guix/blog/2017/running-system-services-in-containers/)
design challenge, a challenge for
[security](https://www.gnu.org/software/guix/blog/2016/timely-delivery-of-security-updates/),
[reproducibility](https://www.gnu.org/software/guix/blog/tags/reproducibility/),
[bootstrapping](https://www.gnu.org/software/guix/blog/tags/bootstrapping/),
usability, and more.  In other words, it’s been a long but insightful
journey! :-)

# What GNU Guix can do for you

Presumably some of readers are discovering Guix today, so let’s recap
what Guix can do for you as a user.  Guix is a complete toolbox for
software deployment in general, which makes it different from most of
the tools you may be familiar with.

![Guix manages packages, environments, containers, and systems.](https://www.gnu.org/software/guix/static/blog/img/guix-hexagons.png)

This may sound a little abstract so let’s look at concrete use cases:

  - *As a user*, Guix allows you to [install applications and to keep
    them
    up-to-date](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-package.html):
    search for software with `guix search`, install it with `guix
    install`, and maintain it up-to-date by regularly running `guix
    pull` and `guix upgrade`.  Guix follows a so-called “rolling
    release” model, so you can run `guix pull` at any time to get the
    latest and greatest bits of free software.
	
	This certainly sounds familiar, but a distinguishing property here
    is _dependability_: Guix is transactional, meaning that you can at
    any time roll back to a previous “generation” of your package set
    with `guix package --roll-back`, inspect differences with `guix
    package -l`, and so on.
	
	Another useful property is _reproducibility_: Guix allows you to
    deploy the _exact same software environment_ on different machines
    or at different points in time thanks to [`guix
    describe`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-describe.html)
    and [`guix
    pull`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-pull.html).
	
	This, coupled with the fact that package management operations do
    not require root access, is invaluable notably in the context of
    high-performance computing (HPC) and reproducible science, which the
    [Guix-HPC effort](https://guix-hpc.bordeaux.inria.fr/) has been
    focusing on.

  - *As a developer*, we hope you’ll enjoy [`guix
    environment`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-environment.html),
    which allows you to spawn one-off software environments.  Suppose
    you’re a GIMP developer: running `guix environment gimp` spawns a
    shell with everything you need to hack on GIMP—much quicker than
    manually installing its many dependencies.
	
	Developers often struggle to push their work to users so they get
    quick feedback.  The [`guix
    pack`](https://www.gnu.org/software/guix/blog/2017/creating-bundles-with-guix-pack/)
    provides an easy way to create _container images_ for use by Docker
    & co., or even [standalone relocatable
    tarballs](https://www.gnu.org/software/guix/blog/2018/tarballs-the-ultimate-container-image-format/)
    that anyone can run, regardless of the GNU/Linux distribution they
    use.
	
	Oh, and you may also like [package transformation
    options](https://www.gnu.org/software/guix/manual/en/html_node/Package-Transformation-Options.html),
    which allow you define package variants from the command line.

  - *As a system administrator*—and actually, we’re all system
    administrators of sorts on our laptops!—, Guix’s declarative and
    unified approach to configuration management should be handy.  It
    surely is a departure from what most people are used to, but it is
    so reassuring: one configuration file is enough to specify [all the
    aspects of the system
    config](https://www.gnu.org/software/guix/manual/en/html_node/Using-the-Configuration-System.html)—services,
    file systems, locale, accounts—all in the same language.
	
	That makes it surprisingly easy to deploy otherwise complex services
    such as applications that depend on Web services.  For instance,
    setting up
    [CGit](https://www.gnu.org/software/guix/manual/en/html_node/Version-Control-Services.html#Cgit-Service)
    or
    [Zabbix](https://www.gnu.org/software/guix/manual/en/html_node/Monitoring-Services.html#Zabbix-front_002dend)
    is a one-liner, even though behind the scenes that involves setting
    up nginx, fcgiwrap, etc.  We’d love to see to what extent this helps
    people self-host services—sort of similar to what
    [FreedomBox](https://freedombox.org/) and
    [YunoHost](https://yunohost.org/) have been focusing on.
	
	With [`guix
    system`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-system.html)
    you can instantiate a configuration on your machine, or in a virtual
    machine (VM) where you can test it, or in a container.  You can also
    provision ISO images, VM images, or container images with a complete
    OS, from the same config, all with `guix system`.
	
The [quick reference
card](https://www.gnu.org/software/guix/guix-refcard.pdf) shows the
important commands.  As you start diving deeper into Guix, you’ll
discover that many aspects of the system are exposed using consistent
[Guile](https://www.gnu.org/software/guile/) programming interfaces:
[package
definitions](https://www.gnu.org/software/guix/manual/en/html_node/Defining-Packages.html),
[system
services](https://www.gnu.org/software/guix/manual/en/html_node/Services.html),
the [“init” system](https://www.gnu.org/software/shepherd/), and a whole
bunch of system-level libraries.  We believe that makes the system very
_hackable_, and we hope you’ll find it as much fun to play with as we do.

So much for the overview!
	
# What’s new since 0.16.0

For those who’ve been following along, a great many things have changed
over the last 5 months since the [0.16.0
release](https://www.gnu.org/software/guix/blog/2018/gnu-guix-and-guixsd-0.16.0-released/)—99
people contributed over 5,700 commits during that time!  Here are the
highlights:

   - The ISO installation image now runs a cute [text-mode graphical
     installer](https://www.gnu.org/software/guix/manual/en/html_node/Guided-Graphical-Installation.html)—big
     thanks to Mathieu Othacehe for writing it and to everyone who
     tested it and improved it!  It is similar in spirit to the Debian
     installer.  Whether you’re a die-hard GNU/Linux hacker or a novice
     user, you’ll certainly find that this makes system installation
     much less tedious than it was!  The installer is fully translated
     to French, German, and Spanish.
   - The new [VM
     image](https://www.gnu.org/software/guix/manual/en/html_node/Running-GuixSD-in-a-VM.html)
     better matches user expectations: whether you want to tinker with
     Guix System and see what it’s like, or whether you want to use it
     as a development environment, this VM image should be more directly
     useful.
   - The user interface was improved: diagnostics are now colorized,
     more operations show a progress bar, there’s a new `--verbosity`
     option recognized by all commands, and most commands are now
     “quiet” by default.
   - There’s a new `--with-git-url` [package transformation
     options](https://www.gnu.org/software/guix/manual/en/html_node/Package-Transformation-Options.html),
     that goes with `--with-branch` and `--with-commit`.
   - Guix now has a first-class, uniform mechanism to configure
     [keyboard
     layout](https://www.gnu.org/software/guix/manual/en/html_node/Keyboard-Layout.html)—a
     long overdue addition.  Related to that, [Xorg
     configuration](https://www.gnu.org/software/guix/manual/en/html_node/X-Window.html)
     has been streamlined with the new `xorg-configuration` record.
   - We introduced `guix pack -R` [a while
     back](https://www.gnu.org/software/guix/blog/2018/tarballs-the-ultimate-container-image-format/):
     it creates tarballs containing _relocatable_ application bundles
     that rely on user namespaces.  Starting from 1.0, `guix pack -RR`
     (like “reliably relocatable”?) generates relocatable binaries that
     fall back to [PRoot](https://proot-me.github.io/) on systems where
     [user
     namespaces](http://man7.org/linux/man-pages/man7/user_namespaces.7.html)
     are not supported.
   - More than 1,100 packages were added, leading to [close to 10,000
     packages](https://www.gnu.org/software/guix/packages), 2,104
     packages were updated, and several system services were
     contributed.
   - The manual has been fully translated to
     [French](https://www.gnu.org/software/guix/manual/fr/html_node/),
     the
     [German](https://www.gnu.org/software/guix/manual/de/html_node/)
     and [Spanish](https://www.gnu.org/software/guix/manual/es/html_node/)
     translations are nearing completion, and work has begun on a
     [Simplified
     Chinese](https://www.gnu.org/software/guix/manual/zh_CN/html_node/)
     translation.  You can help [translate the manual into your
     language](https://translationproject.org/domain/guix-manual.html)
     by [joining the Translation
     Project](https://translationproject.org/html/translators.html).

That’s a long list already, but you can find more details in the
[`NEWS`](https://git.savannah.gnu.org/cgit/guix.git/tree/NEWS?h=version-1.0.0)
file.

# What’s next?

One-point-oh is a major milestone, especially for those of us who’ve
been on board for several years.  But with the wealth of ideas we’ve
been collecting, it’s definitely not the end of the road!

If you’re interested in “devops” and distributed deployment, you will
certainly be happy to help in that area, those interested in OS
development might want to make [the
Shepherd](https://www.gnu.org/software/shepherd/) more flexible and
snappy, furthering integration with [Software
Heritage](https://www.gnu.org/software/guix/blog/2019/connecting-reproducible-deployment-to-a-long-term-source-code-archive/)
will probably be #1 on the to-do list of scientists concerned with
long-term reproducibility, programming language tinkerers may want to
push
[G-expressions](https://www.gnu.org/software/guix/manual/en/html_node/G_002dExpressions.html#G_002dExpressions)
further, etc.  Guix 1.0 is a tool that’s both serviceable for one’s
day-to-day computer usage and a great playground for the tinkerers among
us.

Whether you want to help on design, coding, maintenance, system
administration, translation, testing, artwork, web services, funding…
[your contributions are
welcome](https://www.gnu.org/software/guix/contribute/)!

We’re humans—don’t hesitate to [get in touch with
us](https://www.gnu.org/software/guix/contact/), and enjoy Guix 1.0!


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
