title: GNU Guix and GuixSD 0.16.0 released
date: 2018-12-06 18:00
author: Ludovic Courtès
slug: gnu-guix-and-guixsd-0.16.0-released
tags: Releases
---
We are pleased to announce the new release of GNU Guix and GuixSD,
version 0.16.0!  This release is (hopefully!) the last one before 1.0—we
have been closing [most key items for
1.0](https://git.savannah.gnu.org/cgit/guix/maintenance.git/tree/doc/1.0.org)
over the last few months.

The release comes with [GuixSD ISO-9660 installation
images](https://www.gnu.org/software/guix/manual/en/html_node/System-Installation.html),
a [virtual machine image of
GuixSD](https://www.gnu.org/software/guix/manual/en/html_node/Running-GuixSD-in-a-VM.html),
and with tarballs to install the package manager on top of your
GNU/Linux distro, either [from
source](https://www.gnu.org/software/guix/manual/en/html_node/Requirements.html)
or [from
binaries](https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html).
Guix users can update by running `guix pull`.

It’s been 5 months since the previous release, during which 95 people
contributed code and packages.  Here are the highlights.

  - The default [substitute
    URL](https://www.gnu.org/software/guix/manual/en/html_node/Substitutes.html)
    has been changed to `https://ci.guix.info`.  This is backed by a
    more powerful build farm with terabytes of storage kindly donated by
    the [Bioinformatics platform of the Berlin Institute of Medical
    Systems Biology (BIMSB) at the Max Delbrück Center
    (MDC)](https://www.mdc-berlin.de/bioinformatics).  The build farm
    front-end runs Cuirass, our continuous integration tool that was
    partly developed during two
    [GSoC](https://www.gnu.org/software/guix/blog/2018/gsoc-2018-report-cuirass-web-interface/)
    [internships](https://www.gnu.org/software/guix/blog/2016/gnu-guix-welcomes-four-students-for-gsoc/).
  - `guix pull` now [lists new and upgraded
    packages](https://www.gnu.org/software/guix/blog/2018/multi-dimensional-transactions-and-rollbacks-oh-my/),
    and it has a `--profile` option that allows you to keep several
    Guix revisions in parallel.  Related to that, the new [`guix
    describe`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-describe.html)
    command displays information about the Guix revision you are using.
  - `guix pull` now supports
    [_channels_](https://www.gnu.org/software/guix/manual/en/html_node/Channels.html).
    In a nutshell, you can specify in `~/.config/guix/channels.scm` the
    channels from which `guix pull` will fetch Guix as well as,
    optionally, third-party package repositories.  Again `guix describe`
    displays all the channels in use and `guix describe -f channels`
    produces a “pinned channel” specification that can be used as the
    `channels.scm` file of `guix pull`.
  - Using the new [_inferior_
    mechanism](https://www.gnu.org/software/guix/manual/en/html_node/Inferiors.html),
    you can now interact with a different revision of Guix and even
    _compose packages coming from different revisions of Guix_!
  - The output of the command-line tools has been noticeably improved:
    important events are colorized, `guix package` and `guix system` no
    longer display build logs, and `guix build` colorizes build logs (in
    a way that is similar to what
    [Emacs-Guix](https://emacs-guix.gitlab.io/website/) does.)
  - There are new [package transformation
    options](https://www.gnu.org/software/guix/manual/en/html_node/Package-Transformation-Options.html):
    `--with-branch` and `--with-commit` allow you to obtain a package
    variant straight from its Git repository.
  - Guix had reproducible builds and now it has “reproducible source
    code downloads”: when a package refers to a Git repository that has
    disappeared (which is unfortunately not uncommon!), the checkout can
    be fetched from [Software
    Heritage](https://www.softwareheritage.org/).  That makes Guix one
    of the first distros to be backed by a long-term archive.  See [this
    issue](https://issues.guix.info/issue/33432) for more info.
  - Our [Rust](https://www.rust-lang.org) packages are now _fully
    bootstrapped from source_, starting from
    [mrustc](https://github.com/thepowersgang/mrustc), a Rust compiler
    written in C++.  This is a victory on this instance of [“yogurt
    software”](https://bootstrappable.org/), and Guix is probably the
    first distro to achieve this.  More on that in a future post!
  - On GuixSD, `guix system reconfigure` will now always load
    replacements of system services.  That way, when you deem
    appropriate, you can run `herd restart SERVICE` to start the
    upgraded service.
  - As usual, 985 packages were added and 1,945 were upgraded, notably
    the GNU C Library now at version 2.28 (which, incidentally, allowed
    us to [get rid of our Hurd-specific glibc
    variant](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=2d546858b139e5fcf2cbdf9958a17fd98803ac4c),
    at last!).  Today Guix provides [8,715
    packages](https://guix-hpc.bordeaux.inria.fr/browse).
  - The manual is now partially [translated into
    German](https://www.gnu.org/software/guix/manual/de/html_node/).
    The [French
    translation](https://www.gnu.org/software/guix/manual/fr/html_node/)
    is now 90% complete.  You can help [translate the manual into your
    native
    language](https://translationproject.org/domain/guix-manual.html) by
    [joining the Translation
    Project](https://translationproject.org/html/translators.html).

Pffew, quite a long list already!  The [release
announcement](https://lists.gnu.org/archive/html/guix-devel/2018-12/msg00141.html)
lists additional noteworthy changes and bug fixes you may be interested
in.

Enjoy!

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
