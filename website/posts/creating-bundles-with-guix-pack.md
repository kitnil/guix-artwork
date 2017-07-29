title: Creating bundles with guix pack
date: 2017-03-20 14:45
author: Ludovic Courtès
tags: Software bundles
---
Guix just got a new command,
[dubbed `guix pack`](https://lists.gnu.org/archive/html/guix-devel/2017-03/msg00322.html),
which we think many developers will find useful.

Last week we were celebrating the
[release of GNU Guile 2.2.0](https://www.gnu.org/software/guile/news/gnu-guile-220-released.html),
the Scheme implementation that powers Guix.  This is a major milestone
and Guile developers naturally wanted to make it easy for users to
discover all the goodies of 2.2.0 as soon as possible.  One of the major
roadblocks to that, as for any non-trivial piece of software, is
deployment: because your distro is unlikely to have Guile 2.2.0 packaged
on Day 1, you have to build it by yourself, which means getting the
right dependencies installed and then building Guile itself.  That’s not
difficult for a developer, but it’s certainly cumbersome.

Andy Wingo, the driving force behind Guile, thought that it would be
nice to propose a binary tarball of Guile 2.2.0 on the day of its
release.  Guix had already been providing
[binary tarballs](https://www.gnu.org/software/guix/manual/html_node/Binary-Installation.html)
for a couple of years, so why not do the same for Guile?  Essentially,
the new `guix pack` command is a generalization of what Guix was already
using.

#### Making packs

So how does it work?  The basic idea is simple: you type

```
guix pack guile
```

and the command returns in `/gnu/store` a good old tarball that
contains binaries for Guile and all its dependencies.  If you run, say,

```
guix pack guile emacs geiser
```

then you get a complete “Guile SDK” containing Guile, Emacs,
[Geiser](http://nongnu.org/geiser), and all their dependencies.

When you extract the tarball, you get a `/gnu/store` directory with a
bunch of sub-directories with
[these long hashes](https://www.gnu.org/software/guix/manual/html_node/Features.html),
one of which is the “profile” containing Guile, Emacs, and Geiser.

You wouldn’t want to ask users to type
`/gnu/store/war325pv1iixj13k6y8yplzagpknfn0c-profile/bin/guile` to
launch Guile, though.  So `guix pack` has a command-line option to
create symlinks in the image.

```
guix pack -S /opt/gnu/bin=bin guile emacs geiser
```

The command above creates a `/opt/gnu/bin` symlink to the `bin`
directory of the profile in the tarball, such that users can simply type
`/opt/gnu/bin/guile` to run Guile.

Recipients of a binary tarball are expected to either extract it in
their root file system (yes!) where it will create `/gnu` and `/opt/gnu`
in this case:

```
# cd /
# tar xf /path/to/pack.tar.gz
# /opt/gnu/bin/guile --version
guile (GNU Guile) 2.2.0
```

… or they can chroot into it, possibly relying on
[user namespaces](http://man7.org/linux/man-pages/man7/user_namespaces.7.html)
and thereby avoiding root privileges:

```
$ mkdir /tmp/pack
$ cd /tmp/pack
$ tar xf /path/to/pack.tar.gz
$ unshare -mrf chroot . /opt/gnu/bin/guile --version
guile (GNU Guile) 2.2.0
```

The good thing with this is that, because Guix captures the _complete
dependency graph_ of packages, the tarball contains everything that’s
needed to run Guile and is going to work in exactly the same way on any
system that runs the kernel Linux!

#### Bells and whistles

Of course a popular approach to run such “application bundles” is
[Docker](https://www.docker.com).  Since the image format for Docker is
[documented](https://github.com/docker/docker/blob/master/image/spec/v1.2.md)
and fairly easy to produce, we added an option to produce images in this
format (Ricardo Wurmus
[initially contributed Docker support](https://lists.gnu.org/archive/html/guix-devel/2017-01/msg00188.html)
for the low-level
[`guix archive`](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-archive.html)
tool but we found that it made more sense to have it in `guix pack`):

```
guix pack -f docker -S /opt/gnu=/ guile emacs geiser
```

The resulting tarball can be passed to
[`docker load`](https://docs.docker.com/engine/reference/commandline/load/),
and people can then use `docker run` to actually run the application.

One of the goodies that comes for free is cross-compilation: Guix
supports cross-compilation, so you can create a pack consisting of
software cross-compiled for a given platform, specified by the
[usual GNU triplet](https://www.gnu.org/savannah-checkouts/gnu/autoconf/manual/autoconf-2.69/html_node/Specifying-Target-Triplets.html).
For example, the following command creates a pack with binaries for
GNU/Linux on ARMv7:

```
guix pack --target=arm-linux-gnueabihf guile
```

… while the command below creates a pack with Windows binaries using the
MinGW cross-compiler:

```
guix pack --target=i686-w64-mingw32 guile
```

All the
[package transformation options](https://www.gnu.org/software/guix/manual/html_node/Package-Transformation-Options.html)
that Guix supports are available to `guix pack`.  Let’s say you’re a
developer of a large piece of software such as a web browser like
[IceCat](https://gnu.org/s/gnuzilla) and you’d like your users to test
whether the current master branch actually fixes the bug you attempted
to fix.  In this case, you can build a pack of IceCat, but replace the
source that’s specified in the distribution with the snapshot of master
you’re interested in:

```
guix pack icecat --with-source=./icecat-48.8.0.master.tar.gz
```

Of course the resulting pack is going to be pretty big in this case, but
I’m sure the general pattern can be useful.

#### Wait, didn’t you say that “app bundles get it wrong”?

It turns out that we Guix developers
[have](https://archive.fosdem.org/2016/schedule/event/deployments_with_gnu_guix/)
[been](https://arxiv.org/abs/1506.02822)
[saying](https://fosdem.org/2017/schedule/event/hpc_deployment_guix/)
that binary “application bundles” à la Docker are problematic for a
number of reasons:

  1. Composability: each bundle comes with a complete operating system,
     minus the kernel, and there is little or no sharing happening among
     bundles, notably in terms of disk space and memory usage.
  2. Security updates: since an “app bundle” is essentially a complete
     operating system, one has to be careful and apply security updates
     to all the software in each bundle.  Unfortunately, that doesn’t
     always happen as has been
     [famously](http://www.vitavonni.de/blog/201503/2015031201-the-sad-state-of-sysadmin-in-the-age-of-containers.html)
     [reported](https://www.infoq.com/news/2015/05/Docker-Image-Vulnerabilities)
     on several occasions.
  3. Reproducibility: Docker images, for instance, are often hardly
     “reproducible” in the sense of a
     [reproducible build process](https://reproducible-builds.org/docs/definition/).
     First, `Dockerfile`s start out with a “base layer” that is
     typically a huge binary blob of some major distro.  On top of that,
     they run a number of commands such as `apt-get install` whose
     result likely depends on the time at which they are run.  Docker’s
     [best practices document](https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/)
     suggests ways to mitigate the problem, such as “version pinning”,
     but the whole approach remains rather brittle.
  4. Experimentation: Once you have this big binary blob, sure you can
     run the application you wanted, but you can do little more than
     that—you may or may not be able to find the corresponding source
     code, and you’d have a hard time fiddling with one of the
     components of the software stack.

We pride ourselves with having a tool set that caters to some of the use
cases that “app bundles” and “containerization” try to address while
having none of these drawbacks.  So how do Guix packs fit into that
picture?

First of all, the intended use case is different: we view `guix pack` as
a tool that makes it easy to try out a piece of software on a non-Guix
machine.  But it is clear that for production, our recommendation is to
use Guix directly, to get security updates and generally address all the
above issues.  :-)

That said, let’s see how these issues affect Guix packs.  First,
composability of Guix packs turns out to be pretty good.  If you receive
two different Guix packs for different pieces of software, you can
unpack both in your root directory (or union-mount them in the same
place): packages that differ have a different `/gnu/store` file name
with a different hash, so they won’t collide; packages that are
identical (say the C library or GTK+) will have the same `/gnu/store`
file name so they’ll actually be shared.

That means that for security updates, you could always fetch a new pack
of your application with the security updates and extract it in place.
However, that requires you as a user to manually pay attention to
vulnerabilities in all the software that comes with the pack, so clearly
you’re better off using Guix instead and regularly upgrading.  No
wonders.

Packs themselves are reproducible bit-by-bit.  If you know the Guix
commit that was used to build a given pack, you can thus run the same
`guix pack` command on another machine and verify that you get the exact
same tarball.  Currently not 100% of the packages Guix provides are
reproducible bit-by-bit; we’re getting closer to that goal though, in
part due to the fact that Guix builds are isolated by default, and also
thanks to the efforts of everyone in the
[Reproducible Builds](https://reproducible-builds.org) project to
address sources of non-determinism in free software.

Because Guix packs are reproducible, you can not only reproduce the
exact same pack but also create packs with variants of the software—for
instance, changing the version of one of the packages in the stack.  Of
course this part requires you to have Guix installed somewhere, but at
least you can easily fiddle with the software stack and “compile” your
own variant of the software stack down to a new pack.

We hope you’ll enjoy packs and Guix, and would welcome your feedback on
[the `guix-devel` mailing list](https://www.gnu.org/software/guix/about/#contact)
and on `#guix` on Freenode!


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
mips64el and armv7.
