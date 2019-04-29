title: Tarballs, the ultimate container image format
date: 2018-05-16 11:00
author: Ludovic Courtès
tags: Containers, Software development
---

A year ago [we introduced `guix
pack`](https://www.gnu.org/software/guix/blog/2017/creating-bundles-with-guix-pack/),
a tool that allows you to create “application bundles” from a set of Guix
package definitions.  On your Guix machine, you run:

```sh
guix pack -S /opt/gnu/bin=bin guile gnutls guile-json
```

and you get a tarball containing your favorite programming language
implementation and a couple of libraries, where `/opt/gnu/bin` is a
symlink to the `bin` directory containing, in this case, the `guile`
command.  Add `-f docker` and, instead of a tarball, you get an image in
the Docker format that you can pass to `docker load` on any machine
where Docker is installed.  Overall that’s a relatively easy way to
share software stacks with machines that do not run Guix.

The tarball format is plain and simple, it’s the one we know and love,
and it’s been there “forever” [as its name
suggests](https://www.gnu.org/software/tar/manual/en/html_node/Introduction.html).
The tarball that `guix pack` produces can be readily extracted on
another machine, one that doesn’t run Guix, and you’re done.  The
problem though, is that you’ll need to either unpack the tarball in the
root file system or to play tricks with the `unshare` command, as we saw
[in the previous
post](https://www.gnu.org/software/guix/blog/2017/creating-bundles-with-guix-pack/).
Why can’t we just extract such a tarball in our home directory and
directly run `./opt/gnu/bin/guile` for instance?

# Relocatable packages

The main issue is that, except in the uncommon case where developers
went to great lengths to make it possible (as with
[GUB](http://lilypond.org/gub/), see the [`*-reloc*.patch`
files](https://github.com/gperciva/gub/tree/master/patches)), packages
built for GNU/Linux are not relocatable.  ELF files embed things like
the absolute file name of the dynamic linker, directories where
libraries are to be search for (they can be relative file names with
`$ORIGIN` but usually aren’t), and so on; furthermore, it’s very common
to embed things like the name of the directory that contains locale data
or other application-specific data.  For Guix-built software, all these
are absolute file names under `/gnu/store` so Guix-built binaries won’t
run unless those `/gnu/store` files exist.

On machines where support for [“user
namespaces”](http://man7.org/linux/man-pages/man7/user_namespaces.7.html)
is enabled, we can easily “map” the directory where users unpacked the
tarball that `guix pack` produced to `/gnu/store`, as shown in the
previous post:

```sh
$ tar xf /path/to/pack.tar.gz
$ unshare -mrf chroot . /opt/gnu/bin/guile --version
guile (GNU Guile) 2.2.0
```

It does the job but remains quite tedious.  Can’t we automate that?

# `guix pack --relocatable`

The `--relocatable` (or `-R`) option of `guix pack`, which landed [a few
days ago](https://bugs.gnu.org/31360), produces tarballs with
automatically relocatable binaries.  Back to our earlier example, let’s
say you produce a tarball with this new option:

```sh
guix pack --relocatable -S /bin=bin -S /etc=etc guile gnutls guile-json
```

You can send the resulting tarball to any machine that runs the kernel
Linux (it [doesn’t even have to be
GNU/Linux](https://www.gnu.org/software/guix/blog/2018/guix-on-android/))
with user namespace support—which, unfortunately, is disabled by default
on some distros.  There, as a regular user, you can run:

```sh
$ tar xf /path/to/pack.tar.gz
$ source ./etc/profile    # define ’GUILE_LOAD_PATH’, etc.
$ ./bin/guile
guile: warning: failed to install locale
GNU Guile 2.2.3
Copyright (C) 1995-2017 Free Software Foundation, Inc.

Guile comes with ABSOLUTELY NO WARRANTY; for details type `,show w'.
This program is free software, and you are welcome to redistribute it
under certain conditions; type `,show c' for details.

Enter `,help' for help.
scheme@(guile-user)> ,use(json)
scheme@(guile-user)> ,use(gnutls)
```

We were able to run Guile and to use our Guile libraries since sourcing
`./etc/profile` augmented the `GUILE_LOAD_PATH` environment variable
that tells Guile where to look for libraries.  Indeed we can see it by
inspecting the value of `%load-path` at the Guile prompt:

```scheme
scheme@(guile-user)> %load-path
$1 = ("/gnu/store/w9xd291967cvmdp3m0s7739icjzgs8ns-profile/share/guile/site/2.2" "/gnu/store/b90y3swxlx3vw2yyacs8cz59b8cbpbw5-guile-2.2.3/share/guile/2.2" "/gnu/store/b90y3swxlx3vw2yyacs8cz59b8cbpbw5-guile-2.2.3/share/guile/site/2.2" "/gnu/store/b90y3swxlx3vw2yyacs8cz59b8cbpbw5-guile-2.2.3/share/guile/site" "/gnu/store/b90y3swxlx3vw2yyacs8cz59b8cbpbw5-guile-2.2.3/share/guile")
```

Wait, it’s all `/gnu/store`!  As it turns out, `guix pack --relocatable`
created a wrapper around `guile` that populates `/gnu/store` in the
mount namespace of the process.  Even though `/gnu/store` does not exist
on that machine, our `guile` process “sees” our packages under
`/gnu/store`:

```scheme
scheme@(guile-user)> ,use(ice-9 ftw)
scheme@(guile-user)> (scandir "/gnu/store")
$2 = ("." ".." "0249nw8c7z626fw1fayacm160fpd543k-guile-json-0.6.0R" "05dvazr5wfh7lxx4zi54zfqnx6ha8vxr-bash-static-4.4.12" "0jawbsyafm93nxf4rcmkf1rsk7z03qfa-libltdl-2.4.6" "0z1r7ai6syi2qnf5z8w8n25b1yv8gdr4-info-dir" "1n59wjm6dbvc38b320iiwrxra3dg7yv8-libunistring-0.9.8" "2fg01r58vv9w41kw6drl1wnvqg7rkv9d-libtasn1-4.12" "2ifmksc425qcysl5rkxkbv6yrgc1w9cs-gcc-5.5.0-lib" "2vxvd3vls7c8i9ngs881dy1p5brc7p85-gmp-6.1.2" "4sqaib7c2dfjv62ivrg9b8wa7bh226la-glibc-2.26.105-g0890d5379c" "5kih0kxmipzjw10c53hhckfzkcs7c8mm-gnutls-3.5.13" "8hxm8am4ll05sa8wlwgdq2lj4ddag464-zlib-1.2.11" "90vz0r78bww7dxhpa7vsiynr1rcqhyh4-nettle-3.4" "b90y3swxlx3vw2yyacs8cz59b8cbpbw5-guile-2.2.3" "c4jrwbv7qckvnqa7f3h7bd1hh8rbg72y-libgc-7.6.0" "f5lw5w4nxs6p5gq0c2nb3jsrxc6mmxbi-libgc-7.6.0" "hjxic0k4as384vn2qp0l964isfkb0blb-guile-json-0.6.0" "ksyja5lbwy0mpskvn4rfi5klc00c092d-libidn2-2.0.4" "l15mx9lrwdflyvmb4a05va05v5yqizg5-libffi-3.2.1" "mm0zclrzj3y7rj74hzyd0f224xly04fh-bash-minimal-4.4.12" "vgmln3b639r68vvy75xhcbi7d2w31mx1-pkg-config-0.29.2" "vz3zfmphvv4w4y7nffwr4jkk7k4s0rfs-guile-2.2.3" "w9xd291967cvmdp3m0s7739icjzgs8ns-profile" "x0jf9ckd30k3nhs6bbhkrxsjmqz8phqd-nettle-3.4" "x8z6cr7jggs8vbyh0xzfmxbid63z6y83-guile-2.2.3R" "xbkl3nx0fqgpw2ba8jsjy0bk3nw4q3i4-gnutls-3.5.13R" "xh4k91vl0i8nlyrmvsh01x0mz629w5a9-gmp-6.1.2" "yx12x8v4ny9f6fipk8285jgfzqavii83-manual-database" "zksh1n0p9x903kqbvswgwy2vsk2b7255-libatomic-ops-7.4.8")
```

The wrapper is a small statically-linked [C
program](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/aux-files/run-in-namespace.c).
(Scheme would be nice and would allow us to reuse
[`call-with-container`](https://www.gnu.org/software/guix/blog/2015/container-provisioning-with-guix/),
but it would also take up more space.)  All it does is create a child
process with separate mount and user namespaces, which in turn mounts
the tarball’s `/gnu/store` to `/gnu/store`, bind-mounts other entries
from the host root file system, and `chroot`s into that.  The result is
a binary that sees everything a “normal” program sees on the host, but
with the addition of `/gnu/store`, with minimal startup overhead.

In a way, it’s a bit of a hack: for example, what gets bind-mounted in
the mount namespace of the wrapped program is hard-coded, which is OK,
but some flexibility would be welcome (things like Flatpak’s [sandbox
permissions](http://docs.flatpak.org/en/latest/sandbox-permissions.html),
for instance).  Still, that it Just Works is a pretty cool feature.

# Tarballs vs. Snap, Flatpak, Docker, & co.

Come to think of it: if you’re a developer, `guix pack` is probably
one of the easiest ways to create an “application bundle” to share with
your users; and as a user, these relocatable tarballs are about the
simplest thing you can deal with since you don’t need anything but
`tar`—well, and user namespace support.  Plus, since they are
[bit-reproducible](https://www.gnu.org/software/guix/blog/tags/reproducible-builds/),
anyone can rebuild them to ensure they [do not contain
malware](https://www.omgubuntu.co.uk/2018/05/ubuntu-snap-malware) or to
[check the provenance and licensing of its
contents](https://lwn.net/Articles/752982/).

Application bundles cannot replace full-blown package management, which
allows users to upgrade, get security updates, use storage and memory
efficiently, and so on.  For the purposes of quickly sharing packages
with users or with Guix-less machines, though, you might find Guix packs
to be more convenient than Snap, Flatplak, or Docker.  Give it a spin
and [let us know](https://www.gnu.org/software/guix/contact/)!

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
