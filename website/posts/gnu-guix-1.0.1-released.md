title: GNU Guix 1.0.1 released
date: 2019-05-19 23:30
author: Ludovic Courtès
slug: gnu-guix-1.0.1-released
tags: Releases, System tests
---
We are pleased to announce the release of GNU Guix version 1.0.1.  This
new version fixes bugs in the [graphical
installer](https://www.gnu.org/software/guix/manual/en/html_node/Guided-Graphical-Installation.html)
for the standalone Guix System.

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

It’s been just over two weeks since we [announced
1.0.0](https://www.gnu.org/software/guix/blog/2019/gnu-guix-1.0.0-released/)—two
weeks and 706 commits by 40 people already!

This is primarily a bug-fix release, specifically focusing on issues in
the graphical installer for the standalone system:

  - The most [embarrassing bug](https://issues.guix.gnu.org/issue/35541)
    would lead the graphical installer to produce a configuration where
    [`%base-packages`](https://www.gnu.org/software/guix/manual/en/html_node/Using-the-Configuration-System.html#index-_0025base_002dpackages)
    was omitted from the `packages` field.  Consequently, the freshly
    installed system would not have the usual commands in `$PATH`—`ls`,
    `ps`, etc.—and Xfce would fail to start for that reason.  See below
    for a “post-mortem” analysis.
  - The `wpa-supplicant` service would [sometimes fail to
    start](https://issues.guix.gnu.org/issue/35550) in the installation
    image, thereby breaking network access; this is now fixed.
  - The installer [now](https://issues.guix.gnu.org/issue/35540) allows
    you to toggle the visibility of passwords and passphrases, and it [no
    longer](https://issues.guix.gnu.org/issue/35716) restricts their
    length.
  - The installer [can now
    create](https://issues.guix.gnu.org/issue/35716) Btrfs file
    systems.
  - `network-manager-applet` is [now](https://issues.guix.gnu.org/35554)
    part of
    [`%desktop-services`](https://www.gnu.org/software/guix/manual/en/html_node/Desktop-Services.html#index-_0025desktop_002dservices),
    and thus readily usable not just from GNOME but also from Xfce.
  - The
    [`NEWS`](https://git.savannah.gnu.org/cgit/guix.git/tree/NEWS?h=version-1.0.1)
    file has more details, but there were also minor bug fixes for
    [`guix environment`](https://bugs.gnu.org/35618), [`guix
    search`](https://bugs.gnu.org/35588), and [`guix
    refresh`](https://bugs.gnu.org/35684).

A couple of new features were reviewed in time to make it into 1.0.1:

  - [`guix system
    docker-image`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-system.html)
    now produces an OS image with an “entry point”, which makes it
    easier to use than before.
  - [`guix system
    container`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-system.html)
    has a new `--network` option, allowing the container to share
    networking access with the host.
  - 70 new packages were added and 483 packages were updated.
  - Translations were updated as usual and we are glad to announce a
    20%-complete [Russian translation of the
    manual](https://www.gnu.org/software/guix/manual/ru/html_node).


# Recap of [bug #35541](https://issues.guix.gnu.org/issue/35541)

The 1.0.1 release was primarily motivated by [bug
#35541](https://issues.guix.gnu.org/issue/35541), which was reported
shortly after the 1.0.0 release.  If you installed Guix System with the
[graphical
installer](https://www.gnu.org/software/guix/manual/en/html_node/Guided-Graphical-Installation.html),
chances are that, because of this bug, you ended up with a system where
all the usual GNU/Linux commands—`ls`, `grep`, `ps`, etc.—were _not_ in
`$PATH`.  That in turn would also prevent Xfce from starting, if you
chose that desktop environment for your system.

We quickly published a note in the [system installation
instructions](https://www.gnu.org/software/guix/manual/en/html_node/Guided-Graphical-Installation.html)
explaining how to work around the issue:

  - First, install packages that provide those commands, along with the
    text editor of your choice (for example, `emacs` or `vim`):
	
	```
    guix install coreutils findutils grep procps sed emacs vim
	```

  - At this point, the essential commands you would expect are
    available.  Open your configuration file with your editor of choice,
    for example `emacs`, running as root:

    ```
    sudo emacs /etc/config.scm
	```

  - Change the `packages` field to add the “base packages” to the list of
    globally-installed packages, such that your configuration looks like
    this:

    ```scheme
	(operating-system
	  ;; … snip …
	  (packages (append (list (specification->package "nss-certs"))
						%base-packages))
	  ;; … snip …
	  )
    ```

  - Reconfigure the system so that your new configuration is in effect:

    ```
    guix pull && sudo guix system reconfigure /etc/config.scm
	```

If you already installed 1.0.0, you can perform the steps above to get
all these core commands back.

Guix is _purely declarative_: if you give it [an operating system
definition](https://www.gnu.org/software/guix/manual/en/html_node/Using-the-Configuration-System.html)
where the “base packages” are not [available
system-wide](https://www.gnu.org/software/guix/manual/en/html_node/Using-the-Configuration-System.html#Globally_002dVisible-Packages),
then it goes ahead and installs precisely that.  That’s exactly what
happened with this bug: the installer generated such a configuration and
passed it to `guix system init` as part of the installation process.

# Lessons learned

Technically, this is a “trivial” bug: it’s fixed by adding one line to
your operating system configuration and reconfiguring, and the fix for
the installer itself [is also a
one-liner](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=ecb0df6817eb3767e6b4dcf1945f3c2dfbe3b44f).
Nevertheless, it’s obviously a serious bug for the impression it
gives—this is _not_ the user experience we want to offer.  So how did such
a serious bug go through unnoticed?

For several years now, Guix has had a number of automated [_system
tests_](https://www.gnu.org/software/guix/blog/2016/guixsd-system-tests/)
running in virtual machines (VMs).  These tests primarily ensure that
[system
services](https://www.gnu.org/software/guix/manual/en/html_node/Services.html)
work as expected, but some of them [specifically test system
installation](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/tests/install.scm):
installing to a RAID or encrypted device, with a separate `/home`, using
Btrfs, etc.  These tests even run on our [continuous integration
service](https://ci.guix.gnu.org/jobset/guix-master) (search for the
“tests.*” jobs there).

Unfortunately, those installation tests target the so-called [“manual”
installation
process](https://www.gnu.org/software/guix/manual/en/html_node/Manual-Installation.html),
which is scriptable.  They do _not_ test the installer’s graphical user
interface.  Consequently, testing the user interface (UI) itself was a
manual process.  Our attention was, presumably, focusing more on UI
aspects since—so we thought—the actual installation tests were already
taken care of by the system tests.  That the generated system
configuration could be syntactically correct but definitely wrong from a
usability viewpoint perhaps didn’t occur to us.  The end result is that
the issue went unnoticed.

The lesson here is that: manual testing should _also_ look for issues in
“unexpected places”, and more importantly, we need automated tests for
the graphical UI.  The Debian and Guix installer UIs are similar—both
using the [Newt](https://pagure.io/newt) toolkit.  Debian tests its
installer using
[“pre-seeds”](https://wiki.debian.org/DebianInstaller/Preseed)
([code](https://salsa.debian.org/installer-team/preseed)), which are
essentially answers to all the questions and choices the UI would
present.  We could adopt a similar approach, or we could test the UI
itself at a lower level—reading the screen, and simulating key strokes.
UI testing is notoriously tricky so we’ll have to figure out how to get
there.

# Conclusion

Our 1.0 party was a bit spoiled by this bug, and we are sorry that
installation was disappointing to those of you who tried 1.0.  We hope
1.0.1 will allow you to try and see what declarative and programmable
system configuration management is like, because that’s where the real
value of Guix System is—the graphical installer is icing on the cake.

Join us [on `#guix` and on the mailing
lists](https://www.gnu.org/software/guix/contact/)!


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

