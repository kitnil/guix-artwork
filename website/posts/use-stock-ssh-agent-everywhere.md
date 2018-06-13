title: Customize GuixSD: Use Stock SSH Agent Everywhere!
date: 2018-05-26 17:00
author: Chris Marusich
tags: Desktop environments, Functional package management, Programming interfaces, Scheme API, System services
---

I frequently use SSH.  Since I don't like typing my password all the
time, I use an SSH agent.  Originally I used the GNOME Keyring as my
SSH agent, but recently I've switched to using the `ssh-agent` from
OpenSSH.  I accomplished this by doing the following two things:

* Replace the default GNOME Keyring with a custom-built version that
disables the SSH agent feature.

* Start my desktop session with OpenSSH's `ssh-agent` so that it's
always available to any applications in my desktop session.

Below, I'll show you in detail how I did this.  In addition to being
useful for anyone who wants to use OpenSSH's `ssh-agent` in GuixSD, I
hope this example will help to illustrate how GuixSD enables you to
customize your entire system to be just the way you want it!

# The Problem: GNOME Keyring Can't Handle My SSH Keys

On GuixSD, I like to use the [GNOME desktop
environment](https://www.gnome.org).  GNOME is just one of [the
various desktop environments that GuixSD
supports](https://www.gnu.org/software/guix/manual/html_node/Desktop-Services.html).
By default, the GNOME desktop environment on GuixSD comes with a lot
of goodies, including the [GNOME
Keyring](https://wiki.gnome.org/Projects/GnomeKeyring), which is
GNOME's integrated solution for securely storing secrets, passwords,
keys, and certificates.

The GNOME Keyring has many useful features.  One of those is [its SSH
Agent feature](https://wiki.gnome.org/Projects/GnomeKeyring/Ssh).
This feature allows you to use the GNOME Keyring as an SSH agent.
This means that when you invoke a command like `ssh-add`, it will add
the private key identities to the GNOME Keyring.  Usually this is
quite convenient, since it means that GNOME users basically get an SSH
agent for free!

Unfortunately, up until [GNOME 3.28 (the current
release)](https://www.gnome.org/news/2018/03/gnome-3-28-released/),
the GNOME Keyring's SSH agent implementation was not as complete as
the stock SSH agent from OpenSSH.  As a result, [earlier versions of
GNOME Keyring did not support many use
cases](https://bugzilla.gnome.org/show_bug.cgi?id=775981).  This was a
problem for me, since GNOME Keyring couldn't read my modern SSH keys.
To make matters worse, by design the SSH agent for GNOME Keyring and
OpenSSH both use the same environment variables (e.g.,
`SSH_AUTH_SOCK`).  This makes it difficult to use OpenSSH's
`ssh-agent` everywhere within my GNOME desktop environment.

Happily, starting with GNOME 3.28, [GNOME Keyring delegates all SSH
agent functionality to the stock SSH agent from
OpenSSH](https://bugzilla.gnome.org/show_bug.cgi?id=775981).  They
have removed their custom implementation entirely.  This means that
today, I could solve my problem simply by using the most recent
version of GNOME Keyring.  I'll probably do just that when the new
release gets included in Guix.  However, when I first encountered this
problem, GNOME 3.28 hadn't been released yet, so the only option
available to me was to customize GNOME Keyring or remove it entirely.

In any case, I'm going to show you how I solved this problem by
modifying the default GNOME Keyring from the Guix package collection.
The same ideas can be used to customize any package, so hopefully it
will be a useful example.  And what if you don't use GNOME, but you do
want to use OpenSSH's `ssh-agent`?  In that case, you may still need
to customize your GuixSD system a little bit.  Let me show you how!

# The Solution: `~/.xsession` and a Custom GNOME Keyring

The goal is to make OpenSSH's `ssh-agent` available everywhere when we
log into our GNOME desktop session.  First, we must arrange for
`ssh-agent` to be running whenever we're logged in.

There are many ways to accomplish this.  For example, I've seen people
implement shell code in their shell's start-up files which basically
manages their own `ssh-agent` process.  However, I prefer to just
start `ssh-agent` once and not clutter up my shell's start-up files
with unnecessary code.  So that's what we're going to do!

# Launch OpenSSH's `ssh-agent` in Your `~/.xsession`

By default, GuixSD uses the [SLiM desktop
manager](https://sourceforge.net/projects/slim.berlios).  When you log
in, SLiM presents you with a menu of so-called "desktop sessions",
which correspond to the desktop environments you've declared in your
[operating system
declaration](https://www.gnu.org/software/guix/manual/html_node/operating_002dsystem-Reference.html).
For example, if you've added the
[gnome-desktop-service](https://www.gnu.org/software/guix/manual/html_node/Desktop-Services.html)
to your operating system declaration, then you'll see an option for
GNOME at the SLiM login screen.

You can further customize your desktop session with the `~/.xsession`
file.  The contract for this file in GuixSD is the same as it is for
many GNU/Linux distributions: [if it exists, then it will be
executed](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/services/xorg.scm?id=263c9941a1e523b360ca9f42d1ed6b11e6e6e285#n392).
The arguments passed to it will be the command line invocation that
would normally be executed to start the desktop session that you
selected from the SLiM login screen.  Your `~/.xsession` is expected
to do whatever is necessary to customize and then start the specified
desktop environment.  For example, when you select GNOME from the SLiM
login screen, your `~/.xsession` file will basically be executed like
this (for the exact execution mechanism, please refer to the source
code linked above):

```shell
$ ~/.xsession gnome-session
```

The upshot of all this is that the `~/.xsession` is an *ideal* place
to set up your SSH agent!  If you start an SSH agent in your
`~/.xsession` file, you can have the SSH agent available everywhere,
automatically!  Check it out: Put this into your `~/.xsession` file,
and make the file executable:

```shell
#!/run/current-system/profile/bin/bash
exec ssh-agent "$@"
```

When you invoke `ssh-agent` in this way, it executes the specified
program in an environment where commands like `ssh-add` just work.  It
does this by setting environment variables such as `SSH_AUTH_SOCK`,
which programs like `ssh-add` find and use automatically.  Because
GuixSD allows you to customize your desktop session like this, you can
use any SSH agent you want in any desktop environments that you want,
automatically!

Of course, if you're using GNOME Keyring version 3.27 or earlier (like
I was), then this isn't quite enough.  In that case, the SSH agent
feature of GNOME Keyring will override the environment variables set
by OpenSSH's `ssh-agent`, so commands like `ssh-add` will wind up
communicating with the GNOME Keyring instead of the `ssh-agent` you
launched in your `~/.xsession`.  This is bad because, as previously
mentioned, GNOME Keyring version 3.27 or earlier doesn't support as
many uses cases as OpenSSH's `ssh-agent`.

How can we work around this problem?

# Customize the GNOME Keyring

One heavy-handed solution would be to remove GNOME Keyring entirely.
That would work, but then you would lose out on all the other great
features that it has to offer.  Surely we can do better!

The GNOME Keyring documentation
[explains](https://wiki.gnome.org/Projects/GnomeKeyring/Ssh) that one
way to disable the SSH agent feature is to include the
`--disable-ssh-agent` configure flag when building it.  Thankfully,
Guix provides some ways to customize software in *exactly* this way!

Conceptually, we "just" have to do the following two things:

* Customize the existing `gnome-keyring` package.

* Make the `gnome-desktop-service` use our custom `gnome-keyring`
package.

# Create a Custom GNOME Keyring Package

Let's begin by defining a custom `gnome-keyring` package, which we'll
call `gnome-keyring-sans-ssh-agent`.  With Guix, we can do this in
less than ten lines of code:

```scheme
(define-public gnome-keyring-sans-ssh-agent
  (package
    (inherit gnome-keyring)
    (name "gnome-keyring-sans-ssh-agent")
    (arguments
     (substitute-keyword-arguments
         (package-arguments gnome-keyring)
       ((#:configure-flags flags)
        `(cons "--disable-ssh-agent" ,flags))))))
```

Don't worry if some of that code is unclear at first.  I'll clarify it
now!

In Guix, a `<package>` record like the one above is defined by a macro
called `define-record-type*` ([defined in the file guix/records.scm in
the Guix
source](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/records.scm?id=263c9941a1e523b360ca9f42d1ed6b11e6e6e285#n178)).
It's similar to an [SRFI-9
record](https://www.gnu.org/software/guile/manual/html_node/SRFI_002d9-Records.html#SRFI_002d9-Records).
The `inherit` feature of this macro is very useful: it creates a new
copy of an existing record, overriding specific fields in the new copy
as needed.

In the above, we define `gnome-keyring-sans-ssh-agent` to be a copy of
the `gnome-keyring` package, and we use `inherit` to change the `name`
and `arguments` fields in that new copy.  We also use the
`substitute-keyword-arguments` macro ([defined in the file
guix/utils.scm in the Guix
source](https://git.savannah.gnu.org/cgit/guix.git/tree/guix/utils.scm?id=263c9941a1e523b360ca9f42d1ed6b11e6e6e285#n345))
to add `--disable-ssh-agent` to the list of [configure
flags](https://www.gnu.org/software/guix/manual/html_node/Build-Systems.html)
defined in the `gnome-keyring` package.  The effect of this is to
define a new GNOME Keyring package that is built exactly the same as
the original, but in which the SSH agent is disabled.

I'll admit this code may seem a little opaque at first, but all code
does when you first learn it.  Once you get the hang of things, you
can customize packages any way you can imagine.  If you want to learn
more, you should read the docstrings for the `define-record-type*` and
`substitute-keyword-arguments` macros in the Guix source code.  It's
also very helpful to `grep` the source code to see examples of how
these macros are used in practice.  For example:

```shell
$ # Search the currently installed Guix for the current user.
$ grep -r substitute-keyword-arguments ~/.config/guix/latest
$ # Search the Guix Git repository, assuming you've checked it out here.
$ grep -r substitute-keyword-arguments ~/guix
```

# Use the Custom GNOME Keyring Package

OK, we've created our own custom GNOME Keyring package.  Great!  Now,
how do we use it?

In GuixSD, the GNOME desktop environment is treated as a [system
service](
https://www.gnu.org/software/guix/manual/html_node/Services.html).  To
make GNOME use our custom GNOME Keyring package, we must somehow
customize the `gnome-desktop-service` ([defined in the file
gnu/services/desktop.scm](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/services/desktop.scm?id=263c9941a1e523b360ca9f42d1ed6b11e6e6e285#n795))
to use our custom package.  How do we customize a service?  Generally,
the answer depends on the service.  Thankfully, many of GuixSD's
services, including the `gnome-desktop-service`, follow a similar
pattern.  In this case, we "just" need to pass a custom
`<gnome-desktop-configuration>` record to the `gnome-desktop-service`
procedure in our operating system declaration, like this:

```scheme
(operating-system

  ...

  (services (cons*
             (gnome-desktop-service
              #:config my-gnome-desktop-configuration)
             %desktop-services)))
```

Here, the `cons*` procedure just adds the GNOME desktop service to the
`%desktop-services` list, returning the new list.  For details, please
refer to [the Guile
manual](https://www.gnu.org/software/guile/manual/html_node/List-Constructors.html#index-cons_002a).

Now the question is: what should `my-gnome-desktop-configuration` be?
Well, if we examine [the definition of this record type in the Guix
source](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/services/desktop.scm?id=263c9941a1e523b360ca9f42d1ed6b11e6e6e285#n799),
we see the following:

```scheme
(define-record-type* <gnome-desktop-configuration> gnome-desktop-configuration
  make-gnome-desktop-configuration
  gnome-desktop-configuration
  (gnome-package gnome-package (default gnome)))
```

The `gnome` package referenced here is a "meta" package: it exists
only to aggregate many GNOME packages together, including
`gnome-keyring`.  To see [its
definition](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/gnome.scm?id=263c9941a1e523b360ca9f42d1ed6b11e6e6e285#n5977),
we can simply invoke `guix edit gnome`, which [opens the file where
the package is
defined](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-edit.html#Invoking-guix-edit):

```scheme
(define-public gnome
  (package
    (name "gnome")
    (version (package-version gnome-shell))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
    (propagated-inputs
     ;; TODO: Add more packages according to:
     ;;       <https://packages.debian.org/jessie/gnome-core>.
     `(("adwaita-icon-theme"        ,adwaita-icon-theme)
       ("baobab"                    ,baobab)
       ("font-cantarell"            ,font-cantarell)
       [... many packages omitted for brevity ...]
       ("gnome-keyring"             ,gnome-keyring)
       [... many packages omitted for brevity ...]
    (synopsis "The GNU desktop environment")
    (home-page "https://www.gnome.org/")
    (description
     "GNOME is the graphical desktop for GNU.  It includes a wide variety of
applications for browsing the web, editing text and images, creating
documents and diagrams, playing media, scanning, and much more.")
    (license license:gpl2+)))
```

Apart from being a little long, this is [just a normal package
definition](https://www.gnu.org/software/guix/manual/html_node/Defining-Packages.html#Defining-Packages).
We can see that `gnome-keyring` is included in the list of
`propagated-inputs`.  So, we need to create a replacement for the
`gnome` package that uses our `gnome-keyring-sans-ssh-agent` instead
of `gnome-keyring`.  The following package definition accomplishes
that:

```scheme
(define-public gnome-sans-ssh-agent
  (package
    (inherit gnome)
    (name "gnome-sans-ssh-agent")
    (propagated-inputs
     (map (match-lambda
            ((name package)
             (if (equal? name "gnome-keyring")
                 (list name gnome-keyring-sans-ssh-agent)
                 (list name package))))
          (package-propagated-inputs gnome)))))
```

As before, we use `inherit` to create a new copy of the `gnome`
package that overrides the original `name` and `propagated-inputs`
fields.  Since Guix packages are just defined using good old scheme,
we can use existing language features like
[`map`](https://www.gnu.org/software/guile/manual/html_node/List-Mapping.html#index-map)
and
[`match-lambda`](https://www.gnu.org/software/guile/manual/html_node/Pattern-Matching.html#Pattern-Matching)
to manipulate the list of propagated inputs.  The effect of the above
is to create a new package that is the same as the `gnome` package but
uses `gnome-keyring-sans-ssh-agent` instead of `gnome-keyring`.

Now that we have `gnome-sans-ssh-agent`, we can create a custom
`<gnome-desktop-configuration>` record and pass it to the
`gnome-desktop-service` procedure as follows:

```scheme
(operating-system

  ...

  (services (cons*
             (gnome-desktop-service
              #:config (gnome-desktop-configuration
                        (gnome-package gnome-sans-ssh-agent)))
             %desktop-services)))
```

# Wrapping It All Up

Finally, you need to run the following commands as `root` to create
and boot into the new [system
generation](https://www.gnu.org/software/guix/manual/html_node/Invoking-guix-system.html)
(replace `MY-CONFIG` with the path to the customized operating system
configuration file):

```shell
# guix system reconfigure MY-CONFIG
# reboot
```

After you log into GNOME, any time you need to use SSH, the stock SSH
agent from OpenSSH that you started in your `~/.xsession` file will be
used instead of the GNOME Keyring's SSH agent.  It just works!  Note
that it still works even if you select a non-GNOME desktop session
(like XFCE) at the SLiM login screen, since the `~/.xsession` is not
tied to any particular desktop session,

In the unfortunate event that something went wrong and things just
aren't working when you reboot, don't worry: with GuixSD, you can
safely roll back to the previous system generation via [the usual
mechanisms](https://www.gnu.org/software/guix/manual/html_node/Using-the-Configuration-System.html#index-roll_002dback_002c-of-the-operating-system).
For example, you can run this from the command line to roll back:

```shell
# guix system roll-back
# reboot
```

This is one of the great benefits that comes from the fact that [Guix
follows the functional software deployment
model](https://www.gnu.org/software/guix/manual/html_node/Introduction.html#Introduction).
However, note that because the `~/.xsession` file (like many files in
your home directory) is not managed by Guix, you must manually undo
the changes that you made to it in order to roll back fully.

# Conclusion

I hope this helps give you some ideas for how you can customize your
own GuixSD system to make it exactly what you want it to be.  Not only
can you customize your desktop session via your `~/.xsession` file,
but Guix also provides tools for you to modify any of the default
packages or services to suit your specific needs.

Happy hacking!

# Notices

[![CC0](https://licensebuttons.net/p/zero/1.0/88x31.png "CC0 1.0
Universal")](http://creativecommons.org/publicdomain/zero/1.0/)

To the extent possible under law, Chris Marusich has waived all
copyright and related or neighboring rights to this article,
"Customize GuixSD: Use Stock SSH Agent Everywhere!".  This work is
published from: United States.

The views expressed in this article are those of Chris Marusich and do
not necessarily reflect the views of his past, present, or future
employers.

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
