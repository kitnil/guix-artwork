title: Running system services in containers
date: 2017-04-14 14:45
author: Ludovic Courtès
tags: system services containers shepherd
---

At FOSDEM, in the awesome
[Guile track](https://fosdem.org/2017/schedule/track/gnu_guile/), I
briefly demoed a new experimental GuixSD feature as part my
[talk on system services](https://fosdem.org/2017/schedule/event/composingsystemservicesinguixsd/):
the ability to run system services in containers or “sandboxes”.  This
post discusses the rationale, status, and implementation of this
feature.

#### The problem

Our computers run many programs that talk to the Internet, and the
Internet is an unsafe place as we all know—with states and assorted
organizations
[collecting “zero-day exploits”](https://www.wired.com/2014/04/obama-zero-day/)
to exploit them as they see fit.  One of the big tasks of operating
system distributions has been to keep track of known software
vulnerabilities and patch their packages as soon as possible.

When we look closer, many vulnerabilities out there can be exploited
because of a combination of two major weaknesses of GNU/Linux and
similar Unix-like operating systems: lack of memory-safety in the C
language family, and
[ambient authority](https://en.wikipedia.org/wiki/Ambient_authority) in
the operating system itself.  The former leads to a huge class of bugs
that become security issues: buffer overflows, use-after-free, and so
on.  The latter makes them more exploitable because processes have
access to many resources beyond those they really need.

Security-sensitive software is now increasingly written in memory-safe
languages, as is the case for Guix and GuixSD.  Projects that have been
using C are even considering a complete rewrite,
[as is the case for Tor](https://lists.torproject.org/pipermail/tor-dev/2017-March/012088.html).
Of course the switch away from memory-unsafe languages won’t happen
overnight, but it’s good to see a consensus emerging.

The operating system side of things is less bright.  Although the
[principle of least authority (POLA)](https://en.wikipedia.org/wiki/Principle_of_least_authority)
has been well-known in operating system circles for a long time, it
remains foreign to Unix and GNU/Linux.  Processes run with the full
authority of their user.  On top of that, until recent changes to the
Linux kernel, resources were global and there was essentially a unique
view of the file system, of the process hierarchy, and so on.  So when a
remote-code-execution vulnerability affects a system service—like
[in the BitlBee instant messaging gateway (CVE-2016-10188)](https://bugs.bitlbee.org/ticket/1281)
running on my laptop—an attacker could potentially do a lot on your
machine.

Fortunately, many daemons have built-in mechanisms to work around this
operating system defect.  For instance,
[BitlBee](https://github.com/bitlbee/bitlbee/blob/master/unix.c#L155),
and
[Tor](https://gitweb.torproject.org/tor.git/tree/src/or/config.c#n1338)
can be told to switch to a separate unprivileged user,
[`avahi-daemon`](https://github.com/lathiat/avahi/blob/master/avahi-daemon/chroot.c)
and
[`ntpd`](https://github.com/ntp-project/ntp/blob/stable/ntpd/ntpd.c#L1000)
can do that and also change root.  These techniques do reduce the
privileges of those processes, but they are still imperfect and _ad
hoc_.

#### Increasing process isolation with containers

The optimal solution to this problem would be to honor POLA in the first
place.  As an example, the venerable GNU/Hurd is a
[capability-based operating system](https://en.wikipedia.org/wiki/Capability-based_security).
Thus, GNU/Hurd has supported fine-grained virtualization from the start:
a newly-created process can be given a capability to its own `proc`
server (which implements the POSIX notion of processes), to a specific
TCP/IP server, etc.  In addition, its POSIX personality offers
interesting extensions, such as the fact that processes run with the
authority of
[_zero_](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/mach/hurd/getuid.c;h=ff95fd58b8a9819fd0525d42ed8c3d85dbb6cb99;hb=HEAD#l42)
or more UIDs.  For instance, the Hurd’s
[`login` program](https://git.savannah.gnu.org/cgit/hurd/hurd.git/tree/utils/login.c)
starts off with zero UIDs and gains a UID when someone has been
authenticated.

Back to GNU/Linux,
[“namespaces”](http://man7.org/linux/man-pages/man7/namespaces.7.html)
have been introduced as a way to retrofit per-process views of the
system resources, and thus improve isolation among processes.  Each
process can run in a separate namespace and thus have a different view
of the file system, process tree, and so on (a process running in
separate namespaces is often referred to as a “container”, although that
term is sometimes used to denote much larger tooling and practices built
around namespaces.)  Why not use that to better isolate system services?

Apparently this idea has been floating around.  systemd has been
[considering to extend its “unit files”](https://lwn.net/Articles/706025/)
to include directives instructing systemd to run daemons in separate
namespaces.  GuixSD uses
[the Shepherd](https://www.gnu.org/software/shepherd) instead of
systemd, but running system services in separate namespaces is something
we had been considering for a while.

In fact, adding the ability to run system services in containers was a
low-hanging fruit: we already had
[`call-with-container`](https://www.gnu.org/software/guix/news/container-provisioning-with-guix.html)
to run code in containers, so all we needed to do was to
[provide a containerized service starter](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=63302a4e55241a41eab4c21d7af9fbd0d5817459)
that uses `call-with-container`.

The Shepherd itself remains unaware of namespaces, it simply ends up
calling
[`make-forkexec-constructor/container`](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/build/shepherd.scm#n108)
instead of
[`make-forkexec-constructor`](https://www.gnu.org/software/shepherd/manual/html_node/Service-De_002d-and-Constructors.html#index-make_002dforkexec_002dconstructor)
and that’s it.  The changes to the service definitions of
[BitlBee](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=a062b6ca99ad61c9df473fe49a93d69f9698c59d)
and
[Tor](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=ee295346ce81c276ffb4ee34cc6f5b134b415097)
are minimal.  The end result, for Tor, looks like this:

```scheme
(let ((torrc (tor-configuration->torrc config)))
  (with-imported-modules (source-module-closure
						  '((gnu build shepherd)
							(gnu system file-systems)))
	(list (shepherd-service
		   (provision '(tor))
		   (requirement '(user-processes loopback syslogd))

		   (modules '((gnu build shepherd)
					  (gnu system file-systems)))

		   (start #~(make-forkexec-constructor/container
					 (list #$(file-append tor "/bin/tor") "-f" #$torrc)

					 #:mappings (list (file-system-mapping
									   (source "/var/lib/tor")
									   (target source)
									   (writable? #t))
									  (file-system-mapping
									   (source "/dev/log") ;for syslog
									   (target source)))))
		   (stop #~(make-kill-destructor))
		   (documentation "Run the Tor anonymous network overlay.")))))
```

The
[`with-imported-modules`](https://www.gnu.org/software/guix/manual/html_node/G_002dExpressions.html#index-with_002dimported_002dmodules)
form above instructs Guix to _import_ our `(gnu build shepherd)`
library, which provides `make-forkexec-constructor/container`, into
PID 1.  The `start` method of the service specifies the command to start
the daemon, as well as file systems to map in its mount name space
(“bind mounts”).  Here all we need is write access to `/var/lib/tor` and
to `/dev/log` (for logging _via_ syslogd).  In addition to these two
mappings, `make-forkexec-constructor/container` automatically adds
`/gnu/store` and a bunch of files in `/etc` as we will see below.

#### Containerized services in action

So what do these containerized services look like when they’re running?
When we run
[`herd status bitblee`](https://www.gnu.org/software/shepherd/manual/html_node/Invoking-herd.html),
disappointingly, we don’t see anything special:

```
charlie@guixsd ~$ sudo herd status bitlbee
Status of bitlbee:
  It is started.
  Running value is 487.
  It is enabled.
  Provides (bitlbee).
  Requires (user-processes networking).
  Conflicts with ().
  Will be respawned.
charlie@guixsd ~$ ps -f 487
UID        PID  PPID  C STIME TTY      STAT   TIME CMD
bitlbee    487     1  0 Apr11 ?        Ss     0:00 /gnu/store/pm05bfywrj2k699qbxpjjqfyfk3grz2i-bitlbee-3.5.1/sbin/bitlbee -n -F -u bitlbee -c /gnu/store/y4jfxya56i1hl9z0a2h4hdar2wm
```

Again this is because the Shepherd has no idea what a namespace is, so
it just displays the daemon’s PID in the global namespace, `487`.  The
process is running as user `bitlbee`, as requested by the `-u bitlbee`
command-line option.

We can invoke
[`nsenter`](http://man7.org/linux/man-pages/man1/nsenter.1.html) and
take a look at what the BitlBee process “sees” in its namespace:

```
charlie@guixsd ~$ sudo nsenter -t 487 -m -p -i -u $(readlink -f $(type -P bash))
root@guixsd /# echo /*
/dev /etc /gnu /proc /tmp /var
root@guixsd /# echo /proc/[0-9]*
/proc/1 /proc/5
root@guixsd /# read line < /proc/1/cmdline
root@guixsd /# echo $line
/gnu/store/pm05bfywrj2k699qbxpjjqfyfk3grz2i-bitlbee-3.5.1/sbin/bitlbee-n-F-ubitlbee-c/gnu/store/y4jfxya56i1hl9z0a2h4hdar2wmivgbl-bitlbee.conf
root@guixsd /# echo /etc/*
/etc/hosts /etc/nsswitch.conf /etc/passwd /etc/resolv.conf /etc/services
root@guixsd /# echo /var/*
/var/lib /var/run
root@guixsd /# echo /var/lib/*
/var/lib/bitlbee
root@guixsd /# echo /var/run/*
/var/run/bitlbee.pid /var/run/nscd
```

There’s no `/home` and generally very little in BitlBee’s mount
namespace.  Notably, the namespace lacks `/run/setuid-programs`, which
is where
[setuid programs](https://www.gnu.org/software/guix/manual/html_node/Setuid-Programs.html)
live in GuixSD.  Its `/etc` directory contains the minimal set of files
needed for proper operation rather than the complete `/etc` of the host.
`/var` contains nothing but BitlBee’s own state files, as well as the
socket to libc’s name service cache daemon (`nscd`), which runs in the
host system and performs name lookups on behalf of applications.

As can be seen in `/proc`, there’s only a couple of processes in there
and “PID 1” in that namespace is the `bitlbee` daemon.  Finally, the
`/tmp` directory is a private tmpfs:

```
root@guixsd /# : > /tmp/hello-bitlbee
root@guixsd /# echo /tmp/*
/tmp/hello-bitlbee
root@guixsd /# exit
charlie@guixsd ~$ ls /tmp/*bitlbee
ls: cannot access '/tmp/*bitlbee': No such file or directory
```

Our `bitlbee` process runs in a separate mount, PID, and IPC namespace,
but it runs in the global user namespace.  The reason for this is that
we want the `-u bitlbee` option (which instructs `bitlbee` to setuid to
an unprivileged user at startup) to work as expected.  It also shares
the network namespace because obviously it needs to access the network.

A nice side-effect of these fully-specified execution environments for
services is that it makes them more likely to behave in a reproducible
fashion across machines—just like fully-specified build environments
[help achieve reproducible builds](https://www.gnu.org/software/guix/news/reproducible-builds-a-means-to-an-end.html).

#### Conclusion

GuixSD `master` and its upcoming release include this feature and a
couple of containerized services, and it works like a charm!  Yet, there
are still open questions as to the way forward.

First, we only looked at “simple” services so far, with simple static
file system mappings.  Good candidates for increased isolation are HTTP
servers such as NGINX.  However, for these, it’s more difficult to
determine the set of file system mappings that must be made.  GuixSD has
the advantage that it knows
[how NGINX is configured](https://www.gnu.org/software/guix/manual/html_node/Web-Services.html)
and could potentially derive file system mappings from that information.
Getting it right may be trickier than it seems, though, so this is
something we’ll have to investigate.

Another open question is how the service isolation work should be split
between the distro, the init system, and the upstream service author.
Authors of daemons already do part of the work _via_ `setuid` and
sometimes `chroot`.  Going beyond that would often hamper portability
(the namespace interface is specific to the kernel Linux) or even
functionality if the daemon ends up lacking access to resources it
needs.

The init system alone also lacks information to decide what goes into
the namespaces of the service.  For instance, neither the upstream
author nor the init system “knows” whether the distro is running `nscd`
and thus they cannot tell whether the `nscd` socket should be
bind-mounted in the service’s namespace.  A similar issue is that of
D-Bus policy files discussed in
[this LWN article](https://lwn.net/Articles/706025/).  Moving D-Bus
functionality into the init system itself to solve this problem, as the
article suggests, seems questionable, notably because it would add more
code to this critical process.  Instead, on GuixSD, a service author can
make the right policy files available in the sandbox; in fact, GuixSD
already knows which policy files are needed thanks to its service
framework so we might even be able to automate it.

At this point it seems that tight integration between the distro and the
init system is the best way to precisely define system service
sandboxes.  GuixSD’s
[declarative approach to system services](https://www.gnu.org/software/guix/manual/html_node/Using-the-Configuration-System.html#System-Services)
along with tight Shepherd integration help a lot here, but it remains to
be seen how difficult it is to create sandboxes for complex system
services such as NGINX.

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
