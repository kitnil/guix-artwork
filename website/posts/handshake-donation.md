title: GNU Guix receives donation from the Handshake project
date: 2018-12-03 22:00
author: Ludovic Courtès, Tobias Geerinckx-Rice, Ricardo Wurmus
tags: Outreachy, Build farm, Fundraising
---

Just a few days after [it turned
six](https://lists.gnu.org/archive/html/guix-devel/2018-11/msg00446.html),
Guix received a great birthday present: the [Handshake
project](https://www.handshake.org/), which works on the design and
implementation of a decentralized naming protocol compatible with the
Domain Name System (DNS), [made a large donation to the GNU Project
_via_ the Free Software Foundation
(FSF)](https://www.fsf.org/news/free-software-foundation-receives-1-million-from-handshake).
Of this donation, 100,000 USD go to GNU Guix.

![Handshake & Guix logos.](https://www.gnu.org/software/guix/static/blog/img/handshake+guix.png)

This donation will allow the project to guarantee its independence,
invest in hardware for its build farm, and develop new features to
benefit all our users.

We will be able to grow the performance and reliability of our existing
infrastructure.  We also envision better support for new and liberating
architectures, and more resilient long-term storage of binaries and
source code.

It will also allow us to continue our outreach efforts and attract new
interns, for example through [Outreachy](https://www.outreachy.org/), to
further improve and promote the project.

The project funds are held by the FSF and spending is overseen by a
committee currently consisting of Ricardo Wurmus, Tobias Geerinckx-Rice,
and Ludovic Courtès, all core Guix developers.  The FSF is the fiscal
sponsor of [free software
efforts](https://www.fsf.org/working-together/fund), including Guix, as
part of its [Working Together for Free Software
fund](https://my.fsf.org/associate/donate/working-together).  Other
recipients include GNU Octave, the GNU Tool Chain, and Replicant.
Congratulations to them as well!

Above all, a big thank you to Handshake for its support of free software
and GNU!  The least we could do to show our appreciation was to add a
package definition for the Handshake resolver daemon, [which we just
did](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=91a4863d9d727754d1743f4c0591c63b950494cf).


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

GuixSD can be used on i686, x86_64, ARMv7, and AArch64 machines.  It
is also possible to use Guix on top of an already installed GNU/Linux
system, including on mips64el and aarch64.
