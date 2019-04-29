title: State of aarch64 on Guix
date: 2017-07-24 15:30
author: Efraim Flashner
tags: ARM, Cross-compilation
---
Since the recent 0.13.0 release, Guix supports building software for
aarch64 (64-bit ARM architecture).  Here’s the current status.

Currently aarch64 support in Guix is pretty good, as long as you don't
mind compiling for yourself :). Potential downfalls are too little RAM
(I limited my boards to 2GB minimum) and using an SD card. For building
packages I made sure that between RAM and swap I have at least 6 GB,
which I don't recall giving me any issues.

There were problems with actually building the Guix binary in time for
the 0.13 release. It has since been fixed and I have an unoffical
aarch64 binary install tarball at http://flashner.co.il/~efraim/. Also there is
the signing key for my odroid running 
[`guix publish`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-publish.html).
The URL of my `guix publish` server is `http://git.flashner.co.il:8181`.

General problem points/packages:

  - Java is currently out, `sablevm-classpath` doesn't compile, so currently
there is no path for Java.  A quick check showed about 140 packages
depend on `sablevm-classpath`.
  - Go: go-1.4.x doesn't support aarch64 (or mips). I have a patch against
our GCC to build gccgo, and it produces a `go` binary, but it fails to
actually build anything.  When I checked Debian I saw they cross-compile
their arm64 `go` binary from amd64.  I believe there may be an issue with
using gccgo and linking against glibc.
  - OCaml 4.01.0: Doesn't build on aarch64, haven't investigated.
  - Julia: aarch64 is officially supported, but it has only been tested on
superpowerful boards, like the ThunderX. I haven't gotten it to build
yet. The issue is related to `__fp16`.
  - clisp: our current version doesn't build on aarch64, there isn't
support yet. There are newer builds but no offical release yet, and I
haven't tested those yet.
  - gprolog: No upstream support and AFAICT no one is working on it.
  - LDC: 1.x is supposed to support aarch64, 0.17.x, aka ldc-bootstrap,
doesn't, it fails while compiling phobos, which has no aarch64 support
in that version.
  - Rust: Has upstream support, our package uses the i686 version as a
bootstrap, so only i686 and x86_64 have support in guix ATM.
  - Haskell: There is no upstream aarch64 binary to use for bootstrapping.
I'm thinking of trying to use qemu-system-x86_64 as the shell and
emulate x86_64 on my aarch64 board to cross-compile it to aarch64. `guix
package -A ghc | wc -l` shows 293 packages.
  - Qt 4: does not build, I've hardly put any time into it.
  - Gnucash: The ancient WebKit version they use didn't build on aarch64,
I haven't tried to fix it.
  - Linux-libre: While many boards do require specific patches and
	versions of the kernel, there have been great increases in recent
	kernel versions for many ARM boards. It remains to be seen how much
	support these boards have after the kernel has been deblobbed.

It sounds like its all doom and gloom, but its not too bad. `guix
package -A | wc -l` shows me 5,341 (5,208 without `sablevm-classpath`),
compared with ~5,600 on x86_64. Most of the difference is Haskell. In
addition, I personally believe that aarch64 actually has fewer 
packages that fail to build than armhf.

Currently the project’s build farm lacks aarch64 build machines.  If you
would like to help,
please [get in touch with us](https://gnu.org/software/guix/donate/)!


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

