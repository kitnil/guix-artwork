title: QA on non-Intel at Guix Days
date: 2019-02-06 19:30
author: Efraim Flashner
tags: ARM, Guix Hackathon, FOSDEM
---

During the second day of Guix Days (a FOSDEM [fringe
event](https://fosdem.org/2019/fringe)) we split up into smaller working groups
based on our areas of interest. I led a group which aimed to address some of
the package issues which exist on non-Intel architectures. Of course not
everyone has access to an ARM board, but with the
[`qemu-binfmt-service`](https://www.gnu.org/software/guix/manual/en/html_node/Virtualization-Services.html#index-binfmt_005fmisc)
service it is possible to use QEMU and the `binfmt_misc` functionality of the
Linux kernel to emulate these systems. Many have reported that this system
emulation is comparable in speed to many of the available ARM boards on the
market. Yet another possibility would be to do the hacking on an x86_64 system
and, when we had a working prototype, to test it with QEMU or on actual ARM
hardware.

Our group decided to tackle Go, which was lacking support in Guix on armhf and
aarch64. Upon checking the build logs from Cuirass and the source code for Go we
determined that Go did indeed require the `gold` linker from the GNU Binutils. We
didn't want to modify the copy of Binutils in Guix since it is part of our
bootstrap story, so we quickly put together a new package definition which added
the configure flag to enable `gold`.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     (substitute-keyword-arguments (package-arguments binutils)
       ((#:configure-flags flags)
        `(cons "--enable-gold=default" ,flags))))))
```

This was an obvious first step, and one which we knew would fail. Had it been
this easy `gold` would have been enabled back in 2012 when it was first added.
Our error came in the form of one of the binaries not being able to link against
`libstdc++.so`, which is in the `gcc:lib` output. This was quickly added and we
were off and building again.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     (substitute-keyword-arguments (package-arguments binutils)
       ((#:configure-flags flags)
        `(cons "--enable-gold=default" ,flags))))
    (inputs
     `(("gcc:lib" ,gcc "lib")))))
```

Once again this failed. What were we missing? The correct paths were included,
the file was indeed in the `gcc:lib` output. We inspected the original
`binutils` package again noticed that it was built against a static libgcc, so
of course it wouldn't find the shared library. In order to work quickly we
copied the configure flags rather than inheriting them from `binutils` and
tried our build again.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     (substitute-keyword-arguments (package-arguments binutils)
       ((#:configure-flags flags)
        `(cons* "--enable-gold=default"
                "--enable-new-dtags"
                "--with-lib-path=/no-ld-lib-path"
                "--enable-install-libbfd"
                "--enable-deterministic-archives"))))
    (inputs
     `(("gcc:lib" ,gcc "lib")))))
```

This time we made it through the full build phase and we knew we were almost
there. Our enthusiasm was quickly dampened when we got the error during the
tests: `unable to find the 'dc' program`. What is this `dc` program? This isn't
any package any of us had heard of before. It definitely wasn't packaged in
Guix. A quick `apt-cache search dc` search in Ubuntu showed they didn't have
package either. A second search of Ubuntu, `apt-file search dc | grep '/bin/dc'`
quickly showed us it was in the `bc` package, and soon we were building
`binutils-gold` again.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     (substitute-keyword-arguments (package-arguments binutils)
       ((#:configure-flags flags)
        `(cons* "--enable-gold=default"
                "--enable-new-dtags"
                "--with-lib-path=/no-ld-lib-path"
                "--enable-install-libbfd"
                "--enable-deterministic-archives"))))
    (native-inputs
     `(("bc" ,bc)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))))
```

Approaching the end of the `check` phase we soon ran into another error, there
was an unpatched `/bin/sh` somewhere in the source code which was generated
during the check phase. Based on the build logs we were able to track down
approximately where the code should be, so we downloaded the source `tar xf
$(guix build --source binutils)` and started looking. There were many obvious
`/bin/sh` calls which we cross-referenced with the build logs and the
`patch-source-shebangs` phase, and this left us with some code in
`gold/Makefile.in`, which by default is not included in the
`patch-source-shebangs` and would need to be fixed manually.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* "gold/Makefile.in"
               (("/bin/sh") (which "sh")))
             #t)))
       ,@(substitute-keyword-arguments (package-arguments binutils)
         ((#:configure-flags flags)
          `(cons* "--enable-gold=default"
                  "--enable-new-dtags"
                  "--with-lib-path=/no-ld-lib-path"
                  "--enable-install-libbfd"
                  "--enable-deterministic-archives")))))
    (native-inputs
     `(("bc" ,bc)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))))
```

One more build cycle later and we did it! `/gnu/store/…-binutils-gold-2.31.1`
existed! We now did two things, we copied our patch over to an aarch64 build
machine and we started cleaning up our package definition on our x86_64 build
machine, where we knew we had a working package definition.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* "gold/Makefile.in"
               (("/bin/sh") (which "sh")))
             #t)))
       ,@(substitute-keyword-arguments (package-arguments binutils)
         ((#:configure-flags flags)
          `(cons* "--enable-gold=default"
                  (delete "LDFLAGS=-static-libgcc" ,flags))))))
    (native-inputs
     `(("bc" ,bc)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))))
```

Fortunately for us the changes in the code worked on x86_64 and we still got a
working `binutils-gold` output. On our aarch64 side the build was progressing
nicely and everything seemed fine... until we suddenly were presented with big
red errors about unrelocatable code. How could it? Everything was working so
well! Undeterred, we built the source again, this time targeting armhf and were
unfortunately presented with similar errors. Deciding to address the test
failures later (It's ARM! It's not as well tested as other architectures!
Right?) we disabled the tests and unsurprisingly `binutils-gold` built on both
aarch64 and armhf.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* "gold/Makefile.in"
               (("/bin/sh") (which "sh")))
             #t)))
       ,@(substitute-keyword-arguments (package-arguments binutils)
         ((#:tests? _ #f) #f)
         ((#:configure-flags flags)
          `(cons* "--enable-gold=default"
                  (delete "LDFLAGS=-static-libgcc" ,flags))))))
    (native-inputs
     `(("bc" ,bc)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))))
```

Now for the real test. Due to bootstrapping issues with Go and aarch64, aarch64
uses Go@1.4 built for armhf. Go@1.11 failed to build until now because it was
missing the `gold` linker. Surely using the `gold` linker would be a good test
if our package worked. Since Go for aarch64 is 'more complex' due to the
bootstrapping using armhf's Go, we decided to test armhf first. `binutils-gold`
was added and our build started.

```diff
    (native-inputs
     `(("go" ,go-1.4)
+      ,@(match (%current-system)
+          ((or "armhf-linux" "aarch64-linux")
+           `(("gold" ,binutils-gold)))
+          (_ `()))
       ,@(package-native-inputs go-1.4)))
```

First build, success! `/gnu/store/…-go-1.11.5` exists! OK, but does it
actually work? `guix build syncthing --system=armhf-linux`.
`/gnu/store/…-syncthing-1.0.0` exists too! A quick check of `guix refresh
--list-dependent go@1.4` showed that we had unlocked 176 new packages for armhf.
Even better, since they had all failed by default due to go@1.11 failing to
build, for each package that did build meant one fewer package which failed to
build which should take a big bite out of our build failures.

Our next test was syncthing for aarch64. `/gnu/store/…-go-1.11.5` exists!
`/gnu/store/…-syncthing-1.0.0` ... does not. "`unknown architecture 'armv7-a'`."
It seems that Go is confused which architecture it is building for.
Unfortunately we were reaching the end of our time for hacking, so that will
have to wait for another day. All that was left now was the test failures on
`binutils-gold` for the ARM systems. Some attempts at cargo-culting other code
failed (per-architecture tests we had and overriding flags in
`substitute-keyword-arguments` we had, but not together), but after some attempts
we were able to create a working package definition we were happy with.

```scheme
(define-public binutils-gold
  (package
    (inherit binutils)
    (name "binutils-gold")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* "gold/Makefile.in"
               (("/bin/sh") (which "sh")))
             #t)))
       ,@(substitute-keyword-arguments (package-arguments binutils)
         ; Upstream is aware of unrelocatable test failures on arm*.
         ((#:tests? _ #f)
          (if (any (cute string-prefix? <> (or (%current-target-system)
                                               (%current-system)))
                   '("i686" "x86_64"))
              '#t '#f))
         ((#:configure-flags flags)
          `(cons* "--enable-gold=default"
                 (delete "LDFLAGS=-static-libgcc" ,flags))))))
     (native-inputs
      `(("bc" ,bc)))
     (inputs
      `(("gcc:lib" ,gcc "lib")))))
```

This patch was pushed to the master branch as
[28317d499034b00cf1f08a9efd39bd2bc3425b19](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=28317d499034b00cf1f08a9efd39bd2bc3425b19),
and the
[commit](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=1009e6e7ecc00b72a51778e90b0212ccc33bfa7a)
following uses it as a native-input for Go@1.9 and Go@1.11. Go@1.4 was added in
[June
2016](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=7a2941a83ee8ecac9ca7a3a076b1231805b39bbd)
and Go@1.6 that
[August](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=ec91bcb500a2540b91fca6fd2a93a0562c427712),
with our first go packages being added in [October
2017](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=56a37713c3fada3010666278eb37873980746572).
That same
[October](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=dda785f66081871501236722f68c8eaa31103186)
Go@1.4 had support limited to Intel and armhf and in October 2018, in an effort
to work toward a resolution, a
[patch](https://git.savannah.gnu.org/cgit/guix.git/commit/?id=2ab321ca37d1c00c1540d78d587226d3d487b2d4)
was added to have aarch64 use Go@1.4 built for armhf for it's bootstrap path.
Basically since the addition of the Go language support into Guix there was not
a time when it was usable on armhf or aarch64. Hopefully we will soon finish
getting full Go support on aarch64 and we can move all 352 dependents of Go@1.4
from "failing" to "succeeding" and have these architectures better supported.

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
