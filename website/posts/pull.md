title: Multi-dimensional transactions and rollbacks, oh my!
date: 2018-07-24 14:30
author: Ludovic Courtès
tags: Software development, Reproducibility, Programming interfaces, Scheme API
---
One of the [highlights of version
0.15.0](https://www.gnu.org/software/guix/blog/2018/gnu-guix-and-guixsd-0.15.0-released/)
was the overhaul of [`guix
pull`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-pull.html),
the command that updates Guix and its package collection.  In Debian
terms, you can think of `guix pull` as:

```
apt-get update && apt-get install apt
```

Let’s be frank, `guix pull` does not yet run as quickly as this
`apt-get` command—in the “best case”, when pre-built binaries are
available, it currently runs in about 1m30s on a recent laptop.  More
about the performance story in a future post…

One of the key features of the new `guix pull` is the ability to _roll
back_ to previous versions of Guix.  That’s a distinguishing feature
that opens up new possibilities.

# “Profile generations”

Transactional upgrades and rollbacks have been a distinguishing feature
of Guix since Day 1.  They come for free as a consequence of the
functional package management model inherited from the Nix package
manager.  To many users, this alone is enough to justify using a
functional package manager: if an upgrade goes wrong, you can always
roll back.  Let’s recap how this all works.

As a user, you install packages in your own _profile_, which defaults to
`~/.guix-profile`.  Then from time to time you update Guix and its
package collection:

```
$ guix pull
```

This updates `~/.config/guix/current`, giving you an updated `guix`
executable along with an updated set of packages.  You can now upgrade
the packages that are in your profile:

```
$ guix package -u
The following packages will be upgraded:
   diffoscope	93 → 96     /gnu/store/…-diffoscope-96
   emacs    25.3 → 26.1     /gnu/store/…-emacs-26.1
   gimp     2.8.22 → 2.10.4 /gnu/store/…-gimp-2.10.4
   gnupg    2.2.7 → 2.2.9   /gnu/store/…-gnupg-2.2.9
```

The upgrade creates a new _generation_ of your profile—the previous
generation of your profile, with diffoscope 93, emacs 25.3, and so on is
still around.  You can list profile generations:

```
$ guix package --list-generations
Generation 1  Jun 08 2018 20:06:21
   diffoscope	93     out   /gnu/store/…-diffoscope-93
   emacs        25.3   out   /gnu/store/…-emacs-25.3
   gimp         2.8.22 out   /gnu/store/…-gimp-2.8.22
   gnupg        2.2.7  out   /gnu/store/…-gnupg-2.2.7
   python       3.6.5  out   /gnu/store/…-python-3.6.5

Generation 2  Jul 12 2018 12:42:08     (current)
-  diffoscope	93     out   /gnu/store/…-diffoscope-93
-  emacs        25.3   out   /gnu/store/…-emacs-25.3
-  gimp         2.8.22 out   /gnu/store/…-gimp-2.8.22
-  gnupg        2.2.7  out   /gnu/store/…-gnupg-2.2.7
+  diffoscope	96     out   /gnu/store/…-diffoscope-96
+  emacs        26.1   out   /gnu/store/…-emacs-26.1
+  gimp         2.10.4 out   /gnu/store/…-gimp-2.10.4
+  gnupg        2.2.9  out   /gnu/store/…-gnupg-2.2.9
```

That shows our two generations with the diff between Generation 1 and
Generation 2.  We can at any time run `guix package --roll-back` and get
our previous versions of gimp, emacs, and so on.  Each generation is
just a bunch of symlinks to those packages, so what we have looks like
this:

![Image of the profile generations.](https://www.gnu.org/software/guix/static/blog/img/guix-pull1.png)

Notice that python was not updated, so it’s shared between both
generations.  And of course, all the dependencies that didn’t change in
between—e.g., the C library—are shared among all packages.

# `guix pull` generations

Like I wrote above, `guix pull` brings the latest set of package
definitions from Git `master`.  The Guix package collection usually only
contains only the latest version of each package; for example, current
`master` only has version 26.1 of Emacs and version 2.10.4 of the GIMP
(there are notable exceptions such as GCC or Python.)  Thus, `guix
package -i gimp`, from today’s master, can only install gimp 2.10.4.
Often, that’s not a problem: you can keep old profile generations
around, so if you really need that older version of Emacs, you can run
it from your previous generation.

Still, having `guix pull` keep track of the changes to Guix and its
package collection is useful.  Starting from 0.15.0, `guix pull` creates
a new generation, just like `guix package` does.  After you’ve run `guix
pull`, you can now list Guix generations as well:

```
$ guix pull -l
Generation 10	Jul 14 2018 00:02:03
  guix 27f7cbc
    repository URL: https://git.savannah.gnu.org/git/guix.git
    branch: origin/master
    commit: 27f7cbc91d1963118e44b14d04fcc669c9618176
Generation 11	Jul 20 2018 10:44:46
  guix 82549f2
    repository URL: https://git.savannah.gnu.org/git/guix.git
    branch: origin/master
    commit: 82549f2328c59525584b92565846217c288d8e85
  14 new packages: bsdiff, electron-cash, emacs-adoc-mode,
    emacs-markup-faces, emacs-rust-mode, inchi, luakit, monero-gui,
	nethack, openbabel, qhull, r-txtplot, stb-image, stb-image-write
  52 packages upgraded: angband@4.1.2, aspell-dict-en@2018.04.16-0,
    assimp@4.1.0, bitcoin-core@0.16.1, botan@2.7.0, busybox@1.29.1,
    …
Generation 12	Jul 23 2018 15:22:52	(current)
  guix fef7bab
    repository URL: https://git.savannah.gnu.org/git/guix.git
    branch: origin/master
    commit: fef7baba786a96b7a3100c9c7adf8b45782ced37
  20 new packages: ccrypt, demlo, emacs-dired-du,
    emacs-helm-org-contacts, emacs-ztree, ffmpegthumbnailer, 
	go-github-com-aarzilli-golua, go-github-com-kr-text, 
	go-github-com-mattn-go-colorable, go-github-com-mattn-go-isatty, 
	go-github-com-mgutz-ansi, go-github-com-michiwend-golang-pretty, 
	go-github-com-michiwend-gomusicbrainz, go-github-com-stevedonovan-luar, 
	go-github-com-wtolson-go-taglib, go-github-com-yookoala-realpath, 
	go-gitlab-com-ambrevar-damerau, go-gitlab-com-ambrevar-golua-unicode,
    guile-pfds, u-boot-cubietruck
  27 packages upgraded: c-toxcore@0.2.4, calibre@3.28.0,
    emacs-evil-collection@20180721-2.5d739f5, 
    …
```

The nice thing here is that `guix pull` provides high-level information
about the differences between two subsequent generations of Guix.

In the end, Generation 1 of our profile was presumably built with Guix
Generation 11, while Generation 2 of our profile was built with Guix
Generation 12.  We have a clear mapping between Guix generations as
created by `guix pull` and profile generations as created with `guix
package`:

![Image of the Guix generations.](https://www.gnu.org/software/guix/static/blog/img/guix-pull3.png)

Each generation created by `guix pull` corresponds to one commit in the
Guix repo.  Thus, if I go to another machine and run:

```
$ guix pull --commit=fef7bab
```

then I know that I get the exact same Guix instance as my Generation 12
above.  From there I can install diffoscope, emacs, etc. and I know I’ll
get the exact same binaries as those I have above, thanks to
[reproducible builds](https://reproducible-builds.org/docs/definition/).

These are very strong guarantees in terms of reproducibility and
provenance tracking—properties that are
[typically](https://github.com/canonical-websites/snapcraft.io/issues/651)
[missing](https://lwn.net/Articles/752982/) from “applications bundles”
à la Docker.

In addition, you can easily run an older Guix.  For instance, this is
how you would install the version of gimp that was current as of
Generation 10:

```
$ ~/.config/guix/current-10-link/bin/guix package -i gimp
```

At this point your profile contains gimp coming from an old Guix along
with packages installed from the latest Guix.  Past and present coexist
in the same profile.  The historical dimension of the profile no longer
matches exactly the history of Guix itself.

# Composing Guix revisions

Some people have expressed interest in being able to compose packages
coming from different revisions of Guix—say to create a profile
containing old versions of Python and NumPy, but also the latest and
greatest GCC.  It may seem far-fetched but it has very real
applications: there are large collections of scientific packages and in
particular bioinformatics packages that don’t move as fast as our
beloved flagship free software packages, and users may require ancient
versions of some of the tools.

We could keep old versions of many packages but maintainability costs
would grow exponentially.  Instead, Guix users can take advantage of the
version control history of Guix itself to mix and match packages coming
from different revisions of Guix.  As shown above, it’s already possible
to achieve this by running the `guix` program off the generation of
interest.  It does the job, but can we do better?

In the process of enhancing `guix pull` we developed a high-level API
that allows an instance of Guix to “talk” to a different instance of
Guix—[an
“inferior”](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32115).  It’s
what allows `guix pull` to display the list of packages that were added
or upgraded between two revisions.  The next logical step will be to
provide seamless integration of packages coming from an inferior.  That
way, users would be able to refer to “past” package graphs right from [a
profile
manifest](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix-package.html#index-profile-manifest)
or from the command-line.  Future work!

# On coupling

The time traveler in you might be wondering: Why are package definitions
coupled with the package manager, doesn’t it make it harder to compose
packages coming from different revisions?  Good point!

Tight coupling certainly complicates this kind of composition: we can’t
just have any revision of Guix load package definitions from any other
revision; this could fail altogether, or it could provide a different
build result.  Another potential issue is that `guix pull`ing an older
revision not only gives you an older set of packages, it also gives you
older tools, bug-for-bug.

The reason for this coupling is that a package definition [like this
one](https://www.gnu.org/software/guix/manual/en/html_node/Defining-Packages.html)
doesn’t exist in a vacuum.  Its meaning is defined by the implementation
of [package
objects](https://www.gnu.org/software/guix/manual/en/html_node/package-Reference.html),
by
[`gnu-build-system`](https://www.gnu.org/software/guix/manual/en/html_node/Build-Systems.html),
by a number of [lower-level](https://arxiv.org/abs/1305.4584)
[abstractions](https://hal.inria.fr/hal-01580582/en) that are all
defined as extensions of the Scheme language in Guix itself, and
ultimately by [Guile](https://www.gnu.org/software/guile/), which
implements the language Guix is written in.  Each instance created by
`guix pull` brings all these components.  Because Guix is implemented as
a set of programming language extensions and libraries, that package
definitions depend on all these parts becomes manifest.  Instead of
being frozen, the APIs and package definitions evolve together, which
gives us developers a lot of freedom on the changes we can make.

[Nix](https://nixos.org/nix/) results from a different design choice.
Nix-the-package-manager implements the Nix language, which acts as a
“frozen” interface.  Package definitions in Nixpkgs are written in that
language, and a given version of Nix can _possibly_ interpret both
current and past package definitions without further ado.  The Nix
language does evolve though, so at one point an old Nix inevitably
[becomes unable to evaluate a new
Nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/lib/minver.nix),
and _vice versa_.

These two approaches make different tradeoffs.  Nix’ loose coupling
simplifies the implementation and makes it easy to compose old and new
package definitions, to some extent; Guix’ tight coupling makes such
composition more difficult to implement, but it leaves developers more
freedom and, we hope, may support “time travels” over longer period of
times.  Time will tell!

# It’s like driving a DeLorean

![Inside the cabin of the DeLorean time machine in “Back to the Future.”](https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/TeamTimeCar.com-BTTF_DeLorean_Time_Machine-OtoGodfrey.com-JMortonPhoto.com-05.jpg/800px-TeamTimeCar.com-BTTF_DeLorean_Time_Machine-OtoGodfrey.com-JMortonPhoto.com-05.jpg)

That profile generations are kept around already gave users a time
machine of sorts—you can always roll back to a previous state of your
software environment.  With the addition of roll-back support for `guix
pull`, this adds another dimension to the time machine: you can
roll-back to a previous state of Guix itself and from there create
alternative futures or even mix bits from the past with bits from the
present.  We hope you’ll enjoy it!
