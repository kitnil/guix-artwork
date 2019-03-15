title: Documentation video creation
date: 2019-02-11 15:40
author: Laura Lazzati
tags: Guix Days, Outreachy
---
My goal in this round as an [Outreachy](https://www.outreachy.org/)
intern for the December/18 to March/19 period consists of creating introductory documentation videos
about different topics for people who would like to use GNU Guix,
admins and/or those who would like to join Guix community and don’t
know where to start. Even interested or having a clear documentation, they
might feel overwhelmed by it. I experienced this issue in the past with people in another context.

My main tasks consist of creating a workflow for
automating as much as possible the process of creating the videos, as well as,
of course, creating the videos themselves. Creating the videos is not that easy
as it might seem, I have to design them (I cannot automate that part), let
the audio match the video, and matching the exact timing is quite difficult.
Something very important that I should mention is that the workflow currently
allows translations to other languages.

It is a work in progress for too many reasons, specially because it keeps
being improved all the time.

Also, I had to study tools deeply both for the creation of the workflow and the videos
because I did not know them beforehand or I knew just the basics.

After trying several approaches for the workflow, the current one consists of
creating "pieces of videos" and gluing them together in the end.

These "pieces of videos" may consist of:
- Slide videos: they contain only a sequence of one or more slides.
- Command line session videos: they contain only Guix or shell commands and
their output, without showing any slide at all.

##### Workflow for creating each slide video.

![slide](https://git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/website/static/blog/img/outreachy-2019-outreachy-2019-slide-video.png)

The inputs are .svg files and audio files.
First, .svgs are converted to .pngs ("the slides").
Then, a text file having the order in which each slide will
appear and the duration of the audio that matches it is created.
An audio text file containing all the audio files sorted to
have a complete audio file is created too.
Lastly, with the slides' text file that has the reference to the slide files
and the glued audio file the final slide video is made.

##### Workflow for creating each command line session video.

![cli](https://git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/website/static/blog/img/outreachy-2019-outreachy-2019-cli-video.png)

The input is a session text file that has commands or meta-commands that
are used to simulate, for example, the typing of a command, or the printing of
it’s output.
This file is passed to a Guile script that is in charge of executing the
commands defined in the input text file and take text snapshots at a fixed time
 interval. Then, all these files are converted to postscript format. After
that, they are transformed to .svg format. Finally, the process is repeated and
the audio and the slides are glued to have final command line session video.

##### Workflow for creating the final video.

![gluing](https://git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/website/static/blog/img/outreachy-2019-outreachy-2019-gluing-video.png)

Slide videos and command line videos are a "bunch of videos"
that need to be glued into the final one. They are sorted, and using the
same tool for video creation our final introductory video is created.

#### About GNU Guix

[GNU Guix](https://www.gnu.org/software/guix) is a transactional package
manager for the GNU system. The Guix System Distribution or GuixSD is
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
