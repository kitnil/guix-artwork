title: Creating and using a custom Linux kernel on Guix System
date: 2019-05-20 00:00
author: Efraim Flashner
tags: kernel, customization
---

Guix is, at its core, a source based distribution with
[substitutes](https://www.gnu.org/software/guix/manual/en/html_node/Substitutes.html),
and as such building packages from their source code is an expected part
of regular package installations and upgrades.  Given this starting
point, it makes sense that efforts are made to reduce the amount of time
spent compiling packages, and recent changes and upgrades to the
building and distribution of substitutes continues to be a topic of
discussion within Guix.

One of the packages which I prefer to not build myself is the
Linux-Libre kernel.  The kernel, while not requiring an overabundance of
RAM to build, does take a very long time on my build machine (which my
children argue is actually their Kodi computer), and I will often delay
reconfiguring my laptop while I want for a substitute to be prepared by
the official build farm.  The official kernel configuration, as is the
case with many GNU/Linux distributions, errs on the side of
inclusiveness, and this is really what causes the build to take such a
long time when I build the package for myself.

The Linux kernel, however, can also just be described as a package
installed on my machine, and as such can be customized just like any
other package.  The procedure is a little bit different, although this
is primarily due to the nature of how the package definition is written.

The
[`linux-libre`](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/linux.scm#n294)
kernel package definition is actually a procedure
which creates a package.

```scheme
(define* (make-linux-libre version hash supported-systems
                           #:key
                           ;; A function that takes an arch and a variant.
                           ;; See kernel-config for an example.
                           (extra-version #f)
                           (configuration-file #f)
                           (defconfig "defconfig")
                           (extra-options %default-extra-linux-options)
                           (patches (list %boot-logo-patch)))
  ...)
```

The current `linux-libre` package is for the 5.1.x series, and is
declared like this:

```scheme
(define-public linux-libre
  (make-linux-libre %linux-libre-version
                    %linux-libre-hash
                    '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
                    #:patches %linux-libre-5.1-patches
                    #:configuration-file kernel-config))
```

Any keys which are not assigned values inherit their default value from
the make-linux-libre definition.  When comparing the two snippets above,
you may notice that the code comment in the first doesn't actually refer
to the extra-version keyword; it is actually for configuration-file.
Because of this, it is not actually easy to include a custom kernel
configuration from the definition, but don't worry, there are other ways
to work with what we do have.

There are two ways to create a kernel with a custom kernel configuration.
The first is to provide a standard `.config` file during the build
process by including an actual `.config` file as a native-input to our
custom kernel.  The
[following](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/linux.scm#n379)
is a snippet from the custom 'configure phase of the `make-linux-libre`
package definition:

```scheme
(let ((build  (assoc-ref %standard-phases 'build))
      (config (assoc-ref (or native-inputs inputs) "kconfig")))

  ;; Use a custom kernel configuration file or a default
  ;; configuration file.
  (if config
      (begin
        (copy-file config ".config")
        (chmod ".config" #o666))
      (invoke "make" ,defconfig))
```

Below is a sample kernel package for one of my computers.  Linux-Libre
is just like other regular packages and can be inherited and overridden
like any other:

```scheme
(define-public linux-libre/E2140
  (package
    (inherit linux-libre)
    (native-inputs
     `(("kconfig" ,(local-file "E2140.config"))
      ,@(alist-delete "kconfig"
                      (package-native-inputs linux-libre))))))
```

In the same directory as the file defining `linux-libre-E2140` is a file
named `E2140.config`, which is an actual kernel configuration file.  I
left the defconfig keyword of `make-linux-libre` blank, so the only
kernel configuration in the package is the one which I included as a
native-input.

The second way to create a custom kernel is to pass a new value to the
extra-options keyword of the `make-linux-libre` procedure.  The
extra-options keyword works with another function defined right below it:

```scheme
(define %default-extra-linux-options
  `(;; https://lists.gnu.org/archive/html/guix-devel/2014-04/msg00039.html
   ("CONFIG_DEVPTS_MULTIPLE_INSTANCES" . #t)
   ;; Modules required for initrd:
   ("CONFIG_NET_9P" . m)
   ("CONFIG_NET_9P_VIRTIO" . m)
   ("CONFIG_VIRTIO_BLK" . m)
   ("CONFIG_VIRTIO_NET" . m)
   ("CONFIG_VIRTIO_PCI" . m)
   ("CONFIG_VIRTIO_BALLOON" . m)
   ("CONFIG_VIRTIO_MMIO" . m)
   ("CONFIG_FUSE_FS" . m)
   ("CONFIG_CIFS" . m)
   ("CONFIG_9P_FS" . m)))

(define (config->string options)
  (string-join (map (match-lambda
                      ((option . 'm)
                       (string-append option "=m"))
                      ((option . #t)
                       (string-append option "=y"))
                      ((option . #f)
                       (string-append option "=n")))
                    options)
               "\n"))
```

And in the custom configure script from the `make-linux-libre` package:

```scheme
;; Appending works even when the option wasn't in the
;; file.  The last one prevails if duplicated.
(let ((port (open-file ".config" "a"))
      (extra-configuration ,(config->string extra-options)))
  (display extra-configuration port)
  (close-port port))

(invoke "make" "oldconfig"))))
```

So by not providing a configuration-file the `.config` starts blank, and
then we write into it the collection of flags that we want.  Here's
another custom kernel which I have:

```scheme
(define %macbook41-full-config
  (append %macbook41-config-options
          %filesystems
          %efi-support
          %emulation
          (@@ (gnu packages linux) %default-extra-linux-options)))

(define-public linux-libre-macbook41
  ;; XXX: Access the internal 'make-linux-libre' procedure, which is
  ;; private and unexported, and is liable to change in the future.
  ((@@ (gnu packages linux) make-linux-libre) (@@ (gnu packages linux) %linux-libre-version)
                      (@@ (gnu packages linux) %linux-libre-hash)
                      '("x86_64-linux")
                      #:extra-version "macbook41"
                      #:patches (@@ (gnu packages linux) %linux-libre-5.1-patches)
                      #:extra-options %macbook41-config-options))
```

From the above example `%filesystems` is a collection of flags I
compiled enabling different filesystem support, `%efi-support` enables
EFI support and `%emulation` enables my x86_64-linux machine to act in
32-bit mode also. `%default-extra-linux-options` are the ones quoted
above, which had to be added in since I replaced them in the
extra-options keyword.

This all sounds like it should be doable, but how does one even know
which modules are required for their system?  The two places I found
most helpful to try to answer this question were the [Gentoo
Handbook](https://wiki.gentoo.org/wiki/Handbook:AMD64/Installation/Kernel),
and the
[documentation](https://www.kernel.org/doc/html/latest/admin-guide/README.html?highlight=localmodconfig)
from the kernel itself.  From the kernel documentation, it seems that
`make localmodconfig` is the command we want.

In order to actually run `make localmodconfig` we first need to get and
unpack the kernel source code:

```shell
tar xf $(guix build linux-libre --source)
```

Once inside the directory containing the source code run `touch .config`
to create an initial, empty `.config` to start with.  `make
localmodconfig` works by seeing what you already have in `.config` and
letting you know what you're missing.  If the file is blank then you're
missing everything.  The next step is to run:

```shell
guix environment linux-libre -- make localmodconfig
```

and note the output.  Do note that the `.config` file is still empty.
The output generally contains two types of warnings.  The first start
with "WARNING" and can actually be ignored in our case.  The second read:

```shell
module pcspkr did not have configs CONFIG_INPUT_PCSPKR
```

For each of these lines, copy the `CONFIG_XXXX_XXXX` portion into the
`.config` in the directory, and append `=m`, so in the end it looks
like this:

```shell
CONFIG_INPUT_PCSPKR=m
CONFIG_VIRTIO=m
```

After copying all the configuration options, run `make localmodconfig`
again to make sure that you don't have any output starting with
"module".  After all of these machine specific modules there are a
couple more left that are also needed.  `CONFIG_MODULES` is necessary so
that you can build and load modules separately and not have everything
built into the kernel.  `CONFIG_BLK_DEV_SD` is required for reading from
hard drives.  It is possible that there are other modules which you
will need.

This post does not aim to be a guide to configuring your own kernel
however, so if you do decide to build a custom kernel you'll have to
seek out other guides to create a kernel which is just right for your
needs.

The second way to setup the kernel configuration makes more use of
Guix's features and allows you to share configuration segments between
different kernels.  For example, all machines using EFI to boot have a
number of EFI configuration flags that they need.  It is likely that all
the kernels will share a list of filesystems to support.  By using
variables it is easier to see at a glance what features are enabled and
to make sure you don't have features in one kernel but missing in another.

Left undiscussed however, is Guix's initrd and its customization.  It is
likely that you'll need to modify the initrd on a machine using a custom
kernel, since certain modules which are expected to be built may not be
available for inclusion into the initrd.

Suggestions and contributions toward working toward a satisfactory
custom initrd and kernel are welcome!

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
