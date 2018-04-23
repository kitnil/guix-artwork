title: Guix on Android!
date: 2018-04-24 10:00
author: Julien Lepiller
tags: ARM
---

Last year I thought to myself: since my phone is just a computer running
an operating system called Android (or [Replicant](https://replicant.us/)!),
and that Android is based on a Linux
kernel, it's just [another foreign distribution I could install GNU Guix
on](https://www.gnu.org/software/guix/manual/html_node/Installation.html),
right? It turned out it was absolutely the case. Today I was
reminded on IRC of my attempt last year at installing GNU Guix on my
phone. Hence this blog post. I'll try to give you all the knowledge and
commands required to install it on your own Android device.

#### Requirements

First of all, you will need an Android or Replicant device. Just like any
installation of GNU Guix, you will need root access on that device.
Unfortunately, in the Android world this is not very often the case by
default. Then, you need a cable to connect your computer to your phone.
Once the hardware is in place, you will need `adb` (the Android
Debugging Bridge):

```
guix package -i adb
```

#### Exploring the device

Every Android device has its own partioning layout, but basically it
works like this:

1. A boot partition for booting the device
2. A recovery partition for booting the device in recovery mode
3. A data partition for user data, including applications, the user
   home, etc
4. A system partition with the base system and applications. This is the
   place where phone companies put their own apps so you can't remove
   them
5. A vendor partition for drivers
6. Some other partitions

During the boot process, the bootloader looks for the boot partition.
It doesn't contain a filesystem, but only a gzipped cpio archive (the
initramfs) and the kernel. The bootloader loads them in memory and
the kernel starts using the initramfs. Then, the init system from this
initramfs loads partitions in their respective directories: the system
partition in `/system`, the vendor partition in `/vendor` and the data
partition in `/data`. Other partitions may be loaded.

And that's it. Android's root filesystem is actually the initramfs so
any modification to its content will be lost after a reboot.
Thankfully(?), Android devices are typically not rebooted often.

Another issue is the Android C library (libc), called Bionic: it has
less functionality and works completely differently from the GNU libc.
Since Guix is built with the Glibc, we will need to do something to
make it work on our device.

#### Installing the necessary files

We will follow the [binary installation
guide](https://www.gnu.org/software/guix/manual/html_node/Binary-Installation.html).
My hardware is aarch64, so I download the corresponding binary release.

Now it's time to start using adb. Connect your device and obtain root
priviledges for adb. You may have to authorize root access to the
computer from your phone:

```bash
adb root
```

Now, we will transfer some necessary files:

```bash
adb push guix-binary-* /data

# Glibc needs these two files for networking.
adb push /etc/protocols /system/etc/
adb push /etc/services /system/etc/

# … and this one to perform DNS queries.  You probably need
# to change nameservers if you use mobile data.
adb push /etc/resolv.conf /system/etc/
```

Note that some devices may not have `/system/etc` available. In that
case, `/etc` may be available. If none is available, create the
directory by using `adb shell` to get a shell on your device, then
push the files to that new directory.

#### Installing Guix itself

Now all the necessary files are present on the device, so we can connect
to a shell on the device:

```bash
adb shell
```

From that shell, we will install Guix. The root filesystem is mounted
read-only as it doesn't make sense to modify it. Remember: it's a RAM
filesystem. Remount-it read-write and create necessary directories:

```bash
mount -o remount,rw /
mkdir /gnu /var
mount -o remount,ro /
```

Now, we can't just copy the content of the binary archive to these
folders because the initramfs has a limited amount of space. Guix
complains when `/gnu` or `/gnu/store` is a symlink. One solution consists in
installing the content of the binary tarball on an existing partition
(because you can't modify the partition layout easily) that has enough
free space, typically the data partition. Then this partition is mounted
on `/var` and `/gnu`.

Before that, you will need to find out what the data partition is in
your system. Simply run `mount | grep /data` to see what partition
was mounted.

We mount the partition, extract the tarball and move the contents to
their final location:

```bash
mount /dev/block/bootdevice/by-name/userdata /gnu
mount /dev/block/bootdevice/by-name/userdata /var
cd /data
tar xf guix-binary-...
mv gnu/store .
mv var/guix .
rmdir gnu
rmdir var
```

Finally, we need to [create users and groups](https://www.gnu.org/software/guix/manual/html_node/Build-Environment-Setup.html) for Guix to work
properly. Since Bionic doesn't use `/etc/passwd` or `/etc/group`
to store the users, we need to create them from scratch. Note
the addition of the root user and group, as well as the `nobody`
user.


```bash
# create guix users and root for glibc
cat > /etc/passwd << EOF
root:x:0:0:root:/data:/sbin/sh
nobody:x:99:99:nobody:/:/usr/bin/nologin
guixbuilder01:x:994:994:Guix build user 01:/var/empty:/usr/bin/nologin
guixbuilder02:x:993:994:Guix build user 02:/var/empty:/usr/bin/nologin
guixbuilder03:x:992:994:Guix build user 03:/var/empty:/usr/bin/nologin
guixbuilder04:x:991:994:Guix build user 04:/var/empty:/usr/bin/nologin
guixbuilder05:x:990:994:Guix build user 05:/var/empty:/usr/bin/nologin
guixbuilder06:x:989:994:Guix build user 06:/var/empty:/usr/bin/nologin
guixbuilder07:x:988:994:Guix build user 07:/var/empty:/usr/bin/nologin
guixbuilder08:x:987:994:Guix build user 08:/var/empty:/usr/bin/nologin
guixbuilder09:x:986:994:Guix build user 09:/var/empty:/usr/bin/nologin
guixbuilder10:x:985:994:Guix build user 10:/var/empty:/usr/bin/nologin
EOF

cat > /etc/group << EOF
root:x:0:root
guixbuild:x:994:guixbuilder01,guixbuilder02,guixbuilder03,guixbuilder04,guixbuilder05,guixbuilder06,guixbuilder07,guixbuilder08,guixbuilder09,guixbuilder10
EOF
```

#### Running Guix

First, we install the root profile somewhere:

```bash
export HOME=/data
ln -sf /var/guix/profiles/per-user/root/guix-profile \
         $HOME/.guix-profile
```

Now we can finally run the Guix daemon. Chrooting is impossible on
my device so I had to disable it:

```bash
export PATH="$HOME/.guix-profile/bin:$HOME/.guix-profile/sbin:$PATH"
guix-daemon --build-users-group=guixbuild --disable-chroot &
```

To finish with, it's a good idea to allow substitutes from hydra:

```bash
mkdir /etc/guix
guix archive --authorize < \
  $HOME/.guix-profile/share/guix/hydra.gnu.org.pub
```

#### Enjoy!

```bash
guix pull
```

![Mobile phone running 'guix pull'.](https://www.gnu.org/software/guix/static/blog/img/android.jpg)

#### Future work

So, now we can enjoy the Guix package manager on Android! One of the
drawbacks is that after a reboot we will have to redo half of the
steps: recreate `/var` and `/gnu`, mount the partitions to them. Everytime
you launch a shell, you will have to export the `PATH` to be able to run
`guix`. You will have to run `guix-daemon` manually. To solve all of these
problems at once, you should modify the boot image. That's tricky and I
already put some effort to it, but the phone always ends up in a boot
loop after I flash a modified boot image. The nice folks at `#replicant`
suggested that I soldered some cable to access a serial console where
debug messages may be dropped. Let's see how many fingers I burn before
I can boot a custom boot image!

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
