title: Towards Guix for DevOps
date: 2019-07-11 13:14
author: Jakob L. Kreuze
tags: GSoC, Programming interfaces, Scheme API
---

Hey, there! I'm Jakob, a Google Summer of Code intern and new contributor to
Guix. Since May, I've been working on a DevOps automation tool for the Guix
System, which we've been calling 'guix deploy'.

The idea for a Guix DevOps tool has been making rounds on the mailing lists for
some time now. Years, in fact; Dave Thompson and Chris Webber put together a
proof-of-concept for it way back in 2015. Thus, we've had plenty of time to gaze
upon the existing tools for this sort of thing -- [Ansible](https://www.ansible.com/), [NixOps](https://nixos.org/nixops/) -- and
fantasize about a similar tool, albeit with the expressive power of Guile scheme
and the wonderful system configuration facilities of Guix. And now, those
fantasies are becoming a reality.

"DevOps" is a term that might be unfamiliar to a fair number of Guix users. I'll
spare you the detour to Wikipedia and give a brief explanation of what 'guix
deploy' does.

Imagine that you've spent the afternoon playing around with Guile's `(web)`
module, developing software for a web forum. Awesome! But a web forum with no
users is pretty boring, so you decide to shell out a couple bucks for a virtual
private server to run your web forum. You feel that Wildebeest admirers on the
internet deserve a platform of their own for discussion, and decide to dedicate
the forum to that.

As it turns out, *C. gnou* is a more popular topic than you ever would have
imagined. Your web forum soon grows in size -- attracting hundreds of thousands
of simultaneous users. Despite Guile's impressive performance characteristics,
one lowly virtual machine is too feeble to support such a large population of
Wildebeest fanatics. So you decide to use Apache as a load-balancer, and shell
out a couple more bucks for a couple more virtual private servers. Now you've
got a problem on your hands; you're the proud owner of five or so virtual
machines, and you need to make sure they're all running the most recent version
of either your web forum software or Apache.

This is where 'guix deploy' comes into play. Just as you'd use an
`operating-system` declaration to configure services and user accounts on a
computer running the Guix System, you can now use that same `operating-system`
declaration to remotely manage any number of machines. A "deployment" managing
your Wildebeest fan site setup might look something like this:

```scheme
...

;; Service for our hypothetical guile web forum application.
(define guile-forum-service-type
  (service-type (name 'guile-forum)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          guile-forum-shepherd-service)
                       (service-extension account-service-type
                                          (const %guile-forum-accounts))))
                (default-value (guile-forum-configuration))
                (description "A web forum written in GNU Guile.")))

...

(define %forum-server-count 4)

(define (forum-server n)
  (operating-system
    (host-name (format #f "forum-server-~a" n))
    ...
    (services
     (append (list (service guile-forum-service-type
                            (guile-forum-configuration
                             "GNU Fan Forum!")))
             %base-services))))

(define load-balancer-server
  (operating-system
    (host-name "load-balancer-server"
    ...
    (services
     (append (list (service httpd-service-type
                            (httpd-configuration
                             ...)))
             %base-services)))))

;; One machine running our load balancer.
(cons (machine
       (system load-balancer-server)
       (environment manged-host-environment-type)
       (configuration (machine-ssh-configuration
                       ...)))

      ;; And a couple running our forum software!
      (let loop ((n 1)
                 (servers '()))
        (if (> n %forum-server-count)
            servers
            (loop (1+ n)
                  (cons (machine
                         (system (forum-server n))
                         (environment manged-host-environment-type)
                         (configuration (machine-ssh-configuration
                                         ...)))
                        servers)))))
```

The take-away from that example is that there's a new `machine` type atop the
good ol' `operating-system` type, specifying how the machine should be
_provisioned_. The version of 'guix deploy' that's currently on the master
branch only supports `managed-host-environment-type`, which is used for machines
that are already up and running the Guix System. Provisioning, in that sense,
only really involves opening an SSH connection to the host. But I'm sure you can
imagine a `linode-environment-type` which automatically sets up a virtual
private server through Linode, or a `libvirt-environment-type` that spins up a
virtual machine for running your services. Those types are what I'll be working
on in the coming months, in addition to cleaning up the code that's there now.

And yes, you did read that right. 'guix deploy' is on the Guix master branch
right now! In fact, we've already a successful deployment right here on
[ci.guix.gnu.org](http://ci.guix.gnu.org/). So, if this sounds as though it'd be up your alley, run `guix
pull`, crack open the [manual](http://guix.gnu.org/manual/en/html_node/Invoking-guix-deploy.html#Invoking-guix-deploy), and let us know how it goes!
