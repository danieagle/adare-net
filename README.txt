
Adare_Net
=====================

Version: 0.0.128

This is a small, portable and easy to use Ada network lib.
It supports ipv4 ipv6 udp and tcp, Socket Synchronous I/O Multiplexing
and can 'listen' with ipv6, too.

The powerfull buffer feature can support all Ada types, tagged types included,
and with a more refined treatment, you can use endian proof records and
unconstrained arrays.

All supported OS's have exactly the same visible packages and children
packages names! No need for have a separate src file in a project for
each OS just to 'with' different package names.

Before Version 0.1.0 this is a work in progress,
but the API is already in a resonable stable state,
and most changes are or new subprograms and packages
or just changing internal parts without changing the visible API.

Maintenance Status: Very Active.

Adare_net is written in Ada2012 and C.

It need gprbuild to build.

If you don't have a make command, use the commands from Makefile directly or
use VScode.

How a Bonus Almost all packages in this lib is "Ada Preelaborate"! :-)

USAGE
-----

To compile:
  make AOS=OS_TYPE <enter>
or
  make AOS=OS_TYPE all-debug <enter>

To Install only runtime (as root or admin user):
  make AOS=OS_TYPE install <enter>
or
  make AOS=OS_TYPE install-debug <enter>


To Install runtime, adare_net.gpr and development files (as root or admin user):
  make AOS=OS_TYPE install-dev <enter>
or
  make AOS=OS_TYPE install-dev-debug <enter>


To Uninstall (as root or admin user):
  make uninstall <enter>


Note from Makefile:

  OS_TYPE is one of linux, mswindows, macosx, bsd, unix, other

  When using Gprbuild, the option OS_TYPE control:
    1)  The name of this library
    2.1)  Which libraries need to be linked when building Adare_Net.
    2.2)  When compiling, the link of Adare_Net and libs need by Adare_Net.

  If you need other OS's, new features or bug fix:
    Just file an issue and we'll trie reasonable hard to make you
      Happy.
    Forking and contribute with Pull Requests is yet the more speedy manner to
      get things included.

  Main repository => https://github.com/danieagle/adare-net

  All help, tests and pull requests Are Welcome, but the main need now
    is testing in MSWindows (from Win10_22H2_x64), Linux and MACOSX
    for release the upcoming v.0.1.0

  Volunteers Are Welcome :-) you want do above ? Very Very Very Thanks!!

For documentation, see Doc/adare_net.pdf

For examples and how compile in your own projects, see:
  adare_net/src/*.ad{s,b}
  example/tcp1/ex.gpr and example/tcp1/*.adb
  example/udp1/ex.gpr and example/udp1/*.adb

  Enjoy!!

CONTRIBUTING
------------

We will be happy in known where you use Adare_net. :-)


THANKS
------

To Brian "Beejs Jorgensen" Hall, because his book "the Beej's Guide to Network Programming"
and to All people, past present and future, that made Life better to Live and
that include the Fellows from Ada Newsgroup. Thanks All!!


LICENSE
-------

The Adare_net license 2020-Present

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>

SPDX-License-Identifier: Unlicense
