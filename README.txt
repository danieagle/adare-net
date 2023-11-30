
Adare_Net
=====================

Version: 0.1.1-dev

Adare_Net is a small, powerfull, portable and easy to use Ada network lib.
It supports ipv4 ipv6 udp and tcp, Synchronous I/O Multiplexing
and can 'listen' with ipv6, too.

The powerfull buffer feature can support all Ada types, tagged types included,
and with a more refined treatment, you can use endian proof records and
unconstrained arrays.

By using _Synchronous I/O Multiplexing_ and _Ada Tasks_ makes the
use of non-blocking sockets _unnecessary_.

All supported OS's have exactly the same visible packages names and children
packages names! Just 'with' Adare_Net packages _without_ worrying about
different package names.

Before Version 1.2.0 this is a work in progress,
but the API is already in a reasonable stable state,
and most changes are or new subprograms and packages
or just changing internal parts without changing the visible API.

Maintenance Status: Very Active.

Adare_net is written in Ada2012 and C.

It need Alire to build.

Adare_Net crate will be published in Public Alire after
 Adare_Net Version 1.2.0. Enjoy!!

How a Bonus Almost all packages in this lib is "Ada Preelaborate"! :-)

USAGE
-----

   Building before publishing in Public Alire Arquive:

      git clone https://github.com/danieagle/adare-net.git
      cd adare-net/adare_net
      alr build --release

   And Be Happy :-D

  Main repository => https://github.com/danieagle/adare-net.git

  All help, tests and pull requests Are Welcome, but the More Important/Urgent
   need now is Testing in MACOSX !

   Testing in MSWindows can be from Win10_22H2_x64 and upward.

   Linux Ubuntu AMD64 23.10 is the mostly tested OS.

  Volunteers Are Welcome :-) you want do above ? Very Very Very Thanks!!

For documentation, see Doc/adare_net.pdf

For examples and how compile in your own projects, see:
  adare_net/src/*.ad{s,b}
  adare_net_examples/tcp/*.adb
  adare_net_examples/udp/*.adb

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
