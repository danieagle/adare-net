
Adare_Net
=====================

Version: 0.9.10-dev

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

The Adare_Net main lib types is relocatable (vulgo shared), static and staic-pic.

Maintenance Status: Very Active.

Adare_net is written in Ada2012 and C.

It need Alire to build.

Adare_Net crate will be published in Public Alire after
 Adare_Net Version 1.2.0. Enjoy!!

How a Bonus Almost all packages in this lib is "Ada Preelaborate"! :-)

USAGE
-----

   Building before publishing in Public Alire Arquive:

    1) enter in your alire project:
    2) alr with adare_net --use https://github.com/danieagle/adare-net.git

    3.1) mybe use: alr build -- -XLIBRARY_TYPE=relocatable
      or
    3.2) mybe use: alr build -- -XLIBRARY_TYPE=static
      or
    3.3) mybe use: alr build -- -XLIBRARY_TYPE=static-pic

   And Be Happy :-D

  Main repository => https://github.com/danieagle/adare-net.git

  Adare_Net is in proccess of rework his poll types and some HouseKeeping. :-D

  See file AlreadyTestedIn.txt

  Enjoy!!

For documentation, see Doc/adare_net.pdf

For examples and how compile in your own projects, see:
  src/*.ad{s,b}
  adare_net_examples/tcp_new/*.adb
  adare_net_examples/udp_new/*.adb

  Enjoy!!

CONTRIBUTING
------------

We will be happy in known where you use Adare_net. :-)


THANKS
------

To Brian "Beejs Jorgensen" Hall, because his book "the Beej's Guide to Network Programming"
and to All people, past present and future, that made Life better to Live and
that include the Fellows from Ada Newsgroup and from forum.ada-lang.io. Thanks All!!


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
