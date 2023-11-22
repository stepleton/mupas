Perplex: cooperative line-bounce screensaver for two 4050-series machines
=========================================================================

![Two Tektronix 4051 computers running Perplex](perplex.jpg
"Two Tektronix 4051 computers running Perplex")

Perplex is a very boring zero-player graphical game... or, it's an homage to
"line bounce" screensavers from the 1990s like ["Mystify Your Mind"](
https://youtu.be/queN9r3Leis) from Microsoft Windows 3.1. Perplex's only
gimmick is that it runs on a pair of Tektronix 4050-series computers connected
to each other via Option 1, the RS-232 serial port option supplied with some
machines. In this configuration, the displays on both computers give the
impression of looking out onto the same "field" where a single line is bouncing
around. (Or, put differently, the line appears to bounce between and across the
screens of both machines.)

Some geometric assumptions within the program assume that the two computers are
specifically Tektronix 4051s: if not, Perplex will still work, but the illusion
of a line bouncing between both screens will not be quite as convincing.

To compile Perplex into Tektronix 4050-series BASIC, just run GNU make in the
current directory. The resulting file `perplex.out` can be loaded into the
[Tektronix 4051 web emulator](
https://jonbstanley.github.io/Tek405xEmulator/jsTEKTRONIX4051.html) (although
this does you little good as it emulates just one computer) or onto a real
Tektronix 4050-series machine via whatever contrivance works for you.  I use
the ["4050 GPIB Flash Drive"](
https://github.com/mmcgraw74/Tektronix-4050-GPIB-Flash-Drive) tape drive
emulator.

When starting Perplex, arrange your 4050-series computers side-by-side. Run the
program on the computer on the right first, then run the program on the
left-hand computer.


Nobody owns Perplex
-------------------

Perplex and any supporting programs, software libraries, and documentation
distributed alongside it are released into the public domain without any
warranty. See the [LICENSE](../../LICENSE) file for details.


-- _[Tom Stepleton](mailto:stepleton@gmail.com), 22 November 2023, St. Louis_
