Fourspite: a game for one or two Tektronix 4050-series computers
================================================================

![Two Tektronix 4051 computers after a round of Fourspite has been won](
fourspite.jpg
"Two Tektronix 4051 computers after a round of Fourspite has been won")

Fourspite is a quick two-player strategy game for one or two Tektronix
4050-series computers. For more information on the game, see the comments
at the top of [`fourspite.pas`](fourspite.pas).

To compile Fourspite into Tektronix 4050-series BASIC, just run GNU make in
the current directory. The resulting file `fourspite.out` can be loaded into
the [Tektronix 4051 web emulator](
https://jonbstanley.github.io/Tek405xEmulator/jsTEKTRONIX4051.html) or onto
a real Tektronix 4050-series machine via whatever contrivance works for you.
I use the ["4050 GPIB Flash Drive"](
https://github.com/mmcgraw74/Tektronix-4050-GPIB-Flash-Drive) tape drive
emulator.

**NOTE:** For systems with the R12 ROM, Fourspite is configured for use with
Tektronix 4051 computers. To change for 4052 or 4054 systems with R12, either
change the string "MUZAKT" to "MUSIC" on line 210 of the compiled program, or
change the definition of `kR12MusicCall` as documented in the global CONST
section of `fourspite.pas`.


Nobody owns Fourspite
---------------------

Fourspite and any supporting programs, software libraries, and documentation
distributed alongside it are released into the public domain without any
warranty. See the [LICENSE](../../LICENSE) file for details.


-- _[Tom Stepleton](mailto:stepleton@gmail.com), 22 November 2023, St. Louis_
