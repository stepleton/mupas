{ Draw a Koch curve fractal on a Tektronix 4050-series computer.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This muPas program draws a Koch curve fractal, or one third of a Koch
snowflake. It makes use of muPas extensions defined for the Tektronix
4050-series BASIC target.

To run on real hardware: compile with the muPas compiler (preferably with -O)
and copy the result to 4050-series magnetic tape (or an emulator) somehow. Load
and run the BASIC program in the conventional way.

To run on emulated hardware: compile, then visit an instance of the Tektronix
4051 browser-based emulator such as
https://jonbstanley.github.io/Tek405xEmulator/jsTEKTRONIX4051.html . Supply the
compiled program to the emulator via the `GPIB PROGRAM LOAD` button at bottom
right, then type:
    FIND@1:1
    OLD@1:
    RUN

This program makes heavy use of recursion, which is not the easiest thing to
accomplish in ordinary 4050-series BASIC. It's somewhat easier on the 4052A and
4054A models with their expanded capabilities, but those aren't used by the
muPas compiler. }

PROGRAM KochCurve;

CONST
  depth_start = 4;     { Recurse this many times. }
  x_start = 0.0;       { Curve starts at this X location. }
  y_start = 31.236;    { Curve starts at this Y location. }
  leg_start = 43.333;  { Leg length is this long: 130 / 3. }

VAR
  x, y: Real;    { Current "turtle" location, in user data units. }
  dx, dy: Real;  { Current "turtle" heading, as one "FD 1" unit of motion. }

{ Initialise "turtle graphics" state to CONST values, pointed right. }
PROCEDURE InitTurtle;
BEGIN
  SetDegrees;
  x := x_start;
  y := y_start;
  dx := 1.0;
  dy := 0.0;
  MoveCursor(x, y);
END;

{ Advance the turtle forward a specified amount, in user data units. }
PROCEDURE Fd(distance: Real);
BEGIN
  x := x + distance * dx;
  y := y + distance * dy;
  Draw(x, y);
END;

{ Turn the turtle by a specified amount, in degrees. Positive turns left. }
PROCEDURE Turn(angle: Real);
VAR
  s, c, tmp_dx: Real;
BEGIN
  s := Sin(angle);
  c := Cos(angle);
  tmp_dx := dx*c - dy*s;
  dy := dx*s + dy*c;
  dx := tmp_dx;
END;

{ Draw one segment of the Koch curve.

Args:
  depth: Recurse this many more times before drawing.
  leg: 1/3 of the length of this leg in user-defined units. }
PROCEDURE Leg(depth: Integer; leg: Real);
VAR
  leg_div_3: Real;
BEGIN
  depth := depth - 1;
  IF depth < 0 THEN BEGIN
    Fd(leg);
    Turn(60.0);
    Fd(leg);
    Turn(-120.0);
    Fd(leg);
    Turn(60.0);
    Fd(leg);
  END ELSE BEGIN
    leg_div_3 := leg / 3.0;
    Leg(depth, leg_div_3);
    Turn(60.0);
    Leg(depth, leg_div_3);
    Turn(-120.0);
    Leg(depth, leg_div_3);
    Turn(60.0);
    Leg(depth, leg_div_3);
  END;
END;

BEGIN
  InitTurtle;
  Leg(depth_start, leg_start);
END.
