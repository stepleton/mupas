{ Perplex -- a cooperative line-bounce screensaver for two 4050-series machines

  Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

  Two 4050-series computers connected to each other via their Option 1 serial
  communicatons interface present a single line segment bouncing around their
  screens. Occasionally the screens clear themselves so that we can start
  afresh with a new image.

  To use, arrange the computers side-by-side, connect them with an appropriate
  serial cable, and load this program. RUN the program on the machine on the
  right first, then on the machine on the left. The systems will establish
  communications and then the left machine will ask for the width of the gap
  between the two screens (in centimetres). Answer it and the line bouncing
  will begin. Use the BREAK key on both machines to exit the program.

  This program is a bit repetitive to avoid expensive procedure calls. }
PROGRAM Perplex;


CONST
  kMaxLines = 400;   { Clear the screen after drawing this many lines }
  kMinSpeed = 1;     { X or Y absolute speed is at least this in GDUs/step }
  kSpeedBandX = 10;  { X absolute speeds range in (kMinSpeed, kMinSpeed+this) }
  kSpeedBandY = 6;   { Y absolute speeds range in (kMinSpeed, kMinSpeed+this) }


VAR
  { The computer on the left is the LEADER. This machine calculates the
    location of the line segment and sends it to the REMOTE, the computer on
    the right, in the REMOTE's own GDU coordinates. Both machines plot whatever
    portion of the line segment appears on their screens. This process repeats
    forever. }
  role: (LEADER, REMOTE);

  { Screen geometry variables (filled in by SetupGeometry):
     ___________                  ___________
    |  LEADER   |                |  REMOTE   |
    |           |                |           |
    |<-- 130 -->|<-- gap_size -->|<-- 130 -->|  ("130" == 130 GDUs)
    |___________|                |___________|
     <------- remote_edge ------>
     <------------ total_width -------------> }
  gap_size, remote_edge, total_width: Real;

  x1, y1, x2, y2: Real;      { Line segment endpoint locations }
  dx1, dy1, dx2, dy2: Real;  { Line segment endpoint velocities }
  num_lines: Integer;        { Number of line segments drawn so far }
  screen_dirty: Boolean;     { Whether any segment has appeared on our screen }
  lx, ly: Real;              { Temporaries }


{ EstablishComms -- Set up the communications channel and identify our role

  Uses the simple handshake documented in the code to determine who is LEADER
  and who is REMOTE. Ultimately, whichever computer runs this procedure first
  will be REMOTE and should be on the right. This implementation is no doubt
  full of race conditions and can do undesirable things if you have really
  unlucky timing (e.g. both machines run this function at the exact same time),
  but that's probably not a big deal. Just don't do that.

  Globals used: role }
PROCEDURE EstablishComms;
CONST
  kHello = 'Hello out there?';
  kReply = 'I hear you!';
VAR
  buffer: String[20];
BEGIN
  Page;
  Write('Establishing communications... ');

  { Set the comm port parameters }
  Call('RATE', 2400{=>baud}, 5{=>8N1}, 2{=>Log errors only});

  { Flush the input buffer }
  WriteLn('@40,30');

  { Send the "hello" out into the void, then listen for a reply. }
  WriteLn('@40', kHello);
  Read('@40', buffer);

  { If what we hear back is "I hear you!", then there was another computer
    waiting to hear us. This makes us the LEADER. No more comms needed now. }
  IF buffer = kReply THEN BEGIN
    WriteLn('connected. I''m the leader.');
    role := LEADER;

  { If what we hear back is the hello message, than another computer came on
    line after we got started. This makes us the REMOTE, so we reply back with
    the "I hear you!" message so that the other computer becomes the LEADER. }
  END ELSE IF buffer = kHello THEN BEGIN
    WriteLn('connected. I''m the remote.');
    WriteLn('@40', kReply);
    role := REMOTE;

  { But if we got back something else, then give up. }
  END ELSE BEGIN
    WriteLn('failed. We heard: ', buffer);
    Exit;
  END;
END;


{ SetupGeometry -- Get gap size from the user, derive other geometry

  See diagram in the global VAR section.

  Globals used: gap_size, remote_edge, total_width }
PROCEDURE SetupGeometry;
BEGIN
  Write('What''s the distance between both screens (in cm)? >: ');
  Read(gap_size);
  gap_size := gap_size * 6.5;  { Convert from cm to GDUs, more or less }
  remote_edge := 130 + gap_size;
  total_width := 130 + remote_edge;
END;


{ InitialiseLeader -- Carry out bits of global LEADER setup

  Globals used: num_lines, screen_dirty,
                x1, y1, x2, y2, dx1, dy1, dx2, dy2, lx, ly }
PROCEDURE InitialiseLeader;
BEGIN
  { No lines on the screen just yet }
  num_lines := 0;
  screen_dirty := FALSE;

  { Choose random endpoints for the line segment }
  x1 := total_width * Rand;
  y1 := 100 * Rand;
  x2 := total_width * Rand;
  y2 := 100 * Rand;

  { Choose random velocities for the endpoints }
  dx1 := 2 * kSpeedBandX * Rand - kSpeedBandX;
  dy1 := 2 * kSpeedBandY * Rand - kSpeedBandY;
  dx2 := 2 * kSpeedBandX * Rand - kSpeedBandX;
  dy2 := 2 * kSpeedBandY * Rand - kSpeedBandY;

  { We guarantee enough velocity to be interesting like so: }
  dx1 := dx1 + kMinSpeed * Sign(dx1);
  dy1 := dy1 + kMinSpeed * Sign(dy1);
  dx2 := dx2 + kMinSpeed * Sign(dx2);
  dy2 := dy2 + kMinSpeed * Sign(dy2);

  { Enforce that x1 < x2 always }
  IF x1 > x2 THEN BEGIN
    lx := x1; x1 := x2; x2 := lx;
    ly := y1; y1 := y2; y2 := ly;
    lx := dx1; dx1 := dx2; dx2 := lx;
    ly := dy1; dy1 := dy2; dy2 := ly;
  END;
END;


{ UpdateState -- Calculate next line segment location and endpoint velocities

  Globals used: total_width, x1, y1, x2, y2, dx1, dx2, dy1, dy2, lx, ly }
PROCEDURE UpdateState;
BEGIN
  { Advance line endpoint locations }
  x1 := x1 + dx1; y1 := y1 + dy1;
  x2 := x2 + dx2; y2 := y2 + dy2;

  { "Bounce" points off of the edges of the draw area; note same guarantee of
    interesting velocities }
  IF x1 < 0 THEN BEGIN
    x1 := -x1;
    dx1 := kMinSpeed + kSpeedBandX * Rand;
  END ELSE IF x1 > total_width THEN BEGIN
    x1 := 2 * total_width - x1;
    dx1 := -kMinSpeed - kSpeedBandX * Rand;
  END;

  IF y1 < 0 THEN BEGIN
    y1 := -y1;
    dy1 := kMinSpeed + kSpeedBandY * Rand;
  END ELSE IF y1 > 100 THEN BEGIN
    y1 := 200 - y1;
    dy1 := -kMinSpeed - kSpeedBandY * Rand;
  END;

  IF x2 < 0 THEN BEGIN
    x2 := -x2;
    dx2 := kMinSpeed + kSpeedBandX * Rand;
  END ELSE IF x2 > total_width THEN BEGIN
    x2 := 2 * total_width - x2;
    dx2 := -kMinSpeed - kSpeedBandX * Rand;
  END;

  IF y2 < 0 THEN BEGIN
    y2 := -y2;
    dy2 := kMinSpeed + kSpeedBandY * Rand;
  END ELSE IF y2 > 100 THEN BEGIN
    y2 := 200 - y2;
    dy2 := -kMinSpeed - kSpeedBandY * Rand;
  END;

  { Enforce that x1 < x2 always }
  IF x1 > x2 THEN BEGIN
    lx := x1; x1 := x2; x2 := lx;
    ly := y1; y1 := y2; y2 := ly;
    lx := dx1; dx1 := dx2; dx2 := lx;
    ly := dy1; dy1 := dy2; dy2 := ly;
  END;
END;


{ BeLeader -- Main loop for the LEADER

  Globals used: all used by UpdateState plus num_lines, screen_dirty }
PROCEDURE BeLeader;
BEGIN
  SetupGeometry;
  InitialiseLeader;
  WHILE TRUE DO BEGIN
    { Update state and send coordinates to the REMOTE; note X coordinate shift
      because the REMOTE draws the right half of the display }
    UpdateState;
    WriteLnUsing('@40', '4(FD.2D" ")',  { The format string means we send    }
                 x1 - remote_edge, y1,  { 10ths+100ths of a GDU; more digits }
                 x2 - remote_edge, y2); { are useless and slow at 2400 baud. }

    { Draw the line if it is present at all on the left half of the display;
      remember the constraint that x1 < x2 always }
    IF x1 < 130 THEN BEGIN
      MoveCursorGdu(x1, y1);
      IF x2 < 130 THEN BEGIN
        lx := x2;
        ly := y2;
      END ELSE BEGIN
        lx := 130;
        ly := y1 + (y2 - y1) * (130 - x1) / (x2 - x1);
      END;
      DrawGdu(lx, ly);
      screen_dirty := TRUE;
    END;

    { Clear the screen if it has any contents and we've drawn too many lines }
    num_lines := num_lines + 1;
    IF screen_dirty AND (num_lines >= kMaxLines) THEN BEGIN
      Page;
      num_lines := 0;
      screen_dirty := FALSE;
    END;
  END;
END;


{ BeRemote -- Main loop for the REMOTE

  Globals used: num_lines, screen_dirty, x1, y1, x2, y2, lx, ly }
PROCEDURE BeRemote;
BEGIN
  num_lines := 0;  { The only initialisation needed for REMOTE }
  screen_dirty := FALSE;
  WHILE TRUE DO BEGIN
    { Read the current line state from the LEADER, in "local" coordinates }
    Read('@40', x1, y1, x2, y2);

    { Draw the line if it is present at all on the right half of the display;
      remember the constraint that x1 < x2 always }
    IF x2 > 0 THEN BEGIN
      MoveCursorGdu(x2, y2);
      IF x1 > 0 THEN BEGIN
        lx := x1;
        ly := y1;
      END ELSE BEGIN
        lx := 0;
        ly := y2 + (y1 - y2) * x2 / (x2 - x1);
      END;
      DrawGdu(lx, ly);
      screen_dirty := TRUE;
    END;

    { Clear the screen if it has any contents and we've drawn too many lines }
    num_lines := num_lines + 1;
    IF screen_dirty AND (num_lines >= kMaxLines) THEN BEGIN
      Page;
      num_lines := 0;
      screen_dirty := FALSE;
    END;
  END;
END;


BEGIN
  Randomize;
  EstablishComms;
  IF role = LEADER THEN BeLeader ELSE BeRemote;
END.
