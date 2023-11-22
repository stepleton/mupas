{ I/O routines for Fourspite 

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details. }


{ IoSetup -- set up keyboard and serial port I/O used by Fourspite

  Sets configuration values that direct how keyboard and (if enabled) serial
  I/O work; the procedure name may be a bit misleading because the procedure
  actually asks the user for information about this computer's capabilities and
  about whether they want to play an opponent over the serial port.

  If the user would like to do that, this routine then configures the serial
  port and carries out the handshake with the remote computer. The progress of
  the handshake also determines which computer will make the first move in the
  game, which is not so much about I/O but oh well.

  This routine is meant to be called very early on in Fourspite so that the
  user won't have to wait by the computer until it's ready to ask questions. An
  early call also allows a bad handshake to fail early, which hopefully also
  saves time and limits frustration.

  Globals used: flag_has_r12, flag_use_serial, we_play_first }
PROCEDURE IoSetup;
CONST
  kYesNo = 'YyNn';
  kYes = 'Yy';
  kHello = 'Knock knock!';
  kReply = 'Who''s there?';
VAR
  typed_response: String[1];
  serial_buffer: String[13];
BEGIN
  Page;

  { If R12 is fitted, we can poll for keystrokes instead of using Read }
  REPEAT
    Write('Does this computer have the R12 ROM pack fitted? (Y/N) > ');
    Read(typed_response);
  UNTIL Pos(typed_response, kYesNo) <> 0;
  flag_has_r12 := Pos(typed_response, kYes) <> 0;

  { We're creeping a bit beyond "I/O initialisation" by asking whether the
    user wants to play a "networked" game, but oh well... }
  REPEAT
    Write('Play a remote opponent over serial (if fitted)?  (Y/N) > ');
    Read(typed_response);
  UNTIL Pos(typed_response, kYesNo) <> 0;
  flag_use_serial := Pos(typed_response, kYes) <> 0;

  { If the user wants to be networked, then initialise the serial port and do
    the initial handshake; the computer that's first to the handshake gets the
    first move (so yet more straying beyond basic initialisation) }
  IF flag_use_serial THEN BEGIN
    Page;
    Write('Establishing communications... ');

    { Set the comm port parameters }
    Call('RATE', 2400{=>baud}, 5{=>8N1}, 2{=>Log errors only});

    { Flush the input buffer }
    WriteLn('@40,30');

    { Send the hello out into the void, then listen for a reply. }
    WriteLn('@40', kHello);
    Read('@40', serial_buffer);

    { If what we hear back is the hello message, than another computer came on
      line after we got started. This means we get to go first. We reply back
      with the reply message so that the other computer knows that it will go
      second. }
    IF serial_buffer = kHello THEN BEGIN
      WriteLn('connected. We''ll go first!');
      WriteLn('@40', kReply);
      we_play_first := TRUE;

    { If what we hear back is the reply message, then there was another
      computer waiting to hear us. This means they get to play first. }
    END ELSE IF serial_buffer = kReply THEN BEGIN
      WriteLn('connected. They were faster; we''ll go second.');
      we_play_first := FALSE;

    { But if we heard back something else, then give up. }
    END ELSE BEGIN
      WriteLn('failed. We heard: ', serial_buffer);
      Halt;
    END;

  { If we're not using the serial port, then we still have to choose a value
    for we_play_first, so we say yes, "we" do play first }
  END ELSE we_play_first := TRUE;
END;


{ GetStd -- get a keyboard input as an integer using INPUT

  Retrieve a single-key keyboard input from the user, which we map to integers
  ranging in `lower_bound..upper_bound`. A keyboard input of `Chr(first_char)`
  will yield a `lower_bound` return value, `Chr(first_char+1)` yields
  `lower_bound+1`, and so on. If the keyboard input mapped this way would fall
  outside of the `lower_bound..upper_bound` range, it is ignored and the user
  must try again.

  NOTE: Keyboard inputs in 'a'..'z' will be transformed to 'A'..'Z' before
  any of the `lower_bound..upper_bound` mapping and bounds-checking logic
  described above takes place. You probably don't want to use values in
  `Ascii('a')..Ascii('z')` as values for the `first_char` argument.

  A '>' prompt is placed at the spcified x, y location, and the user must then
  type a key and press Return (basically, the T4050 BASIC `INPUT` routine is
  used). There's nothing that prevents the user from typing more than one
  character (characters 2 and onward are ignored) nor the location after the
  '>' prompt (if used more than once) from becoming an overstruck mess.
  Args:
    first_char: Keyboard key input that is mapped to `lower_bound`; see
        function docstring for more details
    lower_bound: Lower bound of integer return value range; see function
        docstring for more details
    upper_bound: Upper bound of integer return value range; see function
        docstring for more details
    x, y: Location of the 5-character-wide "chasing lights" display that
        prompts for user input, in GDUs

  Returns: an integer in `lower_bound..upper_bound`

  Globals used: none }
FUNCTION GetKeyToIntStd(first_char: Char;
                        lower_bound, upper_bound: Integer;
                        x, y: Real): Integer;
VAR
  key: String[1];
  value: Integer;
BEGIN
  REPEAT
    REPEAT
      { Present '>' prompt }
      MoveCursorGdu(x, y);
      Write('>');
      { Get keyboard input from the user }
      Read(key);
    UNTIL Length(key) > 0;
    { Map input to range from lower_bound }
    value := Ascii(key);
    IF (value >= 97) AND (value <= 122) THEN value := value - 32;
    value := value - first_char + lower_bound;
  UNTIL (value >= lower_bound) AND (value <= upper_bound);

  GetKeyToIntStd := value;
END;


{ GetR12 -- get a keyboard input as an integer using R12 routines

  Retrieve a single-key keyboard input from the user, which we map to integers
  ranging in `lower_bound..upper_bound`. A keyboard input of `Chr(first_char)`
  will yield a `lower_bound` return value, `Chr(first_char+1)` yields
  `lower_bound+1`, and so on. If the keyboard input mapped this way would fall
  outside of the `lower_bound..upper_bound` range, it is ignored and the user
  must try again.

  NOTE: Keyboard inputs in 'a'..'z' will be transformed to 'A'..'Z' before
  any of the `lower_bound..upper_bound` mapping and bounds-checking logic
  described above takes place. You probably don't want to use values in
  `Ascii('a')..Ascii('z')` as values for the `first_char` argument.

  While awaiting user input, a 5-character-wide "chasing lights" display
  appears at the x, y input, alerting the user that it's time to press a key.

  Args:
    first_char: Keyboard key input that is mapped to `lower_bound`; see
        function docstring for more details
    lower_bound: Lower bound of integer return value range; see function
        docstring for more details
    upper_bound: Upper bound of integer return value range; see function
        docstring for more details
    x, y: Location of the 5-character-wide "chasing lights" display that
        prompts for user input, in GDUs

  Returns: an integer in `lower_bound..upper_bound`

  Globals used: none }
FUNCTION GetKeyToIntR12(first_char: Char;
                        lower_bound, upper_bound: Integer;
                        x, y: Real): Integer;
VAR
  keys: String[28];
  dont_care_x, dont_care_y: Real;
  value: Integer;
BEGIN
  { Flush keyboard buffer }
  R12JoyGraphicInput(0, dont_care_x, dont_care_y, keys);

  REPEAT
    { Await keyboard input from the user }
    REPEAT
      ChasePrompt(1, x, y, 5);
      R12JoyGraphicInput(0, dont_care_x, dont_care_y, keys);
    UNTIL Length(keys) > 0;
    { Map input to range from lower_bound }
    value := Ascii(keys);
    IF (value >= 97) AND (value <= 122) THEN value := value - 32;
    value := value - first_char + lower_bound;
  UNTIL (value >= lower_bound) AND (value <= upper_bound);

  GetKeyToIntR12 := value;
END;


{ GetIntSerial -- Read a tagged integer value from the serial port

  This routine waits for and receives integers (as ASCII optional-sign+digit
  sequences) over the serial port that were transmitted by the remote
  computer's call to PutIntSerial. These are prefixed by a "tag" integer: a
  second known value that this routine looks for in order to confirm (or at
  least be reasonably certain of) reliable transmission and framing.

  Updates a five-character-wide "chasing lights" display while awaiting data
  from the remote.

  Args:
    expected_tag: Integer tag that we expect to prefix the transferred integer
    lower_bound: We expect the transferred integer to be greater than or equal
        to this value
    upper_bound: We expect the transferred integer to be less than or equal to
        this value
    x: Left edge of the chasing lights display, in GDUs
    y: Bottom edge of the chasing lights display, in GDUs

  Returns: an integer in lower_bound..upper_bound received over the serial port

  Globals used: i, j, chase_prompt_state }
FUNCTION GetIntSerial(expected_tag, lower_bound, upper_bound: Integer;
                      x, y: Real): Integer;
VAR
  input_waiting, tag, value: Integer;
  failure: Boolean;
BEGIN
  { Await remote input }
  REPEAT
    ChasePrompt(1, x, y, 5);
    Read('@40,0', input_waiting);
  UNTIL input_waiting;

  { Read remote input and check it for errors }
  Read('@40', tag, value);
  { This disjunction is broken up over several statements to avoid generating a
    BASIC statement that's too long }
  failure := (tag <> expected_tag) OR
             (value < lower_bound) OR
             (value > upper_bound);
  IF failure THEN BEGIN
    Home;
    WriteLn('Comm error: want ',
            expected_tag, ',', lower_bound, '..', upper_bound);
    WriteLn('             got ', tag, ',', value);
    Halt;
  END;
  GetIntSerial := value;
END;


{ PutIntSerial -- Send a tagged integer value out over the serial port

  Sends an integer (as an ASCII optional-sign+digit sequence) out the serial
  port. The integer is "tagged", meaning that it's preceded by another known
  integer that the companion receiver function GetIntSerial will be expecting.
  Receiving this integer will validate (or at least make it much more likely)
  that the transmission channel is dependable and that both sides are in
  synch.

  Args:
    tag: Integer tag that prefixes the transferred integer
    value: Integer value to send out over the serial port

  Globals used: none }
PROCEDURE PutIntSerial(tag, value: Integer);
BEGIN
  WriteLn('@40', tag, value);
END;
