{ Fourspite -- A simple two-player strategy game for Tektronix 4050 computers

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

Forspite is a game for two players with one Tektronix 4050-series computer or
two 4050-series computers connected over serial link. The game proceeds in
rounds that continue until the players get tired of playing. In each round:

* Players take turns dropping tetrominos onto a playing field. Once dropped, a
  tetromino falls right down the field without sliding left or right until any
  part of it rests atop previously-dropped tetrominos or the playing field's
  "floor". Tetrominos cannot be turned or otherwise "bumped" or "steered" as
  they fall.

* Players earn "potential points" for each tetromino they drop. The number of
  points earned is the same as the height of the tetromino, so a taller piece
  earns more points than a shorter one.

* The main objective in a round is to force your opponent to overfill the
  playing field; that is, to be the first to drop a piece such that any part
  rests above the line marked "TOP". If your opponent loses the round in that
  way, then you convert all of the "potential points" you accumulated by
  dropping pieces during the round (see above) into actual points. Your
  opponent loses all of their potential points and earns nothing during the
  round.

* Although no player wants to be the first across the "TOP" line, it's good to
  be the first player across the lower "BONUS" line, since this doubles all of
  the potential points that the player accumulates during the round.

* If no player overfills the playing field after fifteen turns (for the
  one-computer version of the game) or thirty turns (for the two-computer
  version of the game), then the round is a draw and no player earns any
  points.

* Players choose the tetromino they wish to drop from a "rack" of tetrominos
  (think of a rack of letter tiles in Scrabble). When a tetromino is dropped,
  a new one is drawn at random to replace it in the rack. Tetrominos are drawn
  without replacement: this means that once a tetromino has been drawn, it
  won't be drawn again until all nineteen tetrominos have been drawn; once
  that happens, the "bag" of tetrominos is refilled with the full nineteen in
  preparation for the next draw. (Note that this refill event isn't really
  possible in the game as configured today, since no round can last longer
  than 15 turns.)

* In the two-computer version of Fourspite, each player has a private "rack"
  that the other player can't see (not without looking at their opponent's
  screen, at least) along with a separate "bag" of tetrominos used to
  replenish the rack after plays. (This makes two-computer Fourspite a
  two-player pen-and-paper game with partial information; other such games
  include _Guess Who_ and _Battleship_.) In the one-computer version, both
  players share the same rack. This makes for two different games with distinct
  strategies.

On startup, the game asks the player whether they wish to play someone else
over the serial port (this determines whether the game will be the one-computer
or two-computer version), as well as whether they have the R12 ROM fitted. R12
is convenient for sound effects and friendlier keyboard input, but is not
required to fully enjoy the game. }
PROGRAM Fourspite;

CONST
  { These constants may be varied to fine-tune game dynamics }
  kColumns = 6;        { Number of columns in the playing field }
  kRowTop = 14;        { Height of the playing field }
  kRowBonus = 7;       { Row number of the "bonus row" }
  kRackSize = 3;       { How many pieces players keep in reserve }
  kRacksPerGroup = 5;  { How many racks in each of the three rack groups }

  { Which call produces music from the R12 ROM? }
  kR12MusicCall = 'MUZAKT';  { 4051 }
  (* kR12MusicCall = 'MUSIC';   { 4052/4054 } *)

TYPE
  { Note: code depends on the ordering of these blocks. Sorry! }
  BlockType = (BLOCK,
               T_UP, T_LEFT, T_DOWN, T_RIGHT,
               G_UP, G_LEFT, G_DOWN, G_RIGHT,
               L_UP, L_LEFT, L_DOWN, L_RIGHT,
               S_LOWERED, S_RAISED,
               Z_LOWERED, Z_RAISED,
               I_LOWERED, I_RAISED);

  BlockOrnamentType = (
      ZIG_ZAG, SOLIDUS, BEVELED, HELICAL, THICKER, LAZERZZ, KNOTTED);

  Column = 1..kColumns;

  RackColumn = 1..kRackSize;

  ChaseId = 1..4;

VAR
  { Generic loop index/integer temporaries }
  i, j, k: Integer;

  { Tetromino shapes and decorations: see define_tetrominoes.pas for details }
  blocks_x: ARRAY [BlockType,1..4] OF Integer;
  blocks_y: ARRAY [BlockType,1..4] OF Integer;
  blocks_ornament_x: ARRAY [BlockOrnamentType, 1..5] OF Real;
  blocks_ornament_y: ARRAY [BlockOrnamentType, 1..5] OF Real;
  blocks_which_ornament: ARRAY [BlockType] OF BlockOrnamentType;

  { The next block of globals holds the "game geometry" globals }

  { Computed scaling parameters for the game field (for the Window command) }
  field_x_min, field_x_max, field_y_min, field_y_max: Real;
  { Derived coordinates for various game field ornaments }
  field_half_gdu, field_ylabel_x, field_ylabel_tick, field_xlabel_y: Real;
  { Display sizing information for rack items }
  rack_square_size, rack_spacing: Real;

  { The next block of globals holds "game state" globals }

  { The playing field: for each column, we only need to know its "height":
    which row is occupied by the highest block }
  field: ARRAY [Column] OF Integer;
  { The player's rack of tetrominoes }
  rack: ARRAY [1..kRackSize] OF BlockType;
  { The bag of tetrominoes that new rack pieces will draw from }
  bag: ARRAY [BLOCK..I_RAISED] OF BlockType;
  bag_index: BLOCK..I_RAISED;
  { Scores, for us and for our opponent }
  score_us, score_them: Integer;

  { Miscellaneous globals }

  { State for "chase" dynamic graphical elements }
  chase_prompt_state: ARRAY [ChaseId] OF Integer;
  { Config: is an R12 ROM fitted; is the opponent over the serial port? }
  flag_has_r12, flag_use_serial: Boolean;
  { Will we be the first to play the next round? }
  we_play_first: Boolean;


{ ResetGame -- Reset the game state

  Args:
    zero_the_score: Zero the scores for us and our opponent.

  Globals used: i and all game-state globals (see note at global VAR) }
PROCEDURE ResetGame(zero_the_score: Boolean);
BEGIN
  ResetBag;
  field := 0;
  FOR i := 1 TO kRackSize DO rack[i] := DrawFromBag;

  IF zero_the_score THEN BEGIN
    score_us := 0;
    score_them := 0;
  END;
END;


{ ResetBag -- Sample a new shuffled bag and point bag_index at the top of it

  Globals used: bag, bag_index }
PROCEDURE ResetBag;
VAR
  temp, other_index: BlockType;
BEGIN
  { Initialise the bag with the tetrominoes in sequential order }
  FOR bag_index := BLOCK TO I_RAISED DO bag[bag_index] := bag_index;

  { Now permute the bag with a Fisher-Yates shuffle }
  FOR bag_index := I_RAISED DOWNTO T_UP DO BEGIN
    other_index := BLOCK + Int(Rand * (bag_index + 1));
    temp := bag[other_index];
    bag[other_index] := bag[bag_index];
    bag[bag_index] := temp;
  END;

  { The next item to draw from the bag is the first item in the bag }
  bag_index := BLOCK;
END;


{ DrawFromBag -- Draw a tetromino from the bag

  If the bag is empty after the draw, place a new collection of tetrominoes
  into the bag.

  Returns: 
    The tetromino drawn from the bag.

  Globals used: bag, bag_index }
FUNCTION DrawFromBag: BlockType;
BEGIN
  DrawFromBag := bag[bag_index];
  IF bag_index >= I_RAISED THEN ResetBag ELSE bag_index := Succ(bag_index);
END;


{ CanDrop -- can you drop a certain kind of tetromino in a specific column?

  Args:
    block: The kind of tetromino you want to drop
    column: The column where you want to put its leftmost squares

  Returns:
    True iff the specified type of tetromino won't extend beyond the right edge
    of the field if its leftmost squares occupy the specified column.

  Globals used: blocks_x, i }
FUNCTION CanDrop(block: BlockType; column: Column): Boolean;
BEGIN
  CanDrop := TRUE;
  FOR i := 1 TO 4 DO
    IF (blocks_x[block, i] + column) > kColumns THEN CanDrop := FALSE;
END;


{ DropBottom -- what row will the tetromino drop down to?

  Args:
    block: The kind of tetromino you want to drop
    column: The column where you want to put its leftmost squares; we assume
        that the tetromino will have no squares that extend beyond the right
        edge of the field (use CanDrop to check)

  Returns:
    The row where the tetromino's lowest squares will rest when the block is
    dropped onto the field with its leftmost squares in the specified column.
    Note that the bottom row is Row 1.

  Globals used: blocks_x, blocks_y, field, i }
FUNCTION DropBottom(block: BlockType; column: Column): Integer;
VAR
  bottom, highest_bottom: Integer;
BEGIN
  highest_bottom := 0;
  FOR i := 1 TO 4 DO BEGIN
    bottom := field[column + blocks_x[block, i]] - blocks_y[block, i];
    IF bottom > highest_bottom THEN highest_bottom := bottom;
  END;
  DropBottom := highest_bottom + 1;
END;


{ Drop -- drop a tetromino onto the playing field

  Updates the `field` global to describe the new heights of each column on the
  playing field, and also draws the tetromino onto the playing field display.

  Args:
    block: The kind of tetromino you want to drop
    column: The column where you want to put its leftmost squares; we assume
        that the tetromino will have no squares that extend beyond the right
        edge of the field (use CanDrop to check)

  Globals used: blocks_x, blocks_y, field, i }
PROCEDURE Drop(block: BlockType; column: Column);
VAR
  bottom: Integer;
BEGIN
  { Calculate how far the tetromino falls }
  bottom := DropBottom(block, column);
  FOR i := 1 TO 4 DO BEGIN
    field[column + blocks_x[block, i]] := bottom + blocks_y[block, i];
  END;

  { Set up geometry for the playing field and draw the tetromino }
  { ViewPort(3, 45, 7, 85); }
  ViewPort(3, 45, 7, 100);
  Window(field_x_min, field_x_max, field_y_min, field_y_max);
  DrawBlock(block, column, bottom, {with_ornament=}TRUE);
END;


{ GameRound -- Play a round of Fourspite

  Assumes an empty playing field has been drawn with DrawEmptyBoard and the
  game has just been reset with ResetGame.

  Globals used: i, j, k, field, we_play_first, plus all tetromino, game state,
      and miscellaneous globals (see note at global VAR) }
PROCEDURE GameRound;
CONST
  kBlockTag = $B10;
  kColumnTag = $C01;
  kSelectionNoise = '---...///000111222333444555666777888999';
  kNewRoundSong = 'T9C28DEDCDE24CR99C24';
  kYourTurnSong = 'T9C316GC4';
  kGameOverSong = 'T9E112FGEFDC';
  kBoastingSong = 'T9R16E164FGEGDC';
VAR
  { Which rack are we playing right now? As a hack to avoid filling the stack
    with booleans, we use the sign bit (negative values) to indicate that we
    need to draw a new rack }
  rack_row: Integer;
  { Who is playing the current move? }
  player_is_us_not_them: Boolean;
  { Did either player achieve the bonus? }
  we_have_x2, they_have_x2: Boolean;
  { How many points has each player got (not counting bonus)? }
  points_pot_us, points_pot_them: Integer;
  { What's the tallest the field gets right now? }
  max_height: Integer;
  { Temporaries for collecting user input }
  block: BlockType; column: Column;

  { GetBlock -- Retrieve the current player's choice of tetromino }
  PROCEDURE GetBlock;
  VAR
    rack_choice: RackColumn;
  BEGIN
    { Retrieve a block choice locally, which we do if it's the local player's
      turn or if this is a local-only game }
    IF player_is_us_not_them OR NOT flag_use_serial THEN BEGIN
      IF flag_has_r12 THEN BEGIN
        Call(kR12MusicCall, kYourTurnSong);
        rack_choice := GetKeyToIntR12(Ascii('A'), 1, kRackSize, 54, 7);
        R12Sounds(kSelectionNoise);
      END ELSE rack_choice := GetKeyToIntStd(Ascii('A'), 1, kRackSize, 54, 7);
      { Having retrieved locally, we now need to circle the choice }
      DrawRackItem(BLOCK, {circle_instead=}TRUE, rack_row, rack_choice);
      { Now record the choice and update the rack }
      block := rack[rack_choice];
      rack[rack_choice] := DrawFromBag;
      { Send the choice to the remote if this is a "networked" game }
      IF flag_use_serial THEN PutIntSerial(kBlockTag, block);

    { Retrieve a block choice remotely, which we do if it's not the local
      player's turn AND if it's a "networked" game }
    END ELSE block := GetIntSerial(kBlockTag, BLOCK, I_RAISED, 54, 11);
  END;

  { GetColumm -- Get the current player's column choice; call AFTER GetBlock }
  PROCEDURE GetColumn;
  BEGIN
    { Retrieve a column choice locally, which we do if it's the local player's
      turn or if this is a local-only game }
    IF player_is_us_not_them OR NOT flag_use_serial THEN BEGIN
      { The user must choose a column that fits the block }
      IF flag_has_r12 THEN Call(kR12MusicCall, kYourTurnSong);
      REPEAT
        IF flag_has_r12
           THEN column := GetKeyToIntR12(Ascii('1'), 1, kColumns, 54, 3)
           ELSE column := GetKeyToIntStd(Ascii('1'), 1, kColumns, 54, 3);
      UNTIL CanDrop(block, column);
      IF flag_has_r12 THEN R12Sounds(kSelectionNoise);
      { Send the choice to the remote if this is a "networked" game }
      IF flag_use_serial THEN PutIntSerial(kColumnTag, column);

    { Retrieve a column choice remotely, which we do if it's not the local
      player's turn AND if it's a "networked" game }
    END ELSE column := GetIntSerial(kColumnTag, 1, kColumns, 54, 11);
  END;

  { RoundNotOver -- is the current round over? }
  FUNCTION RoundNotOver: Boolean;
  VAR
    more_racks_to_go: Boolean;
  BEGIN
    { Have we BOTH run out of racks? The second condition of this disjunction
      only matters if we've run out of racks but the other player (in a
      "networked" game) can play one final move because they haven't used up
      all of their racks yet. }
    more_racks_to_go := (Abs(rack_row) <= (3 * kRacksPerGroup));
    { Actually we need to split the disjunction over multiple lines to avoid
      making BASIC statements that are too long }
    more_racks_to_go := more_racks_to_go OR
        (flag_use_serial AND we_play_first AND NOT player_is_us_not_them);
    { Additionally, have we overtopped the playing field? }
    RoundNotOver := (max_height <= kRowTop) AND more_racks_to_go;
  END;

BEGIN
  { A new round! Play the tune }
  IF flag_has_r12 THEN Call(kR12MusicCall, kNewRoundSong);

  { NOTE: The implementation betrays a slight desire to avoid branching }
  { Prepare to play an exciting round of Fourspite! }
  rack_row := -1;  { Negative means that we draw a new rack }
  player_is_us_not_them := we_play_first;
  points_pot_us := 0;
  points_pot_them := 0;
  they_have_x2 := FALSE;
  we_have_x2 := FALSE;
  max_height := 0;  { precondition: field is empty }

  { Inner user interface loop: repeat until the round is over }
  WHILE RoundNotOver DO BEGIN
    { Flush the serial buffer if we're playing a "networked" game }
    IF flag_use_serial THEN WriteLn('@40,30');

    { Draw a new rack row if necessary, which we only do if we're playing
      a local game or if it's our turn. The reasons for this are suspense(!)
      and also because otherwise the opponent might go ahead and choose a
      piece (and tell us about it) while we are still busy drawing the rack. }
    IF (player_is_us_not_them OR NOT flag_use_serial) AND (rack_row < 0) THEN
    BEGIN
      rack_row := -rack_row;
      IF NOT flag_use_serial THEN
          DrawRackLabel(player_is_us_not_them, rack_row);
      FOR k := 1 TO kRackSize DO
          DrawRackItem(rack[k], {circle_instead=}FALSE, rack_row, k);
    END;

    { Get the block and column from the user, or the remote opponent }
    GetBlock;
    GetColumn;

    { Drop the block at the chosen column }
    Drop(block, column);

    { Update points pot tally displays --- we REALLY try to avoid branching! }
    j := blocks_y[block, 4] + 1;  { Points earned from dropping the block }
    DrawTallies(
        points_pot_us + 1,
        player_is_us_not_them * (points_pot_us + j),
        77, 92);
    DrawTallies(
        points_pot_us + 1,
        player_is_us_not_them * we_have_x2 * (points_pot_us + j),
        77, 89);
    DrawTallies(
        points_pot_them + 1,
        (NOT player_is_us_not_them) * they_have_x2 * (points_pot_them + j),
        77, 85);
    DrawTallies(
        points_pot_them + 1,
        (NOT player_is_us_not_them) * (points_pot_them + j),
        77, 82);

    { Update points pots --- again, no branching }
    points_pot_us := points_pot_us + player_is_us_not_them * j;
    points_pot_them := points_pot_them + (NOT player_is_us_not_them) * j;

    { Move to the next rack if appropriate }
    IF player_is_us_not_them OR NOT flag_use_serial THEN BEGIN
      CrossOutRack(rack_row);
      rack_row := -Succ(rack_row);
    END;

    { Calculate the new max height; can only stay the same or go higher }
    FOR i := 1 TO kColumns DO
      IF max_height < field[i] THEN max_height := field[i];

    { Enable the bonus if someone crossed the bonus threshold }
    IF (max_height > kRowBonus) AND
       NOT (we_have_x2 OR they_have_x2) THEN BEGIN
      we_have_x2 := player_is_us_not_them;
      they_have_x2 := NOT player_is_us_not_them;
      DrawTallies(1, player_is_us_not_them * points_pot_us, 77, 89);
      DrawTallies(1, (NOT player_is_us_not_them) * points_pot_them, 77, 85);
    END;

    { Now it's the other player's turn }
    player_is_us_not_them := NOT player_is_us_not_them;
  END;

  { The round is over! Identify the winner (if any), award points, etc. }
  MoveCursorGdu(9, 91);
  IF max_height > kRowTop THEN BEGIN
    IF player_is_us_not_them THEN BEGIN  { Meaning *they* lost }
      score_us :=
          score_us + points_pot_us + we_have_x2 * points_pot_us;
      WriteUsing('"You win "2D" points!"',
                 points_pot_us + we_have_x2 * points_pot_us);
    END ELSE BEGIN
      score_them :=
          score_them + points_pot_them + they_have_x2 * points_pot_them;
      WriteUsing('"They win "2D" points"',
                 points_pot_them + they_have_x2 * points_pot_them);
    END;
  END ELSE BEGIN
    Write('DRAW -- No winner!');
  END;

  { Play the "game over" music }
  IF flag_has_r12 THEN BEGIN
    Call(kR12MusicCall, kGameOverSong);
    FOR i := 1 TO 3 DO Call(kR12MusicCall, kBoastingSong);
  END;

  { Draw "chasing lights" around the game result }
  FOR i := 1 TO 18 DO BEGIN
    ChasePrompt(1, 9, 94, 18);
    ChasePrompt(2, 9, 88, 18);
  END;
END;


{$I graphics.pas}
{$I io.pas}


BEGIN
  IoSetup;
  Write('Please wait... ');
  Randomize;
  SetDegrees;

{ Load tetrominoes }
{$I define_tetrominoes.pas}

  CalculateGeometry;
  chase_prompt_state := 0;  { miscellaneous initialisation }

  ResetGame({zero_the_score=}TRUE);
  WHILE TRUE DO BEGIN
    DrawEmptyBoard;
    { Play this round }
    GameRound;
    { Prepare for the next round }
    ResetGame({zero_the_score=}FALSE);
    we_play_first := NOT we_play_first;
  END;
END.
