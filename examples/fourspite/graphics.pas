{ Graphics routines for Fourspite 

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details. }


{ CalculateGeometry -- calculate useful values for graphical layout

  Sets global variables used by graphics routines that draw or update the
  playing field and the rack display. Naturally, this procedure ought to be
  called before calling any other routine that uses those globals.

  Globals used: all of the "game geometry" globals (see note at global VAR) }
PROCEDURE CalculateGeometry;
VAR
  padding: Real;
BEGIN
  { ##### Playing field geometry ##### }

  { Would the playing field fit in the available space if X stretched to fill
    all of the availale room? }
  IF 30 * (kRowTop + 1) / kColumns > 75 THEN BEGIN
    { If here, the field is too tall to fit if we stretch X to fill the
      available space. Shrink the field down so that Y just fits, which shrinks
      X. We begin by computing how much empty padding we'll need around X to
      keep the field square. }
    padding := 42 - 75 * kColumns / (kRowTop + 1) - 12;

    { Compute X layout to incorporate padding }
    field_half_gdu := (kRowTop + 1) / 150;
    field_x_min := -(padding + 24) * field_half_gdu;
    field_x_max := kColumns + padding * field_half_gdu;

    { Set Y such that Y fills the available space }
    field_y_min := -0.04 * (kRowTop + 1);
    field_y_max := (kRowTop + 1);

    { Finally, locate ornaments }
    field_ylabel_x := -0.16 * (kRowTop + 1);
    field_ylabel_tick := -2 * (kRowTop + 1) / 75;
    field_xlabel_y := field_y_min;

  END ELSE BEGIN
    { If here, the field is too short to fill the available space even if we
      stretch X across all of it. So we want to add vertical padding to centre
      the board. }
    padding := 78 - 30 * (kRowTop + 1) / kColumns - 3;

    { Set X such that X fills the available space }
    field_x_min := -0.4 * kColumns;
    field_x_max := kColumns;

    { Compute Y layout to incorporate padding }
    field_half_gdu := kColumns / 60;
    field_y_min := -(padding + 6) * field_half_gdu;
    field_y_max := kRowTop + 1 + padding * field_half_gdu;

    { Finally, locate ornaments }
    field_ylabel_x := field_x_min;
    field_ylabel_tick := -kColumns / 15;
    field_xlabel_y := -kColumns / 10;
  END;

  { ##### Rack display geometry ##### }

  rack_square_size := 161 / (8 * kRackSize - 1);
  rack_spacing := rack_square_size * 8 / 7;

  { ##### Playing field geometry again ##### }

  { Previously the top of the playing field extended only to 85 Y GDUs, but
    this had the side-effect of cutting off the tops of many pieces that
    extended beyond the top of the field. So now the playing field geometry
    reaches the top of the display itself, but rather than recalculate all of
    the bounds above, we instead just amend field_y_max to extend upwards into
    the extra 15 GDUs. This probably also makes the padding calculations
    simpler, but really that's just an excuse. }
  field_y_max := field_y_max + 30 * field_half_gdu;
END;


{ DrawEmptyBoard -- Clear the screen and draw a complete empty game board

  When this procedure is finished, the viewport and window configuration will
  be set up for drawing to the playing field.

  Globals used: i, j, and all of the "game geometry" globals (see note at
      global VAR) that start with "field_". }
PROCEDURE DrawEmptyBoard;
VAR
  column_label: String[1];
BEGIN
  Page;  { Clear the screen; restore default viewport and window config }
  ViewPort(0, 130, 0, 130);
  Window(0, 130, 0, 130);

  { Draw fixed screen text and ornaments }
  MoveCursorGdu(7, 4);
  Write('Place leftmost squares');

  MoveCursorGdu(50, 100);
  DrawGdu(50, 80);
  DrawGdu(130, 80);
  MoveCursorGdu(52, 96);
  Write('SCORES  now + to the winner');
  MoveCursorGdu(52, 92);
  WriteUsing('  "  You: "4D" +"', score_us);
  MoveCursorGdu(52, 82);
  WriteUsing('  " Them: "4D" +"', score_them);

  MoveCursorGdu(50, 0);
  DrawGdu(50, 20);
  DrawGdu(130, 20);
  MoveCursorGdu(52, 15);
  Write('WHAT NOW?');
  MoveCursorGdu(65, 11);
  Write('Wait for the other player''s move');
  MoveCursorGdu(65, 7);
  Write('Choose a piece to drop : A,B,C...');
  MoveCursorGdu(65, 3);
  Write('Choose where to drop it: 1,2,3...');

  { Draw the "rack sheet" }
  MoveCursorGdu(78, 23);
  DrawGdu(78, 76);
  MoveCursorGdu(103, 23);
  DrawGdu(103, 76);
  FOR j := 0 TO 2 DO BEGIN
    FOR i := 0 TO (kRackSize - 1) DO BEGIN
      MoveCursor(53.5 + 25 * j + rack_spacing * i + rack_square_size / 2, 74);
      column_label := Chr(65 + i);
      Write(column_label);
    END;
  END;

  { Draw the field and its ornaments }
  { ViewPort(3, 45, 7, 85); }                                { Set up geometry }
  ViewPort(3, 45, 7, 100);                               { Set up geometry }
  Window(field_x_min, field_x_max, field_y_min, field_y_max);
  MoveCursor(0, kRowTop + 1);                            { Draw the field }
  Draw(0, 0);
  Draw(kColumns, 0);
  Draw(kColumns, kRowTop + 1);
  MoveCursor(field_ylabel_x, kRowTop - field_half_gdu);  { TOP, BONUS lines }
  Write('  TOP');
  MoveCursor(field_ylabel_tick, kRowTop);
  Draw(kColumns, kRowTop);
  MoveCursor(field_ylabel_x, kRowBonus - field_half_gdu);
  Write('BONUS');
  MoveCursor(field_ylabel_tick, kRowBonus);
  Draw(kColumns, kRowBonus);
  FOR i := 1 TO kColumns DO BEGIN                        { Column labels }
    MoveCursor(i - 0.5 - field_half_gdu, field_xlabel_y);
    column_label := Chr(48 + i);
    Write(column_label);
  END;
END;


{ DrawBlock -- Draw a tetromino

  Tetromino squares are drawn to be one UDU (user data unit) in height and
  width.  "Columns"/"rows" are therefore 1 UDU wide/tall. They count up from 1,
  so the leftmost/bottom-most column/row is 1. This procedure does not alter
  the way the viewport and window are configured, since it's used for drawing
  blocks on the playing field and within racks.
  
  Args:
    block: Which tetromino to draw
    column: Column receiving the tetromino's leftmost square(s), in UDUs
    row: Row receiving the tetromino's bottom-most square(s), in UDUs
    with_ornament: Whether to draw the decoration found inside the tetromino
  
  Globals used: i, j, blocks_[xy], blocks_ornament_[xy] }
PROCEDURE DrawBlock(block: BlockType;
                    column: Column; row: Integer;
                    with_ornament: Boolean);
VAR
  ornament: BlockOrnamentType;
BEGIN
  { Draw each box }
  FOR i := 1 TO 4 DO BEGIN
    { Draw the block's exterior box }
    MoveCursor(column + blocks_x[block, i], row + blocks_y[block, i]);
    RelDraw(0, -1);
    RelDraw(-1, 0);
    RelDraw(0, 1);
    RelDraw(1, 0);

    { Draw the ornamentation inside the box }
    IF with_ornament THEN BEGIN
      ornament := blocks_which_ornament[block];
      FOR j := 1 TO 5 DO RelDraw(blocks_ornament_x[ornament, j],
                                 blocks_ornament_y[ornament, j]);
    END;
  END;
END;


{ DrawRackLabel -- Label a rack as belonging to "you" or "them"

  Args:
    player_is_us_not_them: If true, label the rack as "you", otherwise label it
        as "them"
    row: Which rack row to label; rack rows are counted sequentially across all
        columns in column-major order

  Globals used: rack_square_size }
PROCEDURE DrawRackLabel(player_is_us_not_them: Boolean; row: Integer);
VAR
  x, y: Real;
BEGIN
  x := 59 +                                  { Leftmost rack column }
       25 * ((row - 1) DIV kRacksPerGroup);  { Current column group }
  y := 70 - (rack_square_size + 3) * ((row - 1) MOD kRacksPerGroup);
  MoveCursorGdu(x, y);
  IF player_is_us_not_them THEN Write('you') ELSE Write('them');
END;


{ DrawRackItem -- Draw a tetromino (or a circle) in the rack display

  When this procedure is finished, the viewport and window configuration will
  be set up for drawing to the specified rack location. Window configuration
  will differ depending on whether we are drawing a tetromino or a circle.

  Args:
    block: Which tetromino to draw (if we are drawing a tetromino)
    circle_instead: If TRUE, don't draw a tetromino, draw a circle instead
    row: Which rack row to draw to; rack rows are counted sequentially across
        all columns in column-major order
    column: Which rack column to draw to

  Globals used: i, j, blocks_[xy], rack_square_size, rack_spacing }
PROCEDURE DrawRackItem(block: BlockType; circle_instead: Boolean;
                       row: Integer; col: RackColumn);
VAR
  x_min, x_max, y_min, y_max: Real;
BEGIN
  { Viewport positioning }
  x_min := 54 +                                  { Leftmost rack column }
           25 * ((row - 1) DIV kRacksPerGroup);  { Current column group }
  x_min := x_min +                               { Which column in the group }
           (col - 1) * rack_spacing;
  x_max := x_min + rack_square_size;
  y_max := 71 - (rack_square_size + 3) * ((row - 1) MOD kRacksPerGroup);
  y_min := y_max - rack_square_size;
  ViewPort(x_min, x_max, y_min, y_max);

  IF circle_instead THEN BEGIN
    { If in here, the caller wanted to draw a circle, not a tetromino }

    { Coordinate system within the viewport }
    Window(-1, 1, -1, 1);
    MoveCursor(1, 0);
    FOR i := 1 TO 8 DO BEGIN
      j := 45 * i;
      Draw(Cos(j), Sin(j));
    END;

  END ELSE BEGIN
    { If in here, the caller wanted to draw a tetromino, not a circle }

    { Coordinate system within the viewport }
    x_min := blocks_x[block, 4];    { Usually the last block is the rightmost }
    x_min := x_min +                { block, but this hack lets us handle }
             (block = T_UP) +       { cases where it is not. (We use multiple }
             (block = T_RIGHT) +    { statements because otherwise the BASIC }
             (block = G_LEFT) * 2;  { statement is too long for the Tek.) }
    x_min := x_min +                { For y_min, the last block is the }
             (block = L_UP) +       { uppermost block; no hack is required. }
             (block = S_RAISED) +
             (block = Z_LOWERED)
             - 3;
    x_min := x_min / 2;
    x_max := x_min + 4;
    y_min := (blocks_y[block, 4] - 3) / 2;
    y_max := y_min + 4;
    Window(x_min, x_max, y_min, y_max);

    { Draw the block }
    DrawBlock(block, 1, 1, {with_ornament=}FALSE);
  END;
END;


{ CrossOutRack -- Cross out an entire rack in the rack display

  When this procedure is finished, the viewport and window configuration will
  be set up for crossing out the specified rack.

  Args:
    row: Which rack row to cross out; rack rows are counted sequentially across
        all columns in column-major order

  Globals used: rack_square_size }
PROCEDURE CrossOutRack(row: Integer);
VAR
  x_min, y_max: Real;
BEGIN
  x_min := 54 + 25 * ((row - 1) DIV kRacksPerGroup);
  y_max := 71 - (rack_square_size + 3) * ((row - 1) MOD kRacksPerGroup);
  ViewPort(x_min, x_min + 23, y_max - rack_square_size, y_max);
  Window(0, 1, 0, 1);
  MoveCursor(0, 0);
  Draw(1, 1);
  MoveCursor(1, 0);
  Draw(0, 1);
END;


{ ChasePrompt -- Update a non-storing "chasing lights" display.

  By itself, this routine just flashes an asterisk on the screen, but called
  repeatedly with the same arguments, a horizontal "chasing" asterisks effect
  is achieved.

  Args:
    id: Multiple chasing lights can appear on the screen at once. This
        identifier is an index into state information for the lights --- use
        the same unique index for each unique chasing light so that it appears
        to move in a consistent way
    x: Left edge of the chasing lights display, in GDUs
    y: Bottom edge of the chasing lights display, in GDUs
    size: Width of the chasing lights display, in characters

  Globals used: j, chase_prompt_state }
PROCEDURE ChasePrompt(id: ChaseId; x, y: Real; size: Integer);
BEGIN
  j := chase_prompt_state[id];  { For speed, avoiding array indexing }
  IF j >= size THEN j := 0;
  MoveCursorGdu(x + 1.8 * j, y);
  Write('@32,24', '*');
  chase_prompt_state[id] := j + 1;
END;


{ DrawTallies -- Draw portions of a "tally marks" (groups of 5) display.

  When this procedure is finished, the viewport and window configuration will
  be set up for drawing a tally mark display at the specified location.

  Args:
    first: Draw tally marks from this number...
    last: ...up to this number (a value less than `first` draws nothing)
    x: Left edge of the tally marks display, in GDUs
    y: Bottom edge of the tally marks display, in GDUs

  Globals used: i }
PROCEDURE DrawTallies(first, last: Integer; x, y: Real);
BEGIN
  ViewPort(x, x + 100, y, y + 2);
  Window(0, 99, 0, 1);
  IF last >= first THEN BEGIN
    { The enclosing IF statement reflects what's probably a compiler bug? In
      Pascal, `FOR i := 10 to 1` should not iterate at all, but in Tek 4050
      BASIC, it seems it will execute the loop body at least once. For the way
      this procedure is called, it matters... }
    FOR i := first TO last DO BEGIN
      MoveCursor(i, 1);
      IF i MOD 5 THEN Draw(i, 0) ELSE Draw(i - 5, 0);
    END;
  END;
END;
