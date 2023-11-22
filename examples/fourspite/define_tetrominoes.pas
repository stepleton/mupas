{ Tetrominoes definitions for Fourspite.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

Fills two matrices called "blocks_x" and "blocks_y", which define the shapes
of the 19 fixed tetrominoes. Assuming a Cartesian grid where X coordinates
increase rightwards on the horizontal axis and Y coordinates increase upwards
on the vertical axis, (blocks_x[j,k], blocks_y[j,k]) is the coordinate of the
k'th square of j'th tetromino.

All tetrominoes are positioned as close as possible to the origin whilst
remaining in the upper right-hand quadrant of the grid. Tetromino squares are
listed in row-major order: for each row from bottom to top, list square
positions from left to right. Code may (and does) depend on this ordering.

Requirements:
- A 2-D 19x4 array of Integer called "blocks_x".
- A 2-D 19x4 array of Integer called "blocks_y". }

  MatrixFill(blocks_x, 0, 1, 0, 1,   { BLOCK }
                       0, 1, 2, 1,   { T_UP }
                       1, 0, 1, 1,   { T_LEFT }
                       1, 0, 1, 2,   { T_DOWN }
                       0, 0, 1, 0,   { T_RIGHT }
                       0, 0, 0, 1,   { G_UP }
                       0, 1, 2, 0,   { G_LEFT }
                       0, 1, 1, 1,   { G_DOWN }
                       2, 0, 1, 2,   { G_RIGHT }
                       0, 1, 0, 0,   { L_UP }
                       0, 1, 2, 2,   { L_LEFT }
                       1, 1, 0, 1,   { L_DOWN }
                       0, 0, 1, 2,   { L_RIGHT }
                       0, 1, 1, 2,   { S_LOWERED }
                       1, 0, 1, 0,   { S_RAISED }
                       1, 2, 0, 1,   { Z_LOWERED }
                       0, 0, 1, 1,   { Z_RAISED }
                       0, 1, 2, 3,   { I_LOWERED }
                       0, 0, 0, 0);  { I_RAISED }

  MatrixFill(blocks_y, 0, 0, 1, 1,   { BLOCK }
                       0, 0, 0, 1,   { T_UP }
                       0, 1, 1, 2,   { T_LEFT }
                       0, 1, 1, 1,   { T_DOWN }
                       0, 1, 1, 2,   { T_RIGHT }
                       0, 1, 2, 2,   { G_UP }
                       0, 0, 0, 1,   { G_LEFT }
                       0, 0, 1, 2,   { G_DOWN }
                       0, 1, 1, 1,   { G_RIGHT }
                       0, 0, 1, 2,   { L_UP }
                       0, 0, 0, 1,   { L_LEFT }
                       0, 1, 2, 2,   { L_DOWN }
                       0, 1, 1, 1,   { L_RIGHT }
                       0, 0, 1, 1,   { S_LOWERED }
                       0, 1, 1, 2,   { S_RAISED }
                       0, 0, 1, 1,   { Z_LOWERED }
                       0, 1, 1, 2,   { Z_RAISED }
                       0, 0, 0, 0,   { I_LOWERED }
                       0, 1, 2, 3);  { I_RAISED }

  MatrixFill(blocks_ornament_x,  -9,   8,  -8,   8,  -9,   { ZIG_ZAG }
                                  0,  -2,  -8,   2,   8,   { SOLIDUS }
                                 -2,   0,  -6,  -2,   8,   { BEVELED }
                                 -8,   2,   4,  -2,  -1,   { HELICAL }
                                 -1,  -8,   0,   8,   0,   { THICKER }
                                -10,  10,   0, -10,   7,   { LAZERZZ }
                                 -8,   1,   4,   1,  -8);  { KNOTTED }
  blocks_ornament_x := blocks_ornament_x / 10;

  MatrixFill(blocks_ornament_y,  -2,  -2,  -2,  -2,  -2,   { ZIG_ZAG }
                                 -2,   2,  -8,  -2,   8,   { SOLIDUS }
                                 -2,  -6,   0,  -2,   2,   { BEVELED }
                                 -2,  -6,   3,   2,  -2,   { HELICAL }
                                 -1,   0,  -8,   0,   8,   { THICKER }
                                -10,   7,  -2,  -5,  10,   { LAZERZZ }
                                 -4,  -4,   0,   4,   4);  { KNOTTED }
  blocks_ornament_y := blocks_ornament_y / 10;

  MatrixFill(blocks_which_ornament, BEVELED,   { BLOCK }
                                    HELICAL,   { T_UP }
                                    HELICAL,   { T_LEFT }
                                    HELICAL,   { T_DOWN }
                                    HELICAL,   { T_RIGHT }
                                    SOLIDUS,   { G_UP }
                                    SOLIDUS,   { G_LEFT }
                                    SOLIDUS,   { G_DOWN }
                                    SOLIDUS,   { G_RIGHT }
                                    THICKER,   { L_UP }
                                    THICKER,   { L_LEFT }
                                    THICKER,   { L_DOWN }
                                    THICKER,   { L_RIGHT }
                                    LAZERZZ,   { S_LOWERED }
                                    LAZERZZ,   { S_RAISED }
                                    KNOTTED,   { Z_LOWERED }
                                    KNOTTED,   { Z_RAISED }
                                    ZIG_ZAG,   { I_LOWERED }
                                    ZIG_ZAG);  { I_RAISED }
