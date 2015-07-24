# chess_challenge

Designed to solve the chess challenge described in 'ChessChallenge.pdf'.

## Usage

Given a sequence of chess pieces and the dimensions width and height
of the board, returns all configurations for which all of the pieces can be placed
without threatening each other.
A piece must be one of: :king :queen :bishop :rook :knight.

Additonal argument is :count-only, for not collecting and printing all solutions, but only returning
the number of unique configurations.

This is recommended for bigger board sizes, to save memory and not trying to print out millions of boards.

Example for bigger boards:

lein run '[:king :king :queen :queen :bishop :bishop :knight]' 7 7 :count-only true

For smaller boards:

lein run '[:king :king :rook]' 3 3

## License

Copyright © 2015 Michael Sappler

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
