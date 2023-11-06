# STACKS

This is a 1 vs 1 board game. <br>
Each player has 10 pieces (either in red or black), who can be combined in stacks, with up to 4 pieces per stack. At the beginning of the game, all pieces are combined in stacks of 2.<br>
The winner is the one that can place 4 or more of his own pieces into one or more of the opponent's homespaces, or eliminate 6 or more opponent's pieces.

## Board dimensions

The board dimensions are 5x5 with 5 extra homespaces at each side, in which the pieces start the game from.

## Rules

### 1. Playing a turn

The player who has the black pieces is always the first one to play. After that, players take turns alternately.

### 2. Stack sizes

Your pieces can be in stacks of 1, 2, 3 or 4, whether on the grid, or in the home spaces.

### 3. Moving your pieces

Pieces / stacks of pieces can move in any direction across the grid, moving from one square to an adjacent one, horizontally (left and right), vertically (up and down), or diagonally (combining directions), up to their movement allowance.<br>
`A new stack can be created by moving one or more pieces into a square that is already occupied by a friendly piece / stack, but when that happens, it must stop its movement.`<br>
`One or more pieces can also be moved from an existing stack.`<br>
>Hint:<br>
During your move, you may also attack the opponent’s pieces and eliminate some or all of them by moving your pieces / stacks into squares occupied by the opponent.

- ### 3.1. Movement allowances

Movement allowance depends on the stack's size:

- Stack of 1 -> 3 spaces<br>
- Stack of 2 -> 2 spaces<br>
- Stack of 3 -> 1 space<br>
- Stack of 4 -> cannot move<br>
- Instead, a player can move 2 individual pieces up to 2 spaces each

- ### 3.2. Movement restrictions

Your pieces cannot move into spaces occupied by opponent pieces unless your are attacking them.<br>
Your pieces cannot move into spaces with friendly pieces unless you want to make a new stack.<br>
Your pieces cannot move diagonally between two spaces that are both occupied by opponent pieces.<br>

- ### 3.3. Stacks of 4 pieces

If you have one or more stacks of four pieces on the board at the beginning of your turn:

- You must unstack 1 or more pieces from that stack in your turn.
- Pieces that are moved from a stack of four may `not` attack other pieces.
- If you cannot move any pieces from your stack of four (e.g., because the opponent has surrounded it), all pieces belonging to that stack are immediately eliminated.

### 4. Attacking Opponent’s Pieces

A piece or stack attacks an opponent piece when it moves into a space occupied by that opponent piece / stack. You can only attack an opponent with a stack that has `at least one more piece` than the opponent’s stack.<br>
Pieces / stacks can attack on their own, or in combination with friendly pieces or stacks that are adjacent to each other and adjacent to the same opponent piece(s) at the moment that the attack takes place.

- ### 4.1. Moving and Attacking

Attacking is part of a movement. `Your piece can only attack if it has some moves left in its movement allowance`.
Once it has attacked the opponent, your piece / stack cannot move again.

- ### 4.2. Combining / Creating Stacks in an Attack

When two or more of your stacks are adjacent to the same opponent piece,
and adjacent to each other, they can combine their strength to attack the opponent. When this is done, the attacking pieces all move into the space vacated by the opponent pieces, forming a new stack. (since the biggest possible stack is four pieces, you cannot attack an opponent with more than four pieces at a time).

- ### 4.3. Special case: When 2 single pieces are moved 1-2 spaces each and attack

It is allowed to move two individual pieces and to use both those pieces in an attack.<br>
Note that both individual pieces that are moved must have one
of their movement points left over, after moving, to be allowed
to make an attack.
> Remember:<br>
A piece can just attack if it has movements left in its movement allowance.

- ### 4.4 Attacking from a Home Space

Pieces may launch attacks from one or more home spaces. Pieces that attack from home spaces may combine their strength with other
pieces, as described in [4.2].

- ### 4.5. Results of an Attack

When attacked, the opponent's number of pieces' eliminated equal the difference between the strength of the attacking and the number of pieces the opponent had before the attack<br>(for example, if you attack with four pieces, and the opponent has two pieces, the opponent loses two pieces; if you attack with three pieces and the opponent has two, the opponent loses one piece).<br>
Any opponent pieces that are not eliminated by an attack have to retreat 1 space towards the owning player’s board edge (the direction of this retreat is decided by the `attacking` player).<br>
Pieces that are attacked and defeated (but not eliminated) while
occupying one of their own home spaces must retreat to another home space.<br>
The attacking pieces move into the space vacated by
the opponent pieces.

### 5. Eliminated Pieces

Eliminated pieces are placed to the side of the board. They are never returned to the game.

### 6. Home Spaces

A friendly piece or stack that occupies one of your home
spaces can move, attack, and defend from attack as normal.<br>
You may move your pieces: 
- from one friendly homespace to another (as long as it is not occupied by an opponent piece or stack)
- from a friendly homespace to the grid, and vice versa

> Note:<br>
When one or more of your pieces reach an opponent’s home space, they remain there for the rest of the game. They may not move again during the game. Pieces that have reached an opponent’s home space cannot be attacked. If an opponent’s homespace has already been occupied by one or
more of your pieces, your other pieces can move into that homespace later in the game.
No more than 4 pieces are allowed to occupy a homespace.<br>
All the rules of movement and stacking set out in rules [2] and [3]  apply to homespaces.

#### Additional Information
Work done by André Leonor (up201806860) and Rui Carvalho (up202108807) in the context of the course Functional Programming and Logic.
