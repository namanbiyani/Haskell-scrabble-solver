# __Haskell Scrabble Solver__

A scrabble solver written in Haskell, a functional programming language.

![Board](/images/board.png)

## __Features__
Go into the Scrabble folder and run:- runhaskell Scrabble.hs

![Command](/images/cmd.png)

![Scrabble Prompt](/images/2player_1.png)

* ### Two Player Game
The two player game allows two players to play scrabble.
We can add the word manually in the board, or we can choose "Start Game".
In first case we provide the word, its starting coordinates, and orientation(horizontal or vertical).

![2 Player](/images/2player_3.png)

Computer checks the validity of the word in the dictionary and adds it to the board.

![Board Addition](/images/2player_4.png)

In the Start Game option the computer provides us with 7 letters.

![Start Game](/images/game_1.png)

We have to construct a word on the board using the letters.
Computer checks the validity of the word in the dictionary and adds it to the board, it also shows the points scored by forming this board.

![Game](/images/game_2.png)

* ### Play With Me

![Play PC](/images/playpc_1.png)

We can use this feature to play with the computer or to get help in choosing the optimal word(to score highest points).
It asks for 7 letters which it should use to construct the word on the board.

![Ask Letters](/images/playpc_2.png)

The Computer constructs the word which will give a maximum score from these letters on the board and writes it on the board.

![New Board](/images/playpc_3.png)
