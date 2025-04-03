with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Chess_Types; use Chess_Types;
with Showboard; use Showboard;

procedure QueenArmies is
   N, M : Positive;
   Board : Constrained_Board := (others => (others => Empty));
   Solution_Count : Natural := 0;

   function Valid_Input(M, N : Positive) return Boolean is
   begin
      return M in 1..4 and N in 1..10 and 2*M <= N*N;
   end Valid_Input;

   function Is_Safe(Board : in Constrained_Board; Row, Col : Positive; Color : Piece) return Boolean is
      Opposite : Piece := (if Color = Black then White else Black);
   begin
      -- Check row and column
      for J in 1..N loop
         if Board(Row, J) = Opposite or Board(J, Col) = Opposite then
            return False;
         end if;
      end loop;
      -- Check diagonals
      for I in 1..N loop
         for J in 1..N loop
            if (I + J = Row + Col or I - J = Row - Col) and Board(I, J) = Opposite then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Safe;

   procedure Solve(Board : in out Constrained_Board; Black, White : in out Natural; Pos : Positive) is
      Row : Positive := (Pos - 1) / N + 1;
      Col : Positive := (Pos - 1) mod N + 1;
   begin
      if Black = 0 and White = 0 then
         Solution_Count := Solution_Count + 1;
         Put_Line("Solution " & Natural'Image(Solution_Count) & ":");
         Display(Board, N);
         return;
      end if;
      if Pos > N * N then
         return;
      end if;
      -- Try placing black queen
      if Black > 0 and then Is_Safe(Board, Row, Col, Black) then
         Board(Row, Col) := Black;
         Solve(Board, Black - 1, White, Pos + 1);
         Board(Row, Col) := Empty;
      end if;
      -- Try placing white queen
      if White > 0 and then Is_Safe(Board, Row, Col, White) then
         Board(Row, Col) := White;
         Solve(Board, Black, White - 1, Pos + 1);
         Board(Row, Col) := Empty;
      end if;
      -- Skip this position
      Solve(Board, Black, White, Pos + 1);
   end Solve;

begin
   loop
      Put("Enter number of queens per army (m, 1-4): ");
      Get(M);
      Put("Enter board size (n, 1-10): ");
      Get(N);
      if Valid_Input(M, N) then
         exit;
      else
         Put_Line("Invalid input. Please try again.");
      end if;
   end loop;
   Solve(Board, M, M, 1);
   if Solution_Count = 0 then
      Put_Line("No solutions found.");
   else
      Put_Line("Found" & Natural'Image(Solution_Count) & " solutions.");
   end if;
end QueenArmies;
