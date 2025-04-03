with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Chess_Types; use Chess_Types;
with Showboard; use Showboard;

procedure QueenArmies is
   N, M : Positive;
   Board : Constrained_Board := (others => (others => Empty));
   Solution_Count : Natural := 0;

   -- Validate inputs against OEIS A250000 maximums and assignment constraints
   function Valid_Input(M, N : Positive) return Boolean is
      Max_M : constant array(1..10) of Natural := (0, 0, 1, 2, 4, 5, 7, 9, 12, 14);
   begin
      return N in 1..10 and then M in 1..4 and then M <= Max_M(N);
   end Valid_Input;

   -- Check if placing a queen of given color at (Row, Col) is safe
   function Is_Safe(Board : Constrained_Board; Row, Col : Positive; Color : Cell_State) return Boolean is
      Opposite : Cell_State := (if Color = Black then White else Black);
   begin
      -- Check row
      for J in 1..Col - 1 loop
         if Board(Row, J) = Opposite then
            return False;
         end if;
      end loop;
      -- Check column
      for I in 1..Row - 1 loop
         if Board(I, Col) = Opposite then
            return False;
         end if;
      end loop;
      -- Check diagonals
      for I in 1..Row - 1 loop
         for J in 1..Col - 1 loop
            if abs(I - Row) = abs(J - Col) and then Board(I, J) = Opposite then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Safe;

   -- Recursive procedure to place queens and find solutions
   procedure Solve(Board : in out Constrained_Board; Black, White : Natural; Pos : Positive) is
      Row : Positive := (Pos - 1) / N + 1;
      Col : Positive := (Pos - 1) mod N + 1;
   begin
      if Black = 0 and White = 0 then
         Display(Board, N);
         Solution_Count := Solution_Count + 1;
         return;
      end if;
      if Solution_Count >= 2 or Pos > N * N then
         return;
      end if;

      -- Try placing a black queen
      if Black > 0 and then Is_Safe(Board, Row, Col, Chess_Types.Black) then
         Board(Row, Col) := Chess_Types.Black;
         Solve(Board, Black - 1, White, Pos + 1);
         Board(Row, Col) := Chess_Types.Empty;
      end if;

      -- Try placing a white queen
      if White > 0 and then Is_Safe(Board, Row, Col, Chess_Types.White) then
         Board(Row, Col) := Chess_Types.White;
         Solve(Board, Black, White - 1, Pos + 1);
         Board(Row, Col) := Chess_Types.Empty;
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
      end if;
      Put_Line("Invalid input. Please try again.");
   end loop;

   Solve(Board, M, M, 1);
   if Solution_Count = 0 then
      Put_Line("No solutions found.");
   else
      Put_Line("Found" & Natural'Image(Solution_Count) & " solutions.");
   end if;
end QueenArmies;
