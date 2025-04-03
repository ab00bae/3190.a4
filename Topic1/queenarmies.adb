with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Chess_Types; use Chess_Types;
with Showboard; use Showboard;

procedure QueenArmies is
   N, M : Positive := 1;
   Board : Constrained_Board := (others => (others => Empty));

   function Valid_Input(M, N : Positive) return Boolean is
      Max_M : constant array(1..10) of Positive := 
        (1, 1, 2, 4, 5, 7, 9, 12, 14, 17);
   begin
      return N in 1..10 and then M <= Max_M(N);
   end Valid_Input;

   procedure Place_Queens is
   begin
      -- Clear previous placements
      Board := (others => (others => Empty));
      
      -- Place black queens in column 1
      for Row in 1..M loop
         Board(Row, 1) := Black;
      end loop;
      
      -- Place white queens in column 2
      for Row in (N - M + 1)..N loop
         Board(Row, 2) := White;
      end loop;
   end Place_Queens;

begin
   loop
      Put("Enter number of queens per army (m): ");
      Ada.Integer_Text_IO.Get(M);
      Put("Enter board size (n, 1-10): ");
      Ada.Integer_Text_IO.Get(N);
      exit when Valid_Input(M, N);
      Put_Line("Invalid input. Please try again.");
   end loop;
   
   Place_Queens;
   Display(Board, N);
end QueenArmies;
