-- Ebube Esor 1253613
-- Assignment 4 - Topic 2: Chess Armies
-- Places m black and m white queens on an n x n board such that no queen
-- attacks another of a different color. Uses the showboard package.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ShowBoard; use ShowBoard;

procedure QueenArmies is
   Max_Diag : constant Integer := 2 * Max_N - 1;

   type Row_Array is array (1 .. Max_N) of Integer;
   type Diag_Array is array (1 .. Max_Diag) of Integer;

   N : Integer;  -- Board size
   M : Integer;  -- Number of queens per color
   Board : Board_Type := (others => (others => 0));
   Num_Black_Row, Num_Black_Col : Row_Array := (others => 0);
   Num_White_Row, Num_White_Col : Row_Array := (others => 0);
   Num_Black_Fwd_Diag, Num_Black_Bwd_Diag : Diag_Array := (others => 0);
   Num_White_Fwd_Diag, Num_White_Bwd_Diag : Diag_Array := (others => 0);
   Solutions_Found : Integer := 0;
   Solution1, Solution2 : Board_Type;

   -- Function to compare two boards up to size N
   function Boards_Equal(B1, B2 : Board_Type) return Boolean is
   begin
      for R in 1 .. N loop
         for C in 1 .. N loop
            if B1(R, C) /= B2(R, C) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Boards_Equal;

   procedure Solve(K : Integer) is
      R, C : Integer;
      Color : Integer;  -- 1 for black, 2 for white
   begin
      if K = 2 * M then
         -- Solution found
         if Solutions_Found = 0 then
            Solutions_Found := 1;
            Solution1 := Board;
         elsif Solutions_Found = 1 and then not Boards_Equal(Board, Solution1) then
            Solutions_Found := 2;
            Solution2 := Board;
         end if;
         return;
      end if;

      Color := (if K mod 2 = 0 then 1 else 2);  -- Black if even, white if odd
      for R in 1 .. N loop
         for C in 1 .. N loop
            if Board(R, C) = 0 then  -- Square is empty
               if Color = 1 then  -- Black queen
                  if Num_White_Row(R) = 0 and then Num_White_Col(C) = 0 and then
                     Num_White_Fwd_Diag(R - C + N) = 0 and then
                     Num_White_Bwd_Diag(R + C - 1) = 0 then
                     -- Place black queen
                     Board(R, C) := 1;
                     Num_Black_Row(R) := Num_Black_Row(R) + 1;
                     Num_Black_Col(C) := Num_Black_Col(C) + 1;
                     Num_Black_Fwd_Diag(R - C + N) := Num_Black_Fwd_Diag(R - C + N) + 1;
                     Num_Black_Bwd_Diag(R + C - 1) := Num_Black_Bwd_Diag(R + C - 1) + 1;
                     Solve(K + 1);
                     -- Backtrack
                     Board(R, C) := 0;
                     Num_Black_Row(R) := Num_Black_Row(R) - 1;
                     Num_Black_Col(C) := Num_Black_Col(C) - 1;
                     Num_Black_Fwd_Diag(R - C + N) := Num_Black_Fwd_Diag(R - C + N) - 1;
                     Num_Black_Bwd_Diag(R + C - 1) := Num_Black_Bwd_Diag(R + C - 1) - 1;
                  end if;
               else  -- White queen
                  if Num_Black_Row(R) = 0 and then Num_Black_Col(C) = 0 and then
                     Num_Black_Fwd_Diag(R - C + N) = 0 and then
                     Num_Black_Bwd_Diag(R + C - 1) = 0 then
                     -- Place white queen
                     Board(R, C) := 2;
                     Num_White_Row(R) := Num_White_Row(R) + 1;
                     Num_White_Col(C) := Num_White_Col(C) + 1;
                     Num_White_Fwd_Diag(R - C + N) := Num_White_Fwd_Diag(R - C + N) + 1;
                     Num_White_Bwd_Diag(R + C - 1) := Num_White_Bwd_Diag(R + C - 1) + 1;
                     Solve(K + 1);
                     -- Backtrack
                     Board(R, C) := 0;
                     Num_White_Row(R) := Num_White_Row(R) - 1;
                     Num_White_Col(C) := Num_White_Col(C) - 1;
                     Num_White_Fwd_Diag(R - C + N) := Num_White_Fwd_Diag(R - C + N) - 1;
                     Num_White_Bwd_Diag(R + C - 1) := Num_White_Bwd_Diag(R + C - 1) - 1;
                  end if;
               end if;
            end if;
         end loop;
      end loop;
   end Solve;

begin
   Put_Line("Enter board size n (1-10): ");
   Get(N);
   if N < 1 or N > 10 then
      Put_Line("Invalid n. Must be between 1 and 10.");
      return;
   end if;

   Put_Line("Enter number of queens m (1-4): ");
   Get(M);
   if M < 1 or M > 4 then
      Put_Line("Invalid m. Must be between 1 and 4.");
      return;
   end if;

   Solve(0);  -- Start placing queens

   if Solutions_Found >= 1 then
      Put_Line("Solution 1:");
      Show_Board(Solution1, N);
   end if;
   if Solutions_Found = 2 then
      Put_Line("Solution 2:");
      Show_Board(Solution2, N);
   elsif Solutions_Found < 2 then
      Put_Line("Only " & Integer'Image(Solutions_Found) & " solution(s) found.");
   end if;
end QueenArmies;
