with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ShowBoard;

procedure QueenArmies is
   type Board_Type is array (Natural range <>, Natural range <>) of Boolean;
   type Positions is array (Natural range <>) of Natural;

   Max_N : constant := 10;
   subtype Valid_N is Natural range 1..Max_N;

   -- Precomputed maximum m values for n from 1 to 10 (OEIS A250000)
   Max_M_For_N : constant array (Valid_N) of Natural :=
     (1 => 0, 2 => 0, 3 => 1, 4 => 2, 5 => 4, 6 => 5, 7 => 7, 8 => 9, 9 => 12, 10 => 14);

   -- Function to check if m and n are valid
   function Is_Valid(m, n : Natural) return Boolean is
   begin
      if n not in Valid_N then
         return False;
      elsif m > Max_M_For_N(n) or m < 1 then
         return False;
      else
         return True;
      end if;
   end Is_Valid;

   -- Backtracking to find black queen positions
   procedure Find_Solutions(m, n : Natural; Solutions : out Integer) is
      type Cell is record
         Row, Col : Natural;
      end record;
      type Cell_Array is array (Natural range <>) of Cell;

      Attacked : Board_Type(0..n-1, 0..n-1) := (others => (others => False));
      Black_Queens : Cell_Array(1..m);
      White_Queens : Cell_Array(1..m);
      Found : Natural := 0;

      procedure Mark_Attacked(Row, Col : Natural; Attacked : in out Board_Type) is
      begin
         -- Mark row and column
         for I in 0..n-1 loop
            Attacked(Row, I) := True;
            Attacked(I, Col) := True;
         end loop;
         -- Mark diagonals
         declare
            r, c : Integer := Row;
         begin
            while r >= 0 and c >= 0 loop
               Attacked(r, c) := True;
               r := r - 1;
               c := c - 1;
            end loop;
            r := Row + 1;
            c := Col + 1;
            while r < n and c < n loop
               Attacked(r, c) := True;
               r := r + 1;
               c := c + 1;
            end loop;
            r := Row - 1;
            c := Col + 1;
            while r >= 0 and c < n loop
               Attacked(r, c) := True;
               r := r - 1;
               c := c + 1;
            end loop;
            r := Row + 1;
            c := Col - 1;
            while r < n and c >= 0 loop
               Attacked(r, c) := True;
               r := r + 1;
               c := c - 1;
            end loop;
         end;
      end Mark_Attacked;

      -- Backtracking procedure
      procedure Place_Black_Queens(Index : Natural; Start_Row, Start_Col : Natural) is
      begin
         if Index > m then
            -- Find white queens
            declare
               Available : Natural := 0;
            begin
               for I in 0..n-1 loop
                  for J in 0..n-1 loop
                     if not Attacked(I, J) then
                        Available := Available + 1;
                     end if;
                  end loop;
               end loop;
               if Available >= m then
                  -- Found a solution
                  Found := Found + 1;
                  -- TODO: Store the solution
                  if Found = 2 then
                     return;
                  end if;
               end if;
            end;
            return;
         end if;

         for I in Start_Row..n-1 loop
            for J in 0..n-1 loop
               if not Attacked(I, J) then
                  declare
                     Backup : Board_Type := Attacked;
                  begin
                     Mark_Attacked(I, J, Attacked);
                     Black_Queens(Index) := (I, J);
                     Place_Black_Queens(Index + 1, I, J);
                     exit when Found = 2;
                     Attacked := Backup;
                  end;
               end if;
            end loop;
         end loop;
      end Place_Black_Queens;

   begin
      Place_Black_Queens(1, 0, 0);
      Solutions := Found;
   end Find_Solutions;

   m, n : Natural;
begin
   Put_Line("Enter m and n:");
   Get(m);
   Get(n);
   if not Is_Valid(m, n) then
      Put_Line("Invalid m and n.");
      return;
   end if;

   declare
      Solutions : Integer;
   begin
      Find_Solutions(m, n, Solutions);
      if Solutions < 2 then
         Put_Line("Could not find two solutions.");
      else
         -- TODO: Call ShowBoard for each solution
         Put_Line("Solution 1:");
         ShowBoard.Display((others => (others => 'B')), (others => (others => 'W')));
         Put_Line("Solution 2:");
         ShowBoard.Display((others => (others => 'B')), (others => (others => 'W')));
      end if;
   end;
end QueenArmies;
