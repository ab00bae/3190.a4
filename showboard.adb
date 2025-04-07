--Ebube Esor 1253613
--This is body of the visualization package of the chessboard
--containing the logic to display the board with queens placed
--using Unicode characters for an intuitive representation
with Ada.Text_IO; use Ada.Text_IO;

package body ShowBoard is
   procedure Show_Board(Board : in Board_Type; N : in Integer) is
      Cell_Width : constant Integer := 3;  -- Each cell is 3 characters wide
      Black_Queen : constant String := "♛";
      White_Queen : constant String := "♕";
      Horiz : constant String := "─";
      Vert : constant String := "│";
      Top_Left : constant String := "┌";
      Top_Middle : constant String := "┬";
      Top_Right : constant String := "┐";
      Mid_Left : constant String := "├";
      Mid_Middle : constant String := "┼";
      Mid_Right : constant String := "┤";
      Bot_Left : constant String := "└";
      Bot_Middle : constant String := "┴";
      Bot_Right : constant String := "┘";

      -- Helper procedure to print horizontal border lines
      procedure Put_Horiz_Line(Left, Middle, Right : String) is
      begin
         Put(Left);
         for I in 1 .. N - 1 loop
            for J in 1 .. Cell_Width loop
               Put(Horiz);
            end loop;
            Put(Middle);
         end loop;
         for J in 1 .. Cell_Width loop
            Put(Horiz);
         end loop;
         Put_Line(Right);
      end Put_Horiz_Line;

   begin
      -- Print top border
      Put_Horiz_Line(Top_Left, Top_Middle, Top_Right);

      -- Print rows
      for R in 1 .. N loop
         Put(Vert);
         for C in 1 .. N loop
            if Board(R, C) = 1 then
               Put(" " & Black_Queen & " ");  -- 3 characters: space, queen, space
            elsif Board(R, C) = 2 then
               Put(" " & White_Queen & " ");  -- 3 characters: space, queen, space
            else
               Put("   ");  -- 3 spaces for empty cell
            end if;
            Put(Vert);  -- Vertical separator after each cell
         end loop;
         Put_Line("");

         -- Print middle border (if not the last row)
         if R < N then
            Put_Horiz_Line(Mid_Left, Mid_Middle, Mid_Right);
         end if;
      end loop;

      -- Print bottom border
      Put_Horiz_Line(Bot_Left, Bot_Middle, Bot_Right);
   end Show_Board;
end ShowBoard;
