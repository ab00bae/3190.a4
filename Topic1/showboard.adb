with Ada.Text_IO; use Ada.Text_IO;

package body ShowBoard is
   procedure Show_Board(Board : in Board_Type; N : in Integer) is
      Black_Queen : constant String := "♛";
      White_Queen : constant String := "♕";
      Empty : constant String := " ";
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
   begin
      -- Top border
      Put(Top_Left);
      for I in 1 .. N - 1 loop
         Put(Horiz & Top_Middle);
      end loop;
      Put_Line(Horiz & Top_Right);

      -- Rows
      for R in 1 .. N loop
         Put(Vert);
         for C in 1 .. N loop
            if Board(R, C) = 1 then
               Put(" " & Black_Queen & " ");
            elsif Board(R, C) = 2 then
               Put(" " & White_Queen & " ");
            else
               Put(" " & Empty & " ");
            end if;
            if C < N then Put(Vert); end if;
         end loop;
         Put_Line(Vert);

         -- Middle or bottom border
         if R < N then
            Put(Mid_Left);
            for I in 1 .. N - 1 loop
               Put(Horiz & Mid_Middle);
            end loop;
            Put_Line(Horiz & Mid_Right);
         end if;
      end loop;

      -- Bottom border
      Put(Bot_Left);
      for I in 1 .. N - 1 loop
         Put(Horiz & Bot_Middle);
      end loop;
      Put_Line(Horiz & Bot_Right);
   end Show_Board;
end ShowBoard;
