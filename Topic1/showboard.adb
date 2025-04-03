with Ada.Text_IO; use Ada.Text_IO;
with Chess_Types; use Chess_Types;

package body Showboard is
   procedure Display(Board : in Constrained_Board; N : Positive) is
      procedure Print_Line is
      begin
         Put("+");
         for Col in 1..N loop
            Put("---+");
         end loop;
         New_Line;
      end Print_Line;
   begin
      Print_Line;
      for Row in 1..N loop
         Put("|");
         for Col in 1..N loop
            case Board(Row, Col) is
               when Black => Put(" B |");
               when White => Put(" W |");
               when Empty =>
                  if (Row + Col) mod 2 = 0 then
                     Put(" x |");
                  else
                     Put(" o |");
                  end if;
            end case;
         end loop;
         New_Line;
         Print_Line;
      end loop;
   end Display;
end Showboard;
