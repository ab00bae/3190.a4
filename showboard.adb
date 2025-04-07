package body ShowBoard is
   procedure Display(Black, White : Board) is
      Horizontal : constant String := "+---";
      Line : constant String := "+";
   begin
      for Row in Black'Range(1) loop
         Put(Line);
         for Col in Black'Range(2) loop
            Put(Horizontal);
         end loop;
         Put_Line("+");
         Put("|");
         for Col in Black'Range(2) loop
            if Black(Row, Col) = 'B' then
               Put(" B |");
            elsif White(Row, Col) = 'W' then
               Put(" W |");
            else
               Put("   |");
            end if;
         end loop;
         New_Line;
      end loop;
      Put(Line);
      for Col in Black'Range(2) loop
         Put(Horizontal);
      end loop;
      Put_Line("+");
   end Display;
end ShowBoard;
