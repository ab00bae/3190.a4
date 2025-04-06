package ShowBoard is
   Max_N : constant Integer := 10;
   type Board_Type is array (1 .. Max_N, 1 .. Max_N) of Integer;
   procedure Show_Board(Board : in Board_Type; N : in Integer);
end ShowBoard;
