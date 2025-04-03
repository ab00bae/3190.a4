package Chess_Types is
   type Cell_State is (Empty, Black, White);
   type Board_Matrix is array(Positive range <>, Positive range <>) of Cell_State;
   Max_Board_Size : constant := 10;
   subtype Constrained_Board is Board_Matrix(1..Max_Board_Size, 1..Max_Board_Size);
end Chess_Types;
