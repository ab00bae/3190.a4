--Ebube Esor 1253613
--this package defines the types and procedures needed to visualize
--the chessboard. It includes the board type and the
--it stores the procedure to display the board.
package ShowBoard is
   Max_N : constant Integer := 10;
   type Board_Type is array (1 .. Max_N, 1 .. Max_N) of Integer;
   procedure Show_Board(Board : in Board_Type; N : in Integer);
end ShowBoard;
