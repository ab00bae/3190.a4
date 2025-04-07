package ShowBoard is
   type Board is array (Natural range <>, Natural range <>) of Character;
   procedure Display(Black, White : Board);
end ShowBoard;
