with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Calculate_Stats is
   -- Define the array with your specific numbers
   Numbers : array (1 .. 6) of Integer := (3, 7, 2, 9, 5, 4);
   
   Sum     : Integer := 0;
   Min     : Integer := Numbers(1);
   Max     : Integer := Numbers(1);
   Average : Float;
begin
   -- Loop through the array to calculate statistics
   for I in Numbers'Range loop
      Sum := Sum + Numbers(I);
      
      if Numbers(I) < Min then
         Min := Numbers(I);
      end if;
      
      if Numbers(I) > Max then
         Max := Numbers(I);
      end if;
   end loop;

   -- Calculate average (converting Sum to Float for precision)
   Average := Float(Sum) / Float(Numbers'Length);

   -- Output the results
   Put_Line("Statistics for: 3, 7, 2, 9, 5, 4");
   Put_Line("-------------------------------");
   Put_Line("Sum:     " & Integer'Image(Sum));
   Put_Line("Minimum: " & Integer'Image(Min));
   Put_Line("Maximum: " & Integer'Image(Max));
   Put("Average: ");
   Put(Average, Fore => 1, Aft => 2, Exp => 0);
   New_Line;
end Calculate_Stats;