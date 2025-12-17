with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Containers;                    use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;

procedure Log_Analyzer is

   -- ------------- Map: String -> Natural -------------
   function Hash_String (Key : String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Key);
   end Hash_String;

   package Count_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Natural,
      Hash            => Hash_String,
      Equivalent_Keys => "=");

   procedure Inc (M : in out Count_Maps.Map; K : String) is
      Cur : Count_Maps.Cursor;
   begin
      Cur := M.Find (K);
      if Count_Maps.Has_Element (Cur) then
         M.Replace_Element (Cur, Count_Maps.Element (Cur) + 1);
      else
         M.Insert (K, 1);
      end if;
   end Inc;

   -- ------------- Top-N sorting helpers -------------
   type String_Access is access String;
   type Pair is record
      Key   : String_Access;
      Count : Natural;
   end record;

   package Pair_Vecs is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Pair);

   function Less (A, B : Pair) return Boolean is
   begin
      -- Sort by Count descending, then Key ascending
      if A.Count /= B.Count then
         return A.Count > B.Count;
      else
         return A.Key.all < B.Key.all;
      end if;
   end Less;

   procedure Sort (V : in out Pair_Vecs.Vector) is
      -- Simple insertion sort (fast enough for small logs)
      J : Integer;
      Key_Item : Pair;
   begin
      if V.Length <= 1 then
         return;
      end if;

      for I in 2 .. Integer (V.Length) loop
         Key_Item := V (Positive (I));
         J := I - 1;

         while J >= 1 and then Less (Key_Item, V (Positive (J))) loop
            V.Replace_Element (Positive (J + 1), V (Positive (J)));
            J := J - 1;
         end loop;

         V.Replace_Element (Positive (J + 1), Key_Item);
      end loop;
   end Sort;

   function To_Vector (M : Count_Maps.Map) return Pair_Vecs.Vector is
      V : Pair_Vecs.Vector;
      Cur : Count_Maps.Cursor := M.First;
   begin
      while Count_Maps.Has_Element (Cur) loop
         V.Append ((Key => new String'(Count_Maps.Key (Cur)), Count => Count_Maps.Element (Cur)));
         Cur := Count_Maps.Next (Cur);
      end loop;
      return V;
   end To_Vector;

   procedure Print_Top3 (Title : String; M : Count_Maps.Map) is
      V : Pair_Vecs.Vector := To_Vector (M);
      N : Natural;
   begin
      Put_Line (Title);
      if V.Length = 0 then
         Put_Line ("  (none)");
         New_Line;
         return;
      end if;

      Sort (V);

      if V.Length < 3 then
         N := Natural (V.Length);
      else
         N := 3;
      end if;

      for I in 1 .. N loop
         Put_Line ("  " & V (I).Key.all & ": " & Natural'Image (V (I).Count));
      end loop;
      New_Line;
   end Print_Top3;

   -- ------------- Parsing helpers -------------
   function Starts_With (S, Prefix : String) return Boolean is
   begin
      return S'Length >= Prefix'Length and then S (S'First .. S'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   procedure Next_Token (Line : String; Pos : in out Positive; Tok : out String_Access; Ok : out Boolean) is
      -- Extract next whitespace-separated token from Line starting at Pos.
      I : Integer := Pos;
      Start_Tok : Integer;
      Stop_Tok  : Integer;
   begin
      -- Skip spaces
      while I <= Line'Last and then Line (I) = ' ' loop
         I := I + 1;
      end loop;

      if I > Line'Last then
         Ok := False;
         Tok := null;
         return;
      end if;

      Start_Tok := I;

      while I <= Line'Last and then Line (I) /= ' ' loop
         I := I + 1;
      end loop;

      Stop_Tok := I - 1;

      Tok := new String'(Line (Start_Tok .. Stop_Tok));
      Ok  := True;

      if I <= Line'Last then
         Pos := Positive (I);
      else
         -- set Pos to last+1 is not Positive, so keep it at last to avoid constraint issues
         Pos := Positive (Line'Last);
      end if;
   end Next_Token;

   function Parse_Line
     (Line   : String;
      Level  : out String_Access;
      User   : out String_Access;
      Action : out String_Access) return Boolean
   is
      Pos : Positive := Line'First;
      T1, T2, T3, T4, T5 : String_Access;
      Ok : Boolean;

      function Field_Value (Token : String_Access; Prefix : String) return String is
      begin
         return Token.all (Token.all'First + Prefix'Length .. Token.all'Last);
      end Field_Value;

   begin
      -- Expect: date time level user=... action=...
      Next_Token (Line, Pos, T1, Ok); if not Ok then return False; end if;
      Next_Token (Line, Pos, T2, Ok); if not Ok then return False; end if;
      Next_Token (Line, Pos, T3, Ok); if not Ok then return False; end if;
      Next_Token (Line, Pos, T4, Ok); if not Ok then return False; end if;
      Next_Token (Line, Pos, T5, Ok); if not Ok then return False; end if;

      -- Very light validation (enough for this challenge)
      if T1.all'Length /= 10 then
         return False;
      end if;

      if T2.all'Length /= 8 then
         return False;
      end if;

      if not (T3.all = "INFO" or else T3.all = "WARN" or else T3.all = "ERROR") then
         return False;
      end if;

      if not Starts_With (T4.all, "user=") then
         return False;
      end if;

      if not Starts_With (T5.all, "action=") then
         return False;
      end if;

      Level  := T3;
      User   := new String'(Field_Value (T4, "user="));
      Action := new String'(Field_Value (T5, "action="));

      if User.all'Length = 0 or else Action.all'Length = 0 then
         return False;
      end if;

      return True;
   end Parse_Line;

   -- ------------- Main program state -------------
   File_Name : String_Access;
   F         : File_Type;

   Total_Lines     : Natural := 0;
   Malformed_Lines : Natural := 0;

   Info_Count  : Natural := 0;
   Warn_Count  : Natural := 0;
   Error_Count : Natural := 0;

   Users   : Count_Maps.Map;
   Actions : Count_Maps.Map;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: log_analyzer <path_to_log_file>");
      Set_Exit_Status (Failure);
      return;
   end if;

   File_Name := new String'(Argument (1));

   begin
      Open (F, In_File, File_Name.all);
   exception
      when others =>
         Put_Line ("Error: could not open file: " & File_Name.all);
         Set_Exit_Status (Failure);
         return;
   end;

   while not End_Of_File (F) loop
      declare
         Line : constant String := Get_Line (F);
         Level  : String_Access;
         User   : String_Access;
         Action : String_Access;
      begin
         Total_Lines := Total_Lines + 1;

         if Parse_Line (Line, Level, User, Action) then
            if Level.all = "INFO" then
               Info_Count := Info_Count + 1;
            elsif Level.all = "WARN" then
               Warn_Count := Warn_Count + 1;
            else
               Error_Count := Error_Count + 1;
            end if;

            Inc (Users, User.all);
            Inc (Actions, Action.all);
         else
            Malformed_Lines := Malformed_Lines + 1;
         end if;
      end;
   end loop;

   Close (F);

   Put_Line ("Lines read:" & Natural'Image (Total_Lines));
   Put_Line ("Malformed:"  & Natural'Image (Malformed_Lines));
   New_Line;

   Put_Line ("Levels:");
   Put_Line ("  INFO:"  & Natural'Image (Info_Count));
   Put_Line ("  WARN:"  & Natural'Image (Warn_Count));
   Put_Line ("  ERROR:" & Natural'Image (Error_Count));
   New_Line;

   Print_Top3 ("Top users:", Users);
   Print_Top3 ("Top actions:", Actions);

   Set_Exit_Status (Success);
end Log_Analyzer;
