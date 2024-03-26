with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Semaphores; use GNAT.Semaphores;

procedure Philosophersada is
   Count : constant Integer := 5;
   Room_Size : constant Integer := 4;
   type Fork is record
      Id : Integer;
      Lock : Counting_Semaphore (1, Default_Ceiling);
   end record;
   type Fork_Array is array (1 .. Count) of Fork;
   Forks : Fork_Array;
   Room_Semaphore : Counting_Semaphore (Room_Size, Default_Ceiling);

   ------------------------------------------------------------------------------------------

   task type PhilosopherLastChangeHand is
      entry Identify (Id : Integer; Left, Right : Integer);
   end PhilosopherLastChangeHand;

   task body PhilosopherLastChangeHand is
      My_Id, My_Left, My_Right : Integer;
   begin
      accept Identify (Id : Integer; Left, Right : Integer) do
         My_Id := Id;
         My_Left := Left;
         My_Right := Right;
      end Identify;

      for I in 1 .. Count loop
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " is trying to pick up forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
         if  My_Left < My_Right then
            Forks (My_Left).Lock.Seize;
            Forks (My_Right).Lock.Seize;
         else
            Forks (My_Right).Lock.Seize;
            Forks (My_Left).Lock.Seize;
         end if;
         Put_Line ("Philosopher " & Integer'Image (My_Id) & " is eating");
         Forks (My_Left).Lock.Release;
         Forks (My_Right).Lock.Release;
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " puts down forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
      end loop;
   end PhilosopherLastChangeHand;

   ------------------------------------------------------------------------------------------

    task type PhilosopherAsymmetric is
      entry Identify (Id : Integer; Left, Right : Integer);
   end PhilosopherAsymmetric;

   task body PhilosopherAsymmetric is
      My_Id, My_Left, My_Right : Integer;
   begin
      accept Identify (Id : Integer; Left, Right : Integer) do
         My_Id := Id;
         My_Left := Left;
         My_Right := Right;
      end Identify;

      for I in 1 .. Count loop
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " is trying to pick up forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
         if My_Id mod 2 = 0 then
            Forks (My_Left).Lock.Seize;
            Forks (My_Right).Lock.Seize;
         else
            Forks (My_Right).Lock.Seize;
            Forks (My_Left).Lock.Seize;
         end if;
         Put_Line ("Philosopher " & Integer'Image (My_Id) & " is eating");
         Forks (My_Left).Lock.Release;
         Forks (My_Right).Lock.Release;
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " puts down forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
      end loop;
   end PhilosopherAsymmetric;

   ------------------------------------------------------------------------------------------

   task type PhilosopherRoom is
      entry Identify (Id : Integer; Left, Right : Integer);
   end PhilosopherRoom;

   task body PhilosopherRoom is
      My_Id, My_Left, My_Right : Integer;
   begin
      accept Identify (Id : Integer; Left, Right : Integer) do
         My_Id := Id;
         My_Left := Left;
         My_Right := Right;
      end Identify;

      for I in 1 .. Count loop
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " is trying to pick up forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
         Room_Semaphore.Seize;
         Forks (My_Left).Lock.Seize;
         Forks (My_Right).Lock.Seize;
         Put_Line ("Philosopher " & Integer'Image (My_Id) & " is eating");
         Forks (My_Left).Lock.Release;
         Forks (My_Right).Lock.Release;
         Room_Semaphore.Release;
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " puts down forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
      end loop;
   end PhilosopherRoom;

   ------------------------------------------------------------------------------------------

  protected Token is
      procedure Release_Token;
      function Get_Token return Integer;
      procedure Set_Token (Value : Integer);
   private
      Current_Token : Integer := 1;
   end Token;

   protected body Token is
      procedure Release_Token is
      begin
         Current_Token := (Current_Token + 1) mod Count;
      end Release_Token;
       function Get_Token return Integer is
      begin
         return Current_Token;
      end Get_Token;

      procedure Set_Token (Value : Integer) is
      begin
         Current_Token := Value;
      end Set_Token;
   end Token;

   task type PhilosopherToken is
      entry Identify (Id : Integer; Left, Right : Integer);
   end PhilosopherToken;

   task body PhilosopherToken is
      My_Id, My_Left, My_Right,Has_Eaten : Integer;
   begin
      accept Identify (Id : Integer; Left, Right : Integer) do
         My_Id := Id;
         My_Left := Left;
         My_Right := Right;
         Has_Eaten := 0;
      end Identify;

      loop
         if Token.Get_Token = My_Id then
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " is trying to pick up forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
         Forks (My_Left).Lock.Seize;
         Forks (My_Right).Lock.Seize;
         Put_Line ("Philosopher " & Integer'Image (My_Id) & " is eating");
         Has_Eaten := Has_Eaten+1;
         Token.Set_Token((My_Id+2) mod Count+1);
         Forks (My_Left).Lock.Release;
         Forks (My_Right).Lock.Release;
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " puts down forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
            end if;
         exit when Has_Eaten = Count;
         end loop;
   end PhilosopherToken;

   ------------------------------------------------------------------------------------------

    protected type ForkSemaphore (Start_Count : Integer := 1) is

      entry Seize;
      procedure Release;
      function Count return Integer;
   private
      Current_Count : Integer := Start_Count;
   end ForkSemaphore;

   protected body ForkSemaphore is

      entry Seize when Current_Count > 0 is
      begin
         Current_Count := Current_Count - 1;
      end Seize;

      procedure Release is
      begin
         Current_Count := Current_Count + 1;
      end Release;

      function Count return Integer is
      begin
         return Current_Count;
      end Count;

   end ForkSemaphore;

   ForksCanTake : array (1 .. Count) of ForkSemaphore (1);

   task type PhilosopherSeenWant is
      entry Identify (Id : Integer; Left, Right : Integer);
   end PhilosopherSeenWant;

   task body PhilosopherSeenWant is
      My_Id, My_Left, My_Right : Integer;
   begin
      accept Identify (Id : Integer; Left, Right : Integer) do
         My_Id := Id;
         My_Left := Left;
         My_Right := Right;
      end Identify;
      for I in 1 .. Count loop
         Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " is trying to pick up forks " &
                     Integer'Image (My_Left) & " and " &
                     Integer'Image (My_Right));
         if ForksCanTake (My_Left).Count > 0 then
            ForksCanTake (My_Left).Seize;
            if ForksCanTake (My_Right).Count > 0 then
               ForksCanTake (My_Right).Seize;
               Put_Line ("Philosopher " & Integer'Image (My_Id) & " is eating");
               ForksCanTake (My_Left).Release;
               ForksCanTake (My_Right).Release;
               Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " puts down forks " &
               Integer'Image (My_Left) & " and " &
               Integer'Image (My_Right));
            else
               ForksCanTake (My_Left).Release;
               Put_Line ("Philosopher " & Integer'Image (My_Id) &
                     " puts down left fork " & Integer'Image (My_Left));
            end if;
         end if;
      end loop;
   end PhilosopherSeenWant;

  ------------------------------------------------------------------------------------------

  -- Philosophers : array (1 .. Count) of PhilosopherLastChangeHand;
  -- Philosophers : array (1 .. Count) of PhilosopherAsymmetric;
  -- Philosophers : array (1 .. Count) of PhilosopherRoom;
  -- Philosophers : array (1 .. Count) of PhilosopherToken;
   Philosophers : array (1 .. Count) of PhilosopherSeenWant;


begin
   for I in Forks'Range loop
      Forks (I).Id := I;
   end loop;

   for I in Philosophers'Range loop
      Philosophers (I).Identify (I, I, (I mod Count) + 1);
   end loop;
end Philosophersada;
