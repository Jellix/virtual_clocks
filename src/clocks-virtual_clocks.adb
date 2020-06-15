package body Clocks.Virtual_Clocks is

   ---------------------------------------------------------------------
   --  Virtual Clock, tagged protected type
   ---------------------------------------------------------------------
   protected body Virtual_Clock is

      ------------------------------------------------------------------
      --  Virtual_Clock.Activated (private)
      ------------------------------------------------------------------
      function Activated return Boolean is
      begin
         return Is_Active;
      end Activated;

      ------------------------------------------------------------------
      --  Virtual_Clock.Forward_Time
      ------------------------------------------------------------------
      procedure Forward_Time (Amount : in Duration) is
         use type Time;
      begin
         Current_Time := Current_Time + Amount;
      end Forward_Time;

      ------------------------------------------------------------------
      --  Virtual_Clock.Get_Time
      ------------------------------------------------------------------
      function Get_Time return Time is
      begin
         return Current_Time;
      end Get_Time;

      ------------------------------------------------------------------
      --  Virtual_Clock.Pulse
      ------------------------------------------------------------------
      procedure Pulse (Amount : in Duration) is
         use type Time;
      begin
         if Activated then
            Current_Time := Current_Time + (Amount * Speed);
         end if;
      end Pulse;

      ------------------------------------------------------------------
      --  Virtual_Clock.Set_Speed
      ------------------------------------------------------------------
      procedure Set_Speed (New_Speed : in Speedup) is
      begin
         Speed := New_Speed;
      end Set_Speed;

      ------------------------------------------------------------------
      --  Virtual_Clock.Set_Time
      ------------------------------------------------------------------
      procedure Set_Time (New_Time : in Time) is
      begin
         Current_Time := New_Time;
      end Set_Time;

      ----------------------------------------------------------------
      --  Virtual_Clock.Resume
      ----------------------------------------------------------------
      entry Resume when not Is_Active is
      begin
         Is_Active := True;
      end Resume;

      ------------------------------------------------------------------
      --  Virtual_Clock.Suspend
      ------------------------------------------------------------------
      entry Suspend when Is_Active is
      begin
         Is_Active := False;
      end Suspend;

   end Virtual_Clock;

   ---------------------------------------------------------------------
   --  Virtual_Clock.Create
   ---------------------------------------------------------------------
   overriding
   procedure Create (Clock      :    out Virtual_Clock;
                     Start_Time : in     Time    := START_DATE;
                     Speed      : in     Speedup := UNCHANGED_TIME) is
      use type Clock_Lists.Cursor;
   begin
      Clock.Set_Time  (Start_Time);
      Clock.Set_Speed (Speed);

      if Clock_Lists.Find (Clock_List, Clock'Unrestricted_Access) /=
        Clock_Lists.No_Element
      then
         raise Constraint_Error;
      end if;

      Clock_Lists.Append (Clock_List, Clock'Unrestricted_Access);
   end Create;

end Clocks.Virtual_Clocks;
