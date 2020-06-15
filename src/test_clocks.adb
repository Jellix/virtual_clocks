with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Text_IO;

with Clocks;
with Clocks.Virtual_Clocks;

package body Test_Clocks is

   type Clock_Array is array (Positive range <>) of Clocks.Class_Ref;

   ---------------------------------------------------------------------
   --  Dump_Clocks
   ---------------------------------------------------------------------
   procedure Dump_Clocks (Clock : in Clock_Array);
   procedure Dump_Clocks (Clock : in Clock_Array) is

      package TZ renames Ada.Calendar.Time_Zones;

      ------------------------------------------------------------------
      --  Dump_Clocks.Image
      ------------------------------------------------------------------
      function Image
        (Date                  : Clocks.Time;
         Include_Time_Fraction : Boolean        := True;
         Time_Zone             : TZ.Time_Offset := TZ.UTC_Time_Offset)
         return String
         renames Ada.Calendar.Formatting.Image;

   begin --  Dump_Clocks
      for C in Clock'Range loop
         Ada.Text_IO.Put (Image (Clocks.Get_Time (Clock (C).all)) & "  ");
      end loop;

      Ada.Text_IO.New_Line;
   end Dump_Clocks;

   ---------------------------------------------------------------------
   --  Run
   ---------------------------------------------------------------------
   procedure Run is

      use type Clocks.Speedup;
      use type Clocks.Time;

      subtype V_Clock is Clocks.Virtual_Clocks.Virtual_Clock;

      X : array (1 .. 5) of aliased V_Clock;

      Clock     : constant Clock_Array (1 .. 5) :=
                    (1 => X (1)'Unchecked_Access,
                     2 => X (2)'Unchecked_Access,
                     3 => X (3)'Unchecked_Access,
                     4 => X (4)'Unchecked_Access,
                     5 => X (5)'Unchecked_Access);
      Next_Tick : Clocks.Time := Ada.Calendar.Clock;

      ------------------------------------------------------------------
      --  Run.Next_Time
      ------------------------------------------------------------------
      function Next_Time return Clocks.Time;
      function Next_Time return Clocks.Time is
      begin
         Next_Tick := Next_Tick + 0.5;

         return Next_Tick;
      end Next_Time;

   begin --  Run
      declare
         subtype CS is Clocks.Speedup;
      begin
         Ada.Text_IO.Put_Line
           ("Clock multiplicator ranges from"              &
            CS'Image (CS'First)                            &
            " to"                                          &
            CS'Image (CS'Last)                             &
            " with a resolution of"                        &
            Integer'Image (Integer (CS'Small * 1_000_000)) &
            " ppm, required bit size is"                   &
            Integer'Image (CS'Size)                        &
            ".");
      end;

      for C in Clock'Range loop
         Clocks.Create (Clock      => Clock (C).all,
                        Start_Time => Clocks.START_DATE +
                                      86400.0 * Integer'Pred (C),
                        Speed      => Clocks.UNCHANGED_TIME);
      end loop;

      for I in 1 .. 3 loop
         Dump_Clocks (Clock);

         delay until Next_Time;
      end loop;

      for C in Clock'Range loop
         Clocks.Set_Speed (Clock (C).all, C * 10.0);
      end loop;

      for I in 1 .. 3 loop
         Dump_Clocks (Clock);

         delay until Next_Time;
      end loop;

      for C in Clock'Range loop
         Clocks.Set_Speed (Clock (C).all, C * Clocks.MINUTES_PER_SECOND);
      end loop;

      for I in 1 .. 3 loop
         Dump_Clocks (Clock);

         delay until Next_Time;
      end loop;

      for C in Clock'Range loop
         Clocks.Set_Speed (Clock (C).all, 1.0 * Clocks.HOURS_PER_SECOND);
      end loop;

      for I in 1 .. 3 loop
         Dump_Clocks (Clock);

         delay until Next_Time;
      end loop;

      Clocks.Terminate_All;

   end Run;

end Test_Clocks;
