------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by <ada.rocks@jlfencey.com>               --
--                                                                    --
--  This work is free. You can redistribute it and/or modify it under --
--  the terms of the Do What The Fuck You Want To Public License,     --
--  Version 2, as published by Sam Hocevar. See the LICENSE file for  --
--  more details.                                                     --
------------------------------------------------------------------------
pragma License (Unrestricted);

package Clocks.Virtual_Clocks is

   ---------------------------------------------------------------------
   --  Virtual_Clock
   ---------------------------------------------------------------------
   --  Protected type implementing the Clock_Interface.
   ---------------------------------------------------------------------
   protected type Virtual_Clock is new Clock_Interface with

      ------------------------------------------------------------------
      --  Forward_Time
      ------------------------------------------------------------------
      overriding
      procedure Forward_Time (Amount : in Duration);

      ------------------------------------------------------------------
      --  Get_Time
      ------------------------------------------------------------------
      overriding
      function Get_Time return Time;

      ------------------------------------------------------------------
      --  Pulse
      ------------------------------------------------------------------
      overriding
      procedure Pulse (Amount : in Duration);

      ------------------------------------------------------------------
      --  Set_Speed
      ------------------------------------------------------------------
      overriding
      procedure Set_Speed (New_Speed : in Speedup);

      ------------------------------------------------------------------
      --  Set_Time
      ------------------------------------------------------------------
      overriding
      procedure Set_Time (New_Time : in Time);

      ------------------------------------------------------------------
      --  Resume
      ------------------------------------------------------------------
      overriding
      entry Resume;

      ------------------------------------------------------------------
      --  Suspend
      ------------------------------------------------------------------
      overriding
      entry Suspend;

   private

      ------------------------------------------------------------------
      --  Activated
      ------------------------------------------------------------------
      --  Returns the current activation state of the clock.
      ------------------------------------------------------------------
      not overriding
      function Activated return Boolean;

      Current_Time : Time    := START_DATE;
      Speed        : Speedup := UNCHANGED_TIME;
      Is_Active    : Boolean := True;

   end Virtual_Clock;

   ---------------------------------------------------------------------
   --  Virtual_Clock.Create
   ---------------------------------------------------------------------
   overriding
   procedure Create (Clock      :    out Virtual_Clock;
                     Start_Time : in     Time    := START_DATE;
                     Speed      : in     Speedup := UNCHANGED_TIME);

end Clocks.Virtual_Clocks;
