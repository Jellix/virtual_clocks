------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by <ada.rocks@jlfencey.com>               --
--                                                                    --
--  This work is free. You can redistribute it and/or modify it under --
--  the terms of the Do What The Fuck You Want To Public License,     --
--  Version 2, as published by Sam Hocevar. See the LICENSE file for  --
--  more details.                                                     --
------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Exceptions;
with Ada.Text_IO;

package body Clocks is

   ---------------------------------------------------------------------
   --  Clock_Task (spec)
   ---------------------------------------------------------------------
   task Clock_Task is

      ------------------------------------------------------------------
      --  Clock_Task.Start
      ------------------------------------------------------------------
      --  Starts the task after initialization. Called once only!
      ------------------------------------------------------------------
      entry Start;

      ------------------------------------------------------------------
      --  Clock_Task.Stop
      ------------------------------------------------------------------
      --  Stops the world clock task.
      --  Should only be called on termination of main program.
      ------------------------------------------------------------------
      entry Stop;

   end Clock_Task;

   ---------------------------------------------------------------------
   --  Clock_Task (body)
   ---------------------------------------------------------------------
   task body Clock_Task is

      INTERVAL   : constant Duration := 1.0 / 8.0; -- Update rate 8 Hz.
      Next_Cycle : Time;

      use type Time;

      ------------------------------------------------------------------
      --  Tick
      ------------------------------------------------------------------
      --  Local function called by list iterator to trigger all clocks.
      ------------------------------------------------------------------
      procedure Tick (Position : in Clock_Lists.Cursor);
      procedure Tick (Position : in Clock_Lists.Cursor) is
         C : constant Class_Ref := Clock_Lists.Element (Position);
      begin
         Clocks.Pulse (C.all, INTERVAL);
      end Tick;

   begin --  Clock_Task

      ------------------------------------------------------------------
      --  Initial synchronisation point.
      ------------------------------------------------------------------
      accept Start;
      ------------------------------------------------------------------

      Next_Cycle := Ada.Calendar.Clock + INTERVAL;

      loop
         select
            ------------------------------------------------------------
            --  "Normal" operation.
            ------------------------------------------------------------
            --  Wait until the time interval for the task's update rate
            --  has gone by, and then update the world clock unless it
            --  is suspended.
            ------------------------------------------------------------
            delay until Next_Cycle;

            Clock_Lists.Iterate (Clock_List, Tick'Access);

            Next_Cycle := Next_Cycle + INTERVAL;
         or
            ------------------------------------------------------------
            --  Termination request.
            ------------------------------------------------------------
            --  Terminates task, world clock is thus stopped.
            ------------------------------------------------------------
            accept Stop;

            exit; -- Terminate task by immediately leaving body.
         end select;
      end loop;

   exception

      --  Task crashed. Exception information to standard error.
      when E : others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => Ada.Exceptions.Exception_Information (E));

   end Clock_Task;

   ---------------------------------------------------------------------
   --  Terminate_All
   ---------------------------------------------------------------------
   procedure Terminate_All is
   begin
      Clock_Task.Stop;

      --  Stop has been accepted, task is terminating now. So we are
      --  safe to clear the clocks list.
      Clock_Lists.Clear (Clock_List);
   end Terminate_All;

begin

   --  Postpone main program until task is completely initialized and
   --  running.
   Clock_Task.Start;

end Clocks;
