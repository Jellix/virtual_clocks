with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;

------------------------------------------------------------------------
--  Clocks package.
--
--  Defines basic types and the clock interface with its operations.
------------------------------------------------------------------------
package Clocks is

   --  Speedup from "real" time to up to 2 hours / second.
   type Speedup is delta 0.2 range 0.0 .. 2 * 3600.0;

   --  Clock time type, for simplicity use the predefined Time type.
   subtype Time is Ada.Calendar.Time;

   --  Depending on the chosen delta, the precise range may be smaller
   --  than specified. So define safe range constants for convinience.
   MIN_SAFE_SPEEDUP   : constant Speedup;
   MAX_SAFE_SPEEDUP   : constant Speedup;

   --  Provide some multiplicator constants for convinience.
   UNCHANGED_TIME     : constant Speedup; --  "real" time.
   MINUTES_PER_SECOND : constant Speedup; --  1 min/s.
   HOURS_PER_SECOND   : constant Speedup; --  1 h/s.

   START_DATE         : constant Time;

   ---------------------------------------------------------------------
   --  Define clock as (synchronized) interface, the only way to make it
   --  tagged and protected at the same time.
   ---------------------------------------------------------------------
   type Clock_Interface is synchronized interface;

   ---------------------------------------------------------------------
   --  Define the clock class reference type for dynamic dispatching.
   ---------------------------------------------------------------------
   type Class_Ref is not null access all Clock_Interface'Class;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   --  "Creates" and initializes a clock with default values.
   ---------------------------------------------------------------------
   procedure Create (Clock      :    out Clock_Interface;
                     Start_Time : in     Time    := START_DATE;
                     Speed      : in     Speedup := UNCHANGED_TIME)
   is abstract;

   ---------------------------------------------------------------------
   --  Forward_Time
   ---------------------------------------------------------------------
   --  Forwards the current time by given amount (in virtual clock time).
   ---------------------------------------------------------------------
   procedure Forward_Time (Clock  : in out Clock_Interface;
                           Amount : Duration)
   is abstract;

   ---------------------------------------------------------------------
   --  Get_Time
   ---------------------------------------------------------------------
   --  Returns the clock's current time. Actual return value depends on
   --  the set speed and the internal update rate.
   ---------------------------------------------------------------------
   function Get_Time (Clock : in Clock_Interface) return Time
   is abstract;

   ---------------------------------------------------------------------
   --  Pulse
   ---------------------------------------------------------------------
   --  Ticks the clock. The amount given is the real time. Depending on
   --  the clock's current speed, the clock advances a certain amount of
   --  time.
   ---------------------------------------------------------------------
   procedure Pulse (Clock  : in out Clock_Interface;
                    Amount : in     Duration)
   is abstract;

   ---------------------------------------------------------------------
   --  Resume
   ---------------------------------------------------------------------
   --  Resumes updates of the clock.
   --
   --  NOTE: If the clock updates are not suspended, the calling task is
   --        blocked!
   ---------------------------------------------------------------------
   procedure Resume (Clock : in out Clock_Interface)
   is abstract;

   ---------------------------------------------------------------------
   --  Set_Speed
   ---------------------------------------------------------------------
   --  Sets the clock speed, i.e. if set to 60.0 then the clock runs 60
   --  times faster than the real clock.
   ---------------------------------------------------------------------
   procedure Set_Speed (Clock     : in out Clock_Interface;
                        New_Speed : in     Speedup)
   is abstract;

   ---------------------------------------------------------------------
   --  Set_Time
   ---------------------------------------------------------------------
   --  Sets a new clock time. Can be used to reset the clock.
   ---------------------------------------------------------------------
   procedure Set_Time (Clock    : in out Clock_Interface;
                       New_Time : in     Time)
   is abstract;

   ---------------------------------------------------------------------
   --  Suspend
   ---------------------------------------------------------------------
   --  Suspends the clock (and this clock only). To continue the clock's
   --  operation, call Resume.
   --
   --  NOTE: If the clock updates are suspended already, the calling
   --        task is blocked.
   ---------------------------------------------------------------------
   procedure Suspend (Clock : in out Clock_Interface)
   is abstract;

   ---------------------------------------------------------------------
   --  Terminate_All
   ---------------------------------------------------------------------
   --  Terminates the internal clock task. All still existing clocks
   --  will stop then.
   ---------------------------------------------------------------------
   procedure Terminate_All;

private

   MIN_SAFE_SPEEDUP   : constant Speedup := Speedup'First;
   MAX_SAFE_SPEEDUP   : constant Speedup := Speedup'Last;

   UNCHANGED_TIME     : constant Speedup := 1.0;
   MINUTES_PER_SECOND : constant Speedup := 60.0 * UNCHANGED_TIME;
   HOURS_PER_SECOND   : constant Speedup := 60.0 * MINUTES_PER_SECOND;

   START_DATE         : constant Time    :=
     Ada.Calendar.Time_Of (Year => 2000, Month => 1, Day => 1);

   package Clock_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Class_Ref);

   ---------------------------------------------------------------------
   --  Global list of active clocks.
   --  Declared in the private part, so child packages can access it.
   ---------------------------------------------------------------------
   Clock_List : Clock_Lists.List;

end Clocks;
