# Virtual Clocks

An attempt on using synchronized interfaces.

Provides an interface to clocks that can run at different speed(s).

They are active, that means their internal time value gets regularly updated in
the background (but by no means in real-time).

Clocks speeds can be changed at any time, as well as the time can be forwarded
a certain amount of time and clocks can be suspended, i.e frozen.
