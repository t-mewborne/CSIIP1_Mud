## Show Code #1 on 1/31 (4 of 4 points)

Compiled and ran. I felt that the way it ran was smooth and it took my commands the way I wanted to type them.
I'm not big on printing both invetory and room items after every get/drop, but that's a design choice that you
can keep if you like it.

## Show Code #3 on 2/19 (4 of 4 points)

Code compiles and runs fine. Rooms are storedin a Map. You made exits be the Buffer, which seems like an odd 
choice since you are never adding to or removing from the exits and that is the advantage of a Buffer. I also
found a bug in item handling that I left two comments on.

## Show Code #7 on 3/28 (4 of 4 points)

Code compiles and runs as expected.

## Show Code #10 on 4/23 (4 of 4 points)

Compiles and runs. Combat and related commands work as expected. One odd thing that I noticed is that your MUD
seems to use a lot of CPU power. It isn't clear to me why that is the case. The frequency with which you check the
stuff in the actor system should not be causing problems.
