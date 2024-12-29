W i n 3 2  -  F o r t r a n
============================

A basic question for a fortranner is: How to create Fortran
applications with GUI interface? More advanced Fortran GUI programs
could be created with [GTK-Fortran
Library](https://github.com/jerryd/gtk-fortran), i.e. using the
interoperability between C and Fortran, which comes with the Fortran
2003 standard.

Following that example, we have created modules which partially
interface [Borland Graphics
Interface](https://en.wikipedia.org/wiki/Borland_Graphics_Interface)
(BGI).

On Windows we can have Fortran GUI programs using an interface to
Windows itself. This is what we present here: partial interface to
Windows which allow for creating simple Windows applications in
Fortran.

Rudimentary modules which implement this are contained in `win32.f90`,
`*box*.f90` and `win32app.f90` source files. The first contains the
interface itself, the boxes try to recover an old idea we implemented
creating a C++ dialog library with the old *Borland C++ 2.0* compiler
(around 1991). The last tries to do things in `World Coordinate
System`.

A few examples of these applications are included here. As always,
details in the comments.

A special thanks goes to the many fortranners met on the WEB for their
valuable suggestions.
