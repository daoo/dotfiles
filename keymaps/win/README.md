# Dvoormak on Windows

The patches (`*.patch`) applies to the Programmer Dvorak source for windows
(`kbddvp-1_2_8-src-winnt.cab`). However, attempts at building using the Windows
Enterprise WDK on Windows 11 have been unsuccessful. The compilation fails on
`kbddvp.c` because it cannot find `windows.h`, which is a pretty weird error to
get with the WDK. It did work on Windows 10 in the past but don't remember what
I did.

The other method is to use the [Microsoft Keyboard Layout Creator
(MSKLC)](https://www.microsoft.com/en-us/download/details.aspx?id=102134) and
the `dvoormak.klc` project. As MSKLC does not flipping caps lock and escape,
PowerToys Keyboard Manager can be used instead to apply the remapping.
