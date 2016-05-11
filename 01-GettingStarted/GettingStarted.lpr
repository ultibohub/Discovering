program GettingStarted;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 1 - Getting Started                                                  }
{                                                                              }
{ This is the source code for the application used in the video, this version  }
{ is for Raspberry Pi 2 and 3 but you can easily make a version suitable for   }
{ Raspberry Pi A/B/A+/B+/Zero by creating a new project and adding the code    }
{ below.                                                                       }
{                                                                              }
{ To compile the program select Run, Compile (or Run, Build) from the menu     }


uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,

  {The units above are the standard ones included when you create a new project
   for Raspberry Pi 2B, we add the Console unit below so we have access to the
   console functions in our project}
  Console
  { Add additional units here };


{When we create a console window the function will return a handle that can be
 passed to other functions to tell them which window to draw or write on. We
 create a variable to store the handle so it is available when we need it}
var
 Handle:THandle;

begin
 {We create a new console window by calling ConsoleWindowCreate, it needs to know
  which console device to create the window on so we can ask for the default device
  by calling ConsoleDeviceGetDefault.

  Many device types in Ultibo support calling the GetDefault function and many also
  support SetDefault as well.

  The parameter CONSOLE_POSITION_FULL makes our window the full screen, you can find
  this and other console positions in the GlobalConst unit.

  The final parameter says if our window can be the default window, see below for
  more information about what this means}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);


 {To write text on our new window we can call ConsoleWindowWriteLn and pass the
  handle from above as well as the text we want to write.

  Because we made this the default window we could have used a couple of other ways
  to write the text on the window like

   ConsoleWriteLn('Hello Ultibo !!');

  or

   WriteLn('Hello Ultibo !!');

  Try them out for yourself to see what happens}
 ConsoleWindowWriteLn(Handle,'Hello Ultibo !!');


 { Add your program code here }


 {Halt the Main thread just so it never exits}
 ThreadHalt(0);
end.

