program BuildingRTL;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 3 - Building the RTL                                                 }
{                                                                              }
{ There was no example code for this episode, this is the source for the font  }
{ scrolling demo shown in the background on the video. This version is for the }
{ Raspberry Pi 2 and 3 but you can easily make a version suitable for use on a }
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
  
  {The units above are the standard ones included when you create a new Raspberry
   Pi 2B project, the ones below have beed added for this demo. The RaspberryPi2
   unit above ensures that all of the necessary units like USB, MMC and Filesystem
   and the drivers for the Raspberry Pi are included}
  Console,     {Include the console unit to access the default console device}
  Font,        {Include the font unit to allow finding a font by name}
  Hack_30,     {Include the Hack font sample in 24x46 size}
  Sun_12x22;   {Include the Sun font in 12x22 size}

  {The Hack font is created from a freely available font created by Christopher
   Simpkins which you can find on GitHub here https://github.com/chrissimpkins/Hack}
  
var
 {A couple of window handles and a counter variable}
 Window1:THandle;
 Window2:THandle;
 Count:LongWord;

{A function to select the next color in the cycle}
function GetColor(Count:LongWord):LongWord;
begin
 Result:=COLOR_BLACK;

 case (Count mod 5) of
  0:Result:=COLOR_RED;
  1:Result:=COLOR_ORANGE;
  2:Result:=COLOR_GREEN;
  3:Result:=COLOR_PURPLE;
  4:Result:=COLOR_CYAN;
 end;
end;

begin
 {Create a couple of console windows on the left and right}
 Window1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,False);
 Window2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,False);

 {Set the font for each window by calling the FontFindByName() function with the
  name of the font we want to use.
  
  Each console window can be set to a different font but only one font is allowed
  per console window, this is similar to the Windows command prompt or the Linux 
  console}
 ConsoleWindowSetFont(Window1,FontFindByName('Hack_30'));
 ConsoleWindowSetFont(Window2,FontFindByName('Sun-12x22'));

 Count:=0;

 {Do a loop incrementing the count each time}
 while True do
  begin
   Inc(Count);

   {Set the console forecolor based on the result of the GetColor() function above
    then print a message and do the same for the other window}
   ConsoleWindowSetForecolor(Window1,GetColor(Count));
   ConsoleWindowWriteLn(Window1,'Building the RTL (24x46)');
   Sleep(150);

   ConsoleWindowSetForecolor(Window2,GetColor(Count));
   ConsoleWindowWriteLn(Window2,'Building the RTL (12x22)');
   Sleep(250);
  end;
end.

