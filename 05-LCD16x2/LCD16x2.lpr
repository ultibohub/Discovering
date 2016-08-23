program LCD16x2;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 5 - LCD 16x2                                                         }
{                                                                              }
{ This is the source code for the application used in the video, this version  }
{ is for Raspberry Pi 2 and 3 but you can easily make a version suitable for   }
{ Raspberry Pi A/B/A+/B+/Zero by creating a new project and adding the code    }
{ below.                                                                       }
{                                                                              }
{ This uses an Adafruit RGB Positive 16x2 LCD+Keypad Kit for Raspberry Pi      }
{ which you can find here https://www.adafruit.com/products/1109               }
{ The actual LCD uses a Hitachi HD44780 LCD controller along with a MCP23017   }
{ I/O expander both of which are supported directly in Ultibo core.            }
{                                                                              }
{ If your device uses a different combination you may need to modify the driver}
{ or create your own using the existing ones as an example.                    }
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
  AF16x2LCD;   {Include the driver for the Adafruit PiPlate 16x2 Character LCD} 


{The RotateText function is the core of the project, it is very generic and should
 work just as well with other console windows since there is nothing specific about
 the 16x2 LCD in this code}
procedure RotateText(Handle:THandle;const Text:String;Line:LongWord;Right:Boolean);
var
 Width:LongWord;
 
 Buffer:String;
 Len:LongWord;
 Start:LongWord;
begin
 {Perform some simple safety checks}
 if Line < 1 then Exit;
 if Length(Text) < 1 then Exit;
 
 {Add two spaces to the end of the text so our head and tail don't meet up} 
 Buffer:=Text + '  ';

 {Get the length of the text including the spaces}
 Len:=Length(Buffer);
 if Len < 1 then Exit;
 
 {Get the width of the console window, it doesn't have to be 16 for this to work} 
 Width:=ConsoleWindowGetWidth(Handle);
 if Width < 1 then Exit;

 {Check if we are scrolling on the last line of the display}
 if Line = ConsoleWindowGetHeight(Handle) then
  begin
   {If we are on the last line, account for that by subtracting 1 from the width so we don't scroll the screen up when we add our text.
    See the next line for what the rest of this is doing}
   ConsoleWindowWriteEx(Handle,Copy(Buffer,1,Width - 1),1,Line,ConsoleWindowGetForecolor(Handle),ConsoleWindowGetBackcolor(Handle));
  end
 else
  begin 
   {The first step is to put as much text as we can on the screen without scrolling or wrapping. So we use the width of the console
    window to get a full line of text from the buffer. We specify the first character as X and the line we were passed as Y, we need
    to provide the foreground and background colors even though the 16x2 LCD is black and white and will not use them}
   ConsoleWindowWriteEx(Handle,Copy(Buffer,1,Width),1,Line,ConsoleWindowGetForecolor(Handle),ConsoleWindowGetBackcolor(Handle));
  end; 
 Sleep(1000);

 {Check if we are scrolling to the right or the left}
 if not Right then
  begin
   {If we are going left then our starting character for the scroll will be one past the width}
   Start:=Width + 1; 
  end
 else
  begin
   {If we are scrolling right then our starting character will be the end of our buffer}
   Start:=Len; 
  end;
 
 {Now we start a loop with our scrolling} 
 while True do
  begin
   {Check left or right}
   if not Right then
    begin
     {If it's left then use ConsoleWindowScrollLeft() to scroll one whole line one character to the left}
     ConsoleWindowScrollLeft(Handle,Line,Width,1,1);
     
     {Then we fill in the space at the end of the line created by our scroll with the next character from the buffer}
     ConsoleWindowWriteChrEx(Handle,Buffer[Start],Width,Line,ConsoleWindowGetForecolor(Handle),ConsoleWindowGetBackcolor(Handle));
     
     {Increment the start (or next) and check if we have reached the end, if so go back to the start and repeat}
     Inc(Start);
     if Start > Len then Start:=1;
     
     {Sleep for a brief time and do it again}
     Sleep(300);
    end
   else
    begin 
     {When scrolling right we use the ConsoleWindowScrollRight() to scroll one whole line one character to the right}
     ConsoleWindowScrollRight(Handle,Line,1,1,1);    
     
     {This will give us an empty space at the start of the line so again we fill it in with the next character from the buffer}
     ConsoleWindowWriteChrEx(Handle,Buffer[Start],1,Line,ConsoleWindowGetForecolor(Handle),ConsoleWindowGetBackcolor(Handle));
     
     {Decrement the start because we are scrolling backwards and check if we have reached the first character}
     Dec(Start);
     if Start < 1 then Start:=Len;
     
     {Sleep and then repeat}
     Sleep(200);
    end;
  end;
end;
  
    
var
 {We need a window handle and a thread handle}
 WindowHandle:TWindowHandle;
 ThreadHandle:TThreadHandle;
  
 
{This is the thread function for our first line scrolling thread} 
function FirstLineScrollThread(Parameter:Pointer):PtrInt;  
begin
 Result:=0;
 
 {Simply call our rotate text function to scroll the text "Scrolling the Adafruit 16x2 LCD with Ultibo..."
  from right to left on line 1}
 RotateText(WindowHandle,'Scrolling the Adafruit 16x2 LCD with Ultibo...',1,False);
 
 {Unless we get the parameters wrong the thread should never return to here because RotateText does an
  endless loop}
end;


{And this is the thread function for the second line scrolling thread}
function SecondLineScrollThread(Parameter:Pointer):PtrInt;  
begin
 Result:=0;

 {Call our rotate text function again and pass the text "https://ultibo.org" to scroll from left to right on line 2}
 RotateText(WindowHandle,'https://ultibo.org',2,True); 
end;


begin
 {As usual our program will start here, the AF16x2LCD driver unit includes some extra functions to turn on or off
  the backlight, set the color of the backlight or check the status of the buttons}
  
 {Turn on the LCD backlight, notice that we pass INVALID_HANDLE_VALUE. That's because the driver will use the default
  value if we pass that and it saves us looking it up. Chances are there will only be one of these connected anyway.}
 AF16x2LCDBacklightOn(INVALID_HANDLE_VALUE);
 
 {Set the backlight color to White (The values are Red, Green, Blue in order so you can
  choose any color you like}
 AF16x2LCDBacklightColor(INVALID_HANDLE_VALUE,1,1,1);
 
 {Create a window on the console, not the normal console but the console created by the AF16x2LCD driver. We do this
  by using the ConsoleDeviceFindByDescription() function to find a specific console device and pass it the description
  which the driver will have given it. See the AF16X2LCD_CONSOLE_DESCRIPTION constant in the AF16x2LCD unit}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceFindByDescription('Adafruit 16x2 LCD'),CONSOLE_POSITION_FULLSCREEN,True);
 
 {Now we have a handle we can use the normal ConsoleWindowWriteLn() function to draw text on our 16x2 LCD just the
  same as we do with any other console window. If we design our devices and drivers well then we don't need to use
  different functions for every device, they can fit into our existing structure}
 ConsoleWindowWriteLn(WindowHandle,'Hello LCD 16x2');
 Sleep(2000);

 {Now we clear our 2 line LCD ready for the next part} 
 ConsoleWindowClear(WindowHandle);
 
 {You could use a single thread to scroll messages on both lines at once, but threads are easy in Ultibo core and 
  it's fun to show what you can do with them}
 {Start one thread scrolling a message on the first line of the LCD}
 ThreadHandle:=BeginThread(@FirstLineScrollThread,nil,ThreadHandle,THREAD_STACK_DEFAULT_SIZE);   
 
 {And start another thread scrolling a message on the second line}
 ThreadHandle:=BeginThread(@SecondLineScrollThread,nil,ThreadHandle,THREAD_STACK_DEFAULT_SIZE);   
 
 {Halt this thread because we are done}
 ThreadHalt(0);
 
end.

