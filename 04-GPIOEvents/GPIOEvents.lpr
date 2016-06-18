program GPIOEvents;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 4 - GPIO Events                                                      }
{                                                                              }
{ This is the source code for the application used in the video, this version  }
{ is for Raspberry Pi 2 and 3 but you can easily make a version suitable for   }
{ Raspberry Pi A/B/A+/B+/Zero by creating a new project and adding the code    }
{ below.                                                                       }
{                                                                              }
{ This example uses a switch and two LEDs, the switch is on pin 18 and the LEDs}
{ are connected to pins 20 and 21.                                             }
{                                                                              }
{ See the file GPIOEvents.png in this folder for a simple circuit diagram      }
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
  Console,   {Include the console unit to access the default console device}
  GPIO;      {Include the GPIO unit to access the GPIO functions}

const
 {Define constants for the states of our switch (On or Off)}
 SWITCH_STATE_OFF = 0;
 SWITCH_STATE_ON  = 1;

 
var
 {Declare a variable for a window handle and one to hold the switch state}
 State:LongWord;
 Handle:THandle;

{This is the function called when a GPIO event occurs}
procedure GPIOEventFunction(Data:Pointer;Pin,Trigger:LongWord);
begin
 {Sleep for a few milliseconds to allow the switch to settle}
 Sleep(10);

 {Read the current state of the switch (High or Low)}
 if GPIOInputGet(GPIO_PIN_18) = GPIO_LEVEL_LOW then
  begin
   {If the state is Low the switch is On so update the state variable}
   State:=SWITCH_STATE_ON;

   {Print a message on the console in green to say the switch is on}
   ConsoleWindowSetForecolor(Handle,COLOR_GREEN);
   ConsoleWindowWriteLn(Handle,'Switch is ON');

   {Reregister this function to be called when the state changes to High}
   GPIOInputEvent(GPIO_PIN_18,GPIO_TRIGGER_HIGH,INFINITE,@GPIOEventFunction,nil);
  end
 else
  begin
   {The level is High so the switch must be Off, save that to the state variable}
   State:=SWITCH_STATE_OFF;

   {Print the switch is off message in red}
   ConsoleWindowSetForecolor(Handle,COLOR_RED);
   ConsoleWindowWriteLn(Handle,'Switch is OFF');

   {And register again for an event when the level changes to Low}
   GPIOInputEvent(GPIO_PIN_18,GPIO_TRIGGER_LOW,INFINITE,@GPIOEventFunction,nil);
  end;
end;


begin
 {Create a normal console window using the full screen}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Set GPIO pin 18 (our switch) to Pull Up so the pin will be High when the switch 
  is off and Low when the switch is On. Then set the function of pin 18 to be an
  Input so we can read the switch state using the GPIOInputGet() function}
 GPIOPullSelect(GPIO_PIN_18,GPIO_PULL_UP);
 GPIOFunctionSelect(GPIO_PIN_18,GPIO_FUNCTION_IN);

 {Set GPIO pin 20 (an LED) to Pull None and make the function of the pin an Output
  so we can set the level to High or Low to turn the LED On or Off. After setting
  the function we also set the current level to Low so the LED is Off}
 GPIOPullSelect(GPIO_PIN_20,GPIO_PULL_NONE);
 GPIOFunctionSelect(GPIO_PIN_20,GPIO_FUNCTION_OUT);
 GPIOOutputSet(GPIO_PIN_20,GPIO_LEVEL_LOW);
 
 {Do the same for GPIO pin 21 (the other LED)}
 GPIOPullSelect(GPIO_PIN_21,GPIO_PULL_NONE);
 GPIOFunctionSelect(GPIO_PIN_21,GPIO_FUNCTION_OUT);
 GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_LOW);

 {Read the current state of the switch (pin 18) to work out which event we want to
  register for}
 if GPIOInputGet(GPIO_PIN_18) = GPIO_LEVEL_LOW then
  begin
   {The level is Low so the switch is On}
   {Save the switch state for later}
   State:=SWITCH_STATE_ON;

   {Register for an event by calling the GPIOInputEvent() function.
   
    We need to specify a few things to GPIOInputEvent()
    
    The pin number for the event which is GPIO_PIN_18.
    What the trigger for the event is, GPIO_TRIGGER_HIGH because the pin is currently low.
    How long to wait for the event, INFINITE to wait forever.
    The function to call when the event happens which is our GPIOEventFunction above.
    Any private data we would like passed to our function with the event, in this case nil (or nothing).
   
    GPIOInputEvent will register the requested event internally within the GPIO driver
    and return immediately with success (ERROR_SUCCESS) or failure (any other value).
    If it succeeded then our program can move on with whatever it is doing confident that
    the event function we supplied will be called when the event occurs.}
   GPIOInputEvent(GPIO_PIN_18,GPIO_TRIGGER_HIGH,INFINITE,@GPIOEventFunction,nil);
  end
 else
  begin
   {The level is High (switch is Off) so we do the opposite to above}
   {Save the switch state}
   State:=SWITCH_STATE_OFF;

   {Register with GPIOInputEvent for GPIO_TRIGGER_LOW on GPIO_PIN_18}
   GPIOInputEvent(GPIO_PIN_18,GPIO_TRIGGER_LOW,INFINITE,@GPIOEventFunction,nil);
  end;

 {Loop around checking the state variable and blinking the LEDs either both at
  once or alternately depending on whether the switch is On or Off. This could
  also be done without a loop by using some of the other functions in Ultibo 
  like Timer and Worker events but we'll look at those in another episode}
 while True do
  begin
   {Check the state at the start of each loop}
   if State = SWITCH_STATE_ON then
    begin
     {If the switch is On then turn On both LEDs}
     GPIOOutputSet(GPIO_PIN_20,GPIO_LEVEL_HIGH);
     GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_HIGH);

     {Sleep a little bit}
     Sleep(300);

     {Then turn Off both LEDs}
     GPIOOutputSet(GPIO_PIN_20,GPIO_LEVEL_LOW);
     GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_LOW);

     {And sleep a bit more and start the loop again}
     Sleep(300);
    end
   else
    begin
     {If the switch is Off then turn On one LED and turn Off the other one}
     GPIOOutputSet(GPIO_PIN_20,GPIO_LEVEL_HIGH);
     GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_LOW);

     {Sleep again}
     Sleep(200);

     {And reverse which LED is On and Off}
     GPIOOutputSet(GPIO_PIN_20,GPIO_LEVEL_LOW);
     GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_HIGH);

     {Sleep some more and go around again}
     Sleep(200);
    end;
  end;
end.

