program ExploringUSB;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 2 - Exploring USB                                                    }
{                                                                              }
{ This is the source code for the application used in the video, this version  }
{ is for Raspberry Pi 2 and 3 but you can easily make a version suitable for   }
{ Raspberry Pi A/B/A+/B+/Zero by creating a new project and adding the code    }
{ below.                                                                       }
{                                                                              }
{ To compile the program select Run, Compile (or Run, Build) from the menu     }

uses
  InitUnit,     {There is a reason why this unit is first, open it to see why}
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,

  {The units above (except for InitUnit) are the standard ones included when you
   create a new Raspberry Pi 2B project, the ones below have beed added for this
   demo. The RaspberryPi2 unit above ensures that all of the necessary units like
   USB, MMC and Filesystem and the drivers for the Raspberry Pi are included}
  Logging,         {Include the logging unit for the console logging window}
  Console,         {Include the console unit to access the default console device}
  ConsoleShell,    {This unit provides the console shell window}
  ShellFileSystem; {This unit adds the file system commands (DIR, COPY, DEL etc)}

begin
 {Set the Console Colors}
 {These are from the GlobalConfig unit, we could also call ConsoleWindowSetForecolor
  and ConsoleWindowSetBackcolor after creating a console window}
 WINDOW_DEFAULT_FORECOLOR:=COLOR_WHITE;
 WINDOW_DEFAULT_BACKCOLOR:=COLOR_BLACK;

 {Start the Console shell}
 {First we enable the console shell (see InitUnit for more info) and then we set
  the position where the console shell will appear. The last line passes the default
  console device to the ConsoleShellDeviceAdd function to create a console shell}
 CONSOLE_SHELL_ENABLED:=True;
 CONSOLE_SHELL_POSITION:=CONSOLE_POSITION_TOP;
 ConsoleShellDeviceAdd(ConsoleDeviceGetDefault,False);

 {Start the Console logging}
 {Enable console logging and set the logging position to where we want it, then
  call LoggingConsoleDeviceAdd passing the default console. Finally make the console
  logging device the default so all logging output goes directly to it}
 CONSOLE_REGISTER_LOGGING:=True;
 CONSOLE_LOGGING_POSITION:=CONSOLE_POSITION_BOTTOM;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));

 {Halt the Main thread}
 {Just for safety so our main thread never exits}
 ThreadHalt(0);
end.

