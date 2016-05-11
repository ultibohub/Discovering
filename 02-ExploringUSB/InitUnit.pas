unit InitUnit;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 2 - Exploring USB                                                    }
{                                                                              }
{ This unit is included by the demo project but contains no code, it is simply }
{ included first in the list of units so that the initialization section below }
{ happens before all other units like RaspberryPi2, Console and Logging.       }
{                                                                              }
{ Why do want these things to happen first?                                    }
{ Because CONSOLE_SHELL_ENABLED is set to True by default and if we include the}
{ ConsoleShell unit then it will automatically create a console shell window   }
{ on any console device it finds during initialization.                        }
{                                                                              }
{ The variable CONSOLE_REGISTER_LOGGING is set to False by default but to be   }
{ sure that the console logging device doesn't create a logging window before  }
{ we are ready we also make sure it is set to False.                           }
{                                                                              }
{ When we are ready we can create both the console shell and logger the way we }
{ want them to appear.                                                         }
{                                                                              }
{ You can use this technique in your projects to ensure certain behaviour if   }
{ you need to make sure one thing happens before another.                      }
{                                                                              }

interface

uses
  GlobalConfig, Classes, SysUtils;

implementation

initialization
 {Disable the console shell}
 CONSOLE_SHELL_ENABLED:=False;

 {Disable console logging}
 CONSOLE_REGISTER_LOGGING:=False;
end.

