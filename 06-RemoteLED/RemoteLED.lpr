program RemoteLED;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 6 - Remote LED                                                       }
{                                                                              }
{ This is the source code for the application used in the video, this version  }
{ is for Raspberry Pi 2 and 3 but you can easily make a version suitable for   }
{ Raspberry Pi A/B/A+/B+/Zero by creating a new project and adding the code    }
{ below.                                                                       }
{                                                                              }
{ This example creates two web pages, the first one you can access as:         }
{  http://<IP Address>/RemoteLED                                               }
{ to control turning on or off the Activity and Power LEDs.                    }
{                                                                              }
{ The second page will be found at:                                            }
{  http://<IP Address>/Parameters                                              }
{ and allows you to explore what happens when you pass parameters to the server}
{ from a web browser.                                                          }
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
  HTTP,        {Include the HTTP unit for the HTTPListener class}
  Winsock2,    {Include Winsock2 so we can get the local IP address}
  LEDPage,     {Add our LED page unit for our custom HTTPDocument class}
  ParamsPage;  {And add the params page unit so we can see the parameters passed to the server}

var 
 IPAddress:String;
 WindowHandle:TWindowHandle;
 HTTPListener:THTTPListener;
 RemoteLEDPage:TRemoteLEDPage; 
 ParametersPage:TParametersPage;
 Winsock2TCPClient:TWinsock2TCPClient;
 
begin
 {Create a window, just so that we can print the IP address and URL}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Create a Winsock2TCPClient to get the IP address}
 Winsock2TCPClient:=TWinsock2TCPClient.Create;
 
 {Get the IP address which may be invalid at this point}
 IPAddress:=Winsock2TCPClient.LocalAddress;
 
 {Check the IP address}
 if (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') then
  begin
   {Wait for  a valid IP address}
   while (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') do
    begin
     {Sleep a bit}
     Sleep(1000);

     {Get the address again}
     IPAddress:=Winsock2TCPClient.LocalAddress;
    end;
  end;
 
 {Enable both the Activity and Power LEDs}
 {The Power LED only works on certain models of Raspberry Pi (eg A+/B+/2B) but the Activity
  LED will work on all of them}
 ActivityLEDEnable;
 PowerLEDEnable;
  
 {Create the HTTP listener, this is similar to our WebServer example}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 
 {Now we create an instance of our LED page}
 RemoteLEDPage:=TRemoteLEDPage.Create;
 
 {And register our LED page with the HTTP listener}
 HTTPListener.RegisterDocument('',RemoteLEDPage);
 
 {So you can explore what is going on, we create a parameters page}
 ParametersPage:=TParametersPage.Create;
 
 {And register it as well with the HTTP listener}
 HTTPListener.RegisterDocument('',ParametersPage);
 
 {Report the URL on the console window}
 ConsoleWindowWriteLn(WindowHandle,'Ready, point your browser to http://' + Winsock2TCPClient.LocalAddress + '/RemoteLED');
 
 {Free the Winsock2TCPClient object}
 Winsock2TCPClient.Free;
 
 {And halt this thread because the action happens elsewhere}
 ThreadHalt(0);
 
end.

