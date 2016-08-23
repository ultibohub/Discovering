unit ParamsPage;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 6 - Remote LED                                                       }
{                                                                              }
{ This is the Parameters page of the example, the URL will be:                 }
{  http://<IP Address>/Parameters                                              }
{                                                                              }
{ If you put a ? on the end of the URL and then add name=value like this:      }
{  http://<IP Address>/Parameters?name=value                                   }
{ you will see in the page displayed that name=value is passed as a parameter. }
{                                                                              }
{ You can pass multiple parameters by putting an & in between them like this:  }
{  http://<IP Address>/Parameters?name=value&action=go                         }
{                                                                              }
{ Try out different combinations yourself to see what happens.                 }
{                                                                              }

interface

uses
  GlobalConfig,
  GlobalConst,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  HTTP;
  
type
 {Another descendant of THTTPDocument, this one just shows you what happens with parameters}
 TParametersPage = class(THTTPDocument)
  constructor Create;
  
 private
  procedure AddResult(AResponse:THTTPServerResponse);
  procedure AddHeader(AResponse:THTTPServerResponse);
  procedure AddFooter(AResponse:THTTPServerResponse);
  procedure AddContent(AResponse:THTTPServerResponse;const AContent:String);
  
 protected
  {As with our LED page we still need to override the DoGet method in order to return our content}
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
  
 end;

 
implementation


constructor TParametersPage.Create;
begin
 inherited Create;
 {Set our name to /Parameters which will also be our URL}
 Name:='/Parameters';
end;


procedure TParametersPage.AddResult(AResponse:THTTPServerResponse);
begin
 {Set the response to 200 Status OK}
 AResponse.Version:=HTTP_VERSION;
 AResponse.Status:=HTTP_STATUS_OK;
 AResponse.Reason:=HTTP_REASON_200;
end;


procedure TParametersPage.AddHeader(AResponse:THTTPServerResponse);
begin
 {Add our simple HTML header}
 AddContent(AResponse,'<html>');
 AddContent(AResponse,'<head>');
 AddContent(AResponse,'<title>HTTP Parameters</title>');
 AddContent(AResponse,'</head>');
 AddContent(AResponse,'<body>');
end;


procedure TParametersPage.AddFooter(AResponse:THTTPServerResponse);
begin
 {And a basic HTML footer}
 AddContent(AResponse,'</body>');
 AddContent(AResponse,'</html>');
end;
  
  
procedure TParametersPage.AddContent(AResponse:THTTPServerResponse;const AContent:String);
begin
 {Append to the content to be sent to the browser}
 AResponse.ContentString:=AResponse.ContentString + AContent + HTTP_LINE_END;
end;
  
  
function TParametersPage.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
var
 Count:Integer;
 Param:THTTPParam;
 Header:THTTPHeader;
begin
 Result:=True;
   
 {Send the result}
 AddResult(AResponse);
   
 {Send the header}
 AddHeader(AResponse);
 
 {All this page does is enumerate each of the parameters that have been passed from the browser to the server
  and display them in the resulting HTML page. We also include the headers which are part of every HTTP transaction
  and can be used to control many aspects of the communication between the browser and the server}
  
 {Print a simple header}
 AddContent(AResponse,'<b>Request.Params</b><br>');
 
 {In the LED page example we called the Request.GetParam method to locate a parameter by name, this time we simply
  want all of the parameters in order so we use the Request.Params.GetParam method}
 Param:=ARequest.Params.GetParam(nil);
 while Param <> nil do
  begin
   {Print each of the passed parameters as NAME=VALUE}
   AddContent(AResponse,Param.Name + ' = ' + Param.Value + '<br>');
   
   {Then get the next parameter and repeat until there are no more}
   Param:=ARequest.Params.GetParam(Param);
  end;
 {Add a blank line to make it clearer} 
 AddContent(AResponse,'<br>');

 {Print a header for our headers}
 AddContent(AResponse,'<b>Request.Headers</b><br>');
 
 {As with the parameters, we can use Request.Header.GetHeader to enumerate each of the headers in order, there are also
  methods like Request.GetHeader to look for a header value by name instead}
 Header:=ARequest.Headers.GetHeader(nil);
 while Header <> nil do
  begin
   {Print each of the passed headers}
   AddContent(AResponse,Header.Name + ' = ' + Header.GetValue(0) + '<br>');
   
   {Headers can have multiple values, so we check the count and add any extras that are there}
   if Header.GetCount > 1 then
    begin
     for Count:=1 to Header.GetCount - 1 do
      begin
       {Print the next header value}
       AddContent(AResponse,Header.Name + ' = ' + Header.GetValue(Count) + '<br>');

      end;
    end;
   
   {Get the next header and repeat}
   Header:=ARequest.Headers.GetHeader(Header);
  end;
 AddContent(AResponse,'<br>');
 
 {Send our footer}
 AddFooter(AResponse);
 
 {Return to the web server, it will send our content back to the browser}
 
end;

end.
 