unit LEDPage;

{$mode objfpc}{$H+}

{ Discovering Ultibo                                                           }
{                                                                              }
{ Episode 6 - Remote LED                                                       }
{                                                                              }
{ This is the Remote LED control page of the example, you will find it at:     }
{  http://<IP Address>/RemoteLED                                               }
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
 {To create a custom document for our web server, we need to create a class that is
  descended from THTTPDocument and override at least the DoGet() method. Because we
  register an instance of this new document with the HTTP Listener (our server) then
  if it receives a request for our URL it will pass that request to our custom document
  so we can respond to it anyway we want to}
 TRemoteLEDPage = class(THTTPDocument)
  constructor Create;
  
 private
  procedure AddResult(AResponse:THTTPServerResponse);
  procedure AddHeader(AResponse:THTTPServerResponse);
  procedure AddFooter(AResponse:THTTPServerResponse);
  procedure AddContent(AResponse:THTTPServerResponse;const AContent:String);
  
 protected
  {This is the DoGet method that we need to override, there are some others as well like DoPost and DoHead which
   we can also override if we need to depending on the what we want to create}
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
  
 end;
  
  
implementation


{The constructor for our page simply calls the inherited Create method and then sets the name of the page}
constructor TRemoteLEDPage.Create;
begin
 inherited Create;
 {This is the name of our document, and it also becomes the URL for our page}
 Name:='/RemoteLED';
end;


{AddResult is not a method from our parent class, we just added it here to create a convenient place to
 return the result code to the browser}
procedure TRemoteLEDPage.AddResult(AResponse:THTTPServerResponse);
begin
 {Every web request should have a result so that the browser knows whether to continue or to show an error}
 
 {We set the response to 200, Status OK which means success}
 AResponse.Version:=HTTP_VERSION;
 AResponse.Status:=HTTP_STATUS_OK;
 AResponse.Reason:=HTTP_REASON_200;
end;


{Like AddResult, AddHeader and AddFooter are just methods we have added to our class to make it easier to
 follow. Here we simply add some HTML to create the top part (header) of our page}
procedure TRemoteLEDPage.AddHeader(AResponse:THTTPServerResponse);
begin
 {This is a very simple HTML header but you can make it as simple or as complex as you like}
 AddContent(AResponse,'<html>');
 AddContent(AResponse,'<head>');
 AddContent(AResponse,'<title>Remote LED</title>');
 AddContent(AResponse,'</head>');
 AddContent(AResponse,'<body>');
end;


{AddFooter just closes off the HTML that AddHeader started}
procedure TRemoteLEDPage.AddFooter(AResponse:THTTPServerResponse);
begin
 {Finish off our HTML page with the minimum required}
 AddContent(AResponse,'</body>');
 AddContent(AResponse,'</html>');
end;
  
  
{To save some typing we encapsulate the adding of content to our response within the AddContent method.
 The way the THTTPDocument class is designed, when we return successfully from the DoGet method then
 any text we have added to Response.ContentString will be sent back to the browser that made the request}  
procedure TRemoteLEDPage.AddContent(AResponse:THTTPServerResponse;const AContent:String);
begin
 {Add to the content to be sent to the browser}
 AResponse.ContentString:=AResponse.ContentString + AContent + HTTP_LINE_END;
end;
  
  
{This is our DoGet method which will be called by the THTTPListener class whenever a request is received for our URL}  
function TRemoteLEDPage.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
var
 Action:String;
begin
 Result:=True;
 
 {Get the ACTION parameter from the Request and check the value, if the browser didn't pass an ACTION parameter
  then the GetParam method will return an empty (blank) value, that's the last case below and is where we return
  our web page. 
  
  If the browser passed us the value ON or OFF for the ACTION parameter then we respond to those by either turning
  the LEDs on or off and sending back the appropriate text.}
 Action:=Uppercase(ARequest.GetParam('ACTION'));
 if Action = 'ON' then
  begin
   {Turn both the LEDs On}
   ActivityLEDOn;
   PowerLEDOn;
   
   {Add the request result}
   AddResult(AResponse);
   
   {Send the text "LED is On" as the only response, notice that we don't have to send any HTML to the browser,
    the JavaScript function in our page is expecting just the words "LED is On" or "LED is Off" which it will assign
    to the text of the status button. This example sends plain text but you could send JSON, XML or any sort of
    text information to the browser based on what you are creating}
   AddContent(AResponse,'LED is On');
  end
 else if Action = 'OFF' then
  begin 
   {Turn the LEDs Off}
   ActivityLEDOff;
   PowerLEDOff;
   
   {Add the result again}
   AddResult(AResponse);
   
   {And send the text "LED is Off"}
   AddContent(AResponse,'LED is Off');
  end
 else
  begin 
   {If the ACTION parameter is blank or is not ON or OFF then we send the HTML page which includes our buttons 
    and the JavaScript function to turn the LED on or off}
   {We always add the request result}
   AddResult(AResponse);
   
   {Send the page header first to start our HTML page}
   AddHeader(AResponse);
    
   {Add some JavaScript for the buttons, this function uses the XMLHttpRequest object from JavaScript and you can
    find more information about that here http://www.w3schools.com/xml/dom_http.asp
    
    There is also an example of using XMLHttpRequest here http://www.w3schools.com/xml/tryit.asp?filename=try_dom_xmlhttprequest_first
    which is where I modified this function from} 
   AddContent(AResponse,'<script>');
   AddContent(AResponse,'function RemoteLED(theUrl) {');
   AddContent(AResponse,'  var xmlhttp;');
   AddContent(AResponse,'  if (window.XMLHttpRequest) {');
   AddContent(AResponse,'    xmlhttp = new XMLHttpRequest();');
   AddContent(AResponse,'  } else {');
   AddContent(AResponse,'    // code for older browsers');
   AddContent(AResponse,'    xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");');
   AddContent(AResponse,'  }');
   AddContent(AResponse,'  xmlhttp.onreadystatechange = function() {');
   AddContent(AResponse,'    if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {');
   AddContent(AResponse,'      document.getElementById("status").innerHTML =');
   AddContent(AResponse,'      xmlhttp.responseText;');
   AddContent(AResponse,'    }');
   AddContent(AResponse,'  };');
   AddContent(AResponse,'  xmlhttp.open("GET", theUrl, true);');
   AddContent(AResponse,'  xmlhttp.send();');
   AddContent(AResponse,'}');
   AddContent(AResponse,'</script>');
    
   {Add a div section which is the full width of the screen} 
   AddContent(AResponse,'<div style="width:100%">');
   
   {Add our status button in White, notice that there is no on click action for the button so clicking it does nothing at all}
   AddContent(AResponse,'<button id="status" style="background-color: white; color: black; font-size: 48px; font-weight: bold; cursor: pointer; width: 100%; height: 10%" >LED is Off</button><br>');
   
   {Add our on and off buttons in Green and Red, you can see that the CSS styling has been done inline in this case, but you
    could easily include a CSS file in as part of your project and have the web server provide it when requested}
    
   {For reference here is the W3Schools info on buttons http://www.w3schools.com/tags/tag_button.asp and here is
    some basics on styling a button with CSS http://www.w3schools.com/css/css3_buttons.asp}
   AddContent(AResponse,'<button onclick="RemoteLED(''/RemoteLED?action=on'')" style="background-color: green; color: white; font-size: 96px; font-weight: bold; cursor: pointer; width: 100%; height: 45%" >Turn On</button><br>');
   AddContent(AResponse,'<button onclick="RemoteLED(''/RemoteLED?action=off'')" style="background-color: red; color: white; font-size: 96px; font-weight: bold; cursor: pointer; width: 100%; height: 45%" >Turn Off</button><br>');
   
   {Close off the div section}
   AddContent(AResponse,'</div>');
   
   {And finally send the page footer}
   AddFooter(AResponse);
  end;
end;

end.
  
