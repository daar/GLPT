[![Build Status](https://travis-ci.org/daar/GLPT.svg?branch=master)](https://travis-ci.org/daar/GLPT)

# GLPT :: OpenGL Pascal Toolkit

An easy to use native pascal toolkit that allows to create and manage OpenGL contexts in a platform independent way. GLPT is available under the [MIT license](https://en.wikipedia.org/wiki/MIT_License).

![alt text](https://github.com/daar/GLPT/blob/master/image/simple.png "GLPT in action on Windows")
![alt text](https://github.com/daar/GLPT/blob/master/image/linux.png "GLPT in action on Linux")

## Usage
Using GLPT is easy, you only need to include the GLPT unit and create a window. Then run the event loop and do all your application stuff there. There are numerous examples included to help you to make a jump start.

    uses
      GLPT;
  
      ...

      GLPT_Init;
      window := GLPT_CreateWindow(0, 0, width, height, 'Simple example');

      while not GLPT_WindowShouldClose(window) do
      begin
        //do your application and OpenGL magic here

        GLPT_SwapBuffers(window);
        GLPT_PollEvents;
      end;

      GLPT_Terminate;

## API support
<!-- API-SUPPORT-LIST:START -->
| API                       | Linux (X11)     | Mac OSX (Cocoa) | Windows (GDI)   | 
|---------------------------|-----------------|-----------------|-----------------|
| GLPT_CreateWindow | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_DestroyWindow | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_GetDisplayCoords | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_GetFrameBufferSize | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_GetLastError | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_GetTime | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_GetVersionString | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_Init | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_MakeCurrent | <img src="https://github.com/daar/GLPT/blob/master/doc/red.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_PollEvents | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_SetCursor | <img src="https://github.com/daar/GLPT/blob/master/doc/red.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/orange.svg"> | 
| GLPT_SetErrorCallback | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_SetWindowShouldClose | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_SwapBuffers | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_Terminate | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
| GLPT_WindowShouldClose | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | <img src="https://github.com/daar/GLPT/blob/master/doc/green.svg"> | 
<!-- API-SUPPORT-LIST:END -->

## Design considerations
GLPT is planned to be a simple and easy to use library. When working on the code please consider the following; 

* flat API, so no classes and no objects
* no external dependencies are allowed, also adding FPC units should be considered carefully
* only OpenGL context handling, no audio, font or UI included to name a few

## Contributors
<!-- CONTRIBUTOR-LIST:START -->
* Darius Blaszyk (daar)
* Ryan Joseph (genericptr)
<!-- CONTRIBUTOR-LIST:END -->
