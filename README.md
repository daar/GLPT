# GLPT :: OpenGL Pascal Toolkit

An easy to use native pascal toolkit that allows to create and manage OpenGL contexts in a platform independent way.

![alt text](https://github.com/daar/GLPT/blob/master/image/simple.png "GLPT in action")

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

## Support

| Operating system | API   | Status           |
|------------------|-------|------------------|
| MS Windows       | GDI   | Complete         |
| Linux            | X11   | Almost complete  |
| Mac OSX          | Cocoa | Missing          |

> For Mac OSX only placeholder code is available, without access to a Mac this will probably remain so. Pull requests will be handled with priority.
