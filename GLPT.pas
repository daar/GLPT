{ GLPT :: OpenGL Pascal Toolkit

  Copyright (c) 2018 Darius Blaszyk

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit GLPT;

{$mode objfpc}

{$IFDEF DARWIN}
  {$modeswitch advancedrecords}
  {$modeswitch objectivec2}
  {$linkframework CoreVideo}
{$ENDIF}

interface

uses
  CThreads, Classes, CTypes,
{$IFDEF MSWINDOWS}
  Windows;
{$ENDIF}
{$IFDEF LINUX}
  Linux, UnixType, X, Xlib, xutil, GLX;
{$ENDIF}
{$IFDEF DARWIN}
  FGL, SysUtils, BaseUnix, Unix, 
  IOKit, MacOSAll, CocoaAll;
{$ENDIF}

const
  //OpenGL Pascal Toolkit version
  GLPT_VERSION_MAJOR = 0;
  GLPT_VERSION_MINOR = 1;
  GLPT_VERSION_REVISION = 2;

  //mouse buttons.
  GLPT_MOUSE_BUTTON_LEFT = $00000001;
  GLPT_MOUSE_BUTTON_MIDDLE = $00000002;
  GLPT_MOUSE_BUTTON_RIGHT = $00000003;

  //mouse button state.
  GLPT_RELEASE = $00000000;
  GLPT_PRESS = $00000001;
  GLPT_REPEAT = $00000002;

  //standard cursor shapes
  GLPT_ARROW_CURSOR = $00000001;
  GLPT_IBEAM_CURSOR = $00000002;
  GLPT_CROSSHAIR_CURSOR = $00000003;
  GLPT_HAND_CURSOR = $00000004;
  GLPT_HRESIZE_CURSOR = $00000005;
  GLPT_VRESIZE_CURSOR = $00000006;

  // messages
  GLPT_MESSAGE_PAINT = 1;
  GLPT_MESSAGE_ACTIVATE = 2;
  GLPT_MESSAGE_DEACTIVATE = 3;
  GLPT_MESSAGE_KEYPRESS = 4;
  GLPT_MESSAGE_KEYRELEASE = 5;
  GLPT_MESSAGE_KEYCHAR = 6;
  GLPT_MESSAGE_MOUSEDOWN = 7;
  GLPT_MESSAGE_MOUSEUP = 8;
  GLPT_MESSAGE_MOUSEMOVE = 9;
  GLPT_MESSAGE_DOUBLECLICK = 10;
  GLPT_MESSAGE_MOUSEENTER = 11;
  GLPT_MESSAGE_MOUSEEXIT = 12;
  GLPT_MESSAGE_CLOSE = 13;
  GLPT_MESSAGE_SCROLL = 14;
  GLPT_MESSAGE_RESIZE = 15;
  GLPT_MESSAGE_MOVE = 16;
  GLPT_MESSAGE_POPUPCLOSE = 17;
  GLPT_MESSAGE_HINTTIMER = 18;
  GLPT_MESSAGE_FREEME = 19;
  GLPT_MESSAGE_DROPENTER = 20;
  GLPT_MESSAGE_DROPEXIT = 21;
  GLPT_MESSAGE_HSCROLL = 22;
  GLPT_MESSAGE_ABOUT = 23;
  // TODO: we need added/removed events and up/down for controllers
  GLPT_MESSAGE_CONTROLLER_HAT = 24;
  GLPT_MESSAGE_CONTROLLER_AXIS = 25;
  GLPT_MESSAGE_CONTROLLER_BUTTON = 26;
  GLPT_MESSAGE_USER = 50000;
  GLPT_MESSAGE_KILLME = MaxInt;

  // scancodes
  GLPT_SCANCODE_UNKNOWN = 0;
  GLPT_SCANCODE_A = 4;
  GLPT_SCANCODE_B = 5;
  GLPT_SCANCODE_C = 6;
  GLPT_SCANCODE_D = 7;
  GLPT_SCANCODE_E = 8;
  GLPT_SCANCODE_F = 9;
  GLPT_SCANCODE_G = 10;
  GLPT_SCANCODE_H = 11;
  GLPT_SCANCODE_I = 12;
  GLPT_SCANCODE_J = 13;
  GLPT_SCANCODE_K = 14;
  GLPT_SCANCODE_L = 15;
  GLPT_SCANCODE_M = 16;
  GLPT_SCANCODE_N = 17;
  GLPT_SCANCODE_O = 18;
  GLPT_SCANCODE_P = 19;
  GLPT_SCANCODE_Q = 20;
  GLPT_SCANCODE_R = 21;
  GLPT_SCANCODE_S = 22;
  GLPT_SCANCODE_T = 23;
  GLPT_SCANCODE_U = 24;
  GLPT_SCANCODE_V = 25;
  GLPT_SCANCODE_W = 26;
  GLPT_SCANCODE_X = 27;
  GLPT_SCANCODE_Y = 28;
  GLPT_SCANCODE_Z = 29;
  GLPT_SCANCODE_1 = 30;
  GLPT_SCANCODE_2 = 31;
  GLPT_SCANCODE_3 = 32;
  GLPT_SCANCODE_4 = 33;
  GLPT_SCANCODE_5 = 34;
  GLPT_SCANCODE_6 = 35;
  GLPT_SCANCODE_7 = 36;
  GLPT_SCANCODE_8 = 37;
  GLPT_SCANCODE_9 = 38;
  GLPT_SCANCODE_0 = 39;
  GLPT_SCANCODE_RETURN = 40;
  GLPT_SCANCODE_ESCAPE = 41;
  GLPT_SCANCODE_BACKSPACE = 42;
  GLPT_SCANCODE_TAB = 43;
  GLPT_SCANCODE_SPACE = 44;
  GLPT_SCANCODE_MINUS = 45;
  GLPT_SCANCODE_EQUALS = 46;
  GLPT_SCANCODE_LEFTBRACKET = 47;
  GLPT_SCANCODE_RIGHTBRACKET = 48;
  GLPT_SCANCODE_BACKSLASH = 49;
  GLPT_SCANCODE_NONUSHASH = 50;
  GLPT_SCANCODE_SEMICOLON = 51;
  GLPT_SCANCODE_APOSTROPHE = 52;
  GLPT_SCANCODE_GRAVE = 53;
  GLPT_SCANCODE_COMMA = 54;
  GLPT_SCANCODE_PERIOD = 55;
  GLPT_SCANCODE_SLASH = 56;
  GLPT_SCANCODE_CAPSLOCK = 57;
  GLPT_SCANCODE_F1 = 58;
  GLPT_SCANCODE_F2 = 59;
  GLPT_SCANCODE_F3 = 60;
  GLPT_SCANCODE_F4 = 61;
  GLPT_SCANCODE_F5 = 62;
  GLPT_SCANCODE_F6 = 63;
  GLPT_SCANCODE_F7 = 64;
  GLPT_SCANCODE_F8 = 65;
  GLPT_SCANCODE_F9 = 66;
  GLPT_SCANCODE_F10 = 67;
  GLPT_SCANCODE_F11 = 68;
  GLPT_SCANCODE_F12 = 69;
  GLPT_SCANCODE_PRINTSCREEN = 70;
  GLPT_SCANCODE_SCROLLLOCK = 71;
  GLPT_SCANCODE_PAUSE = 72;
  GLPT_SCANCODE_INSERT = 73;
  GLPT_SCANCODE_HOME = 74;
  GLPT_SCANCODE_PAGEUP = 75;
  GLPT_SCANCODE_DELETE = 76;
  GLPT_SCANCODE_END = 77;
  GLPT_SCANCODE_PAGEDOWN = 78;
  GLPT_SCANCODE_RIGHT = 79;
  GLPT_SCANCODE_LEFT = 80;
  GLPT_SCANCODE_DOWN = 81;
  GLPT_SCANCODE_UP = 82;
  GLPT_SCANCODE_NUMLOCKCLEAR = 83;
  GLPT_SCANCODE_KP_DIVIDE = 84;
  GLPT_SCANCODE_KP_MULTIPLY = 85;
  GLPT_SCANCODE_KP_MINUS = 86;
  GLPT_SCANCODE_KP_PLUS = 87;
  GLPT_SCANCODE_KP_ENTER = 88;
  GLPT_SCANCODE_KP_1 = 89;
  GLPT_SCANCODE_KP_2 = 90;
  GLPT_SCANCODE_KP_3 = 91;
  GLPT_SCANCODE_KP_4 = 92;
  GLPT_SCANCODE_KP_5 = 93;
  GLPT_SCANCODE_KP_6 = 94;
  GLPT_SCANCODE_KP_7 = 95;
  GLPT_SCANCODE_KP_8 = 96;
  GLPT_SCANCODE_KP_9 = 97;
  GLPT_SCANCODE_KP_0 = 98;
  GLPT_SCANCODE_KP_PERIOD = 99;
  GLPT_SCANCODE_NONUSBACKSLASH = 100;
  GLPT_SCANCODE_APPLICATION = 101;
  GLPT_SCANCODE_POWER = 102;
  GLPT_SCANCODE_KP_EQUALS = 103;
  GLPT_SCANCODE_F13 = 104;
  GLPT_SCANCODE_F14 = 105;
  GLPT_SCANCODE_F15 = 106;
  GLPT_SCANCODE_F16 = 107;
  GLPT_SCANCODE_F17 = 108;
  GLPT_SCANCODE_F18 = 109;
  GLPT_SCANCODE_F19 = 110;
  GLPT_SCANCODE_F20 = 111;
  GLPT_SCANCODE_F21 = 112;
  GLPT_SCANCODE_F22 = 113;
  GLPT_SCANCODE_F23 = 114;
  GLPT_SCANCODE_F24 = 115;
  GLPT_SCANCODE_EXECUTE = 116;
  GLPT_SCANCODE_HELP = 117;
  GLPT_SCANCODE_MENU = 118;
  GLPT_SCANCODE_SELECT = 119;
  GLPT_SCANCODE_STOP = 120;
  GLPT_SCANCODE_AGAIN = 121;
  GLPT_SCANCODE_UNDO = 122;
  GLPT_SCANCODE_CUT = 123;
  GLPT_SCANCODE_COPY = 124;
  GLPT_SCANCODE_PASTE = 125;
  GLPT_SCANCODE_FIND = 126;
  GLPT_SCANCODE_MUTE = 127;
  GLPT_SCANCODE_VOLUMEUP = 128;
  GLPT_SCANCODE_VOLUMEDOWN = 129;
  GLPT_SCANCODE_KP_COMMA = 133;
  GLPT_SCANCODE_KP_EQUALSAS400 = 134;
  GLPT_SCANCODE_INTERNATIONAL1 = 135;
  GLPT_SCANCODE_INTERNATIONAL2 = 136;
  GLPT_SCANCODE_INTERNATIONAL3 = 137;
  GLPT_SCANCODE_INTERNATIONAL4 = 138;
  GLPT_SCANCODE_INTERNATIONAL5 = 139;
  GLPT_SCANCODE_INTERNATIONAL6 = 140;
  GLPT_SCANCODE_INTERNATIONAL7 = 141;
  GLPT_SCANCODE_INTERNATIONAL8 = 142;
  GLPT_SCANCODE_INTERNATIONAL9 = 143;
  GLPT_SCANCODE_LANG1 = 144;
  GLPT_SCANCODE_LANG2 = 145;
  GLPT_SCANCODE_LANG3 = 146;
  GLPT_SCANCODE_LANG4 = 147;
  GLPT_SCANCODE_LANG5 = 148;
  GLPT_SCANCODE_LANG6 = 149;
  GLPT_SCANCODE_LANG7 = 150;
  GLPT_SCANCODE_LANG8 = 151;
  GLPT_SCANCODE_LANG9 = 152;
  GLPT_SCANCODE_ALTERASE = 153;
  GLPT_SCANCODE_SYSREQ = 154;
  GLPT_SCANCODE_CANCEL = 155;
  GLPT_SCANCODE_CLEAR = 156;
  GLPT_SCANCODE_PRIOR = 157;
  GLPT_SCANCODE_RETURN2 = 158;
  GLPT_SCANCODE_SEPARATOR = 159;
  GLPT_SCANCODE_OUT = 160;
  GLPT_SCANCODE_OPER = 161;
  GLPT_SCANCODE_CLEARAGAIN = 162;
  GLPT_SCANCODE_CRSEL = 163;
  GLPT_SCANCODE_EXSEL = 164;
  GLPT_SCANCODE_KP_00 = 176;
  GLPT_SCANCODE_KP_000 = 177;
  GLPT_SCANCODE_THOUSANDSSEPARATOR = 178;
  GLPT_SCANCODE_DECIMALSEPARATOR = 179;
  GLPT_SCANCODE_CURRENCYUNIT = 180;
  GLPT_SCANCODE_CURRENCYSUBUNIT = 181;
  GLPT_SCANCODE_KP_LEFTPAREN = 182;
  GLPT_SCANCODE_KP_RIGHTPAREN = 183;
  GLPT_SCANCODE_KP_LEFTBRACE = 184;
  GLPT_SCANCODE_KP_RIGHTBRACE = 185;
  GLPT_SCANCODE_KP_TAB = 186;
  GLPT_SCANCODE_KP_BACKSPACE = 187;
  GLPT_SCANCODE_KP_A = 188;
  GLPT_SCANCODE_KP_B = 189;
  GLPT_SCANCODE_KP_C = 190;
  GLPT_SCANCODE_KP_D = 191;
  GLPT_SCANCODE_KP_E = 192;
  GLPT_SCANCODE_KP_F = 193;
  GLPT_SCANCODE_KP_XOR = 194;
  GLPT_SCANCODE_KP_POWER = 195;
  GLPT_SCANCODE_KP_PERCENT = 196;
  GLPT_SCANCODE_KP_LESS = 197;
  GLPT_SCANCODE_KP_GREATER = 198;
  GLPT_SCANCODE_KP_AMPERSAND = 199;
  GLPT_SCANCODE_KP_DBLAMPERSAND = 200;
  GLPT_SCANCODE_KP_VERTICALBAR = 201;
  GLPT_SCANCODE_KP_DBLVERTICALBAR = 202;
  GLPT_SCANCODE_KP_COLON = 203;
  GLPT_SCANCODE_KP_HASH = 204;
  GLPT_SCANCODE_KP_SPACE = 205;
  GLPT_SCANCODE_KP_AT = 206;
  GLPT_SCANCODE_KP_EXCLAM = 207;
  GLPT_SCANCODE_KP_MEMSTORE = 208;
  GLPT_SCANCODE_KP_MEMRECALL = 209;
  GLPT_SCANCODE_KP_MEMCLEAR = 210;
  GLPT_SCANCODE_KP_MEMADD = 211;
  GLPT_SCANCODE_KP_MEMSUBTRACT = 212;
  GLPT_SCANCODE_KP_MEMMULTIPLY = 213;
  GLPT_SCANCODE_KP_MEMDIVIDE = 214;
  GLPT_SCANCODE_KP_PLUSMINUS = 215;
  GLPT_SCANCODE_KP_CLEAR = 216;
  GLPT_SCANCODE_KP_CLEARENTRY = 217;
  GLPT_SCANCODE_KP_BINARY = 218;
  GLPT_SCANCODE_KP_OCTAL = 219;
  GLPT_SCANCODE_KP_DECIMAL = 220;
  GLPT_SCANCODE_KP_HEXADECIMAL = 221;
  GLPT_SCANCODE_LCTRL = 224;
  GLPT_SCANCODE_LSHIFT = 225;
  GLPT_SCANCODE_LALT = 226;
  GLPT_SCANCODE_LGUI = 227;
  GLPT_SCANCODE_RCTRL = 228;
  GLPT_SCANCODE_RSHIFT = 229;
  GLPT_SCANCODE_RALT = 230;
  GLPT_SCANCODE_RGUI = 231;
  GLPT_SCANCODE_MODE = 257;
  GLPT_SCANCODE_AUDIONEXT = 258;
  GLPT_SCANCODE_AUDIOPREV = 259;
  GLPT_SCANCODE_AUDIOSTOP = 260;
  GLPT_SCANCODE_AUDIOPLAY = 261;
  GLPT_SCANCODE_AUDIOMUTE = 262;
  GLPT_SCANCODE_MEDIASELECT = 263;
  GLPT_SCANCODE_WWW = 264;
  GLPT_SCANCODE_MAIL = 265;
  GLPT_SCANCODE_CALCULATOR = 266;
  GLPT_SCANCODE_COMPUTER = 267;
  GLPT_SCANCODE_AC_SEARCH = 268;
  GLPT_SCANCODE_AC_HOME = 269;
  GLPT_SCANCODE_AC_BACK = 270;
  GLPT_SCANCODE_AC_FORWARD = 271;
  GLPT_SCANCODE_AC_STOP = 272;
  GLPT_SCANCODE_AC_REFRESH = 273;
  GLPT_SCANCODE_AC_BOOKMARKS = 274;
  GLPT_SCANCODE_BRIGHTNESSDOWN = 275;
  GLPT_SCANCODE_BRIGHTNESSUP = 276;
  GLPT_SCANCODE_DISPLAYSWITCH = 277;
  GLPT_SCANCODE_KBDILLUMTOGGLE = 278;
  GLPT_SCANCODE_KBDILLUMDOWN = 279;
  GLPT_SCANCODE_KBDILLUMUP = 280;
  GLPT_SCANCODE_EJECT = 281;
  GLPT_SCANCODE_SLEEP = 282;
  GLPT_SCANCODE_APP1 = 283;
  GLPT_SCANCODE_APP2 = 284;
  GLPT_NUM_SCANCODES = 283;

  // keycodes
  GLPT_KEY_RETURN = $D;
  GLPT_KEY_ESCAPE = $1B;
  GLPT_KEY_BACKSPACE = $8;
  GLPT_KEY_TAB = $9;
  GLPT_KEY_SPACE = Ord(' ');
  GLPT_KEY_EXCLAIM = Ord('!');
  GLPT_KEY_QUOTEDBL = Ord('"');
  GLPT_KEY_HASH = Ord('#');
  GLPT_KEY_DOLLAR = Ord('$');
  GLPT_KEY_PERCENT = Ord('%');
  GLPT_KEY_AMPERSAND = Ord('&');
  GLPT_KEY_QUOTE = Ord('''');
  GLPT_KEY_LEFTPAREN = Ord('(');
  GLPT_KEY_RIGHTPAREN = Ord(')');
  GLPT_KEY_ASTERISK = Ord('*');
  GLPT_KEY_PLUS = Ord('+');
  GLPT_KEY_COMMA = Ord(',');
  GLPT_KEY_MINUS = Ord('-');
  GLPT_KEY_PERIOD = Ord('.');
  GLPT_KEY_SLASH = Ord('/');
  GLPT_KEY_0 = Ord('0');
  GLPT_KEY_1 = Ord('1');
  GLPT_KEY_2 = Ord('2');
  GLPT_KEY_3 = Ord('3');
  GLPT_KEY_4 = Ord('4');
  GLPT_KEY_5 = Ord('5');
  GLPT_KEY_6 = Ord('6');
  GLPT_KEY_7 = Ord('7');
  GLPT_KEY_8 = Ord('8');
  GLPT_KEY_9 = Ord('9');
  GLPT_KEY_COLON = Ord(':');
  GLPT_KEY_SEMICOLON = Ord(';');
  GLPT_KEY_LESS = Ord('<');
  GLPT_KEY_EQUALS = Ord('=');
  GLPT_KEY_GREATER = Ord('>');
  GLPT_KEY_QUESTION = Ord('?');
  GLPT_KEY_AT = Ord('@');
  GLPT_KEY_LEFTBRACKET = Ord('[');
  GLPT_KEY_BACKSLASH = Ord('\');
  GLPT_KEY_RIGHTBRACKET = Ord(']');
  GLPT_KEY_CARET = Ord('^');
  GLPT_KEY_UNDERSCORE = Ord('_');
  GLPT_KEY_BACKQUOTE = Ord('`');
  GLPT_KEY_a = Ord('a');
  GLPT_KEY_b = Ord('b');
  GLPT_KEY_c = Ord('c');
  GLPT_KEY_d = Ord('d');
  GLPT_KEY_e = Ord('e');
  GLPT_KEY_f = Ord('f');
  GLPT_KEY_g = Ord('g');
  GLPT_KEY_h = Ord('h');
  GLPT_KEY_i = Ord('i');
  GLPT_KEY_j = Ord('j');
  GLPT_KEY_k = Ord('k');
  GLPT_KEY_l = Ord('l');
  GLPT_KEY_m = Ord('m');
  GLPT_KEY_n = Ord('n');
  GLPT_KEY_o = Ord('o');
  GLPT_KEY_p = Ord('p');
  GLPT_KEY_q = Ord('q');
  GLPT_KEY_r = Ord('r');
  GLPT_KEY_s = Ord('s');
  GLPT_KEY_t = Ord('t');
  GLPT_KEY_u = Ord('u');
  GLPT_KEY_v = Ord('v');
  GLPT_KEY_w = Ord('w');
  GLPT_KEY_x = Ord('x');
  GLPT_KEY_y = Ord('y');
  GLPT_KEY_z = Ord('z');
  GLPT_KEY_CAPSLOCK = $40000039;
  GLPT_KEY_F1 = $4000003A;
  GLPT_KEY_F2 = $4000003B;
  GLPT_KEY_F3 = $4000003C;
  GLPT_KEY_F4 = $4000003D;
  GLPT_KEY_F5 = $4000003E;
  GLPT_KEY_F6 = $4000003F;
  GLPT_KEY_F7 = $40000040;
  GLPT_KEY_F8 = $40000041;
  GLPT_KEY_F9 = $40000042;
  GLPT_KEY_F10 = $40000043;
  GLPT_KEY_F11 = $40000044;
  GLPT_KEY_F12 = $40000045;
  GLPT_KEY_PRINTSCREEN = $40000046;
  GLPT_KEY_SCROLLLOCK = $40000047;
  GLPT_KEY_PAUSE = $40000048;
  GLPT_KEY_INSERT = $40000049;
  GLPT_KEY_HOME = $4000004A;
  GLPT_KEY_PAGEUP = $4000004B;
  GLPT_KEY_DELETE = $7F;
  GLPT_KEY_END = $4000004D;
  GLPT_KEY_PAGEDOWN = $4000004E;
  GLPT_KEY_RIGHT = $4000004F;
  GLPT_KEY_LEFT = $40000050;
  GLPT_KEY_DOWN = $40000051;
  GLPT_KEY_UP = $40000052;
  GLPT_KEY_NUMLOCKCLEAR = $40000053;
  GLPT_KEY_KP_DIVIDE = $40000054;
  GLPT_KEY_KP_MULTIPLY = $40000055;
  GLPT_KEY_KP_MINUS = $40000056;
  GLPT_KEY_KP_PLUS = $40000057;
  GLPT_KEY_KP_ENTER = $40000058;
  GLPT_KEY_KP_1 = $40000059;
  GLPT_KEY_KP_2 = $4000005A;
  GLPT_KEY_KP_3 = $4000005B;
  GLPT_KEY_KP_4 = $4000005C;
  GLPT_KEY_KP_5 = $4000005D;
  GLPT_KEY_KP_6 = $4000005E;
  GLPT_KEY_KP_7 = $4000005F;
  GLPT_KEY_KP_8 = $40000060;
  GLPT_KEY_KP_9 = $40000061;
  GLPT_KEY_KP_0 = $40000062;
  GLPT_KEY_KP_PERIOD = $40000063;
  GLPT_KEY_APPLICATION = $40000065;
  GLPT_KEY_POWER = $40000066;
  GLPT_KEY_KP_EQUALS = $40000067;
  GLPT_KEY_F13 = $40000068;
  GLPT_KEY_F14 = $40000069;
  GLPT_KEY_F15 = $4000006A;
  GLPT_KEY_F16 = $4000006B;
  GLPT_KEY_F17 = $4000006C;
  GLPT_KEY_F18 = $4000006D;
  GLPT_KEY_F19 = $4000006E;
  GLPT_KEY_F20 = $4000006F;
  GLPT_KEY_F21 = $40000070;
  GLPT_KEY_F22 = $40000071;
  GLPT_KEY_F23 = $40000072;
  GLPT_KEY_F24 = $40000073;
  GLPT_KEY_EXECUTE = $40000074;
  GLPT_KEY_HELP = $40000075;
  GLPT_KEY_MENU = $40000076;
  GLPT_KEY_SELECT = $40000077;
  GLPT_KEY_STOP = $40000078;
  GLPT_KEY_AGAIN = $40000079;
  GLPT_KEY_UNDO = $4000007A;
  GLPT_KEY_CUT = $4000007B;
  GLPT_KEY_COPY = $4000007C;
  GLPT_KEY_PASTE = $4000007D;
  GLPT_KEY_FIND = $4000007E;
  GLPT_KEY_MUTE = $4000007F;
  GLPT_KEY_VOLUMEUP = $40000080;
  GLPT_KEY_VOLUMEDOWN = $40000081;
  GLPT_KEY_KP_COMMA = $40000085;
  GLPT_KEY_KP_EQUALSAS400 = $40000086;
  GLPT_KEY_ALTERASE = $40000099;
  GLPT_KEY_SYSREQ = $4000009A;
  GLPT_KEY_CANCEL = $4000009B;
  GLPT_KEY_CLEAR = $4000009C;
  GLPT_KEY_PRIOR = $4000009D;
  GLPT_KEY_RETURN2 = $4000009E;
  GLPT_KEY_SEPARATOR = $4000009F;
  GLPT_KEY_OUT = $400000A0;
  GLPT_KEY_OPER = $400000A1;
  GLPT_KEY_CLEARAGAIN = $400000A2;
  GLPT_KEY_CRSEL = $400000A3;
  GLPT_KEY_EXSEL = $400000A4;
  GLPT_KEY_KP_00 = $400000B0;
  GLPT_KEY_KP_000 = $400000B1;
  GLPT_KEY_THOUSANDSSEPARATOR = $400000B2;
  GLPT_KEY_DECIMALSEPARATOR = $400000B3;
  GLPT_KEY_CURRENCYUNIT = $400000B4;
  GLPT_KEY_CURRENCYSUBUNIT = $400000B5;
  GLPT_KEY_KP_LEFTPAREN = $400000B6;
  GLPT_KEY_KP_RIGHTPAREN = $400000B7;
  GLPT_KEY_KP_LEFTBRACE = $400000B8;
  GLPT_KEY_KP_RIGHTBRACE = $400000B9;
  GLPT_KEY_KP_TAB = $400000BA;
  GLPT_KEY_KP_BACKSPACE = $400000BB;
  GLPT_KEY_KP_A = $400000BC;
  GLPT_KEY_KP_B = $400000BD;
  GLPT_KEY_KP_C = $400000BE;
  GLPT_KEY_KP_D = $400000BF;
  GLPT_KEY_KP_E = $400000C0;
  GLPT_KEY_KP_F = $400000C1;
  GLPT_KEY_KP_XOR = $400000C2;
  GLPT_KEY_KP_POWER = $400000C3;
  GLPT_KEY_KP_PERCENT = $400000C4;
  GLPT_KEY_KP_LESS = $400000C5;
  GLPT_KEY_KP_GREATER = $400000C6;
  GLPT_KEY_KP_AMPERSAND = $400000C7;
  GLPT_KEY_KP_DBLAMPERSAND = $400000C8;
  GLPT_KEY_KP_VERTICALBAR = $400000C9;
  GLPT_KEY_KP_DBLVERTICALBAR = $400000CA;
  GLPT_KEY_KP_COLON = $400000CB;
  GLPT_KEY_KP_HASH = $400000CC;
  GLPT_KEY_KP_SPACE = $400000CD;
  GLPT_KEY_KP_AT = $400000CE;
  GLPT_KEY_KP_EXCLAM = $400000CF;
  GLPT_KEY_KP_MEMSTORE = $400000D0;
  GLPT_KEY_KP_MEMRECALL = $400000D1;
  GLPT_KEY_KP_MEMCLEAR = $400000D2;
  GLPT_KEY_KP_MEMADD = $400000D3;
  GLPT_KEY_KP_MEMSUBTRACT = $400000D4;
  GLPT_KEY_KP_MEMMULTIPLY = $400000D5;
  GLPT_KEY_KP_MEMDIVIDE = $400000D6;
  GLPT_KEY_KP_PLUSMINUS = $400000D7;
  GLPT_KEY_KP_CLEAR = $400000D8;
  GLPT_KEY_KP_CLEARENTRY = $400000D9;
  GLPT_KEY_KP_BINARY = $400000DA;
  GLPT_KEY_KP_OCTAL = $400000DB;
  GLPT_KEY_KP_DECIMAL = $400000DC;
  GLPT_KEY_KP_HEXADECIMAL = $400000DD;
  GLPT_KEY_LCTRL = $400000E0;
  GLPT_KEY_LSHIFT = $400000E1;
  GLPT_KEY_LALT = $400000E2;
  GLPT_KEY_LGUI = $400000E3;
  GLPT_KEY_RCTRL = $400000E4;
  GLPT_KEY_RSHIFT = $400000E5;
  GLPT_KEY_RALT = $400000E6;
  GLPT_KEY_RGUI = $400000E7;
  GLPT_KEY_MODE = $40000101;
  GLPT_KEY_AUDIONEXT = $40000102;
  GLPT_KEY_AUDIOPREV = $40000103;
  GLPT_KEY_AUDIOSTOP = $40000104;
  GLPT_KEY_AUDIOPLAY = $40000105;
  GLPT_KEY_AUDIOMUTE = $40000106;
  GLPT_KEY_MEDIASELECT = $40000107;
  GLPT_KEY_WWW = $40000108;
  GLPT_KEY_MAIL = $40000109;
  GLPT_KEY_CALCULATOR = $4000010A;
  GLPT_KEY_COMPUTER = $4000010B;
  GLPT_KEY_AC_SEARCH = $4000010C;
  GLPT_KEY_AC_HOME = $4000010D;
  GLPT_KEY_AC_BACK = $4000010E;
  GLPT_KEY_AC_FORWARD = $4000010F;
  GLPT_KEY_AC_STOP = $40000110;
  GLPT_KEY_AC_REFRESH = $40000111;
  GLPT_KEY_AC_BOOKMARKS = $40000112;
  GLPT_KEY_BRIGHTNESSDOWN = $40000113;
  GLPT_KEY_BRIGHTNESSUP = $40000114;
  GLPT_KEY_DISPLAYSWITCH = $40000115;
  GLPT_KEY_KBDILLUMTOGGLE = $40000116;
  GLPT_KEY_KBDILLUMDOWN = $40000117;
  GLPT_KEY_KBDILLUMUP = $40000118;
  GLPT_KEY_EJECT = $40000119;
  GLPT_KEY_SLEEP = $4000011A;

  // OpenGL context profile
  GLPT_CONTEXT_PROFILE_LEGACY = 1;
  GLPT_CONTEXT_PROFILE_CORE = 2;

  // window style flags
  GLPT_WINDOW_DEFAULT = 0;
  GLPT_WINDOW_BORDERLESS = $00000001;
  GLPT_WINDOW_TITLED = $00000002;
  GLPT_WINDOW_CLOSABLE = $00000004;
  GLPT_WINDOW_MINIATURIZABLE = $00000008;
  GLPT_WINDOW_RESIZABLE = $000000010;
  GLPT_WINDOW_FULLSCREEN = $000000020;

  GLPT_WINDOW_POS_CENTER = -MaxInt;

  // errors
  GLPT_ERROR_NONE = 1;
  GLPT_ERROR_PLATFORM = 2;
  GLPT_ERROR_THREADS = 3;
  GLPT_ERROR_UNKNOWN = 255;

  GLPT_ERROR_MUTEX_TIMEDOUT = 256;

type
  { Types used by standard events }
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2);

{$packset 1}
  TShiftState = set of TShiftStateEnum;
{$packset default}
  
  GLPT_Scancode = LongWord;
  GLPT_Keycode = LongWord;

  pGLPT_MessageRec = ^GLPT_MessageRec;

  GLPT_EventCallback = procedure(msg: pGLPT_MessageRec);
  GLPT_ErrorCallback = procedure(error: integer; description: string);

  GLPT_DisplayMode = packed record
    format: integer;               { pixel format }
    w: integer;                    { width }
    h: integer;                    { height }
    refresh_rate: integer;         { refresh rate (or zero for unspecified) }
  end;

  GLPT_Context = record
    colorSize: byte;
    depthSize: byte;
    doubleBuffer: boolean;
    majorVersion: byte;
    minorVersion: byte;
    profile: byte;
    stencilSize: byte;
    vsync: boolean;
    bestResolution: boolean;
  end;

  pGLPTwindow = ^GLPTwindow;

  GLPTWindow = record
    next: pGLPTwindow;        //< Next item in the window list
    prev: pGLPTwindow;        //< Previous item in the window list

    //Window settings and state

    context: GLPT_Context;
    fscreen: boolean;        //< Flag that indictes if the window is fullscreen or not
    shouldClose: boolean;    //< Flag indicating if the window should close

    //callback functions
    event_callback: GLPT_EventCallback;   //< event callback function for this window
    userData: pointer;                    //< custom pointer to user defined data
    flags: longint;

{$IFDEF MSWINDOWS}
    h_Wnd: Windows.HWND;     //< GDI window handle
    h_DC: Windows.HDC;       //< GDI device context
    h_RC: HGLRC;             //< GDI rendering context
{$ENDIF}
{$IFDEF LINUX}
    dpy: pDisplay;            //< X11 display
    Xwin: TWindow;            //< X11 window
    ctx: GLXContext;          //< X11 GLX context
    wmDeleteMessage: TAtom;   //< X11 delete mesage callback
{$ENDIF}
{$IFDEF DARWIN}
    ref: NSWindow;
    GLcontext: NSOpenGLContext;
{$ENDIF}
  end;

  GLPT_MsgParmMouse = record
    x: integer;                //< X position of the mouse
    y: integer;                //< Y position of the mouder
    deltaY: single;            //< scroll wheel delta Y
    buttons: word;             //< button state of the mouse
    shiftstate: TShiftState;   //< shift state of the keyboard
    delta: integer;            //< drag distance of mouse
    timestamp: TDateTime;      //< timestamp when event was generated
  end;

  GLPT_MsgParmKeyboard = record
    keycode: GLPT_Keycode;     
    scancode: GLPT_Scancode;
    shiftstate: TShiftState;   //< shift state of the keyboard
  end;

  GLPT_MsgParmControllerAxis = record
    which: SInt16;
    axis: UInt8;
    value: SInt16;
  end;

  GLPT_MsgParmControllerHat = record
    which: SInt16;
    hat: UInt8;
    value: SInt16;
  end;

  GLPT_MsgParmControllerButton = record
    which: UInt32;
    button: Uint8;
    state: Uint8;
  end;

  GLPT_MsgParmUser = record
    param1: pointer;   //< custom message 1
    param2: pointer;   //< custom message 2
    param3: pointer;   //< custom message 3
  end;

  GLPT_MsgParmRect = record
    top: integer;      //< top of rectangle being sent
    left: integer;     //< left of rectangle being sent
    width: integer;    //< width of rectangle being sent
    height: integer;   //< height of rectangle being sent
  end;

  pGLPT_MessageParams = ^GLPT_MessageParams;
  GLPT_MessageParams = record
    case integer of
      0: (mouse: GLPT_MsgParmMouse);             //< mouse event record
      1: (keyboard: GLPT_MsgParmKeyboard);       //< keyboard event record
      2: (rect: GLPT_MsgParmRect);               //< rectangel event record
      3: (user: GLPT_MsgParmUser);               //< used defined event record
      4: (axis: GLPT_MsgParmControllerAxis);     //< controller (axis)
      4: (hat: GLPT_MsgParmControllerHat);       //< controller (hat/joystick)
      4: (button: GLPT_MsgParmControllerButton); //< controller (button)
  end;

  GLPT_MessageRec = record
    next: pGLPT_MessageRec;       //< next item in the list
    prev: pGLPT_MessageRec;       //< previous item in the list

    win: pGLPTwindow;             //< GLPT window that has sent message
    mcode: integer;               //< message code
    params: GLPT_MessageParams;   //< event data
  end;

  GLPTRect = record
    left: longint;     //< left position of rectangle
    top: longint;      //< top position of rectangle
    right: longint;    //< right position of rectangle
    bottom: longint;   //< bottom position of rectangle
  end;

  // TODO: fix these names
  GLPT_InitFlagsEnum = (
                      GLPT_FlagGamepad
                    );
  GLPT_InitFlags = set of GLPT_InitFlagsEnum;

const
  GLPT_InitFlagsAll = [GLPT_FlagGamepad];

{
   This function returns the GLPT version as string.
   @return the GLPT version
}
function GLPT_GetVersionString: string;

{
   This function returns the last occurred error message. Please note that also the error callback function returns the
   same error message.
   @return the error string
}
function GLPT_GetLastError: string;

{
   This function returns the value of the time elapsed since GLPT was initialized.
   @return the time in seconds
}
function GLPT_GetTime: double;

{
   This function returns the value of the time elapsed (in milliseconds) since GLPT was initialized.
   @return the time in milliseconds
}
function GLPT_GetTicks: longint;

{
  Sleeps for specified number of seconds
  @param ms: number of milliseconds to sleep
}
procedure GLPT_Delay(ms: longint);

{
   Initializes the GLPT library
   @return True if successfull otherwise False
}
function GLPT_Init (flags: GLPT_InitFlags = GLPT_InitFlagsAll): boolean;

{
   Terminates the GLPT library
   @return True if successfull otherwise False
}
function GLPT_Terminate: boolean;

{
   This function checks if the internal state variable
   @param win: the window that should be checked
   @return the value of the internal state variable
}
function GLPT_WindowShouldClose(win: pGLPTwindow): boolean;

{
   This procedure terminates the main event loop
   @param win: the window that should be closed
   @param Value: the value that should be given to the internal state variable
}
procedure GLPT_SetWindowShouldClose(win: pGLPTwindow; Value: boolean);

{
  Returns default OpenGL context settings.
  @return filled GLPT_Context record
}
function GLPT_GetDefaultContext: GLPT_Context;

{
   Creates a window and its associated context.
   @param posx: the x position of the window
   @param posy: the y position of the window
   @param sizex: the width of the window
   @param sizey: the height of the window
   @param title: the title of the window
   @return a reference to the created window
}
function GLPT_CreateWindow(posx, posy, sizex, sizey: integer; title: PChar; context: GLPT_Context; flags: longint = GLPT_WINDOW_DEFAULT): pGLPTwindow;

{
   Destroys a window and its associated context.
   @param win: the reference to the window being destroyed
}
procedure GLPT_DestroyWindow(win: pGLPTwindow);

{
   Make the window active
   @param win: the window that should be made active
   @return True if successfull otherwise False
}
function GLPT_MakeCurrent(win: pGLPTwindow): boolean;

{
   Swaps the front and back buffers of the specified window.
   @param win: the reference to the window
}
procedure GLPT_SwapBuffers(win: pGLPTwindow);

{
   Retrieves the size of the framebuffer of the specified window.
   @param win: the reference to the window
   @param width: the width of the window
   @param height: the height of the window
}
procedure GLPT_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);

{
   Sets the error callback procedure
   @param pointer to error callback:
}
procedure GLPT_SetErrorCallback(errorCallback: GLPT_ErrorCallback);

{
   This procedure will poll for any pending events and put them in
   the eventlist. Next the eventlist is checked and if needed the event
   callback function is called.
}
procedure GLPT_PollEvents;

{
   Set the cursor to a predefined one
   @param cursor: the index of the cursor
}
procedure GLPT_SetCursor(cursor: byte);

{
   Retrieve the display coordinate size
   @param dr: the variable holding the display coordinates
}
procedure GLPT_GetDisplayCoords(var dr: GLPTRect);

{
  Use this function to get the directory where the application was run from. 
  This is where the application data directory is.
  @return path of directory
}
function GLPT_GetBasePath: string;

{
  Use this function to get the "pref dir". This is meant to be where the application 
  can write personal files (Preferences and save games, etc.) that are specific to the application. 
  This directory is unique per user and per application.
  @param org: name of organization
  @param app: name of application
  @return path of directory
}
function GLPT_GetPrefPath (org: string; app: string): string;

{
  Converts a scancode to human-readable name
  @return name of scancode
}
function GLPT_GetScancodeName (scancode: GLPT_Scancode): string;

{
  Enables or disables vsync
  @param status: vsync status
}
procedure GLPT_SetVSync(sync: boolean);

{$i include/GLPT_Threads.inc}

implementation
uses
  GL, GLext;

{$IFDEF DARWIN}
{$i include/darwin/ptypes.inc}
{$i include/darwin/pthread.inc}
{$i include/darwin/errno.inc}
{$ENDIF}

{$i include/GLPT_Keyboard.inc}
{$i include/GLPT_Controller.inc}

type
  pLink = ^Link;

  Link = record
    next: pLink;
    prev: pLink;
  end;

  ListBase = record
    first: pLink;
    last: pLink;
  end;

  GLPT_error = record
    error: integer;
    msg: string;
  end;

var
  msglist: ListBase;

  errfunc: GLPT_ErrorCallback = nil;
  windowlist: ListBase;
  current_win: pGLPTwindow;

  inittime: double = 0;
  initticks: longint = 0;
  lasterr: GLPT_error;

//***  Error handling  *************************************************************************************************

function glptError(const error: integer; const msg: string): integer;
var
  errmsg: string;
begin
  case error of
    GLPT_ERROR_NONE: errmsg := 'GLPT_ERROR_NONE : ' + msg;
    GLPT_ERROR_PLATFORM: errmsg := 'GLPT_ERROR_PLATFORM : ' + msg;
    GLPT_ERROR_THREADS: errmsg := 'GLPT_ERROR_THREADS : ' + msg;
    GLPT_ERROR_UNKNOWN: errmsg := 'GLPT_ERROR_UNKNOWN : ' + msg;
    else
      errmsg := 'GLPT_ERROR_UNKNOWN : ' + msg;
  end;

  //store the last error
  lasterr.error := error;
  lasterr.msg := msg;

  if assigned(errfunc) then
    errfunc(error, errmsg);

  result := error;
end;

//***  Memory handling  ************************************************************************************************

function calloc(size: ptruint): pointer;
var
  itm: pointer;
begin
  GetMem(itm, size);
  FillByte(itm^, size, 0);
  exit(itm);
end;

procedure add_tail(var list: listbase; item: pLink);
begin
  item^.next := nil;
  item^.prev := list.last;

  if list.last <> nil then
    list.last^.next := item;

  if list.first = nil then
    list.first := item;

  list.last := item;
end;

procedure remove_link(var list: listbase; item: pLink);
begin
  if item^.next <> nil then
    item^.next^.prev := item^.prev;

  if item^.prev <> nil then
    item^.prev^.next := item^.next;

  if list.last = item then
    list.last := item^.prev;

  if list.first = item then
    list.first := item^.next;
end;

//***  Messaging  ******************************************************************************************************

procedure glptPostMessage(win: pGLPTwindow; MsgCode: integer; Params: GLPT_MessageParams);
var
  msg: pGLPT_MessageRec;
begin
  msg := calloc(sizeof(GLPT_MessageRec));

  msg^.win := win;

  msg^.mcode := MsgCode;
  move(Params, msg^.params, sizeof(GLPT_MessageParams));

  add_tail(msglist, pLink(msg));
end;

function glptReadLastMessage: pGLPT_MessageRec;
begin
  exit(pGLPT_MessageRec(msglist.last));
end;

function glptReadFirstMessage: pGLPT_MessageRec;
begin
  exit(pGLPT_MessageRec(msglist.first));
end;

procedure glptDeleteMessage(msg: pGLPT_MessageRec);
begin
  remove_link(msglist, pLink(msg));
  freemem(msg);
end;

//***  Native functions  ***********************************************************************************************

{$IFDEF MSWINDOWS}

{$ENDIF}
{$IFDEF LINUX}
  {$i include/GLPT_X11.inc}
{$ENDIF}
{$IFDEF DARWIN}
  {$i include/GLPT_Cocoa.inc}
{$ENDIF}

//***  Thread functions  **************************************************************************************************

function GLPT_CreateSemaphore(initial_value: integer = 0): pGLPT_Semaphore;
var
  sem: pGLPT_Semaphore;
  success: boolean;
begin
  sem := pGLPT_Semaphore(calloc(sizeof(GLPT_Semaphore)));
  {$IFDEF MSWINDOWS}
    success := gdi_CreateSemaphore(sem, initial_value);
  {$ENDIF}
  {$IFDEF LINUX}
    success := X11_CreateSemaphore(sem, initial_value);
  {$ENDIF}
  {$IFDEF DARWIN}
    success := Cocoa_CreateSemaphore(sem, initial_value);
  {$ENDIF}
  if success then
    result := sem
  else
    begin
      FreeMem(sem);
      result := nil;
    end;
end;

procedure GLPT_DestroySemaphore(var sem: pGLPT_Semaphore);
begin
  if assigned(sem) then
    begin
      {$IFDEF MSWINDOWS}
        gdi_DestroySemaphore(sem);
      {$ENDIF}
      {$IFDEF LINUX}
        X11_DestroySemaphore(sem);
      {$ENDIF}
      {$IFDEF DARWIN}
        Cocoa_DestroySemaphore(sem);
      {$ENDIF}
      FreeMem(sem);
      sem := nil;
    end;
end;

function GLPT_SemaphoreTryWait(sem: pGLPT_Semaphore): integer;
begin
  if sem = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil semaphore"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_SemaphoreTryWait(sem);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_SemaphoreTryWaitsem);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_SemaphoreTryWait(sem);
  {$ENDIF}
end;

function GLPT_SemaphoreWait(sem: pGLPT_Semaphore): integer;
begin
    if sem = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil semaphore"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_SemaphoreWait(sem);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_SemaphoreWait(sem);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_SemaphoreWait(sem);
  {$ENDIF}
end;

function GLPT_SemaphoreWaitTimeout(sem: pGLPT_Semaphore; timeout: longint): integer;
begin
  if sem = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil semaphore"');
      exit;
    end;

  if timeout = 0 then
    begin
      result := GLPT_SemaphoreTryWait(sem);
      exit;
    end
  else if timeout = -1 then
    begin
      result := GLPT_SemaphoreWait(sem);
      exit;
    end;

  {$IFDEF MSWINDOWS}
    result := gdi_SemaphoreWaitTimeout(sem, timeout);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_SemaphoreWaitTimeout(sem, timeout);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_SemaphoreWaitTimeout(sem, timeout);
  {$ENDIF}
end;

function GLPT_SemaphoreValue(sem: pGLPT_Semaphore): integer;
begin
  {$IFDEF MSWINDOWS}
    result := gdi_SemValue(sem);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_SemValue(sem);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_SemaphoreValue(sem);
  {$ENDIF}
end;

function GLPT_SemaphorePost(sem: pGLPT_Semaphore): integer;
begin
  if sem = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil semaphore"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_SemaphorePost(sem);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_SemaphorePost(sem);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_SemaphorePost(sem);
  {$ENDIF}
end;

function GLPT_CreateCondition: pGLPT_Condition;
var
  condition: pGLPT_Condition;
  success: boolean;
begin
  condition := pGLPT_Condition(calloc(sizeof(GLPT_Condition)));
  {$IFDEF MSWINDOWS}
    success := gdi_CreateCondition(condition);
  {$ENDIF}
  {$IFDEF LINUX}
    success := X11_CreateCondition(condition);
  {$ENDIF}
  {$IFDEF DARWIN}
    success := Cocoa_CreateCondition(condition);
  {$ENDIF}
  if success then
    result := condition
  else
    begin
      FreeMem(condition);
      result := nil;
    end;
end;

procedure GLPT_DestroyCondition(var condition: pGLPT_Condition);
begin
  if assigned(condition) then
    begin
      {$IFDEF MSWINDOWS}
        gdi_DestroyCondition(condition);
      {$ENDIF}
      {$IFDEF LINUX}
        X11_DestroyCondition(condition);
      {$ENDIF}
      {$IFDEF DARWIN}
        Cocoa_DestroyCondition(condition);
      {$ENDIF}
      FreeMem(condition);
      condition := nil;
    end;
end;

function GLPT_ConditionSignal(condition: pGLPT_Condition): integer;
begin
  if condition = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil condition"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_ConditionSignal(condition);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_ConditionSignal(condition);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_ConditionSignal(condition);
  {$ENDIF}
end;

function GLPT_ConditionBroadcast(condition: pGLPT_Condition): integer;
begin
  if condition = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil condition"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_ConditionBroadcast(condition);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_ConditionBroadcast(condition);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_ConditionBroadcast(condition);
  {$ENDIF}
end;

function GLPT_ConditionWaitTimeout(condition: pGLPT_Condition; mutex: pGLPT_Mutex; ms: longint): integer;
begin
  if condition = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil condition"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := Cocoa_ConditionWaitTimeout(condition, mutex, ms);
  {$ENDIF}
  {$IFDEF LINUX}
    result := Cocoa_ConditionWaitTimeout(condition, mutex, ms);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_ConditionWaitTimeout(condition, mutex, ms);
  {$ENDIF}
end;

function GLPT_ConditionWait(condition: pGLPT_Condition; mutex: pGLPT_Mutex): integer;
begin
  if condition = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil condition"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_ConditionWait(condition, mutex);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_ConditionWait(condition, mutex);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_ConditionWait(condition, mutex);
  {$ENDIF}
end;


function GLPT_CreateMutex: pGLPT_Mutex;
var
  mutex: pGLPT_Mutex;
  success: boolean;
begin
  mutex := pGLPT_Mutex(calloc(sizeof(GLPT_Mutex)));
  {$IFDEF MSWINDOWS}
    success := gdi_CreateMutex(mutex);
  {$ENDIF}
  {$IFDEF LINUX}
    success := X11_CreateMutex(mutex);
  {$ENDIF}
  {$IFDEF DARWIN}
    success := Cocoa_CreateMutex(mutex);
  {$ENDIF}
  if success then
    result := mutex
  else
    begin
      FreeMem(mutex);
      result := nil;
    end;
end;

procedure GLPT_DestroyMutex(var mutex: pGLPT_Mutex);
begin
  if assigned(mutex) then
    begin
      {$IFDEF MSWINDOWS}
        gdi_DestroyMutex(mutex);
      {$ENDIF}
      {$IFDEF LINUX}
        X11_DestroyMutex(mutex);
      {$ENDIF}
      {$IFDEF DARWIN}
        Cocoa_DestroyMutex(mutex);
      {$ENDIF}
      FreeMem(mutex);
      mutex := nil;
    end;
end;

function GLPT_LockMutex(mutex: pGLPT_Mutex): integer;
begin
  if mutex = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil mutex"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_LockMutex(mutex);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_LockMutex(mutex);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_LockMutex(mutex);
  {$ENDIF}
end;

function GLPT_UnlockMutex(mutex: pGLPT_Mutex): integer;
begin
  if mutex = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil mutex"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_UnlockMutex(mutex);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_UnlockMutex(mutex);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_UnlockMutex(mutex);
  {$ENDIF}
end;

function GLPT_TryLockMutex(mutex: pGLPT_Mutex): integer;
begin
  if mutex = nil then
    begin
      result := GLPTError(GLPT_ERROR_THREADS, 'Passed a nil mutex"');
      exit;
    end;
  {$IFDEF MSWINDOWS}
    result := gdi_TryLockMutex(mutex);
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_TryLockMutex(mutex);
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_TryLockMutex(mutex);
  {$ENDIF}
end;

function GLPT_ThreadID: GLPT_Thread_ID;
begin
  {$IFDEF MSWINDOWS}
    result := gdi_ThreadID;
  {$ENDIF}
  {$IFDEF LINUX}
    result := X11_ThreadID;
  {$ENDIF}
  {$IFDEF DARWIN}
    result := Cocoa_ThreadID;
  {$ENDIF}
end;

function GLPT_RunThread(thread: pGLPT_Thread): pointer; cdecl;
begin
  thread^.id := GLPT_ThreadID;
  thread^.status := thread^.start(thread^.userData);
  result := nil;
end;

function GLPT_CreateThread(start: GLPT_ThreadFunction; name: string = ''; userData: pointer = nil; stacksize: integer = GLPT_THREAD_DEFAULT_STACK_SIZE): pGLPT_Thread;
var
  success: boolean;
  thread: pGLPT_Thread;
begin
  thread := pGLPT_Thread(calloc(sizeof(GLPT_Thread)));
  thread^.start := start;
  thread^.name := name;
  thread^.userData := userData;
  thread^.status := -1;
  thread^.stacksize := stacksize;

  {$IFDEF MSWINDOWS}
    success := gdi_CreateThread(thread);
  {$ENDIF}
  {$IFDEF LINUX}
    success := X11_CreateThread(thread);
  {$ENDIF}
  {$IFDEF DARWIN}
    success := Cocoa_CreateThread(thread);
  {$ENDIF}

  if success then
    result := thread
  else
    begin
      FreeMem(thread);
      result := nil;
    end;
end;

procedure GLPT_DetachThread(thread: pGLPT_Thread);
begin
  if thread = nil then
    exit;
  {$IFDEF MSWINDOWS}
    gdi_DetachThread(thread);
  {$ENDIF}
  {$IFDEF LINUX}
    X11_DetachThread(thread);
  {$ENDIF}
  {$IFDEF DARWIN}
    Cocoa_DetachThread(thread);
  {$ENDIF}
end;

procedure GLPT_WaitThread(var thread: pGLPT_Thread; var status: integer);
begin
  if assigned(thread) then
    begin
      {$IFDEF MSWINDOWS}
        gdi_WaitThread(thread);
      {$ENDIF}
      {$IFDEF LINUX}
        X11_WaitThread(thread);
      {$ENDIF}
      {$IFDEF DARWIN}
        Cocoa_WaitThread(thread);
      {$ENDIF}
      status := thread^.status;
      FreeMem(thread);
      thread := nil;
    end;
end;

//***  API functions  **************************************************************************************************

function GLPT_GetVersionString: string;
var
  major, minor, revision: string;
begin
  str(GLPT_VERSION_MAJOR, major);
  str(GLPT_VERSION_MINOR, minor);
  str(GLPT_VERSION_REVISION, revision);

  exit(major + '.' + minor + '.' + revision);
end;

function GLPT_GetLastError: string;
begin
  exit(lasterr.msg);
end;

function GLPT_GetTime: double;
begin
{$IFDEF MSWINDOWS}
  exit(gdi_GetTime - inittime);
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_GetTime - inittime);
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_GetTime - inittime);
{$ENDIF}
end;

function GLPT_GetTicks: longint;
begin
  exit(Trunc(GLPT_GetTime * 1000));
end;

procedure GLPT_Delay(ms: longint);
begin
{$IFDEF MSWINDOWS}
  gdi_Delay(ms);
{$ENDIF}
{$IFDEF LINUX}
  X11_Delay(ms);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_Delay(ms);
{$ENDIF}
end;

function GLPT_Init (flags: GLPT_InitFlags = GLPT_InitFlagsAll): boolean;
begin
  //reset the last error
  lasterr.error := GLPT_ERROR_NONE;
  lasterr.msg := '';

  inittime := GLPT_GetTime;
  initticks := GLPT_GetTicks;

{$IFDEF MSWINDOWS}
  exit(gdi_Init(flags));
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_Init(flags));
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_Init(flags));
{$ENDIF}
end;

function GLPT_Terminate: boolean;
var
  win: pGLPTwindow;
begin
  //destroy any windows that are not destroyed yet
  while windowlist.first <> nil do
  begin
    win := pGLPTwindow(windowlist.last);

    GLPT_DestroyWindow(win);
  end;

{$IFDEF MSWINDOWS}
  exit(gdi_Terminate);
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_Terminate);
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_Terminate);
{$ENDIF}
end;

function GLPT_WindowShouldClose(win: pGLPTwindow): boolean;
begin
  exit(win^.shouldClose);
end;

procedure GLPT_SetWindowShouldClose(win: pGLPTwindow; value: boolean);
begin
  win^.shouldClose := value;
end;

function GLPT_GetDefaultContext: GLPT_Context;
begin
  result.colorSize := 32;
  result.depthSize := 32;
  result.doubleBuffer := true;
  result.majorVersion := 2;
  result.minorVersion := 1;
  result.profile := GLPT_CONTEXT_PROFILE_LEGACY;
  result.stencilSize := 8;
  result.vsync := true;
  result.bestResolution := false;
end;

function GLPT_CreateWindow(posx, posy, sizex, sizey: integer; title: PChar; context: GLPT_Context; flags: longint = GLPT_WINDOW_DEFAULT): pGLPTwindow;
var
  win: pGLPTwindow = nil;
  res: boolean = False;
begin
  win := calloc(sizeof(GLPTWindow));
  current_win := win;

  win^.context := context;
  win^.flags := flags;

  //do a sanity check for the context
  if (context.colorSize = 0) or
     (context.depthSize = 0) or
     (context.majorVersion = 0) or
     (context.minorVersion = 0) or
     (context.profile = 0) or
     (context.stencilSize = 0) then
  glptError(GLPT_ERROR_PLATFORM, 'GLPT_Context not setup correctly, please verify');

{$IFDEF MSWINDOWS}
  res := gdi_CreateWindow(win, posx, posy, sizex, sizey, title);
{$ENDIF}
{$IFDEF LINUX}
  res := X11_CreateWindow(win, posx, posy, sizex, sizey, title);
{$ENDIF}
{$IFDEF DARWIN}
  res := Cocoa_CreateWindow(win, posx, posy, sizex, sizey, title);
{$ENDIF}

  if res then
  begin
    add_tail(windowlist, pLink(win));
    exit(win);
  end
  else
  begin
    freemem(win);
    exit(nil);
  end;
end;

procedure GLPT_DestroyWindow(win: pGLPTwindow);
begin
{$IFDEF MSWINDOWS}
  gdi_DestroyWindow(win);
{$ENDIF}
{$IFDEF LINUX}
  X11_DestroyWindow(win);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_DestroyWindow(win);
{$ENDIF}

  //remove and free window object
  remove_link(windowlist, pLink(win));
  freemem(win);
end;

function GLPT_MakeCurrent(win: pGLPTwindow): boolean;
begin
  current_win := win;

{$IFDEF MSWINDOWS}
  exit(gdi_MakeCurrent(win));
{$ENDIF}
{$IFDEF LINUX}
  exit(X11_MakeCurrent(win));
{$ENDIF}
{$IFDEF DARWIN}
  exit(Cocoa_MakeCurrent(win));
{$ENDIF}
end;

procedure GLPT_SwapBuffers(win: pGLPTwindow);
begin
{$IFDEF MSWINDOWS}
  gdi_SwapBuffers(win);
{$ENDIF}
{$IFDEF LINUX}
  X11_SwapBuffers(win);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_SwapBuffers(win);
{$ENDIF}
end;

procedure GLPT_GetFrameBufferSize(win: pGLPTwindow; out width, height: integer);
begin
{$IFDEF MSWINDOWS}
  gdi_GetFrameBufferSize(win, width, height);
{$ENDIF}
{$IFDEF LINUX}
  X11_GetFrameBufferSize(win, width, height);
{$ENDIF}
{$IFDEF Darwin}
  Cocoa_GetFrameBufferSize(win, width, height);
{$ENDIF}
end;

procedure GLPT_SetErrorCallback(errorCallback: GLPT_ErrorCallback);
begin
  errfunc := errorCallback;
end;

procedure GLPT_PollEvents;
var
  event: pGLPT_MessageRec;
  win: pGLPTwindow;
begin
{$IFDEF MSWINDOWS}
  gdi_PollEvents;
{$ENDIF}
{$IFDEF LINUX}
  X11_PollEvents;
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_PollEvents;
{$ENDIF}

  event := glptReadFirstMessage;

  if event <> nil then
  begin
    win := event^.win;
    if win > nil then
      if win^.event_callback <> nil then
        win^.event_callback(event);

    glptDeleteMessage(event);
  end;
end;

procedure GLPT_SetCursor(cursor: byte);
begin
{$IFDEF MSWINDOWS}
  gdi_SetCursor(cursor);
{$ENDIF}
{$IFDEF LINUX}
  X11_SetCursor(cursor);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_SetCursor(cursor);
{$ENDIF}
end;

procedure GLPT_GetDisplayCoords(var dr: GLPTRect);
begin
{$IFDEF MSWINDOWS}
  gdi_GetDisplayCoords(dr);
{$ENDIF}
{$IFDEF LINUX}
  X11_GetDisplayCoords(dr);
{$ENDIF}
{$IFDEF DARWIN}
  Cocoa_GetDisplayCoords(dr);
{$ENDIF}
end;

function GLPT_GetBasePath: string;
begin
{$IFDEF MSWINDOWS}
  result := ExtractFilePath(ParamStr(0));
{$ENDIF}
{$IFDEF LINUX}
  result := ExtractFilePath(ParamStr(0));
{$ENDIF}
{$IFDEF DARWIN}
  result := Cocoa_GetBasePath;
{$ENDIF}
end;

function GLPT_GetPrefPath(org: string; app: string): string;
begin
{$IFDEF MSWINDOWS}
  result := GetAppConfigDir(False);
{$ENDIF}
{$IFDEF LINUX}
  result := GetAppConfigDir(False);
{$ENDIF}
{$IFDEF DARWIN}
  result := Cocoa_GetPrefPath(org, app);
{$ENDIF}
end;

function GLPT_GetScancodeName (scancode: GLPT_Scancode): string;
begin
  result := GLPT_scancode_names[scancode];
end;

procedure GLPT_SetVSync(sync: boolean);
begin
{$IFDEF MSWINDOWS}
  gdi_SetVSync(sync);
{$ENDIF}
{$IFDEF LINUX}
  //X11_SetVSync(sync);
{$ENDIF}
{$IFDEF DARWIN}
  //Cocoa_SetVSync(sync);
{$ENDIF}
end;

end.
