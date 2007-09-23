from win32api import *
try:
    from winxpgui import *
except ImportError:
    from win32gui import *

import win32con
import sys, os
import cairo

def WebColour(sCol):
    #print sCol
    ic = [int(sCol[i:i+2], 16)/255.0 for i in range(1, 7, 2)]
    #print ic
    return ic

def roundedRectangle(cr,x,y,w,h,radius_x=5,radius_y=5):
    #from mono moonlight aka mono silverlight
    #test limits (without using multiplications)
    # http://graphics.stanford.edu/courses/cs248-98-fall/Final/q1.html
    ARC_TO_BEZIER = 0.55228475
    if radius_x > w - radius_x:
        radius_x = w / 2
    if radius_y > h - radius_y:
        radius_y = h / 2

    #approximate (quite close) the arc using a bezier curve
    c1 = ARC_TO_BEZIER * radius_x
    c2 = ARC_TO_BEZIER * radius_y

    cr.new_path();
    cr.move_to ( x + radius_x, y)
    cr.rel_line_to ( w - 2 * radius_x, 0.0)
    cr.rel_curve_to ( c1, 0.0, radius_x, c2, radius_x, radius_y)
    cr.rel_line_to ( 0, h - 2 * radius_y)
    cr.rel_curve_to ( 0.0, c2, c1 - radius_x, radius_y, -radius_x, radius_y)
    cr.rel_line_to ( -w + 2 * radius_x, 0)
    cr.rel_curve_to ( -c1, 0, -radius_x, -c2, -radius_x, -radius_y)
    cr.rel_line_to (0, -h + 2 * radius_y)
    cr.rel_curve_to (0.0, -c2, radius_x - c1, -radius_y, radius_x, -radius_y)
    cr.close_path ()


class MainWindow(object):
    def __init__(self):        
        message_map = {
                win32con.WM_DESTROY: self.OnDestroy,
                win32con.WM_PAINT: self.OnPaint,
                win32con.WM_SETCURSOR: self.OnCursor,
                win32con.WM_ERASEBKGND: self.OnBackgroundErase,
                win32con.WM_LBUTTONDOWN: self.OnClick,
                win32con.WM_KEYUP: self.OnKey,
                #win32con.WM_COMMAND: self.OnCommand,
                #win32con.WM_USER+20 : self.OnTaskbarNotify,
                # owner-draw related handlers.
                #win32con.WM_MEASUREITEM: self.OnMeasureItem,
                #win32con.WM_DRAWITEM: self.OnDrawItem,
        }
        # Register the Window class.
        wc = WNDCLASS()
        hinst = wc.hInstance = GetModuleHandle(None)
        self.hinst = hinst
        wc.lpszClassName = "CairoWindow"
        wc.lpfnWndProc = message_map # could also specify a wndproc.
        classAtom = RegisterClass(wc)
        # Create the Window.
        style = win32con.WS_THICKFRAME | win32con.WS_MAXIMIZEBOX | win32con.WS_MINIMIZEBOX | win32con.WS_SYSMENU | win32con.WS_VISIBLE
        self.hwnd = CreateWindow( classAtom, "Cairo is the greatest thing!", style, \
                0, 0, 550, 350, \
                0, 0, hinst, None)
        UpdateWindow(self.hwnd)


    def OnKey(self, hWnd, msg, wParam, lparam):
        if wParam == 38: #up
            print "up"
        elif wParam == 40: #down
            print "down"

    def OnClick(self, hWnd, msg, wparam, lparam):
        x = LOWORD(lparam)
        y = HIWORD(lparam) 

    def Render(self):
        try:
            InvalidateRect(self.hwnd, None, True)
            return True
        except:
            print "That didn't work"
            return False

    def OnPaint(self, hWnd, msg, wparam, lparam):   
        hdc, ps = BeginPaint(hWnd)
        rc = GetClientRect(hWnd)
        left, top, right, bottom = rc            

        width = right - left
        height = bottom - top
        x = left
        y = top 

        _buffer = CreateCompatibleDC(hdc) 
        #Double Buffer Stage 1
        hBitmap = CreateCompatibleBitmap(hdc,width,height)
        hOldBitmap = SelectObject(_buffer, hBitmap )


        surf = cairo.Win32Surface(_buffer)
        ctx = cairo.Context(surf) 

        ctx.set_source_rgb(1,1,1)
        ctx.paint()


        roundedRectangle(ctx, 50, 50, 250, 250, 10, 10)
        clr = WebColour("#F0FEE9")        
        ctx.set_source_rgb(clr[0],clr[1],clr[2])
        ctx.fill_preserve()
        clr = WebColour("#B3FF8D")        
        ctx.set_source_rgb(clr[0],clr[1],clr[2])
        ctx.stroke()        

        ctx.set_source_rgb(0,0,0)
        ctx.select_font_face("Arial",cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        ctx.set_font_size(10)    

        txt = "Cairo is the greatest thing!"

        ctx.move_to(5,10)        
        ctx.show_text(txt)

        surf.finish()

        BitBlt(hdc,0, 0, width,  height,
          _buffer, x, y, win32con.SRCCOPY) 

        SelectObject( _buffer, hOldBitmap ) 
        DeleteObject( hBitmap ) 
        DeleteDC( _buffer )               

        EndPaint(hWnd,ps)        

    def OnCursor(self, hwnd, msg, wparam, lparam):
        #IDC_HAND = 32649
        #IDC_ARROW
        cur_normal = LoadCursor(0, win32con.IDC_ARROW)
        SetCursor(cur_normal)

    def OnBackgroundErase(self, hwnd, msg, wparam, lparam):
        return False


    def OnDestroy(self, hwnd, msg, wparam, lparam):
        #nid = (self.hwnd, 0)
        #Shell_NotifyIcon(NIM_DELETE, nid)
        PostQuitMessage(0) # Terminate the app. 
        print "Exited."


w=MainWindow()
PumpMessages()
