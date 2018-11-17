// Copyright 2018 The xi-editor Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Creation and management of windows.

#![allow(non_snake_case)]

use std::any::Any;
use std::cell::{Cell, RefCell};
use std::ffi::{OsString, OsStr};
use std::mem;
use std::os::windows::ffi::OsStrExt;
use std::ptr::{null, null_mut};
use std::rc::{Rc, Weak};
use std::sync::{Arc, Mutex};
use std::ptr;
use std::os::windows::ffi::OsStringExt;

use winapi::Interface;
use winapi::ctypes::{c_int, c_void};
use winapi::shared::basetsd::*;
use winapi::shared::dxgi::*;
use winapi::shared::dxgi1_2::*;
use winapi::shared::dxgitype::*;
use winapi::shared::dxgiformat::*;
use winapi::shared::minwindef::*;
use winapi::shared::windef::{self, *};
use winapi::shared::winerror::*;
use winapi::um::d2d1::*;
use winapi::um::unknwnbase::*;
use winapi::um::wingdi::*;
use winapi::um::winnt::*;
use winapi::um::winuser::{self, *};
use winapi::um::shellapi::*;
use winapi::shared::guiddef::{GUID, IID_NULL};
use winapi::um::errhandlingapi::GetLastError;
use winapi::um::winbase::{FORMAT_MESSAGE_FROM_SYSTEM, FormatMessageW};

use Error;
use dialog::{FileDialogOptions, FileDialogType, get_file_dialog_path};
use menu::Menu;
use util::{OPTIONAL_FUNCTIONS, as_result, FromWide, ToWide};
use window::WindowHandle;
/// Builder abstraction for creating new windows.
pub struct TrayBuilder {
    title: String,
    menu: Option<Menu>,
    tray_guid: GUID,
    tray_callback: Option<Box<TrayCallback>>,
}

#[derive(Clone, Default)]
pub struct TrayHandle(Weak<WindowState>);

/// A handle that can get used to schedule an idle handler. Note that
/// this handle is thread safe. If the handle is used after the hwnd
/// has been destroyed, probably not much will go wrong (the WM_USER_RUN_IDLE
/// message may be sent to a stray window).
#[derive(Clone)]
pub struct IdleHandle {
    pub(crate) hwnd: HWND,
    queue: Arc<Mutex<Vec<Box<IdleCallback>>>>,
}

trait IdleCallback: Send {
    fn call(self: Box<Self>, a: &Any);
}

impl<F: FnOnce(&Any) + Send> IdleCallback for F {
    fn call(self: Box<F>, a: &Any) {
        (*self)(a)
    }
}

trait TrayCallback {
    fn call(&mut self, a: TrayHandle);
}

impl<F: FnMut(TrayHandle)> TrayCallback for F {
    fn call(&mut self, a: TrayHandle) {
        (*self)(a)
    }
}

struct WindowState {
    hwnd: Cell<HWND>,
    wndproc: Box<WndProc>,
    idle_queue: Arc<Mutex<Vec<Box<IdleCallback>>>>,
    tray_callback: RefCell<Option<Arc<Mutex<Option<Box<TrayCallback>>>>>>,
    tray_guid: GUID,
    tray_shown: Cell<bool>,
    title: String,
}

/// Generic handler trait for the winapi window procedure entry point.
trait WndProc {
    fn connect(&self, handle: &TrayHandle);

    fn window_proc(&self, hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM)
        -> Option<LRESULT>;
}

// State and logic for the winapi window procedure entry point.
struct MyWndProc {
    handle: RefCell<TrayHandle>,
}

/// Message indicating there are idle tasks to run.
const WM_USER_RUN_IDLE: UINT = WM_USER;
const WM_USER_NOTIFY_CALLBACK: UINT = WM_USER + 1;
const WM_USER_NOTIFY_SETUP: UINT = WM_USER + 2;

impl WndProc for MyWndProc {
    fn connect(&self, handle: &TrayHandle) {
        *self.handle.borrow_mut() = handle.clone();
    }

    fn window_proc(&self, hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM)
        -> Option<LRESULT>
    {
        //println!("wndproc msg: {}", msg);
        match msg {
            WM_CREATE => unsafe {
                if let Some(w) = self.handle.borrow().0.upgrade() {
                    setup_tray(hwnd, w.tray_guid, &w.tray_shown, &w.title);
                } else {
                    println!("WM_CREATE => WM_USER_NOTIFY_SETUP");
                    SetCoalescableTimer(hwnd, 0, 500, mem::zeroed(), TIMERV_DEFAULT_COALESCING);
                }
                Some(0)
            }
            WM_TIMER => {
                if let Some(w) = self.handle.borrow().0.upgrade() {
                    if !w.tray_shown.get() {
                        unsafe { setup_tray(hwnd, w.tray_guid, &w.tray_shown, &w.title); }
                    }
                } else {
                    panic!("Handle should be set by the time the tray is set up!");
                }
                Some(0)
            }
            WM_USER_NOTIFY_CALLBACK => {
                match LOWORD(lparam as u32) as u32 {
                    NIN_SELECT | NIN_KEYSELECT | WM_CONTEXTMENU => {
                        println!("WM_USER_NOTIFY_CALLBACK: clicked");
                        let h = self.handle.borrow();
                        let h2 = h.clone();
                        if let Some(w) = h.0.upgrade() {
                            let mutex = w.tray_callback.borrow();
                            if let Some(ref mutex) = *mutex {
                                let mut cb = mutex.lock().unwrap().take();
                                if let Some(ref mut cb) = cb {
                                    cb.call(h2);
                                }
                                *mutex.lock().unwrap() = cb;
                            }
                        }
                        Some(0)
                    }
                    _ => {
                        println!("WM_USER_NOTIFY_CALLBACK: not clicked");
                        Some(0)
                    }
                }
            }
            _ => None
        }
    }
}

impl TrayBuilder {
    pub fn new() -> TrayBuilder {
        TrayBuilder {
            title: String::new(),
            menu: None,
            tray_guid: IID_NULL,
            tray_callback: None,
        }
    }

    pub fn set_title<S: Into<String>>(&mut self, title: S) {
        self.title = title.into();
    }

    pub fn set_menu(&mut self, menu: Menu) {
        self.menu = Some(menu);
    }

    /// Enable the system tray, calling the given callback when it's clicked.
    pub fn set_tray<F: 'static + FnMut(TrayHandle)>(&mut self, func: F, tray_guid: GUID) {
        assert!(self.tray_callback.is_none());
        self.tray_callback = Some(Box::new(func));
        self.tray_guid = tray_guid;
    }

    pub fn build(self)
        -> Result<TrayHandle, Error>
    {
        unsafe {
            // Maybe separate registration in build api? Probably only need to
            // register once even for multiple window creation.

            // TODO: probably want configurable class name.
            let class_name = "Xi Tray".to_wide();
            let icon = LoadIconW(0 as HINSTANCE, IDI_APPLICATION);
            let brush = CreateSolidBrush(0xffffff);
            let wnd = WNDCLASSW {
                style: 0,
                lpfnWndProc: Some(win_proc_dispatch),
                cbClsExtra: 0,
                cbWndExtra: 0,
                hInstance: 0 as HINSTANCE,
                hIcon: icon,
                hCursor: ptr::null_mut(),
                hbrBackground: brush,
                lpszMenuName: 0 as LPCWSTR,
                lpszClassName: class_name.as_ptr(),
            };
            let class_atom = RegisterClassW(&wnd);
            if class_atom == 0 {
                return Err(Error::Null);
            }

            let wndproc = MyWndProc {
                handle: Default::default(),
            };

            let window = WindowState {
                hwnd: Cell::new(0 as HWND),
                wndproc: Box::new(wndproc),
                idle_queue: Default::default(),
                tray_callback: RefCell::new(Some(Arc::new(Mutex::new(self.tray_callback)))),
                tray_guid: self.tray_guid,
                tray_shown: Cell::new(false),
                title: self.title,
            };
            let win = Rc::new(window);
            let handle = TrayHandle(Rc::downgrade(&win));
            win.wndproc.connect(&handle);

            let hmenu = match self.menu {
                Some(menu) => menu.into_hmenu(),
                None => 0 as HMENU,
            };
            let mut dwExStyle = 0;
            let hwnd = create_window(dwExStyle, class_name.as_ptr(),
                win.title.to_wide().as_ptr(), WS_OVERLAPPEDWINDOW,
                CW_USEDEFAULT, CW_USEDEFAULT, 400, 400, HWND_MESSAGE as HWND, hmenu, 0 as HINSTANCE,
                win.clone());
            if hwnd.is_null() {
                return Err(Error::Null);
            }

            win.hwnd.set(hwnd);
            mem::drop(win);
            Ok(handle)
        }
    }
}

unsafe fn setup_tray(hwnd: HWND, guid: GUID, tray_shown: &Cell<bool>, title: &str) {
    assert!(!tray_shown.get());
    let mut u: NOTIFYICONDATAW_u = mem::zeroed();
    *u.uVersion_mut() = NOTIFYICON_VERSION_4;
    let mut n = NOTIFYICONDATAW {
        cbSize: mem::size_of::<NOTIFYICONDATAW>() as u32,
        hWnd: hwnd,
        uID: 1001,
        uFlags: NIF_ICON | NIF_MESSAGE | NIF_GUID | NIF_TIP | NIF_SHOWTIP,
        uCallbackMessage: WM_USER_NOTIFY_CALLBACK,
        hIcon: LoadIconW(ptr::null_mut(), IDI_APPLICATION),
        szTip: [0; 128],
        dwState: 0,
        dwStateMask: 0,
        szInfo: [0; 256],
        u,
        szInfoTitle: [0; 64],
        dwInfoFlags: 0,
        guidItem: guid,
        hBalloonIcon: LoadIconW(ptr::null_mut(), IDI_APPLICATION)
    };
    let tip: Vec<u16> = OsStr::new(title).encode_wide().collect();
    // assert that the tip text has space for the null terminator
    assert!(tip.len() < (n.szTip.len() - 1));
    n.szTip.split_at_mut(tip.len()).0.copy_from_slice(&tip[..]);
    let r = Shell_NotifyIconW(NIM_ADD, &mut n);
    if r == 0 {
        SetCoalescableTimer(hwnd, 0, 500, mem::zeroed(), TIMERV_DEFAULT_COALESCING);
        let mut err = vec![0u16; 1025];
        FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, ptr::null_mut(), GetLastError(),
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT) as u32, err.as_mut_ptr(), 255, ptr::null_mut());
        println!("setup_tray.NIM_ADD => retry: {:?}", OsString::from_wide(&err[..]).to_str().unwrap());
        return;
    }
    *u.uVersion_mut() = NOTIFYICON_VERSION_4;
    n.u = u;
    let v_r = Shell_NotifyIconW(NIM_SETVERSION, &mut n);
    assert!(v_r != 0);
    tray_shown.set(true);
}

unsafe extern "system" fn win_proc_dispatch(hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM)
    -> LRESULT
{
    if msg == WM_CREATE {
        let create_struct = &*(lparam as *const CREATESTRUCTW);
        let wndproc_ptr = create_struct.lpCreateParams;
        SetWindowLongPtrW(hwnd, GWLP_USERDATA, wndproc_ptr as LONG_PTR);
    }
    let window_ptr = GetWindowLongPtrW(hwnd, GWLP_USERDATA) as *const WindowState;
    let result = {
        if window_ptr.is_null() {
            None
        } else {
            (*window_ptr).wndproc.window_proc(hwnd, msg, wparam, lparam)
        }
    };
    if msg == WM_NCDESTROY {
        if !window_ptr.is_null() {
            SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0);
            mem::drop(Rc::from_raw(window_ptr));
        }
    }
    match result {
        Some(lresult) => lresult,
        None => DefWindowProcW(hwnd, msg, wparam, lparam),
    }
}

/// Create a window (same parameters as CreateWindowExW) with associated WndProc.
unsafe fn create_window(
        dwExStyle: DWORD, lpClassName: LPCWSTR, lpWindowName: LPCWSTR, dwStyle: DWORD, x: c_int,
        y: c_int, nWidth: c_int, nHeight: c_int, hWndParent: HWND, hMenu: HMENU,
        hInstance: HINSTANCE, wndproc: Rc<WindowState>) -> HWND
{
    CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, hInstance, Rc::into_raw(wndproc) as LPVOID)
}

impl TrayHandle {
    pub fn show_popout(&self, window_handle: &WindowHandle) {
        unsafe {
            if let (Some(t), Some(hwnd)) = (self.0.upgrade(), window_handle.get_hwnd()) {
                let nii = NOTIFYICONIDENTIFIER {
                    cbSize: mem::size_of::<NOTIFYICONIDENTIFIER>() as u32,
                    hWnd: mem::zeroed(),
                    uID: 10001,
                    guidItem: t.tray_guid,
                };
                let mut rc_icon = RECT{
                    left: 0,
                    top: 0,
                    right: 0,
                    bottom: 0,
                };
                let hr = Shell_NotifyIconGetRect(&nii, &mut rc_icon);
                if SUCCEEDED(hr) {
                    let pt_anchor = POINT { x: (rc_icon.left + rc_icon.right) / 2, y: (rc_icon.top + rc_icon.bottom)/2 };
                    let mut rc_window = RECT{
                        left: 0,
                        top: 0,
                        right: 0,
                        bottom: 0,
                    };
                    GetWindowRect(hwnd, &mut rc_window);
                    let mut size_window = winuser::SIZE { cx: rc_window.right - rc_window.left, cy: rc_window.bottom - rc_window.top };
                    if CalculatePopupWindowPosition(&pt_anchor, &mut size_window, TPM_VERTICAL | TPM_VCENTERALIGN | TPM_CENTERALIGN | TPM_WORKAREA, &mut rc_icon, &mut rc_window) != 0 {
                        SetWindowPos(hwnd, HWND_TOPMOST, rc_window.left, rc_window.top, 0, 0, SWP_NOSIZE | SWP_SHOWWINDOW);
                    }
                }
                UpdateWindow(hwnd);
                InvalidateRect(hwnd, null(), FALSE);
            }
        }
    }

    /// Get the raw HWND handle, for uses that are not wrapped in
    /// druid_win_shell.
    pub fn get_hwnd(&self) -> Option<HWND> {
        self.0.upgrade().map(|w| w.hwnd.get())
    }

    /// Get a handle that can be used to schedule an idle task.
    pub fn get_idle_handle(&self) -> Option<IdleHandle> {
        self.0.upgrade().map(|w|
            IdleHandle {
                hwnd: w.hwnd.get(),
                queue: w.idle_queue.clone(),
            }
        )
    }

    fn take_idle_queue(&self) -> Vec<Box<IdleCallback>> {
        if let Some(w) = self.0.upgrade() {
            mem::replace(&mut w.idle_queue.lock().unwrap(), Vec::new())
        } else {
            Vec::new()
        }
    }
}

// There is a tiny risk of things going wrong when hwnd is sent across threads.
unsafe impl Send for IdleHandle {}

impl IdleHandle {
    /// Add an idle handler, which is called (once) when the message loop
    /// is empty. The idle handler will be run from the window's wndproc,
    /// which means it won't be scheduled if the window is closed.
    pub fn add_idle<F>(&self, callback: F)
        where F: FnOnce(&Any) + Send + 'static
    {
        let mut queue = self.queue.lock().unwrap();
        if queue.is_empty() {
            unsafe {
                PostMessageW(self.hwnd, WM_USER_RUN_IDLE, 0, 0);
            }
        }
        queue.push(Box::new(callback));
    }

    fn invalidate(&self) {
        unsafe {
            InvalidateRect(self.hwnd, null(), FALSE);
        }        
    }
}
