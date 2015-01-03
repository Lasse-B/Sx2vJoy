/*
 * * * Compile_AHK SETTINGS BEGIN * * *

[AHK2EXE]
Exe_File=%In_Dir%\Sx2vJoy 1.1.exe
No_UPX=1
[ICONS]
Icon_1=%In_Dir%\Sx2vJoy.ico

* * * Compile_AHK SETTINGS END * * *
*/

; AutoHotkey Version: 1.x
; Language:       English
; Platform:       Win 7 x86
; Author:         Mark Chong / Clive Galway
;
; Script Function:
;   Receives HID signals from 3DConnexion devices and chucks them into vJoy axes and buttons
;
; vJoy: http://vjoystick.sourceforge.net
; vJoy/AHK Lib: drop script into same folder as UJR
; AHKHID: https://github.com/jleb/AHKHID - drop this into same folder as script
; 
; original SpaceNavigator to PPJoy script by moatdd: http://www.3dconnexion.com/forum/viewtopic.php?t=4315#p33329
; vJoy integration and logarithmic acceleration by evilC: http://forums.frontier.co.uk/showpost.php?p=423584&postcount=27 & http://forums.frontier.co.uk/showpost.php?p=449012&postcount=37
; most of everything else by Lasse B.
;
; Special Thanks for being helpful testers:
; Rissen - SpaceMouse Pro
; CommanderHaggard - SpaceBall 5000 USB
; Asura - SpacePilot
; Cmdr Zok - SpaceExplorer

if not A_IsAdmin
{
   Run *RunAs "%A_ScriptFullPath%"
   ExitApp
}

version := "1.1 build 6"

#Include %A_ScriptDir%\AHKHIDLib\AHKHID.ahk ; include the HID library
Process, Priority, , High
coordmode, tooltip, screen
#SingleInstance force
#HotkeyInterval 1000
#MaxHotkeysPerInterval 1000

; ---------- vJoy init ----------
axis_list_vjoy := Array("X","Y","Z","RX","RY","RZ")
HID_USAGE_X := 0x30, HID_USAGE_Y := 0x31, HID_USAGE_Z := 0x32, HID_USAGE_RX:= 0x33, HID_USAGE_RY:= 0x34, HID_USAGE_RZ:= 0x35
hDLL := LoadPackagedLibrary()
vjoy_id := _vjoy_id()      ; The current vjoy device the app is trying to use. Also serves as a "last item selected" for the vjoy id dropdown
InitVJoy(vjoy_id) ; Whether the vjoy_id is connected and under the app's control
vJoyButtons := DllCall("vJoyInterface\GetVJDButtonNumber", "Int", vjoy_id)
VJOY_SetAxes(50, 50, 50, 50, 50, 50)
OnExit, AppQuit
; ---------- vJoy init ----------

; ---------- 3DConnexion init ----------
btnsSN := btnsSE := btnsSM := btnsSB := btnsSP := object()
AHKHID_UseConstants()
Gui, +LastFound ;Create GUI to receive messages
hGui := WinExist()
WM_INPUT := 0xFF
OnMessage(WM_INPUT, "InputMsg")
sNavHID := AHKHID_Register(1, 8, hGui, RIDEV_INPUTSINK) ; 3DConnexion
; ---------- 3DConnexion init ----------

; ---------- other init ----------
DllCall("QueryPerformanceFrequency", "Int64*", __cps) ; A non interruptive sleep method is needed. This is part of it
__cps /= 1000
logstart := 0

Menu, Tray, nostandard
Menu, Tray, add, Open Configuration GUI, gui
Menu, Tray, add
Menu, Tray, add, Open Joystick Properties, joy
Menu, Tray, add, About, about
Menu, Tray, add, Exit, AppQuit

axis_x := 1, axis_y := 2, axis_z := 3, axis_xR := 4, axis_yR := 5, axis_zR := 6, buttonlog := -1, setupmode := -1, setupmodeblind := -1, deadzone := 1
pitch := 2, curvature := 3, exponent := 4, is_throttle := 5, inc := 6, zro := 7, invert := 8, throttle_last_pos := 9, axis_suspended := 10
axis_suspend_condition := 11, axis_suspend_start := 12, wheelstate := 13, wheelstate_min = 14, wheelstate_max = 15, virt_axis_pos := 16, axis_move := 17
currentProfile := "", MsgExe := "", oldMsgExe := "", oldActiveID, oldExe

Controller_settings := object()
forcemode = 0, lastGUIprofile = "", showIfActive = 0

hotkey, ~^!s, label_setaxis
hotkey, ~^!d, label_setaxisblind
hotkey, ~^!b, label_buttonlog
hotkey, ~wheeldown, label_down
hotkey, ~wheelup, label_up
hotkey, ~mbutton, label_zero
hotkey, ~wheeldown, off
hotkey, ~wheelup, off
hotkey, ~mbutton, off

gosub, config
settimer, config, 250

;Gui +LastFound 
;hWnd := WinExist()
;DllCall("RegisterShellHookWindow", UInt, Hwnd)
;MsgNum := DllCall("RegisterWindowMessage", Str,"SHELLHOOK")
;OnMessage(MsgNum, "ShellMessage")
; ---------- other init ----------

trayTip, Sx2vJoy v%version%, Running!
return

; ==================================================================================
; HID INPUT
; ==================================================================================
InputMsg(wParam, lParam) {
   Local devh, iKey, sLabel, pointer := ""
    
   Critical
   
   devh := AHKHID_GetInputInfo(lParam, II_DEVHANDLE)

   if (devh == -1)
      return
   
   if (AHKHID_GetDevInfo(devh, DI_DEVTYPE, True) == RIM_TYPEHID) And (AHKHID_GetDevInfo(devh, DI_HID_VENDORID, True) == 1133) {
      device := AHKHID_GetDevInfo(devh, DI_HID_PRODUCTID, True)
      (device = 50721) ? pointer := "btnsSB" ; SpaceBall 5000 (USB)
      (device = 50725) ? pointer := "btnsSP" ; SpacePilot (non-Pro)
      (device = 50726) ? pointer := "btnsSN" ; SpaceNavigator
      (device = 50727) ? pointer := "btnsSE" ; SpaceExplorer
      (device = 50731) ? pointer := "btnsSM" ; SpaceMouse Pro
      
      If (AHKHID_GetInputData(lParam, uData) <> -1) {
         msg := NumGet(uData, 0, "UChar")
         if (msg == 1) { ; retrieve translational axes
            ((setupmode = 1) and (setupmodeblind = -1)) ? _setup(NumGet(uData, 1, "Short"), NumGet(uData, 3, "Short"), NumGet(uData, 5, "Short"), 1)
            ((setupmode = -1) and (setupmodeblind = 1)) ? _setupblind(NumGet(uData, 1, "Short"), NumGet(uData, 3, "Short"), NumGet(uData, 5, "Short"), 1)
            if ((setupmode = -1) and (setupmodeblind = -1))
            {
               SN_xVal_virt := _process_axis("x", NumGet(uData, 1, "Short"))
               SN_yVal_virt := _process_axis("y", NumGet(uData, 3, "Short"))
               SN_zVal_virt := _process_axis("z", NumGet(uData, 5, "Short"))
            }
         }
         else if (msg == 2) { ; retrieve rotational axes
            ((setupmode = 1) and (setupmodeblind = -1)) ? _setup(NumGet(uData, 1, "Short"), NumGet(uData, 3, "Short"), NumGet(uData, 5, "Short"), 2)
            ((setupmode = -1) and (setupmodeblind = 1)) ? _setupblind(NumGet(uData, 1, "Short"), NumGet(uData, 3, "Short"), NumGet(uData, 5, "Short"), 2)
            if ((setupmode = -1) and (setupmodeblind = -1))
            {
               SN_xRVal_virt := _process_axis("xR", NumGet(uData, 1, "Short"))
               SN_yRVal_virt := _process_axis("yR", NumGet(uData, 3, "Short"))
               SN_zRVal_virt := _process_axis("zR", NumGet(uData, 5, "Short"))
            }
         }
         else if (msg == 3) { ; retrieve buttons
            byte0 := NumGet(uData, 1, "Int")
            if (buttonlog = 1)
               _logButton(byte0, device)
            else
            {
               loops := %pointer%[0,0]
               loop, %loops%
               {
                  state := (byte0 & %pointer%[A_Index,0]) ? true : false
                  if (state <> %pointer%[A_Index,4])
                  {
                     %pointer%[A_Index,4] := state
                     (%pointer%[A_Index,1] = "k") ? Kbd_SetBtn(state,pointer,A_Index)
                     (%pointer%[A_Index,1] = "j") ? (%pointer%[A_Index,2] <= vJoyButtons) ? ? DllCall("vJoyInterface\SetBtn", "Int", state, "UInt", vjoy_id, "UChar", %pointer%[A_Index,2])
                  }
               }
            }
         }
         VJOY_SetAxes(SN_xVal_virt, SN_yVal_virt, SN_zVal_virt, SN_xRVal_virt, SN_yRVal_virt, SN_zRVal_virt)
      }
   }
}

Kbd_SetBtn(state,pointer,index) {
   global btnsSN, btnsSM, btnsSE, btnsSB, btnsSP   

   down := %pointer%[index,2]
   up := %pointer%[index,3]
   
   if (state = 1)
      sendinput {blind}%down%
   if (state = 0)
      sendinput {blind}%up%
}

VJOY_SetAxes(SNavX, SNavY, SNavZ, SNavRX, SNavRY, SNavRZ) {
   global vjoy_id, axis_list_vjoy, version
   loop, 6
   {
      ax := axis_list_vjoy[A_Index]
      
      ret := DllCall("vJoyInterface\SetAxis", "Int", 327.68 * SNav%ax%, "UInt", vjoy_id, "UInt", HID_USAGE_%ax%)
      if (!ret) {
         axis_val := 327.68 * SNav%ax%
         usage := HID_USAGE_%ax%
         msgbox,,Sx2vJoy %version%,VJOY_SetAxes`n`naxis: %ax%`nusage: %usage%`naxis value: %axis_val%`nErrorLevel: %ErrorLevel%`nReturned: %ret%
      }
   }
}

_logButton(btnID, device) {
   global logstart
   if (logstart = 1) {
      logstart := 0
      fileappend, %device%`n, Sx2vJoy.log
   }
   
   if (btnID > 0) {
      btnID := round(log(btnID) / log(2))
      fileappend, %btnID%`n, Sx2vJoy.log
   }
}

_timerinit() {
   global __PerformanceFrequency
   if not (__PerformanceFrequency > 0)
      DllCall("QueryPerformanceFrequency", "Int64*", __PerformanceFrequency)
   DllCall("QueryPerformanceCounter", "Int64*", counter)
   return counter
}

_process_axis(axis, axis_phys) {
   global axis_x, axis_y, axis_z, axis_xR, axis_yR, axis_zR, deadzone, pitch, curvature, is_throttle, invert, throttle_last_pos, Controller_settings, exponent
   
   Controller_settings[axis_%axis%,invert] = 0 ? axis_phys *= -1 ; invert
   axis_phys += 350 ; convert to 0 to 700 range from -350 to +350
   
   if (Controller_settings[axis_%axis%,is_throttle] = 0) ; if not throttle
   {
      ; deadzone
      dz := 350 / 100 * Controller_settings[axis_%axis%,deadzone]
      virt_percent := 50
      virt_percent := (axis_phys < (350-dz)) ? 50 / (350-dz) * (axis_phys-dz*0) : virt_percent ; range 0 to 350-deadzone has no deadzone
      virt_percent := (axis_phys > (350+dz)) ? 50 / (350-dz) * (axis_phys-dz*2) : virt_percent ; range 0 to 700 on the other hand has two
      
      ; pitch
      virt_percent := 50 + (50-virt_percent) * Controller_settings[axis_%axis%,pitch]
      (virt_percent > 100) ? virt_percent := 100
      (virt_percent < 0) ? virt_percent := 0

      ; logarithmic sensitivity
      virt_percent := ((virt_percent - 50) * 2) / 100
      virt_percent := (Controller_settings[axis_%axis%,curvature] * virt_percent + (1 - Controller_settings[axis_%axis%,curvature]) * virt_percent ** Controller_settings[axis_x,exponent]) * 50 + 50
   }
   else ; if throttle
   {
      _process_throttle(axis, axis_phys, Controller_settings[axis_%axis%,is_throttle])
      virt_percent := Controller_settings[axis_%axis%,throttle_last_pos]
   }

   return virt_percent 
}

_process_throttle(axis, axis_phys, method) {
   global axis_x, axis_y, axis_z, axis_xR, axis_yR, axis_zR, deadzone, linear_sensitivity, logarithmic_sensitivity, invert, __cps, inc, zro, version
   global throttle_last_pos, Controller_settings, axis_suspended, axis_suspend_condition, axis_suspend_start, wheelstate, wheelstate_max, wheelstate_min, vjoy_id, axis_list_vjoy
   
   ; throttle deadzone
   dz := 350 / 100 * Controller_settings[axis_%axis%, deadzone]
   phys_percent := 50
   phys_percent := (axis_phys < (350-dz)) ? 50 / (350-dz) * (axis_phys-dz*0) : phys_percent ; range (0 to 350-deadzone) has no deadzone
   phys_percent := (axis_phys > (350+dz)) ? 50 / (350-dz) * (axis_phys-dz*2) : phys_percent ; range (0 to 700) on the other hand has two
   
   if (method = 1)
   {
      now := _TimerInit()
      Controller_settings[axis_%axis%,axis_suspended] := now >= (Controller_settings[axis_%axis%,axis_suspend_start] + __cps * 1000) ? 0 : Controller_settings[axis_%axis%,axis_suspended]
      
      speed_mult := (phys_percent - 50) * 0.07
      (speed_mult < 0) ? speed_mult *= -1
      
      if (Controller_settings[axis_%axis%,axis_suspended] = 0)
      {
         (phys_percent > 50) ? Controller_settings[axis_%axis%,throttle_last_pos] += speed_mult
         (phys_percent < 50) ? Controller_settings[axis_%axis%,throttle_last_pos] -= speed_mult
         (Controller_settings[axis_%axis%,throttle_last_pos] < 0) ? Controller_settings[axis_%axis%,throttle_last_pos] := 0
         (Controller_settings[axis_%axis%,throttle_last_pos] > 100) ? Controller_settings[axis_%axis%,throttle_last_pos] := 100
      }
         
      if ((Controller_settings[axis_%axis%,throttle_last_pos] < (Controller_settings[axis_%axis%,zro] - 2)) or (Controller_settings[axis_%axis%,throttle_last_pos] > (Controller_settings[axis_%axis%,zro] +2)) and (Controller_settings[axis_%axis%,axis_suspend_condition] = 0))
         Controller_settings[axis_%axis%,axis_suspend_condition] := 1
         
      if ((Controller_settings[axis_%axis%,throttle_last_pos] >= (Controller_settings[axis_%axis%,zro] - 2)) and (Controller_settings[axis_%axis%,throttle_last_pos] <= (Controller_settings[axis_%axis%,zro] + 2)) and (Controller_settings[axis_%axis%,axis_suspend_condition] = 1))
      {
         Controller_settings[axis_%axis%,axis_suspended] := 1
         Controller_settings[axis_%axis%,axis_suspend_condition] := 0
         Controller_settings[axis_%axis%,axis_suspend_start] := _timerinit()
      }
   }
   
   if (method = 2)
   {
      inv := Controller_settings[axis_%axis%,invert] = 0 ? 1 : -1 ; invert
      
      Controller_settings[wheelstate] := Controller_settings[wheelstate] <= Controller_settings[axis_%axis%,wheelstate_min] ? Controller_settings[axis_%axis%,wheelstate_min] : Controller_settings[wheelstate]
      Controller_settings[wheelstate] := Controller_settings[wheelstate] >= Controller_settings[axis_%axis%,wheelstate_max] ? Controller_settings[axis_%axis%,wheelstate_max] : Controller_settings[wheelstate]
      
      state := Controller_settings[wheelstate] * inv
      axis_virt := Controller_settings[axis_%axis%,zro] + state * Controller_settings[axis_%axis%,inc]
      
      ax := axis = "x" ? axis_list_vjoy[1] : ax
      ax := axis = "y" ? axis_list_vjoy[2] : ax
      ax := axis = "z" ? axis_list_vjoy[3] : ax
      ax := axis = "xR" ? axis_list_vjoy[4] : ax
      ax := axis = "yR" ? axis_list_vjoy[5] : ax
      ax := axis = "zR" ? axis_list_vjoy[6] : ax
      
      axis_virt := axis_virt < 0 ? 0 : axis_virt
      axis_virt := axis_virt > 100 ? 100 : axis_virt
      
      Controller_settings[axis_%axis%,throttle_last_pos] := axis_virt
      ret := DllCall("vJoyInterface\SetAxis", "Int", 327.68 * axis_virt, "UInt", vjoy_id, "UInt", HID_USAGE_%ax%)
      if (!ret) {
         axis_val := 327.68 * axis_virt
         usage := HID_USAGE_%ax%
         msgbox,,Sx2vJoy %version%,_Process_Throttle`n`naxis: %ax%`nusage: %usage%`naxis value: %axis_val%`nErrorLevel: %ErrorLevel%`nReturned: %ret%
      }
   }
}

MoveAxis(ax) {
   global axis_list_vjoy, vjoy_id, ;HID_USAGE
   start := 16384
   if strlen(ax) = 2
      ax := "R" . substr(ax, 1, 1)
   loop
   {
      ;VJoy_SetAxis(start, vjoy_id, HID_USAGE_%ax%)
      DllCall("vJoyInterface\SetAxis", "Int", start, "UInt", vjoy_id, "UInt", HID_USAGE_%ax%)
      start -= 150
      if start <= 0
      {
         ;VJoy_SetAxis(16384, vjoy_id, HID_USAGE_%ax%)
         DllCall("vJoyInterface\SetAxis", "Int", 16384, "UInt", vjoy_id, "UInt", HID_USAGE_%ax%)
         break
      }
      sleep, 10
   }
}

_readAxesOrder(profile) {
   global axis_list_vjoy
   tempaxes := ""
   
   iniread, axes, config.ini, %profile%, axes order, "x,y,z,xR,yR,zR"
      
   stringreplace, axes, axes, %A_Space%,,All
   stringupper, axes, axes
   
   loop, parse, axes, `,
   {
      if (A_LoopField <> "X") and (A_LoopField <> "Y") and (A_LoopField <> "Z") and (A_LoopField <> "XR") and (A_LoopField <> "YR") and (A_LoopField <> "ZR")
      {
         msgbox, The values for the axes in config.ini contained something Sx2vJoy cannot work with.`n`nMake sure only x, y, z, xR, yR, zR are listed.
         ExitApp
      }
      
      tempaxis := A_LoopField
      if (tempaxis = "XR")
         tempaxis := "RX"
      if (tempaxis = "YR")
         tempaxis := "RY"
      if (tempaxis = "ZR")
         tempaxis := "RZ"
      
      tempaxes .= tempaxis . ","
   }
   
   stringtrimright, tempaxes, tempaxes, 1
      
   loop, parse, tempaxes, `,
      axis_list_vjoy[A_Index] := A_LoopField
}

_readBtnConfig(profile) {
   global btnsSB, btnsSE, btnsSM, btnsSN, btnsSP
   btnsSB := _BtnConfig2Array(profile, "SpaceBall 5000 (USB)")
   btnsSE := _BtnConfig2Array(profile, "SpaceExplorer")
   btnsSM := _BtnConfig2Array(profile, "SpaceMouse Pro")
   btnsSN := _BtnConfig2Array(profile, "SpaceNavigator")
   btnsSP := _BtnConfig2Array(profile, "SpacePilot")
   ;printarray(btnsSM)
}

_BtnConfig2Array(profile, device) {
   aBtns := object()
   idcount := ""
   data := _IniReadSection("config.ini", profile)
   
   Loop, parse, data, `n, `r
   {
      test := A_LoopField
      RegExMatch(A_LoopField , "i)^" . device . " id(\d{1,2}).*=.*", match)
      if (match <> "")
      {
         action := ""
         action2 := ""
         newstr := regexreplace(match, "\s", "")
         stringsplit, out, newstr, "="
         if not (strlen(out2) >= 2)
            continue
         id_ := match1
         val := out2
         
         ;msgbox, %id_%`n%val%
         
         if (substr(val, 1, 1) <> "j") and (substr(val, 1, 1) <> "k") and (substr(val, 1, 1) <> "")
         {
            msgbox Error.`n`n'%val%' is not a valid joystick button instruction.`n`nCannot continue.
            ExitApp
         }

         if (substr(val, 1, 1) = "j") ; if joystick
         {
            StringReplace, val, val, %A_Space%,,All
            action := substr(val, 3)
            if not _isnum(action)
            {
               msgbox Error.`n`n'%action%' is not a valid joystick button for the %device%.`n`nCannot continue.
               ExitApp
            }
            idcount++
            aBtns[0,0] := idcount
            aBtns[idcount,0] := 2**id_
            aBtns[idcount,1] := substr(val, 1, 1)
            aBtns[idcount,2] := action
            aBtns[idcount,3] := action2
         }
         
         if (substr(val, 1, 1) = "k") ; if keyboard
         {
            ret := _convertKeybInput(substr(val, 3))
            action := ret[1]
            action2 := ret[2]
            idcount++
            aBtns[0,0] := idcount
            aBtns[idcount,0] := 2**id_
            aBtns[idcount,1] := substr(val, 1, 1)
            aBtns[idcount,2] := action
            aBtns[idcount,3] := action2
            aBtns[idcount,4] := 0
         }
      }
   }
   ;printarray(abtns)
   return aBtns
}

_convertKeybInput(keyb) {
   press := ""
   release := ""
   stringsplit, split, keyb, `,
   Loop, %split0%
   {
      btn := split%a_index%
      press = %press%{%btn% down}
      release = {%btn% up}%release%
   }
   ret := object()
   ret[1] := press
   ret[2] := release
   return ret
}

_isNum(num) {
   if num is digit
      return 1
   return 0
}

_calcID(byref input_id) {
   output := ""
   loop, parse, input_id, `,
   {
      output .= 2** (A_LoopField) . ","
   }
   stringtrimright, output, output, 1
   input_id := output
}

_readGeneral() {
   global forcemode, lastGUIprofile, showIfActive
   iniread, forcemode, config.ini, general, forcemode
   iniread, lastGUIprofile, config.ini, general, last GUI profile
   iniread, showIfActive, config.ini, general, show if active
}

_readAxesConfig(profile) {
   global Controller_settings, axis_x, axis_y, axis_z, axis_xR, axis_yR, axis_zR, deadzone, pitch, curvature, is_throttle, inc, zro, invert, exponent
   global throttle_last_pos, axis_suspended, axis_suspend_condition, axis_suspend_start, wheelstate, wheelstate_min, wheelstate_max, virt_axis_pos, axis_move

   iniread, value, config.ini, %profile%, axis x deadzone, 10
   Controller_settings[axis_x, deadzone] := value
   iniread, value, config.ini, %profile%, axis y deadzone, 10
   Controller_settings[axis_y, deadzone] := value
   iniread, value, config.ini, %profile%, axis z deadzone, 10
   Controller_settings[axis_z, deadzone] := value
   iniread, value, config.ini, %profile%, axis xR deadzone, 10
   Controller_settings[axis_xR,deadzone] := value
   iniread, value, config.ini, %profile%, axis yR deadzone, 10
   Controller_settings[axis_yR,deadzone] := value
   iniread, value, config.ini, %profile%, axis zR deadzone, 10
   Controller_settings[axis_zR,deadzone] := value
   iniread, value, config.ini, %profile%, axis x pitch, 1
   Controller_settings[axis_x, pitch] := value
   iniread, value, config.ini, %profile%, axis y pitch, 1
   Controller_settings[axis_y, pitch] := value
   iniread, value, config.ini, %profile%, axis z pitch, 1
   Controller_settings[axis_z, pitch] := value
   iniread, value, config.ini, %profile%, axis xR pitch, 1
   Controller_settings[axis_xR,pitch] := value
   iniread, value, config.ini, %profile%, axis yR pitch, 1
   Controller_settings[axis_yR,pitch] := value
   iniread, value, config.ini, %profile%, axis zR pitch, 1
   Controller_settings[axis_zR,pitch] := value
   iniread, value, config.ini, %profile%, axis x curvature, 1
   Controller_settings[axis_x, curvature] := _logSens(value)
   iniread, value, config.ini, %profile%, axis y curvature, 1
   Controller_settings[axis_y, curvature] := _logSens(value)
   iniread, value, config.ini, %profile%, axis z curvature, 1
   Controller_settings[axis_z, curvature] := _logSens(value)
   iniread, value, config.ini, %profile%, axis xR curvature, 1
   Controller_settings[axis_xR,curvature] := _logSens(value)
   iniread, value, config.ini, %profile%, axis yR curvature, 1
   Controller_settings[axis_yR,curvature] := _logSens(value)
   iniread, value, config.ini, %profile%, axis zR curvature, 1
   Controller_settings[axis_zR,curvature] := _logSens(value)
   iniread, value, config.ini, %profile%, axis x is throttle, 0
   Controller_settings[axis_x, is_throttle] := value
   iniread, value, config.ini, %profile%, axis y is throttle, 0
   Controller_settings[axis_y, is_throttle] := value
   iniread, value, config.ini, %profile%, axis z is throttle, 0
   Controller_settings[axis_z, is_throttle] := value
   iniread, value, config.ini, %profile%, axis xR is throttle, 0
   Controller_settings[axis_xR,is_throttle] := value
   iniread, value, config.ini, %profile%, axis yR is throttle, 0
   Controller_settings[axis_yR,is_throttle] := value
   iniread, value, config.ini, %profile%, axis zR is throttle, 0
   Controller_settings[axis_zR,is_throttle] := value
   iniread, value, config.ini, %profile%, axis x increments, 5
   Controller_settings[axis_x, inc] := value
   iniread, value, config.ini, %profile%, axis y increments, 5
   Controller_settings[axis_y, inc] := value
   iniread, value, config.ini, %profile%, axis z increments, 5
   Controller_settings[axis_z, inc] := value
   iniread, value, config.ini, %profile%, axis xR increments, 5
   Controller_settings[axis_xR,inc] := value
   iniread, value, config.ini, %profile%, axis yR increments, 5
   Controller_settings[axis_yR,inc] := value
   iniread, value, config.ini, %profile%, axis zR increments, 5
   Controller_settings[axis_zR,inc] := value
   iniread, value, config.ini, %profile%, axis x zero, 50
   Controller_settings[axis_x, zro] := value
   iniread, value, config.ini, %profile%, axis y zero, 50
   Controller_settings[axis_y, zro] := value
   iniread, value, config.ini, %profile%, axis z zero, 50
   Controller_settings[axis_z, zro] := value
   iniread, value, config.ini, %profile%, axis xR zero, 50
   Controller_settings[axis_xR,zro] := value
   iniread, value, config.ini, %profile%, axis yR zero, 50
   Controller_settings[axis_yR,zro] := value
   iniread, value, config.ini, %profile%, axis zR zero, 50
   Controller_settings[axis_zR,zro] := value
   iniread, value, config.ini, %profile%, axis x invert, 0
   Controller_settings[axis_x, invert] := value
   iniread, value, config.ini, %profile%, axis y invert, 0
   Controller_settings[axis_y, invert] := value
   iniread, value, config.ini, %profile%, axis z invert, 0
   Controller_settings[axis_z, invert] := value
   iniread, value, config.ini, %profile%, axis xR invert, 0
   Controller_settings[axis_xR,invert] := value
   iniread, value, config.ini, %profile%, axis yR invert, 0
   Controller_settings[axis_yR,invert] := value
   iniread, value, config.ini, %profile%, axis zR invert, 0
   Controller_settings[axis_zR,invert] := value
   iniread, value, config.ini, %profile%, axis x exponent, 0
   Controller_settings[axis_x,exponent] := value
   iniread, value, config.ini, %profile%, axis y exponent, 0
   Controller_settings[axis_y,exponent] := value
   iniread, value, config.ini, %profile%, axis z exponent, 0
   Controller_settings[axis_z,exponent] := value
   iniread, value, config.ini, %profile%, axis xR exponent, 0
   Controller_settings[axis_xR,exponent] := value
   iniread, value, config.ini, %profile%, axis yR exponent, 0
   Controller_settings[axis_yR,exponent] := value
   iniread, value, config.ini, %profile%, axis zR exponent, 0
   Controller_settings[axis_zR,exponent] := value
   
   value := ""
   Controller_settings[wheelstate] := 0

   loop, 6
   {
      Controller_settings[A_Index,throttle_last_pos] := 0 
      Controller_settings[A_Index,axis_suspended] := 0
      Controller_settings[A_Index,axis_suspend_condition] := 0
      Controller_settings[A_Index,axis_timer] := 0
      Controller_settings[A_Index,axis_move] := 0
   }
   
   if (Controller_settings[axis_x, is_throttle] = 2) or (Controller_settings[axis_y, is_throttle] = 2) or (Controller_settings[axis_z, is_throttle] = 2) or (Controller_settings[axis_xR, is_throttle] = 2) or (Controller_settings[axis_yR, is_throttle] = 2) or (Controller_settings[axis_zR, is_throttle] = 2)
   {
      hotkey, ~wheeldown, on
      hotkey, ~wheelup, on
      hotkey, ~mbutton, on
   }
   else
   {
      hotkey, ~wheeldown, off
      hotkey, ~wheelup, off
      hotkey, ~mbutton, off
   }
   
   ; we have to jump through a couple of hoops to reach 0% and 100% thrust while retaining the ability to get back to zero position exactly, wherever that may be, just by scrolling the mousewheel
   loop, 6
   {
      Controller_settings[A_Index,wheelstate_max] := ceil((100 - Controller_settings[A_Index,zro]) / Controller_settings[A_Index,inc])
      Controller_settings[A_Index,wheelstate_min] := floor((0 - Controller_settings[A_Index,zro]) / Controller_settings[A_Index,inc])
      Controller_settings[A_Index,invert] <> 0 ? Controller_settings[A_Index,zro] := 100 - Controller_settings[A_Index,zro]
   }
   
   ;printarray(Controller_settings)
}

FileGetTime(fn, time="M", tC=4, tA=12, tM=20){
  If VarSetCapacity(s,342,0) && DllCall("FindClose", UInt,DllCall("FindFirstFile", str,fn, UInt,&s))
  && DllCall("FileTimeToLocalFileTime", UInt, &s+t%time%, UInt, _:=&s+318)
  && DllCall("FileTimeToSystemTime", UInt, _+0, UInt, _+=8){
   Loop 7
     out .= (n:=NumGet(_+0, A_Index*2-2, "UShort")) < 10 ? 0 n : n
   Return SubStr(out, 1, 6) SubStr(out, 9) SubStr("00" NumGet(_+0, 14, "UShort"), -2)
}}

_logSens(value) {
   if (value = 0)
      return 1
   value := ((value - 100) * -1) / 100
   return value
}

InitVJoy(vjoy_id) {
   global version
   if (vjoy_id <> 0) and (vjoy_id <> "")
      _VJoy_Close()
   
   vjoy_status := DllCall("vJoyInterface\GetVJDStatus", "UInt", vjoy_id)
   
   if (vjoy_status = 0) {
      msgbox,,Sx2vJoy %version%,Sx2vJoy already has control of vJoy ID %vjoy_id%.
   ;} else if (vjoy_status = 1) { ; this is what we ideally have, so no need to bug the user about it
   ;   msgbox,,Sx2vJoy %version%,No feeder already has control of vJoy ID %vjoy_id%.
   } else if (vjoy_status = 2) {
      msgbox,,Sx2vJoy %version%,Another feeder already has control of vJoy ID %vjoy_id%.
   }  else if (vjoy_status >= 3) {
      msgbox,,Sx2vJoy %version%,vJoy device ID %vjoy_id% does not exist or driver is down.
   }  else if (vjoy_status >= 4) {
      msgbox,,Sx2vJoy %version%,Unknown. Sorry.
   }
   if (vjoy_status <= 1) {
      DllCall("vJoyInterface\AcquireVJD", "UInt", vjoy_id)
      DllCall("vJoyInterface\ResetVJD", "UInt", vjoy_id)
      return 1
   }
}

LoadPackagedLibrary() {
   global version
   dllpath := (A_PtrSize < 8) ? "VJoyLib\x86\vJoyInterface.dll" : "VJoyLib\x64\vJoyInterface.dll"
   hDLL := DLLCall("LoadLibrary", "Str", dllpath)
   if (!hDLL)
      msgbox,,Sx2vJoy %version%,Couldn't load vJoyInterface.dll from path %dllpath%.
   return hDLL
}

_setup(x, y, z, mode) {
   VJOY_SetAxes(50, 50, 50, 50, 50, 50)
   axis := ""
   
   (mode = 1) and ((x >= (350/2)) or (x <= (-350/2))) ? axis := "x"
   (mode = 1) and ((y >= (350/2)) or (y <= (-350/2))) ? axis := "y"
   (mode = 1) and ((z >= (350/2)) or (z <= (-350/2))) ? axis := "z"
   (mode = 2) and ((x >= (350/2)) or (x <= (-350/2))) ? axis := "xR"
   (mode = 2) and ((y >= (350/2)) or (y <= (-350/2))) ? axis := "yR"
   (mode = 2) and ((z >= (350/2)) or (z <= (-350/2))) ? axis := "zR"
   
   if (axis = "")
      return
   
   sleep, 1000

   loop, 6
   {
      remain := 6 - A_Index
      if (remain = 0)
         break
      msg := "Axis recognized:   "%remain%
      msg2 := "`n`n1) Let your controller go now.`n2) Select which function to assign axis movement to.`n`n"
      msg3 := " seconds until movement"
      
      SplashImage, "", B W375 C0 x0 y0 fs12, %msg% %axis% %msg2%%remain% %msg3%
      sleep, 1000
   }
   
   SplashImage, "", B W375 C0 x0 y0 fs12, Moving...
   sleep,500
   moveaxis(axis)
   SplashImage, "", B W375 C0 x0 y0 fs12, Done.
   sleep,2500
   msg := "1) Move axis on your controller.`n2) Wait for the moved axis to appear in this tooltip.`n3) Let go of your controller. Hands off completely.`n4) In the game's controls configuration menu`, select which function to assign axis movement to.`n`nPress Ctrl+Alt+S again to exit setup mode."
   SplashImage, "", B W375 C0 x0 y0 fs12, %msg%
}

_setupblind(x, y, z, mode) {
   VJOY_SetAxes(50, 50, 50, 50, 50, 50)
   axis := ""
   (mode = 1) and ((x >= (350/2)) or (x <= (-350/2))) ? axis := "x"
   (mode = 1) and ((y >= (350/2)) or (y <= (-350/2))) ? axis := "y"
   (mode = 1) and ((z >= (350/2)) or (z <= (-350/2))) ? axis := "z"
   (mode = 2) and ((x >= (350/2)) or (x <= (-350/2))) ? axis := "xR"
   (mode = 2) and ((y >= (350/2)) or (y <= (-350/2))) ? axis := "yR"
   (mode = 2) and ((z >= (350/2)) or (z <= (-350/2))) ? axis := "zR"
   
   if (axis = "")
      return
   
   ComObjCreate("SAPI.SpVoice").Speak("2")
   sleep, 5000
   
   moveaxis(axis)
   
   sleep, 500
   
   ComObjCreate("SAPI.SpVoice").Speak("3")
   sleep, 500
   ComObjCreate("SAPI.SpVoice").Speak("1")
}

_vjoy_id() {
   global target_device, version
   vJoys := object()
   vJoys[0] := 0
   vJoy_list := ""
   
   loop, 16
   {
      if (DllCall("vJoyInterface\GetVJDStatus", "UInt", A_Index) = 1)
      {
         vJoys[0]++
         vJoys[vJoys[0]] := A_Index
      }
   }
   
   if (vJoys[0] = 0)
   {
      msgbox,,Sx2vJoy %version%,No vJoy sticks found. Cannot continue.
      ExitApp
   }
   
   target_device := vJoys[1]

   if (vJoys[0] > 1)
   {
      gui, name:new,Hwndhwnd_sx,Sx2vJoy %version%
      gui, name:add, text,w160 h15, Select the vJoy target device:
      
      num_vjoys := vJoys[0]
      loop %num_vjoys%
      {
         vJoy_list .= (A_Index <> vJoys[0]) ? vJoys[A_Index] . "|" : vJoys[A_Index]
         ;vJoy_list .= (A_Index = 1) ? "|" : "" ; preselect lowest vJoy target device
         vJoy_list .= (A_Index = vJoys[0]) ? "||" : "" ; preselect highest vJoy target device
      }
      gui, name:add, dropdownlist, x160 y2 w50 vtarget_device, %vJoy_list%
      gui, name:add, button, x85 y35 w50 Default gnamebtn, OK
      gui, name:show, AutoSize Center

      Loop
      {
         sleep, 10
         winget, out, list, ahk_id %hwnd_sx%
         if (out = 0)
            break
      }
   }
   return %target_device%
}

PrintArray(Array, Display=1, Level=0)
	{
		Global PrintArray
				
		Loop, % 4 + (Level*8)
		Tabs .= A_Space
		
		Output := "Array`r`n" . SubStr(Tabs, 5) . "(" 
		
		For Key, Value in Array
		  {
				If (IsObject(Value))
				  {
            Level++
						Value := PrintArray(Value, 0, Level)
						Level--
					}
				
				Output .= "`r`n" . Tabs . "[" . Key . "] => " . Value
			}
		
		Output .= "`r`n" . SubStr(Tabs, 5) . ")"
		
		
		If (!Display)
	  Return Output
	  
		Gui, PrintArray:+MaximizeBox +Resize
		Gui, PrintArray:Font, s9, Courier New
	  Gui, PrintArray:Add, Edit, x12 y10 w450 h350 vPrintArray ReadOnly HScroll, %Output%
    Gui, PrintArray:Show, w476 h374, PrintArray
	  Gui, PrintArray:+LastFound
	  ControlSend, , {Right}
	  WinWaitClose
    Return Output

	  PrintArrayGuiSize:
    Anchor("PrintArray", "wh")
    Return

    PrintArrayGuiClose:
    Gui, PrintArray:Destroy
    Return
	}

/*
	Function: Anchor
		Defines how controls should be automatically positioned relative to the new dimensions of a window when resized.

	Parameters:
		cl - a control HWND, associated variable name or ClassNN to operate on
		a - (optional) one or more of the anchors: 'x', 'y', 'w' (width) and 'h' (height),
			optionally followed by a relative factor, e.g. "x h0.5"
		r - (optional) true to redraw controls, recommended for GroupBox and Button types

	Examples:
> "xy" ; bounds a control to the bottom-left edge of the window
> "w0.5" ; any change in the width of the window will resize the width of the control on a 2:1 ratio
> "h" ; similar to above but directrly proportional to height

	Remarks:
		To assume the current window size for the new bounds of a control (i.e. resetting) simply omit the second and third parameters.
		However if the control had been created with DllCall() and has its own parent window,
			the container AutoHotkey created GUI must be made default with the +LastFound option prior to the call.
		For a complete example see anchor-example.ahk.

	License:
		- Version 4.60a <http://www.autohotkey.net/~Titan/#anchor>
		- Simplified BSD License <http://www.autohotkey.net/~Titan/license.txt>
*/
Anchor(i, a = "", r = false) {
	static c, cs = 12, cx = 255, cl = 0, g, gs = 8, gl = 0, gpi, gw, gh, z = 0, k = 0xffff
	If z = 0
		VarSetCapacity(g, gs * 99, 0), VarSetCapacity(c, cs * cx, 0), z := true
	If (!WinExist("ahk_id" . i)) {
		GuiControlGet, t, Hwnd, %i%
		If ErrorLevel = 0
			i := t
		Else ControlGet, i, Hwnd, , %i%
	}
	VarSetCapacity(gi, 68, 0), DllCall("GetWindowInfo", "UInt", gp := DllCall("GetParent", "UInt", i), "UInt", &gi)
		, giw := NumGet(gi, 28, "Int") - NumGet(gi, 20, "Int"), gih := NumGet(gi, 32, "Int") - NumGet(gi, 24, "Int")
	If (gp != gpi) {
		gpi := gp
		Loop, %gl%
			If (NumGet(g, cb := gs * (A_Index - 1)) == gp) {
				gw := NumGet(g, cb + 4, "Short"), gh := NumGet(g, cb + 6, "Short"), gf := 1
				Break
			}
		If (!gf)
			NumPut(gp, g, gl), NumPut(gw := giw, g, gl + 4, "Short"), NumPut(gh := gih, g, gl + 6, "Short"), gl += gs
	}
	ControlGetPos, dx, dy, dw, dh, , ahk_id %i%
	Loop, %cl%
		If (NumGet(c, cb := cs * (A_Index - 1)) == i) {
			If a =
			{
				cf = 1
				Break
			}
			giw -= gw, gih -= gh, as := 1, dx := NumGet(c, cb + 4, "Short"), dy := NumGet(c, cb + 6, "Short")
				, cw := dw, dw := NumGet(c, cb + 8, "Short"), ch := dh, dh := NumGet(c, cb + 10, "Short")
			Loop, Parse, a, xywh
				If A_Index > 1
					av := SubStr(a, as, 1), as += 1 + StrLen(A_LoopField)
						, d%av% += (InStr("yh", av) ? gih : giw) * (A_LoopField + 0 ? A_LoopField : 1)
			DllCall("SetWindowPos", "UInt", i, "Int", 0, "Int", dx, "Int", dy
				, "Int", InStr(a, "w") ? dw : cw, "Int", InStr(a, "h") ? dh : ch, "Int", 4)
			If r != 0
				DllCall("RedrawWindow", "UInt", i, "UInt", 0, "UInt", 0, "UInt", 0x0101) ; RDW_UPDATENOW | RDW_INVALIDATE
			Return
		}
	If cf != 1
		cb := cl, cl += cs
	bx := NumGet(gi, 48), by := NumGet(gi, 16, "Int") - NumGet(gi, 8, "Int") - gih - NumGet(gi, 52)
	If cf = 1
		dw -= giw - gw, dh -= gih - gh
	NumPut(i, c, cb), NumPut(dx - bx, c, cb + 4, "Short"), NumPut(dy - by, c, cb + 6, "Short")
		, NumPut(dw, c, cb + 8, "Short"), NumPut(dh, c, cb + 10, "Short")
	Return, true
}

_IniReadSection(iniFile, Section) {
   FileRead, DAT, %iniFile%
   StringReplace, DAT, DAT, `r`n, `n, All
   sStr:="[" Section "]", sPos:=InStr(DAT,sStr)+(L:=StrLen(sStr)), End:=StrLen(DAT)-sPos
   rStr := SubStr( DAT, sPos, ((E:=InStr(DAT,"`n[",0,sPos)) ? E-sPos : End) )
   Return RegExReplace( rStr, "^\n*(.*[^\n])\n*$", "$1", "m" )
}

_configMain(parameter) {
   global currentProfile, forcemode, lastGUIprofile, MsgExe, lastConfigCheck, showIfActive, oldMsgExe, oldExe
   
   currentProfile := "default"
   _readGeneral()
   if (forcemode = 1) and not (lastGUIprofile = "") ; use profile as (last) set in GUI
      currentProfile := lastGUIprofile
   else
   {
      Loop, Read, config.ini
      {
         RegExMatch(A_LoopReadLine , "^\[(.+)]", match)
         If (match1)
         {
            iniread, tempForce, config.ini, %match1%, force
            iniread, tempExtbl, config.ini, %match1%, executable
            if (tempForce = 1) ; use "always active" profile
            {
               currentProfile := match1
            }
            if (tempForce = 2) and not (tempExtbl = "") and not (tempExtbl = " ") ; switch between "specific application" profiles
            {
               ;if (tempExtbl = MsgExe)
               if (tempExtbl = oldExe)
               {
                  currentProfile := match1
               }
            }
         }
      }
   }
   
   ;oldMsgExe := MsgExe
   if (showIfActive = 1) 
      TrayTip, Sx2vJoy, Profile: %currentProfile%`nProcess: %oldExe%, 5
      ;TrayTip, Sx2vJoy, Profile: %currentProfile%`nProcess: %MsgExe%, 5
      
   _readAxesConfig(currentProfile)
   _readBtnConfig(currentProfile)
   _readAxesOrder(currentProfile)
   lastConfigCheck := A_Now . A_MSec
}

ShellMessage(wParam, lParam)
{
   global MsgExe, oldMsgExe, currentProfile
   ;If (wParam & 4) { ;HSHELL_WINDOWACTIVATED
      WinGet, MsgExe, ProcessName, ahk_id %lParam%
   ;   if not (MsgExe = "")
      if not (oldMsgExe = MsgExe)
      {
         _configMain(1)
         oldMsgExe := MsgExe
      }
   ;}
}

_activeWinCheck() {
   global oldExe
   WinGet, currentActiveID, ID, A
   WinGet, currentExe, ProcessName, ahk_id %currentActiveID%
   if not (oldExe = currentExe)
   {
      oldExe := currentExe
      ;tooltip, %currentActiveID%`n%currentExe%, 0, 0
      _configMain(1)
   }
}

_vJoy_Close() {
   DllCall("vJoyInterface\RelinquishVJD", "UInt", vjoy_id)
   DllCall("vJoyInterface\ResetVJD", "UInt", vjoy_id)
}

AppQuit:
if (vjoy_id <> 0) and (vjoy_id <> "")
   _VJoy_Close()
DLLCall("FreeLibrary", "Ptr", hDLL)
ExitApp
return

joy:
run joy.cpl
return

gui:
SxGUI := A_ScriptDir . "\Sx2vJoy Config GUI.exe"
ifnotexist, %SxGUI%
   msgbox,,Sx2vJoy %version%,Sx2vJoy Config GUI not found.
else
   run, "%SxGUI%"
return

config:
filetime := filegettime("config.ini")
_activeWinCheck()
if (filetime > lastConfigCheck)
   _configMain(0)
return

label_down:
Controller_settings[wheelstate] -= 1
axis := 0
axis := Controller_settings[axis_x, is_throttle] = 2 ? "x" : axis
axis := Controller_settings[axis_y, is_throttle] = 2 ? "y" : axis
axis := Controller_settings[axis_z, is_throttle] = 2 ? "z" : axis
axis := Controller_settings[axis_xR, is_throttle] = 2 ? "xR" : axis
axis := Controller_settings[axis_yR, is_throttle] = 2 ? "yR" : axis
axis := Controller_settings[axis_zR, is_throttle] = 2 ? "zR" : axis

if (axis <> 0)
   _process_throttle(axis, 0, 2)
return

label_up:
Controller_settings[wheelstate] += 1
axis := 0
axis := Controller_settings[axis_x, is_throttle] = 2 ? "x" : axis
axis := Controller_settings[axis_y, is_throttle] = 2 ? "y" : axis
axis := Controller_settings[axis_z, is_throttle] = 2 ? "z" : axis
axis := Controller_settings[axis_xR, is_throttle] = 2 ? "xR" : axis
axis := Controller_settings[axis_yR, is_throttle] = 2 ? "yR" : axis
axis := Controller_settings[axis_zR, is_throttle] = 2 ? "zR" : axis

if (axis <> 0)
   _process_throttle(axis, 0, 2)
return

label_zero:
Controller_settings[wheelstate] := 0
axis := 0
axis := Controller_settings[axis_x, is_throttle] = 2 ? "x" : axis
axis := Controller_settings[axis_y, is_throttle] = 2 ? "y" : axis
axis := Controller_settings[axis_z, is_throttle] = 2 ? "z" : axis
axis := Controller_settings[axis_xR, is_throttle] = 2 ? "xR" : axis
axis := Controller_settings[axis_yR, is_throttle] = 2 ? "yR" : axis
axis := Controller_settings[axis_zR, is_throttle] = 2 ? "zR" : axis

if (axis <> 0)
   _process_throttle(axis, 0, 2)
return

label_setaxis:
setupmode *= -1
if (setupmode = 1) {
   msg := "1) Move axis on your controller.`n2) Wait for the moved axis to appear in this tooltip.`n3) Let go of your controller. Hands off completely.`n4) In the game's controls configuration menu`, select which function to assign axis movement to.`n`nPress Ctrl+Alt+S again to exit setup mode."
   SplashImage, "", B W375 C0 x0 y0 fs12, %msg%
}
else {
   SplashImage, off
}
return

label_setaxisblind:
setupmodeblind *= -1
if (setupmodeblind = 1) {
   ComObjCreate("SAPI.SpVoice").Speak("1")
}
else {
   ComObjCreate("SAPI.SpVoice").Speak("4")
}
return

label_buttonlog:
buttonlog *= -1
if (buttonlog = 1) {
   filedelete, Sx2vJoy.log
   logstart := 1
   msg := "1) Press all of the buttons on your 3DConnexion device`, one after the other.`n2) Press hotkey again to exit logging mode.`n3) See Sx2vJoy.log for details."
   SplashImage, "", B W550 C0 x0 y0 fs12, %msg%
}
else {
   logstart := 0
   SplashImage, off
}
return

about:
msgbox,,Sx2vJoy, Sx2vJoy v%version% by Lasse B.
return

namebtn:
gui, submit, nohide
gui, destroy
return