# **FMX-Terminal (Skia)**

**FMX-Terminal** is a high-performance terminal emulator component (VT100/ANSI) for Delphi FireMonkey, leveraging the **Skia** library for hardware-accelerated rendering.

Designed for developers who need a fast, smooth, and cross-platform terminal implementation within FMX applications.

## **🔥 Features**

* **High Performance**:  
  * Powered by **Skia** for GPU-accelerated rendering.  
  * **Batch Rendering**: Groups characters to minimize draw calls.  
  * **Dirty Lines**: Only modified lines are repainted to save resources.  
  * **Hardware Scroll**: Scrolling is implemented via bitmap shifting (Backbuffer), ensuring smoothness even with high data throughput.  
* **Compatibility**:  
  * Supports major VT100/Xterm/ANSI control sequences.  
  * Correct rendering of **Line Drawing** characters — borders in mc, htop, and ncdu look perfect.  
  * Support for 256 colors and TrueColor.  
* **Functionality**:  
  * Mouse text selection (similar to Putty/VSCode).  
  * Clipboard support (Ctrl+Shift+C / Ctrl+Shift+V).  
  * Mouse reporting support (clicks, scroll) in console applications (SGR mouse mode).  
  * Theme support (loading from .theme files).  
  * Smart Scrollback history.

## **📦 Requirements**

* **Delphi 12 Athens** (Recommended) — Skia is built-in, no additional installation required.  
* *Or* Delphi 11 Alexandria — requires **Skia4Delphi** installed via GetIt.

## **🚀 Installation**

1. Download this repository.  
2. Add the source code folder to your IDE's Library Path or place the files next to your project.  
3. Enable Skia in your project:  
   * **For Delphi 12**: In the .dpr file or initialization code, ensure GlobalUseSkia := True; is set.  
   * **For older versions**: Right-click the project \-\> Enable Skia.

## **💻 Usage**

The TnbTerminal component (descendant of TSkPaintBox) can be created dynamically or used at Design-time (if registered).

### **Runtime Creation Example**

uses  
  Terminal.Control, Terminal.Theme;

procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Create terminal  
  Terminal := TnbTerminal.Create(Self);  
  Terminal.Parent := LayoutClient;  
  Terminal.Align := TAlignLayout.Client;  
    
  // Configure font (monospaced font is required)  
  Terminal.FontFamily := 'Consolas';   
  Terminal.FontSize := 13;

  // Attach data handler (captures user input)  
  Terminal.OnData := DoTerminalData;  
end;

// Send data to server (e.g., via socket)  
procedure TFormMain.DoTerminalData(const S: string);  
begin  
  MySocket.Send(S);  
end;

// Receive data from server  
procedure TFormMain.OnSocketRead(const Data: string);  
begin  
  Terminal.WriteText(Data);  
end;

### **Theme Management**

You can load color schemes from files:

Terminal.Theme.LoadThemeFromFile('Solarized-Dark.theme');

## **🎮 Demo**

The Demo folder contains a sample application using **SecureBridge** (Devart) to connect to SSH servers.

**Note:** The demo requires SecureBridge components to compile. However, the terminal component itself is network-agnostic and can be easily used with Indy, TClientSocket, or any other library.

## **🤝 Contributing**

Contributions are welcome\! If you find a bug in ANSI parsing or want to add a new feature:

1. Fork the repository.  
2. Create a branch (git checkout \-b feature/NewFeature).  
3. Commit your changes.  
4. Create a Pull Request.

## **📄 License**

This project is licensed under the MIT License. See the [LICENSE](https://www.google.com/search?q=LICENSE) file for details.