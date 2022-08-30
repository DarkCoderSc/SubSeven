<p align="center">
<img src="Assets\screenshots\banner.png"/>
</p>

# SubSeven Legacy

SubSeven Legacy is a complete remake of the infamous SubSeven Backdoor (a.k.a Sub7) remote access trojan very popular during late 90s.

You will find more information about former Sub7 Backdoor on the official [Wikipedia Page](https://en.wikipedia.org/wiki/Sub7). 

As for the former and original Sub7 program, Sub7 Legacy is entirely coded in Delphi and support the latest version of the Delphi IDE/Compiler.

However, in contrary of the original Sub7, Sub7 Legacy does not come with malicious features. 

To avoid possible misuse and respect the old look and feel, Sub7 Legacy still work as a direct connection remote access tool and is implanted as a Microsoft Windows Service (requires administrative privilege) to offer more control over the remote system.

The main goal of the project was to bring some good memories for those like me who grows with such awesome projects.  

Included Features:

-	SubSeven 2.2.X branch UX theme in pure VCL/WinAPI.
-	Pure Socket API with support of latest OpenSSL version. No extra libraries / components.
-	Multi-Threaded / Concurrency.
-	File Manager (Reactive). 
-	Process Manager.
-	Remote Terminal.
-	Windows Session Manager.
-	Run As / Create Process As (Windows Session Supported)


Finally, the project is now already considered as paused/dead since I can’t find the time to update it. I’m today very busy on other projects.

This project was made possible because of my long and boring nights between each 🍼during the first six months of my daughter so don’t be rude with me for some funny things you might find in the code (including very experimental stuff)



https://user-images.githubusercontent.com/2520298/187400911-6d8713a6-7acb-472a-b807-ba24c885cf3a.mp4



## VCL Packages / Components

I minimized the need of external components while making this project. Only two external components are required:

-	Virtual TreeView (Free and open source: https://www.jam-software.com/virtual-treeview)
-	SynEdit (Free and open source: https://github.com/SynEdit/SynEdit )


Other required components (custom components) mostly for the SubSeven UX can be found in the project:

-	SubSeven Viewer UX Components (`Viewer\VCL\*`)
-	SubSeven Tray Components (`Tray\VCL\*`)
-	Common / Shared Components (`Shared\VCL\*`)

If you are not familiar with Delphi, installing components could be quite confusing. Basically, open each components corresponding `.dpk` files then right click on solution explorer and click install. 

You must then tell Delphi Compiler where to find the source code of each components (see: https://docwiki.embarcadero.com/RADStudio/Sydney/en/Installing_Component_Packages). 

## Screen Shots of the Viewer

### Main Window (Viewer)

<p align="center">
<img src="Assets\screenshots\main.png"/>
</p>

### Main Window (Certificate Information)

<p align="center">
<img src="Assets\screenshots\main-key.png"/>
</p>

### Remote Terminal Concurrency

<p align="center">
<img src="Assets\screenshots\terminal.png"/>
</p>

### Remote File Browser Concurrency + Reactive

<p align="center">
<img src="Assets\screenshots\files.png"/>
</p>

### Remote Process List

<p align="center">
<img src="Assets\screenshots\process.png"/>
</p>

