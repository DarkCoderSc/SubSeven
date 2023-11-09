<p align="center">
<img src="Assets\screenshots\banner.png"/>
</p>

# SubSeven Legacy

"SubSeven Legacy is a complete remake of the infamous SubSeven Backdoor (also known as Sub7), a popular remote access trojan from the late 90s. More information about the original Sub7 Backdoor can be found on the official [Wikipedia page](https://en.wikipedia.org/wiki/Sub7).

Like the original Sub7 program, SubSeven Legacy is entirely coded in Delphi and supports the latest version of the Delphi IDE/Compiler. However, unlike the original Sub7, SubSeven Legacy does not include any malicious features.

To preserve the old look and feel and prevent misuse, SubSeven Legacy still functions as a direct connection remote access tool and is installed as a Microsoft Windows service (which requires administrative privileges) to provide more control over the remote system.

The main goal of this project is to bring back good memories for those who, like me, grew up with such awesome projects."

## Included Features:

<p align="center">
<img src="Assets\screenshots\features.png"/>
</p>

-	SubSeven 2.2.X branch UX theme in pure VCL/WinAPI.
-	Pure Socket API with support of latest OpenSSL version. No extra libraries / components.
-	Multi-Threaded / Concurrency.
-	File Manager (Reactive). 
-	Process Manager.
-	Remote Terminal.
-	Windows Session Manager.
-	Run As / Create Process As (Windows Session Supported)


"Unfortunately, I have not had the time to update this project in recent years and it is currently considered paused/dead. I am currently very busy with other projects.

This project was made possible during my long, sleepless nights with a newborn daughter, so please be understanding if you come across any humorous or experimental elements in the code. Thank you for your understanding."

https://user-images.githubusercontent.com/2520298/187401573-f5b3702b-15ea-4771-93eb-8b0cc20de01d.mp4



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

⚠️ **Import Notice**: The latest version of Delphi now supports HDPI for the IDE. However, when working with the SubSeven project, it is recommended to use the Delphi IDE in non-HDPI mode. The HDPI IDE is known to cause issues and negatively impact the user experience of the SubSeven project. To open and compile the project, simply disable HDPI mode in the Delphi IDE.

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

## SubSeven Art Gallery

<p align="center">
<img src="Assets\gfx\art.png"/>
</p>
