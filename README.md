<p align="center">
<img src="Assets\gfx\logo.png"/>
</p>

# SubSeven Legacy

SubSeven Legacy is a complete remake and improvement of the infamous SubSeven (a.k.a Sub7) program from late 90s.

This project was created during the first six months of my daughter (during the long nights between each 🍼; other fathers will understand). 
Today I'm very busy with more important projects and I don't feel the need and the motivation to continue this project. The project at this version contains already tons of cool stuff you might learn from.

The code is far from being perfect and tagged for production; the latest version of this project was 0.1 alpha 2 which is still a very experimental release and code. The project is very stable but the code and project environment still requires tons of optimizations / refactoring. 

Again, this project was created in only six months which a high level of tiredness. So please don't blame myself for some funny things you might see 😛

If you feel grateful, please consider to drop a ⭐ on this repository. This would be the best gift!

## Components

I minized the need for external components for this project. However two external components are required to compiled the whole project and dependencies:

- Virtual TreeView (https://www.jam-software.com/virtual-treeview)
- SynEdit (https://github.com/SynEdit/SynEdit)

Internal components can be found inside this repository:

- SubSeven Viewer (`Viewer\VCL\*`)
- SubSeven Tray (`Tray\VCL\*`)
- Common (`Shared\VCL\*`)

Open each `*.dpk` project file on your IDE for installation. Don't forget to register source path on your IDE settings otherwise, during compile time, compiler won't be able to find corresponding component source files.

## Important Notices

* ⚠️ Unlike the original SubSeven program, SubSeven Legacy does not contain any malicious code / features.
* ⚠️ The whole project is now open source, except for the server (service) at this moment. I'm waiting for some more feedback before releasing the last part of the project.
* ⚠️ If you are not familiar with Delphi environment, it might be quite difficult / confusing to compile yourself this project. I won't go too much in the detail of how to install third-parts components. I hope that the code will still teach you some cool things / tricks.

## Some notable feature

### Feature Rich

SubSeven Legacy is no basic remote-control app. This version is a full-fledged remote control software with extensive capability.

### Multi-thread

Users can perform multiple tasks at the same time without blocking other actions, with support for multiple, simultaneous connections to a single server.

### Secure

Security is part of the design, with network traffic encrypted by the latest version of OpenSSL (with TLS 1.3 AES 256 GCM SHA384) and password or pub key authentication (or both).

### Fast & Reliable

Lightweight native code for everything from the UI to networking and its own OpenSSL socket implementation bound with the API provides 100% control of client-server communication.

### Session Friendly

Unlike other remote-control software, SubSeven Legacy is session friendly, which means you can interact with distinct active Microsoft Windows Sessions.

### Swag

The retro UI design takes users back to late 90’s/early 2000s—a fitting nostalgia for a reboot of one of the most renowned remote access tools ever made.

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

