# haskell-hud-menu

Provides a way to run menubar commands through a searchable list. This was
created as a Haskell learning project, and is not intended to be a fully
fledged and supported application. It should, however, be pretty much
functional if anyone is interested in trying it out. Note that some popular
applications (e.g. Libreoffice) do not support dbusmenu and hence will not yet
work with this application.

## Current features

- Implementation of the Appmenu Registrar to be run as a service.
- HUD style searchable menu for windows supporting dbusmenu using rofi.

## Dependencies

Named as in Arch Linux repositories and AUR. Install the corresponding
packages for your distibution.

- Haskell build tool `stack` for building the project and resolving Haskell-specific dependencies
- `appmenu-gtk-module`
- `rofi`

The following are not strictly required, but may allow more applications to work:

- `libdbusmenu-glib`
- `libdbusmenu-gtk2`
- `libdbusmenu-gtk3`

## Installation

Clone the github repository:

```shell
git clone https://github.com/sixrapid/haskell-hud-menu`
```

Move into the downloaded folder:

```shell
cd haskell-hud-menu
```

Build the project using stack:

```shell
stack build
```

Install executables to your local binary path (probably ~/.local/bin):

```shell
stack install
```

## Usage

Launch Appmenu registrar service if not already running some other implementation:

```shell
appmenu-registrar-service
```

Show the menu:

```shell
hudmenu
```

Bind `hudmenu` to the keybinding of your choice for convinience.

## Todo

- Add support for more interfaces (org.gtk.Menus, new style GTK menus, whatever KDE/Qt uses, ...).
- Systemd service file for Appmenu Registrar service.
- General refactoring for readability and/or performance.
