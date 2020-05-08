# haskell-hud-menu

Provides a way to run menubar commands through a searchable list, written in Haskell.
This project was created mostly for fun and to learn Haskell, and is not intended for 
general usage (at least quite yet).

## TODO

- Fix dbusmenu exploration algorithm
- Add support for more interfaces (org.gtk.Menus, new style GTK menus, whatever KDE/Qt uses, ...)
- Move each interface to its own module so that HudMenu module only handles rofi
- Systemd service file for AppmenuRegistrar implementation & move to own project
- Improve performance
- Improve readability
