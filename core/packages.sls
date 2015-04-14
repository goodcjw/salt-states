acpi:
  pkg:
    - installed
autojump:
  pkg:
    - installed
dstat:
  pkg:
    - installed
emacs24:
  pkg:
    - installed
feh:
  pkg:
    - installed
fonts-inconsolata:
  pkg:
    - installed
gnome-panel:
  pkg:
    - installed
htop:
  pkg:
    - installed
httpie:
  pkg:
    - installed
icedtea-7-plugin:
  pkg:
    - installed
ipython:
  pkg:
    - installed
mu4e:
  pkg:
    - installed
openjdk-7-jre:
  pkg:
    - installed
postgresql-client-9.4:
  pkg:
    - installed
pyflakes:
  pkg:
    - installed
screen:
  pkg:
    - installed
suckless-tools:
  pkg:
    - installed
terminator:
  pkg:
    - installed
tree:
  pkg:
    - installed
vim:
  pkg:
    - installed
xfonts-terminus:
  pkg:
    - installed
xmobar:
  pkg:
    - installed
xmonad:
  pkg:
    - installed
zsh:
  pkg:
    - installed

google-chrome-repo:
  pkgrepo.managed:
    - human_name: Google Chrome Repo
    - name: deb http://dl.google.com/linux/chrome/deb/ stable main
    - file: /etc/apt/sources.list.d/google-chrome.list
    - key_url: https://dl-ssl.google.com/linux/linux_signing_key.pub
google-talk-repo:
  pkgrepo.managed:
    - human_name: GoogleTalk Plugin Repo
    - name: deb http://dl.google.com/linux/talkplugin/deb/ stable main
    - file: /etc/apt/sources.list.d/google-talkplugin.list
    - key_url: https://dl-ssl.google.com/linux/linux_signing_key.pub
google-packages:
  pkg.installed:
    - pkgs:
      - google-chrome-stable
      - google-talkplugin
    - require:
      - pkgrepo: google-talk-repo
      - pkgrepo: google-chrome-repo
