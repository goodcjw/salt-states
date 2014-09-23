/home/jwcai/.bashrc:
  file.managed:
    - source: salt://jwcai/managed_files/bashrc
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.bash_aliases:
  file.managed:
    - source: salt://jwcai/managed_files/bash_aliases
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.config/terminator:
  file.directory:
    - user: jwcai
    - group: jwcai
    - mode: 755
    - makedirs: True
    - recurse:
      - user
      - group
      - mode
/home/jwcai/.config/terminator/config:
  file.managed:
    - source: salt://jwcai/managed_files/terminator.conf
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.emacs:
  file.managed:
    - source: salt://jwcai/managed_files/emacs.el
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.gitconfig:
  file.managed:
    - source: salt://jwcai/managed_files/gitconfig.jinja
    - user: jwcai
    - group: jwcai
    - mode: 644
    - template: jinja
/home/jwcai/.xmobarrc:
  file.managed:
    - source: salt://jwcai/managed_files/xmobarrc
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.xmonad:
  file.directory:
    - user: jwcai
    - group: jwcai
    - mode: 755
    - makedirs: True
    - recurse:
      - user
      - group
      - mode
/home/jwcai/.xmonad/xmonad.hs:
  file.managed:
    - source: salt://jwcai/managed_files/xmonad.hs
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.Xmodmap:
  file.managed:
    - source: salt://jwcai/managed_files/xmodmap
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.screenrc:
  file.managed:
    - source: salt://jwcai/managed_files/screenrc
    - user: jwcai
    - group: jwcai
    - mode: 644
/home/jwcai/.zshrc:
  file.managed:
    - source: salt://jwcai/managed_files/zshrc
    - user: jwcai
    - group: jwcai
    - mode: 644
