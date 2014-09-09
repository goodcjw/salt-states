/etc/X11/xorg.conf.d/50-synaptics.conf:
  file.managed:
    - source: salt://magrathea/managed_files/50-synaptics.conf
    - user: root
    - group: root
    - mode: 644
