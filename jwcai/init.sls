include:
  - jwcai.managed_files

jwcai:
  user.present:
    - fullname: Jiwen 'Steve' Cai
    - shell: /usr/bin/zsh
    - home: /home/jwcai
    - groups:
      - sudo
    - optional_groups:
      - docker

https://github.com/zsh-users/antigen.git:
  git.latest:
    - target: /home/jwcai/.antigen
