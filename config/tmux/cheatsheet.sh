#!/usr/bin/env bash
# tmux keybinding cheat sheet, shown in a popup via `bind ?`.
#
# Lives in its own bash script (not inline in tmux.conf) because tmux runs
# display-popup commands through the login shell — which is fish here, and fish
# does not support the `<<EOF` heredoc this used to rely on. The shebang forces
# bash, and the trailing read keeps the popup open until a key is pressed.

cat <<'EOF'
                       tmux keybindings
  ───────────────────────────────────────────────────────────
  Panes (no prefix) — i3 style
    Alt+h/j/k/l ........ focus pane (left/down/up/right)
    Alt+v .............. split vertical  (side-by-side, like i3 Mod+v)
    Alt+s .............. split horizontal (stacked, like i3 Mod+s)
    Alt+n .............. new pane (split right; legacy single-direction)
    Alt+q .............. close pane (confirms)
    Alt+f .............. fullscreen (zoom toggle)
    Alt+= / Alt+- ...... grow / shrink pane
  Panes (prefix C-b)
    " / % .............. split down / right
    z .................. zoom (fullscreen toggle)
    x .................. close pane
    H/J/K/L ............ fine resize

  Windows / tabs (no prefix)
    Alt+1..9 ........... go to window N
    Alt+[ / Alt+] ...... prev / next window
    Alt+i / Alt+o ...... move window left / right
    Alt+t .............. new window     Alt+w ... close window
  Windows (prefix C-b)
    c .................. new window     , ....... rename window

  Session (prefix C-b)
    d .................. detach         s ....... session picker
    p .................. project sessionizer
    X .................. kill session   r ....... reload config

  Copy mode (prefix C-b)
    [ .................. enter scroll/copy   v=select  y=yank

  Menus
    Space .............. action menu     ? ....... this cheatsheet
  ───────────────────────────────────────────────────────────
EOF

printf '\n            (press any key to close)'
read -rsn1
