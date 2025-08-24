# Git Submodule Management

Update and manage git submodules in the dotfiles repository:

1. Update all submodules to their latest commits
2. Check for any submodule conflicts or issues
3. Verify that submodule paths are correct
4. Test that submodule functionality works (oh-my-zsh, dotbot, etc.)

Commands to run:
```bash
cd ~/.dotfiles
git submodule update --init --recursive
git submodule foreach git pull origin main
```

Verify these submodules are working:
- `modules/dotbot/` - Installation automation
- `modules/oh-my-zsh/` - Zsh framework
- `modules/zsh-autosuggestions/` - Zsh plugin
- `modules/zsh-syntax-highlighting/` - Zsh plugin

Check that no submodules are in detached HEAD state and all are on appropriate branches.