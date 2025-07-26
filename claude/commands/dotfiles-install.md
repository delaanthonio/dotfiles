# Dotfiles Installation and Configuration Check

Run the dotfiles installation script and perform configuration checks:

1. Update git submodules to latest versions
2. Run the install script to create symlinks
3. Check for any configuration conflicts
4. Verify that shell configurations are properly linked
5. Test that key tools (starship, zellij, emacs) can find their configs

Use the following commands:
```bash
cd ~/.dotfiles
git submodule update --init --recursive
./install
```

Then verify the installation by checking:
- Shell configuration files are properly symlinked
- Editor configurations are accessible
- No broken symlinks exist
- All expected directories in ~/.config are present