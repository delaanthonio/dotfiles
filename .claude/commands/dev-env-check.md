# Development Environment Health Check

Verify that the complete development environment is properly configured:

1. Test shell environments (zsh, fish) with all plugins loaded
2. Verify editor configurations work (Emacs, Neovim)
3. Check development tools are accessible (git-town, docker, etc.)
4. Validate PATH configurations across shells
5. Test that terminal multiplexer (zellij) works with configurations

Key areas to verify:
- Package managers: nvm, rbenv work in both shells
- Docker and container tools are accessible
- Git and git-town commands function properly
- Kubernetes tools (kubectl) are configured
- Language servers and development tools work from editors
- Terminal prompt (Starship) displays correctly
- File manager and navigation tools work

Run comprehensive tests to ensure the development environment is fully functional across all configured tools and shells.