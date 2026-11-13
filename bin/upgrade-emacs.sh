#!/bin/bash
# Check if Emacs is working, reinstall if broken

# Skip entirely on machines without Emacs (e.g. Linux boxes that only use nvim)
if ! command -v emacs >/dev/null 2>&1; then
    echo "Emacs not installed, skipping Doom upgrade."
    exit 0
fi

if ! emacs --version &>/dev/null 2>&1; then
    echo "Emacs is broken (library mismatch detected), reinstalling..."
    # Auto-detect installed emacs-plus version
    emacs_formula=$(brew list --formula 2>/dev/null | grep "^emacs-plus@" | head -1)
    if [ -n "$emacs_formula" ]; then
        brew reinstall "$emacs_formula"
    else
        echo "Error: No emacs-plus formula found. Please install emacs-plus manually."
        exit 1
    fi
fi

# Run doom sync
exec doom sync -u
