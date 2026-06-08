set -l emacs_libgccjit_root /opt/homebrew/opt/libgccjit/lib/gcc/current
set -l emacs_gcc_root /opt/homebrew/opt/gcc/lib/gcc/current/gcc
set -l emacs_gcc_arch ""
set -l emacs_library_path_parts

for candidate in $emacs_gcc_root/aarch64-apple-darwin*/*
    if test -d "$candidate"
        set emacs_gcc_arch "$candidate"
        break
    end
end

if test -d "$emacs_libgccjit_root"
    set -a emacs_library_path_parts "$emacs_libgccjit_root"
end

if test -n "$emacs_gcc_arch"
    set -a emacs_library_path_parts "$emacs_gcc_arch"
end

if test -d "$emacs_gcc_root"
    set -a emacs_library_path_parts "$emacs_gcc_root"
end

if test -n "$emacs_library_path_parts"
    if set -q LIBRARY_PATH
        for path in (string split : -- "$LIBRARY_PATH")
            if test -n "$path"; and not contains -- "$path" $emacs_library_path_parts
                set -a emacs_library_path_parts "$path"
            end
        end
    end

    set -gx LIBRARY_PATH (string join : -- $emacs_library_path_parts)
end
