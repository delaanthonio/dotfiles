# Project Jump for Fish Shell
# Similar to oh-my-zsh pj plugin

set -gx PROJECT_PATHS $HOME/Developer $HOME/Git

function pj -d "Jump to project directory"
    if test (count $argv) -eq 0
        # Jump to first PROJECT_PATHS directory if no argument provided
        for path in $PROJECT_PATHS
            if test -d $path
                cd $path
                return 0
            end
        end
        echo "No valid PROJECT_PATHS found"
        return 1
    end
    
    set -l project $argv[1]
    
    # Search for project in all PROJECT_PATHS
    for path in $PROJECT_PATHS
        if test -d "$path/$project"
            cd "$path/$project"
            return 0
        end
    end
    
    # If not found as direct match, try fuzzy search
    for path in $PROJECT_PATHS
        set -l matches (find "$path" -maxdepth 1 -type d -iname "*$project*" 2>/dev/null)
        if test (count $matches) -gt 0
            if test (count $matches) -eq 1
                cd $matches[1]
                return 0
            else
                echo "Multiple matches found:"
                for match in $matches
                    echo "  "(basename $match)
                end
                return 1
            end
        end
    end
    
    echo "Project '$project' not found in PROJECT_PATHS"
    return 1
end

# pjo function - open project in editor
function pjo -d "Open project in editor"
    if test (count $argv) -eq 0
        echo "Usage: pjo <project-name>"
        return 1
    end
    
    set -l project $argv[1]
    
    # Find project directory
    for path in $PROJECT_PATHS
        if test -d "$path/$project"
            eval $EDITOR "$path/$project"
            return 0
        end
    end
    
    echo "Project '$project' not found in PROJECT_PATHS"
    return 1
end

# Completion for pj and pjo
function __fish_pj_complete
    for path in $PROJECT_PATHS
        if test -d $path
            for dir in $path/*/
                if test -d $dir
                    basename $dir
                end
            end
        end
    end
end

# List all projects
function pjl -d "List all available projects"
    for path in $PROJECT_PATHS
        if test -d $path
            echo (set_color blue)"Projects in $path:"(set_color normal)
            for dir in $path/*/
                if test -d $dir
                    echo "  "(basename $dir)
                end
            end
        end
    end
end

# Add current directory to PROJECT_PATHS
function pja -d "Add current directory's parent to PROJECT_PATHS"
    set -l parent (dirname (pwd))
    if not contains $parent $PROJECT_PATHS
        set -gx PROJECT_PATHS $PROJECT_PATHS $parent
        echo "Added $parent to PROJECT_PATHS"
    else
        echo "$parent is already in PROJECT_PATHS"
    end
end

# Quick project switch with fzf (if installed)
if command -q fzf
    function pjf -d "Fuzzy find and jump to project"
        set -l project (
            for path in $PROJECT_PATHS
                if test -d $path
                    find $path -maxdepth 1 -type d | tail -n +2
                end
            end | fzf --query="$argv" --select-1 --exit-0
        )
        
        if test -n "$project"
            cd $project
        end
    end
end

complete -c pj -f -a "(__fish_pj_complete)" -d "Project"
complete -c pjo -f -a "(__fish_pj_complete)" -d "Project"
complete -c pjl -f -d "List all available projects"
complete -c pja -f -d "Add current directory's parent to PROJECT_PATHS"
complete -c pjf -f -a "(__fish_pj_complete)" -d "Fuzzy find project"
