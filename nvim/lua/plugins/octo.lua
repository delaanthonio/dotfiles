return {
  "pwntester/octo.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "nvim-tree/nvim-web-devicons",
  },
  cmd = "Octo",
  keys = {
    -- PR Management
    { "<leader>gpl", "<cmd>Octo pr list<cr>", desc = "List PRs" },
    { "<leader>gpc", "<cmd>Octo pr create<cr>", desc = "Create PR" },
    { "<leader>gps", "<cmd>Octo pr search<cr>", desc = "Search PRs" },
    { "<leader>gpv", "<cmd>Octo pr view<cr>", desc = "View PR" },
    { "<leader>gpr", "<cmd>Octo pr ready<cr>", desc = "Mark PR ready" },
    { "<leader>gpm", "<cmd>Octo pr merge<cr>", desc = "Merge PR" },
    { "<leader>gpk", "<cmd>Octo pr checks<cr>", desc = "View PR checks" },
    { "<leader>gpd", "<cmd>Octo pr diff<cr>", desc = "View PR diff" },

    -- Review Commands
    { "<leader>grc", "<cmd>Octo review comments<cr>", desc = "Review comments" },
    { "<leader>grs", "<cmd>Octo review start<cr>", desc = "Start review" },
    { "<leader>grr", "<cmd>Octo review resume<cr>", desc = "Resume review" },
    { "<leader>gra", "<cmd>Octo review submit<cr>", desc = "Submit review (approve)" },

    -- Issue Management
    { "<leader>gil", "<cmd>Octo issue list<cr>", desc = "List issues" },
    { "<leader>gic", "<cmd>Octo issue create<cr>", desc = "Create issue" },
    { "<leader>gis", "<cmd>Octo issue search<cr>", desc = "Search issues" },

    -- Comments (in buffer)
    { "<leader>gca", "<cmd>Octo comment add<cr>", desc = "Add comment", mode = { "n", "v" } },
    { "<leader>gcd", "<cmd>Octo comment delete<cr>", desc = "Delete comment" },

    -- Reactions (in buffer)
    { "<leader>g+1", "<cmd>Octo reaction thumbs_up<cr>", desc = "👍 reaction" },
    { "<leader>g-1", "<cmd>Octo reaction thumbs_down<cr>", desc = "👎 reaction" },
    { "<leader>g❤️", "<cmd>Octo reaction heart<cr>", desc = "❤️ reaction" },
    { "<leader>g🎉", "<cmd>Octo reaction hooray<cr>", desc = "🎉 reaction" },
  },
  opts = {
    use_local_fs = false, -- Use local git state instead of GitHub API for diffs
    enable_builtin = false, -- Shows diff using builtin methods (faster)
    default_remote = { "upstream", "origin" }, -- Order to try remotes
    default_merge_method = "squash", -- Default merge method (squash, merge, rebase)
    ssh_aliases = {}, -- SSH aliases if you use custom git hosts
    picker = "telescope", -- Use telescope for pickers
    picker_config = {
      use_emojis = true, -- Show emojis in picker
    },
    comment_icon = "▎", -- Comment marker icon
    outdated_icon = "󰅒 ", -- Outdated comment marker
    resolved_icon = " ", -- Resolved comment marker
    reaction_viewer_hint_icon = " ", -- Reaction hint icon
    user_icon = " ", -- User icon
    timeline_marker = " ", -- Timeline marker
    timeline_indent = "2", -- Timeline indentation
    right_bubble_delimiter = "", -- Right bubble delimiter
    left_bubble_delimiter = "", -- Left bubble delimiter
    github_hostname = "", -- GitHub Enterprise hostname (leave empty for github.com)
    snippet_context_lines = 4, -- Number of lines around a snippet
    gh_env = {}, -- Extra environment variables for gh CLI
    timeout = 5000, -- Timeout for requests in milliseconds
    ui = {
      use_signcolumn = true, -- Show signs in signcolumn
      use_signstatus = true, -- Show signs in statusline
    },
    issues = {
      order_by = { -- Criteria to order issues
        field = "CREATED_AT",
        direction = "DESC",
      },
    },
    pull_requests = {
      order_by = { -- Criteria to order PRs
        field = "CREATED_AT",
        direction = "DESC",
      },
      always_select_remote_on_create = false, -- Always select remote when creating PR
    },
    file_panel = {
      size = 10, -- Height of file panel
      use_icons = true, -- Use devicons in file panel
    },
    colors = {
      -- Use your theme's colors
      white = "#ffffff",
      grey = "#6e7781",
      black = "#000000",
      red = "#f85149",
      dark_red = "#da3633",
      green = "#3fb950",
      dark_green = "#2ea043",
      yellow = "#d29922",
      dark_yellow = "#9e6a03",
      blue = "#58a6ff",
      dark_blue = "#1f6feb",
      purple = "#bc8cff",
    },
    mappings = {
      -- Mappings when in octo buffer
      issue = {
        close_issue = { lhs = "<space>ic", desc = "close issue" },
        reopen_issue = { lhs = "<space>io", desc = "reopen issue" },
        list_issues = { lhs = "<space>il", desc = "list open issues on same repo" },
        reload = { lhs = "<C-r>", desc = "reload issue" },
        open_in_browser = { lhs = "<C-b>", desc = "open issue in browser" },
        copy_url = { lhs = "<C-y>", desc = "copy url to system clipboard" },
        add_assignee = { lhs = "<space>aa", desc = "add assignee" },
        remove_assignee = { lhs = "<space>ad", desc = "remove assignee" },
        create_label = { lhs = "<space>lc", desc = "create label" },
        add_label = { lhs = "<space>la", desc = "add label" },
        remove_label = { lhs = "<space>ld", desc = "remove label" },
        goto_issue = { lhs = "<space>gi", desc = "navigate to a local repo issue" },
        add_comment = { lhs = "<space>ca", desc = "add comment" },
        delete_comment = { lhs = "<space>cd", desc = "delete comment" },
        next_comment = { lhs = "]c", desc = "go to next comment" },
        prev_comment = { lhs = "[c", desc = "go to previous comment" },
        react_hooray = { lhs = "<space>rp", desc = "add/remove 🎉 reaction" },
        react_heart = { lhs = "<space>rh", desc = "add/remove ❤️ reaction" },
        react_eyes = { lhs = "<space>re", desc = "add/remove 👀 reaction" },
        react_thumbs_up = { lhs = "<space>r+", desc = "add/remove 👍 reaction" },
        react_thumbs_down = { lhs = "<space>r-", desc = "add/remove 👎 reaction" },
        react_rocket = { lhs = "<space>rr", desc = "add/remove 🚀 reaction" },
        react_laugh = { lhs = "<space>rl", desc = "add/remove 😄 reaction" },
        react_confused = { lhs = "<space>rc", desc = "add/remove 😕 reaction" },
      },
      pull_request = {
        checkout_pr = { lhs = "<space>po", desc = "checkout PR" },
        merge_pr = { lhs = "<space>pm", desc = "merge commit PR" },
        squash_and_merge_pr = { lhs = "<space>psm", desc = "squash and merge PR" },
        rebase_and_merge_pr = { lhs = "<space>prm", desc = "rebase and merge PR" },
        list_commits = { lhs = "<space>pc", desc = "list PR commits" },
        list_changed_files = { lhs = "<space>pf", desc = "list PR changed files" },
        show_pr_diff = { lhs = "<space>pd", desc = "show PR diff" },
        add_reviewer = { lhs = "<space>va", desc = "add reviewer" },
        remove_reviewer = { lhs = "<space>vd", desc = "remove reviewer request" },
        close_issue = { lhs = "<space>ic", desc = "close PR" },
        reopen_issue = { lhs = "<space>io", desc = "reopen PR" },
        list_issues = { lhs = "<space>il", desc = "list open issues on same repo" },
        reload = { lhs = "<C-r>", desc = "reload PR" },
        open_in_browser = { lhs = "<C-b>", desc = "open PR in browser" },
        copy_url = { lhs = "<C-y>", desc = "copy url to system clipboard" },
        goto_file = { lhs = "gf", desc = "go to file" },
        add_assignee = { lhs = "<space>aa", desc = "add assignee" },
        remove_assignee = { lhs = "<space>ad", desc = "remove assignee" },
        create_label = { lhs = "<space>lc", desc = "create label" },
        add_label = { lhs = "<space>la", desc = "add label" },
        remove_label = { lhs = "<space>ld", desc = "remove label" },
        goto_issue = { lhs = "<space>gi", desc = "navigate to a local repo issue" },
        add_comment = { lhs = "<space>ca", desc = "add comment" },
        delete_comment = { lhs = "<space>cd", desc = "delete comment" },
        next_comment = { lhs = "]c", desc = "go to next comment" },
        prev_comment = { lhs = "[c", desc = "go to previous comment" },
        react_hooray = { lhs = "<space>rp", desc = "add/remove 🎉 reaction" },
        react_heart = { lhs = "<space>rh", desc = "add/remove ❤️ reaction" },
        react_eyes = { lhs = "<space>re", desc = "add/remove 👀 reaction" },
        react_thumbs_up = { lhs = "<space>r+", desc = "add/remove 👍 reaction" },
        react_thumbs_down = { lhs = "<space>r-", desc = "add/remove 👎 reaction" },
        react_rocket = { lhs = "<space>rr", desc = "add/remove 🚀 reaction" },
        react_laugh = { lhs = "<space>rl", desc = "add/remove 😄 reaction" },
        react_confused = { lhs = "<space>rc", desc = "add/remove 😕 reaction" },
        review_start = { lhs = "<space>vs", desc = "start a review for the current PR" },
        review_resume = { lhs = "<space>vr", desc = "resume a pending review for the current PR" },
      },
      review_thread = {
        goto_issue = { lhs = "<space>gi", desc = "navigate to a local repo issue" },
        add_comment = { lhs = "<space>ca", desc = "add comment" },
        add_suggestion = { lhs = "<space>sa", desc = "add suggestion" },
        delete_comment = { lhs = "<space>cd", desc = "delete comment" },
        next_comment = { lhs = "]c", desc = "go to next comment" },
        prev_comment = { lhs = "[c", desc = "go to previous comment" },
        select_next_entry = { lhs = "]q", desc = "move to previous changed file" },
        select_prev_entry = { lhs = "[q", desc = "move to next changed file" },
        select_first_entry = { lhs = "[Q", desc = "move to first changed file" },
        select_last_entry = { lhs = "]Q", desc = "move to last changed file" },
        close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
        react_hooray = { lhs = "<space>rp", desc = "add/remove 🎉 reaction" },
        react_heart = { lhs = "<space>rh", desc = "add/remove ❤️ reaction" },
        react_eyes = { lhs = "<space>re", desc = "add/remove 👀 reaction" },
        react_thumbs_up = { lhs = "<space>r+", desc = "add/remove 👍 reaction" },
        react_thumbs_down = { lhs = "<space>r-", desc = "add/remove 👎 reaction" },
        react_rocket = { lhs = "<space>rr", desc = "add/remove 🚀 reaction" },
        react_laugh = { lhs = "<space>rl", desc = "add/remove 😄 reaction" },
        react_confused = { lhs = "<space>rc", desc = "add/remove 😕 reaction" },
      },
      submit_win = {
        approve_review = { lhs = "<C-a>", desc = "approve review" },
        comment_review = { lhs = "<C-m>", desc = "comment review" },
        request_changes = { lhs = "<C-r>", desc = "request changes review" },
        close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
      },
      review_diff = {
        submit_review = { lhs = "<leader>vs", desc = "submit review" },
        discard_review = { lhs = "<leader>vd", desc = "discard review" },
        add_review_comment = { lhs = "<space>ca", desc = "add a new review comment" },
        add_review_suggestion = { lhs = "<space>sa", desc = "add a new review suggestion" },
        focus_files = { lhs = "<leader>e", desc = "move focus to changed file panel" },
        toggle_files = { lhs = "<leader>b", desc = "hide/show changed files panel" },
        next_thread = { lhs = "]t", desc = "move to next thread" },
        prev_thread = { lhs = "[t", desc = "move to previous thread" },
        select_next_entry = { lhs = "]q", desc = "move to previous changed file" },
        select_prev_entry = { lhs = "[q", desc = "move to next changed file" },
        select_first_entry = { lhs = "[Q", desc = "move to first changed file" },
        select_last_entry = { lhs = "]Q", desc = "move to last changed file" },
        close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
        toggle_viewed = { lhs = "<leader><space>", desc = "toggle viewer viewed state" },
        goto_file = { lhs = "gf", desc = "go to file" },
      },
      file_panel = {
        submit_review = { lhs = "<leader>vs", desc = "submit review" },
        discard_review = { lhs = "<leader>vd", desc = "discard review" },
        next_entry = { lhs = "j", desc = "move to next changed file" },
        prev_entry = { lhs = "k", desc = "move to previous changed file" },
        select_entry = { lhs = "<cr>", desc = "show selected changed file diffs" },
        refresh_files = { lhs = "R", desc = "refresh changed files panel" },
        focus_files = { lhs = "<leader>e", desc = "move focus to changed file panel" },
        toggle_files = { lhs = "<leader>b", desc = "hide/show changed files panel" },
        select_next_entry = { lhs = "]q", desc = "move to previous changed file" },
        select_prev_entry = { lhs = "[q", desc = "move to next changed file" },
        select_first_entry = { lhs = "[Q", desc = "move to first changed file" },
        select_last_entry = { lhs = "]Q", desc = "move to last changed file" },
        close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
        toggle_viewed = { lhs = "<leader><space>", desc = "toggle viewer viewed state" },
      },
    },
  },
}
