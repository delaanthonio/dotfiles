return {
	{
		"okuuva/auto-save.nvim",
		cmd = "ASToggle", -- optional for lazy loading on command
		event = { "InsertLeave" }, -- lazy load only on InsertLeave
		opts = {
			enabled = true,
			-- Moderate auto-save: save on InsertLeave and when switching buffers/focus lost
			trigger_events = {
				immediate_save = { "FocusLost" }, -- Immediate save when Neovim loses focus
				defer_save = { "InsertLeave", "BufLeave" }, -- Save after exiting insert mode or switching buffer
				cancel_deferred_save = { "InsertEnter" }, -- Cancel pending saves when you start typing again
			},
			write_all_buffers = false, -- Only save current buffer
			debounce_delay = 1000, -- 1 second delay before saving (was 135ms)
			-- Don't auto-save these filetypes
			excluded_filetypes = { "gitcommit", "gitrebase", "hgcommit", "svn", "mail" },
		},
	},
}
