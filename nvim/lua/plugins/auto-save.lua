return {
	{
		"okuuva/auto-save.nvim",
		cmd = "ASToggle", -- optional for lazy loading on command
		event = { "InsertLeave", "TextChanged" }, -- optional for lazy loading on trigger events
		opts = {
			enabled = true,
			execution_message = {
				enabled = true,
				message = function()
					return ("AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"))
				end,
				dim = 0.18,
				cleaning_interval = 1250,
			},
			trigger_events = {
				immediate_save = { "BufLeave", "FocusLost" },
				defer_save = { "InsertLeave", "TextChanged" },
				cancel_deferred_save = { "InsertEnter" },
			},
			write_all_buffers = false,
			debounce_delay = 135,
		},
	},
}
