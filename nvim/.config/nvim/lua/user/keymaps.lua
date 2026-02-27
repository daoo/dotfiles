local wk = require("which-key")

wk.add({
  { "<leader>w",  '<cmd>%s/\\s\\+$//<cr>', desc = "Trim trailing whitespace" },
  { "<c-s>", "<cmd>write<cr>", desc = "Save file", mode = "n" },
  { "<c-s>", "<c-o>:write<cr>", desc = "Save file (insert)", mode = "i" },
  { "n", "nzz", desc = "Next search result (centered)", mode = "n" },
  { "N", "Nzz", desc = "Prev search result (centered)", mode = "n" },

  { "<leader>e", group = "Edit/File" },
  { "<leader>ef", "<cmd>e %<cr>", desc = "Edit current file" },
  { "<leader>eF", "<cmd>e! %<cr>", desc = "Revert current file" },
  { "<leader>ev", "<cmd>e $MYVIMRC<cr>", desc = "Edit config" },
  { "<leader>es", "<cmd>source $MYVIMRC<cr>", desc = "Source config" },
  { "<leader>en", "<Plug>(altr-forward)", desc = "Alternate forward" },
  { "<leader>eb", "<Plug>(altr-backward)", desc = "Alternate backward" },

  { "<leader>s", group = "Spelling" },
  { "<leader>se", "<cmd>setlocal spelllang=en<cr>", desc = "English" },
  { "<leader>ss", "<cmd>setlocal spelllang=sv<cr>", desc = "Swedish" },

  { "<leader>o", group = "Open" },
  { "<c-p>", "<cmd>FzfLua git_files<cr>", desc = "Git files" },
  { "<leader>o?", "<cmd>FzfLua<cr>", desc = "Builtin picker" },
  { "<leader>ob", "<cmd>FzfLua buffers<cr>", desc = "Buffers" },
  { "<leader>od", "<cmd>FzfLua files cwd=%:p:h<cr>", desc = "Files (file dir)" },
  { "<leader>of", "<cmd>FzfLua files<cr>", desc = "Files" },
  { "<leader>ol", "<cmd>FzfLua blines<cr>", desc = "Buffer lines" },
  { "<leader>om", "<cmd>FzfLua marks<cr>", desc = "Marks" },
  { "<leader>ot", "<cmd>FzfLua btags<cr>", desc = "Buffer tags" },

  { "<leader>f", group = "Find/Search" },
  { "<leader>fw", "<cmd>FzfLua grep_cword<cr>", desc = "Grep word" },
  { "<leader>fv", "<cmd>FzfLua grep_visual<cr>", desc = "Grep selection", mode = "v" },
  { "<leader>fl", "<cmd>FzfLua live_grep<cr>", desc = "Live grep" },
  { "<leader>m", "<Plug>(quickhl-manual-this)", desc = "Highlight", mode = { "n","v" } },
  { "<leader>M", "<Plug>(quickhl-manual-reset)", desc = "Clear highlights", mode = { "n","v" } },

  { "<leader>g", group = "Git" },
  { "<leader>gs", "<cmd>FzfLua git_status<cr>", desc = "Status" },
  { "<leader>gb", "<cmd>Git blame<cr>", desc = "Blame" },
  { "<leader>gd", "<cmd>Gvdiffsplit<cr>", desc = "Diff split" },
  { "<leader>gl", "<cmd>GV<cr>", desc = "Log" },

  { "<leader>l", group = "LSP" },
  { "<leader>ld", "<cmd>FzfLua lsp_workspace_diagnostics<cr>", desc = "Diagnostics" },
  { "<leader>ls", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Document symbols" },
  { "<leader>lw", "<cmd>FzfLua lsp_workspace_symbols<cr>", desc = "Workspace symbols" },
})
