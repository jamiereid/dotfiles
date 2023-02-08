return require('packer').startup(function(use)
	-- use 'wbthomason/packer.vim'         -- Packer itself

	use 'ciaranm/securemodelines'                     -- Make sure modelines can't do bad stuff!
	use 'machakann/vim-highlightedyank'               -- make the yanked region apparent!
	use 'andymass/vim-matchup'                        -- extended '%' and match highlight
	use 'tpope/vim-surround'                          -- (c)hange(s)urround etc
	use 'justinmk/vim-sneak'                          -- jump to a location specified by two chars
	--use 'airblade/vim-rooter'                         -- change vim's pwd to the project root when opening file
	use 'norcalli/nvim-colorizer.lua'                 -- high-performance color highlighter

	use { 'nvim-telescope/telescope.nvim', tag = '0.1.0',
  			requires = { {'nvim-lua/plenary.nvim'} } }
	use 'nvim-telescope/telescope-file-browser.nvim'  -- a file browser

	-- lsp support
	use 'neovim/nvim-lspconfig'
	use 'nvim-lua/lsp_extensions.nvim'
	use {'hrsh7th/cmp-nvim-lsp', branch = 'main'}
	use {'hrsh7th/cmp-buffer',   branch = 'main'}
	use {'hrsh7th/cmp-path',     branch = 'main'}
	use {'hrsh7th/nvim-cmp',     branch = 'main'}
	use 'ray-x/lsp_signature.nvim'

	--  because nvim-cmp _requires_ snippets
	use {'hrsh7th/cmp-vsnip', branch = 'main'}
	use 'hrsh7th/vim-vsnip'

	-- syntax
	use 'cespare/vim-toml'
	use 'stephpy/vim-yaml'
	use 'dag/vim-fish'
	use 'plasticboy/vim-markdown'
	use 'godlygeek/tabular'
	use 'ekalinin/Dockerfile.vim'
	use 'momota/cisco.vim'
	use 'rust-lang/rust.vim'
	use 'Glench/Vim-Jinja2-Syntax'

	--use 'vimwiki/vimwiki'

	--- org-mode
	use 'nvim-treesitter/nvim-treesitter'
	--use 'nvim-orgmode/orgmode'
	--use 'akinsho/org-bullets.nvim'
	
	--- wiki
	use 'lervag/wiki.vim'
	use 'habamax/vim-asciidoctor'
	use 'itchyny/calendar.vim'
end)
