local M = {}
local builtin = require "el.builtin"
local extensions = require "el.extensions"
local sections = require "el.sections"
local subscribe = require "el.subscribe"
local lsp_statusline = require "el.plugins.lsp_status"

M.generator = function(window, buffer)
  local segments = {}

  table.insert(segments, extensions.gen_mode { format_string = " %s " })
  table.insert(
    segments,
    subscribe.buf_autocmd("el_git_branch", "BufEnter", function(window, buffer)
      local branch = extensions.git_branch(window, buffer)
      if branch then
        return "  " .. branch
      end
    end)
  )
  table.insert(
    segments,
    subscribe.buf_autocmd("el_fileformat_icon", "BufEnter", function(window, buffer)
      local symbols = {
        unix = "", -- e712
        dos = "", -- e70f
        mac = "", -- e711
      }

      local format = vim.bo.fileformat
      return " " .. symbols[format] or format .. " "
    end)
  )

  table.insert(segments, sections.split)

  table.insert(segments, sections.maximum_width(builtin.file_relative, 0.60))
  table.insert(segments, sections.collapse_builtin { { " " }, { builtin.modified_flag } })
  table.insert(segments, " ")
  table.insert(
    segments,
    subscribe.buf_autocmd("el_git_changes", "BufWritePost", function(window, buffer)
      return extensions.git_changes(window, buffer)
    end)
  )
  table.insert(segments, " ")

  table.insert(segments, sections.split)

  table.insert(segments, function(window, buffer)
    if buffer.filetype == "lua" then
      return ""
    end

    return lsp_statusline.current_function(window, buffer)
  end)

  table.insert(
    segments,
    subscribe.buf_autocmd("el_file_icon", "BufRead", function(_, bufnr)
      local icon = extensions.file_icon(_, bufnr)
      if icon then
        return icon .. " "
      end

      return ""
    end)
  )
  table.insert(segments, " " .. vim.bo.filetype .. " ")
  table.insert(segments, function()
    local c = vim.fn.line "."
    local t = vim.fn.line "$"
    local r
    if c == 1 then
      r = "Top"
    elseif c == t then
      r = "End"
    else
      r = string.format("%2d%%%%", math.floor(c / t * 100))
    end

    return " " .. r .. " "
  end)
  table.insert(segments, builtin.line_number .. ":" .. builtin.column_number .. " ")

  --[[
      sections.collapse_builtin {
        "[",
        builtin.help_list,
        builtin.readonly_list,
        "]",
      },
    },
  }
  ]]
  --

  return segments
end

M.reload = function()
  require("el").setup { generator = M.generator }
end

--M.reload()

return M
