return {
  provider = function()
    local fmt = vim.bo.fileformat
    if fmt == "dos" then
      return "CRLF "
    elseif fmt == "mac" then
      return "CR "
    end
  end,
}
