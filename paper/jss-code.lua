-- JSS house style: inline code is typeset with \code{} rather than \texttt{}.
-- All TeX specials are escaped so that the result also works inside figure
-- and table captions, where jss.cls's catcode changes in \code{} do not apply.
local function tex_escape(s)
  s = s:gsub("\\", "\1")
  s = s:gsub("([{}%%&#_$])", "\\%1")
  s = s:gsub("%^", "\\^{}")
  s = s:gsub("~", "\\textasciitilde{}")
  s = s:gsub("\1", "\\textbackslash{}")
  return s
end

function Code(el)
  if quarto.doc.is_format("pdf") then
    return pandoc.RawInline("tex", "\\code{" .. tex_escape(el.text) .. "}")
  end
end
