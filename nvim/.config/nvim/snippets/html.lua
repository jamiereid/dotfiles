local get_visual = function(args, parent)
  if #parent.snippet.env.SELECT_RAW > 0 then
    return sn(nil, i(1, parent.snippet.env.SELECT_RAW))
  else
    return sn(nil, i(1))
  end
end

local line_begin = require("luasnip.extras.expand_conditions").line_begin

return {
  -- BASE
  s(
    { trig = "base" },
    fmt(
      [[
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>{}</title>
    <link rel="stylesheet" href="style.css">
  </head>
  <body>
    <script src="index.js"></script>
    {}
  </body>
</html>
        ]],
      {
        i(1, "FooBar"),
        i(0),
      }
    ),
    { condition = line_begin }
  ),
  -- HEADER
  s(
    { trig = "h([123456])", regTrig = true, wordTrig = false, snippetType = "autosnippet" },
    fmt(
      [[
          <h{}>{}</h{}>
        ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
        f(function(_, snip)
          return snip.captures[1]
        end),
      }
    ),
    { condition = line_begin }
  ),
  -- PARAGRAPH
  s(
    { trig = "pp", snippetType = "autosnippet" },
    fmt(
      [[
          <p>{}</p>
        ]],
      {
        d(1, get_visual),
      }
    ),
    { condition = line_begin }
  ),
}
