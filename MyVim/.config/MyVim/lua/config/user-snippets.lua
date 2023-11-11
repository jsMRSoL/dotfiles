local luasnip = require('luasnip')
local s = luasnip.snippet
local sn = luasnip.snippet_node
local t = luasnip.text_node
local i = luasnip.insert_node
local c = luasnip.choice_node
local d = luasnip.dynamic_node
local f = luasnip.function_node
local rep = require('luasnip.extras').rep

luasnip.add_snippets('rust', {
  s('pln', {
    -- equivalent to "println!(text{}text);"
    t('println!("'),
    i(1, 'text '),
    c(2, {
      sn(1, {
        t('{'),
        i(1, 'var'),
        t('}'),
        i(2, ' text'),
        t('");'),
        i(0, ''),
      }),
      sn(2, {
        t('{}'),
        i(1, ' text'),
        t('", '),
        i(2, 'var'),
        t(');'),
        i(0, ''),
      }),
      sn(3, {
        t('{'),
        i(1, 'name'),
        t('}'),
        i(2, ' text'),
        t('", '),
        rep(1),
        t('='),
        i(3, 'var'),
        t(');'),
        i(0, ''),
      }),
    }),
  }),
})

local date_input = function(args, snip, old_state, fmt)
  local fmt = fmt or '%Y-%m-%d'
  return sn(nil, i(1, os.date(fmt)))
end

local date_input2 = function (_, _, user_arg1)
  local fmt = user_arg1 or '%Y-%m-%d'
  return os.date(fmt)
end

luasnip.add_snippets('all', {
  s('daydate', {
    d(1, date_input, {}, { user_args = { "%d-%m-%Y %H:%M" } }),
  }),
  s('dydt', {
    f(date_input2, {}, { user_args = { "%d-%m-%Y %H:%M" } }),
  })
})
