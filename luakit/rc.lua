----------------------------------------------------------------------
-- luakit configuration file, more information at http://luakit.org/ --
-----------------------------------------------------------------------

if unique then
  unique.new("org.luakit")
  -- Check for a running luakit instance
  if unique.is_running() then
    if uris[1] then
      for _, uri in ipairs(uris) do
        unique.send_message("tabopen " .. uri)
      end
    else
      unique.send_message("winopen")
    end
    luakit.quit()
  end
end

require "lousy"
require "globals"

globals.homepage = "http://www.google.com/"

search_engines = {
  google = "https://encrypted.google.com/search?hl=en&q=%s",

  cpp = "http://en.cppreference.com/mwiki/index.php?go=Go&search=%s",
  cs  = "http://social.msdn.microsoft.com/search/en-us?query=%s",
  h   = "http://haskell.org/hoogle/?hoogle=%s",
  hy  = "http://en.cppreference.com/mwiki/index.php?go=Go&search=%s",
  j   = "http://search.oracle.com/search/search?q=%s&group=Documentation",
  gl  = "http://www.opengl.org/wiki_132/index.php?search=%s&go=Go",
  php = "http://se.php.net/manual-lookup.php?lang=en&scope=quickref&pattern=%s",

  we  = "https://secure.wikimedia.org/wikipedia/en/w/index.php?search=%s",
  ws  = "https://secure.wikimedia.org/wikipedia/sv/w/index.php?search=%s",
  tfd = "http://www.thefreedictionary.com/%s",
  tr  = "http://folkets-lexikon.csc.kth.se/folkets/folkets.en.html#%s",
  ud  = "http://www.urbandictionary.com/define.php?term=%s",
  w   = "http://www.wolframalpha.com/input/?i=%s",

  daw = "http://dragonage.wikia.com/wiki/index.php?search=%s&fulltext=Search",
  maw = "http://masseffect.wikia.com/wiki/index.php?search=%s",
  mcw = "http://www.minecraftwiki.net/index.php?search=%s",
  pcg = "http://pcgamingwiki.com/index.php?search=%s",

  aur = "https://aur.archlinux.org/packages.php?K=%s",
  pak = "https://www.archlinux.org/packages/?q=%s",
  aw  = "https://wiki.archlinux.org/index.php?go=Go&search=%s",

  imdb = "http://imdb.com/find?s=all&q=%s",
  tvt  = "https://encrypted.google.com/search?hl=en&q=site:tvtropes.org+%s",
  ug   = "http://www.ultimate-guitar.com/search.php?value=%s&search_type=title",
  y    = "http://www.youtube.com/results?search_query=%s",
}
search_engines.default = search_engines.google

lousy.theme.init(lousy.util.find_config("theme.lua"))
theme = assert(lousy.theme.get(), "failed to load theme")

require "window"
require "webview"
require "modes"
require "binds"

add_binds("normal", {
  lousy.bind.buf("^gq$",
  function (w)
    luakit.spawn("quvi -f best --exec 'mplayer -cache-min 15 %u' " .. w.view.uri)
  end
  )
})

----------------------------------
-- Optional user script loading --
----------------------------------

require "bookmarks"
require "bookmarks_chrome"
require "cmdhist"
require "completion"
require "cookies"
require "downloads"
require "downloads_chrome"
require "follow"
require "follow_selected"
require "go_input"
require "go_next_prev"
require "go_up"
require "history"
require "history_chrome"
require "introspector"
require "quickmarks"
require "search"
require "session"
require "tabhistory"
require "taborder"
require "undoclose"
require "webinspector"

local s            = follow.label_styles
follow.label_maker = s.sort(s.reverse(s.charset("ahotenusid")))

-----------------------------
-- End user script loading --
-----------------------------

-- Restore last saved session
local w = (session and session.restore())
if w then
  for i, uri in ipairs(uris) do
    w:new_tab(uri, i == 1)
  end
else
  -- Or open new window
  window.new(uris)
end

-------------------------------------------
-- Open URIs from other luakit instances --
-------------------------------------------

if unique then
  unique.add_signal("message", function (msg, screen)
    local cmd, arg = string.match(msg, "^(%S+)%s*(.*)")
    local w = lousy.util.table.values(window.bywidget)[1]
    if cmd == "tabopen" then
      w:new_tab(arg)
    elseif cmd == "winopen" then
      w = window.new((arg ~= "") and { arg } or {})
    end
    w.win.screen = screen
    w.win.urgency_hint = true
  end)
end
