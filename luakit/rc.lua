-----------------------------------------------------------------------
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

-- Load library of useful functions for luakit
require "lousy"

-- Small util functions to print output (info prints only when luakit.verbose is true)
function warn(...) io.stderr:write(string.format(...) .. "\n") end
function info(...) if luakit.verbose then io.stdout:write(string.format(...) .. "\n") end end

-- Load user settings
require "globals"

globals.homepage            = "http://www.google.com/"
globals.default_window_size = "1920x1073"

search_engines = {
  google = "https://encrypted.google.com/search?hl=en&q=%s",

  h = "http://haskell.org/hoogle/?hoogle=%s",
  j = "http://javadocs.org/%s",

  we  = "https://secure.wikimedia.org/wikipedia/en/w/index.php?search=%s",
  ws  = "https://secure.wikimedia.org/wikipedia/sv/w/index.php?search=%s",
  tfd = "http://www.thefreedictionary.com/%s",
  tr  = "http://folkets-lexikon.csc.kth.se/folkets/folkets.en.html#%s&0",
  ud  = "http://www.urbandictionary.com/define.php?term=%s",
  w   = "http://www.wolframalpha.com/input/?i=%s",

  aur = "https://aur.archlinux.org/packages.php?K=%s",
  pak = "https://www.archlinux.org/packages/?q=%s",

  ug   = "http://www.ultimate-guitar.com/search.php?value=%s&search_type=title",
  y    = "http://www.youtube.com/results?search_query=%s",
  mw   = "http://www.minecraftwiki.net/index.php?search=%s",
  imdb = "http://imdb.com/find?s=all&q=%s",
}
search_engines.default = search_engines.google

lousy.theme.init(lousy.util.find_config("theme.lua"))
theme = assert(lousy.theme.get(), "failed to load theme")

require "window"
require "webview"
require "modes"
require "binds"

----------------------------------
-- Optional user script loading --
----------------------------------

-- Add sqlite3 cookiejar
require "cookies"

-- Cookie blocking by domain (extends cookies module)
-- Add domains to the whitelist at "$XDG_CONFIG_HOME/luakit/cookie.whitelist"
-- and blacklist at "$XDG_CONFIG_HOME/luakit/cookie.blacklist".
-- Each domain must be on it's own line and you may use "*" as a
-- wildcard character (I.e. "*google.com")
--require "cookie_blocking"

-- Block all cookies by default (unless whitelisted)
--cookies.default_allow = false

-- Add uzbl-like form filling
-- require "formfiller"

-- Add proxy support & manager
-- require "proxy"

-- Add quickmarks support & manager
require "quickmarks"

-- Add session saving/loading support
require "session"

-- Add command to list closed tabs & bind to open closed tabs
require "undoclose"

-- Add command to list tab history items
require "tabhistory"

-- Add greasemonkey-like javascript userscript support
require "userscripts"

-- Add bookmarks support
require "bookmarks"

-- Add download support
require "downloads"
require "downloads_chrome"

-- Add vimperator-like link hinting & following
-- (depends on downloads)
require "follow"

-- To use a custom character set for the follow hint labels un-comment and
-- modify the following:
--local s = follow.styles
--follow.style = s.sort(s.reverse(s.charset("asdfqwerzxcv"))) -- I'm a lefty

-- Add command history
require "cmdhist"

-- Add search mode & binds
require "search"

-- Add ordering of new tabs
require "taborder"

-- Save web history
require "history"
require "history_chrome"

-- Add command completion
require "completion"

-- NoScript plugin, toggle scripts and or plugins on a per-domain basis.
-- `,ts` to toggle scripts, `,tp` to toggle plugins, `,tr` to reset.
-- Remove all "enable_scripts" & "enable_plugins" lines from your
-- domain_props table (in config/globals.lua) as this module will conflict.
--require "noscript"

require "follow_selected"
require "go_input"
require "go_next_prev"
require "go_up"

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

