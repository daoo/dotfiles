#
# == Hooks
#

on :client_create do |c|
  cur = Subtlext::View.current

  # Check for empty tags
  if(c.tags.empty?)
    t = Subtlext::Tag[cur.name] rescue nil

    # Create new tag
    if(t.nil?)
      t = Subtlext::Tag.new(cur.name)
      t.save
    end 

    c + t
  end
end

#
# == Sublets
#

sublet :clock do
  interval      30
  format_string "%H:%M %Y-%m-%d"
end

#
# == Options
#
set :border, 1
set :step, 5
set :snap, 10
set :gravity, :center
set :urgent, false
set :resize, false
set :padding, [ 0, 0, 0, 0 ]
set :font, "xft:sans-8"
set :gap, 0
set :separator, "|"
set :outline, 0

#
# == Screen
#

screen 1 do
  stipple false
  top     [ :views, :title, :spacer, :tray, :sublets ]
  bottom  [ ]
end

screen 2 do
  stipple false
  top     [ ]
  bottom  [ ]
end

#
# == Colors
#

# Colors of focus window title
color :title_fg,        "#fecf35"
color :title_bg,        "#202020"
color :title_border,    "#303030"

# Colors of the active views
color :focus_fg,        "#fecf35"
color :focus_bg,        "#202020"
color :focus_border,    "#303030"

# Colors of urgent window titles and views
color :urgent_fg,       "#ff9800"
color :urgent_bg,       "#202020"
color :urgent_border,   "#303030"

# Colors of occupied views (views with clients)
color :occupied_fg,     "#b8b8b8"
color :occupied_bg,     "#202020"
color :occupied_border, "#303030"

# Color of view buttons
color :views_fg,        "#757575"
color :views_bg,        "#202020"
color :views_border,    "#303030"

# Colors of sublets
color :sublets_fg,      "#b8b8b8"
color :sublets_bg,      "#202020"
color :sublets_border,  "#303030"

# Border colors of active/inactive windows
color :client_active,   "#303030"
color :client_inactive, "#202020"

# Background colors of panels
color :panel,           "#202020"

# Color of the stipple (if enabled)
color :stipple,         "#757575"

# Color of the separator
color :separator,       "#757575"

#
# == Gravities
#
# Top left
gravity :top_left,       [   0,   0,  50,  50 ]
gravity :top_left66,     [   0,   0,  50,  66 ]
gravity :top_left33,     [   0,   0,  50,  34 ]

# Top
gravity :top,            [   0,   0, 100,  50 ]
gravity :top66,          [   0,   0, 100,  66 ]
gravity :top33,          [   0,   0, 100,  34 ]

# Top right
gravity :top_right,      [ 100,   0,  50,  50 ]
gravity :top_right66,    [ 100,   0,  50,  66 ]
gravity :top_right33,    [ 100,   0,  50,  34 ]

# Left
gravity :left,           [   0,   0,  50, 100 ]
gravity :left66,         [   0,  50,  50,  34 ]
gravity :left33,         [   0,  50,  25,  34 ]

# Center
gravity :center,         [   0,   0, 100, 100 ]
gravity :center66,       [   0,  50, 100,  34 ]
gravity :center33,       [  50,  50,  50,  34 ]

# Right
gravity :right,          [ 100,   0,  50, 100 ]
gravity :right66,        [ 100,  50,  50,  34 ]
gravity :right33,        [ 100,  50,  25,  34 ]

# Bottom left
gravity :bottom_left,    [   0, 100,  50,  50 ]
gravity :bottom_left66,  [   0, 100,  50,  66 ]
gravity :bottom_left33,  [   0, 100,  50,  34 ]

# Bottom
gravity :bottom,         [   0, 100, 100,  50 ]
gravity :bottom66,       [   0, 100, 100,  66 ]
gravity :bottom33,       [   0, 100, 100,  34 ]

# Bottom right
gravity :bottom_right,   [ 100, 100,  50,  50 ]
gravity :bottom_right66, [ 100, 100,  50,  66 ]
gravity :bottom_right33, [ 100, 100,  50,  34 ]

# Gimp
gravity :gimp_image,     [  50,  50,  80, 100 ]
gravity :gimp_toolbox,   [   0,   0,  10, 100 ]
gravity :gimp_dock,      [ 100,   0,  10, 100 ]

# Pidgin
gravity :pidgin_buddylist, [ 100,   0,  20, 100 ]
gravity :pidgin_chat,      [   0,   0,  80, 100 ]

#
# == Grabs
#

# ALT-Tab
grab "A-Tab" do |c|
  sel     = 0
  clients = Subtlext::View.current.clients

  clients.each_index do |idx|
    if (clients[idx].id == c.id)
      sel = idx + 1 if (idx < clients.size - 1)
    end
  end

  clients[sel].focus
end

# Switch current view
views = [:ViewSwitch0, :ViewSwitch1, :ViewSwitch2, :ViewSwitch3, :ViewSwitch4, :ViewSwitch5, :ViewSwitch6, :ViewSwitch7, :ViewSwitch8, :ViewSwitch9]
views.each_with_index do |v, i|
  grab "W-#{i}", v
end

# Move mouse to screen1, screen2, ...
#grab "W-A-1", :ScreenJump1
#grab "W-A-2", :ScreenJump2
#grab "W-A-3", :ScreenJump3
#grab "W-A-4", :ScreenJump4

def validSyntax()
  return `ruby -c ~/.config/subtle/subtle.rb`.chop.downcase == "syntax ok"
end

grab "W-C-q" do |c|
  if validSyntax()
    Subtlext::Subtle.reload()
  else
    puts "Incorrect Syntax, not reloading."
  end
end

grab "W-C-S-q" do |c|
  if validSyntax()
    Subtlext::Subtle.restart()
  else
    puts "Incorrect Syntax, not restarting."
  end
end

grab "W-C-r", :SubtleQuit

grab "W-B1", :WindowMove
grab "W-B3", :WindowResize

grab "W-f", :WindowFloat
grab "W-space", :WindowFull
grab "W-s", :WindowStick
grab "W-k", :WindowRaise
grab "W-j", :WindowLower

grab "W-Left",  :WindowLeft
grab "W-Down",  :WindowDown
grab "W-Up",    :WindowUp
grab "W-Right", :WindowRight

grab "W-S-c", :WindowKill

# Cycle between given gravities
grab "W-KP_7", [ :top_left,     :top_left66,     :top_left33     ]
grab "W-KP_8", [ :top,          :top66,          :top33          ]
grab "W-KP_9", [ :top_right,    :top_right66,    :top_right33    ]
grab "W-KP_4", [ :left,         :left66,         :left33         ]
grab "W-KP_5", [ :center,       :center66,       :center33       ]
grab "W-KP_6", [ :right,        :right66,        :right33        ]
grab "W-KP_1", [ :bottom_left,  :bottom_left66,  :bottom_left33  ]
grab "W-KP_2", [ :bottom,       :bottom66,       :bottom33       ]
grab "W-KP_3", [ :bottom_right, :bottom_right66, :bottom_right33 ]

grab "W-Return", "urxvt"
grab "W-b" do
  tag = Subtlext::Tag.find( "scratchpad" )
  if tag and not tag.clients.empty?
    client = tag.clients[0]
    tag.clients.each do |c|
      puts c.flags
      if c.hidden?
        c.show()
        #c.focus()
      else
        c.hide()
      end
    end
  else
    Subtlext::Subtle.spawn( "urxvt -name scratchpad" )
  end
end

grab "S-F2" do |c|
  puts c.name
end

#
# == Tags
#

# Simple tags
tag "terms"      , "xterm|[u]?rxvt"
tag "browser"    , "firefox|navigator"
tag "other"      , "transmission"
tag "music"      , "spotify"
tag "editor1"    , "eclipse"
tag "editor2"    , "gvim"
tag "scratchpad" , "scratchpad"

# Modes
tag "resize" do
  match "gvim"
  resize true
end

tag "stick" do
  match "mplayer|scratchpad"
  float true
  stick true
end

tag "float" do
  match "display"
  float true
end

# IM
tag "pidgin_chat" do
  match   :role => "conversation"
  gravity :pidgin_chat
end

tag "pidgin_roster" do
  match   :role => "buddy_list"
  gravity :pidgin_buddylist
end

# Gimp
tag "gimp", "gimp.*"

tag "gimp_image" do
  match   :role => "gimp-image-window"
  gravity :gimp_image
end

tag "gimp_toolbox" do
  match   :role => "gimp-toolbox"
  gravity :gimp_toolbox
end

tag "gimp_dock" do
  match   :role => "gimp-dock"
  gravity :gimp_dock
end

#
# == Views
#

view "im",    "pidgin_.*"
view "terms", "terms|default"
view "www",   "browser"
view "dev",   "editor1"
view "dev2",  "editor2"
view "music", "music"
view "other", "other|gimp"

#
# == Launcher
#

begin
  require "/stuff/programming/ruby/launcher/launcher.rb"
rescue LoadError => error
  puts error
end

grab "W-p" do
  Launcher::Launcher.instance.run
end

