#
# == Functions
#

def ensure_tag( name )
  if not Subtlext::Tag[ name ]
    t = Subtlext::Tag.new( name )
    t.save
    
    return t
  end

  return Subtlext::Tag[ name ]
end

def create_tags()
  all = ensure_tag( "all" )
  ensure_tag( "none" )

  Subtlext::View.all.each do |v|
    v.tag( [ all ] )

    ensure_tag( v.name )
  end

  Subtlext::View.current.jump
end

def log( msg )
  puts "subtle: #{msg}"
end

def tag_exists?( tag )
  return Subtlext::Tag[ tag ] ? 'Yes' : 'No'
end

#
# == Hooks
#

$previous_view = nil
on :view_jump do |v|
  if not $previous_view
    $previous_view = Subtlext::View.current
  elsif Subtlext::View.current.name != v.name
    $previous_view = Subtlext::View.current
  end
end

on :client_create do |c|
  if c.has_tag?( "scratchpad" )
    view = Subtlext::View.current
    view.tag( view.name ) unless view.tags.include?( view.name )
    c.tags = [ "scratchpad", view.name ]
  end
end

on :reload do
  create_tags()

  tag = Subtlext::Tag[ "scratchpad" ]
  if tag
    if tag.clients and not tag.clients.empty?
      client = tag.clients.first
      if client.has_tag?( "default" )
        client.tags = [ "scratchpad", "none" ]
      end
    end
  end
end


on :start do
  create_tags()
end

#
# == Sublets
#

sublet :clock do
  interval      30
  format_string "%H:%M %Y-%m-%d"
end

sublet :cpu do
  interval 10
end

sublet :memory do
  interval 10
end

sublet :wifi do
  interval 10
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
set :separator, ""
set :outline, 0

#
# == Screen
#

screen 1 do
  stipple false
  top     [ :views, :title, :spacer, :tray, :sublets ]
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

# Switch current view
views = [:ViewSwitch0, :ViewSwitch1, :ViewSwitch2, :ViewSwitch3, :ViewSwitch4, :ViewSwitch5, :ViewSwitch6, :ViewSwitch7, :ViewSwitch8, :ViewSwitch9]
views.each_with_index do |v, i|
  grab "W-#{i}", v
end

grab "W-w" do
  if $previous_view
    $previous_view.jump
  end
end

grab "W-C-r", :SubtleQuit

grab "W-B1", :WindowMove
grab "W-B3", :WindowResize

grab "W-u", :WindowFloat
grab "W-i", :WindowFull
grab "W-o", :WindowStick
grab "W-j" do |c|
  sel     = 0
  clients = Subtlext::Client.visible()

  clients.each_index do |i|
    if clients[i].id == c.id
      sel = i + 1
      break
    end
  end

  clients[sel % clients.size].focus()
end

grab "W-k" do |c|
  sel     = 0
  clients = Subtlext::Client.visible()

  clients.each_index do |i|
    if clients[i].id == c.id
      sel = i - 1
      break
    end
  end

  clients[sel % clients.size].focus()
end

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

# Programs
$browser = ENV["BROWSER"]
grab "W-Return", "urxvt"
grab "W-x", "gvim"
grab "W-z", $browser
grab "W-n", "xscreensaver-command --lock"
grab "W-grave" do
  tag  = Subtlext::Tag[ "scratchpad" ]
  view = Subtlext::View.current
  if not tag or tag.clients.empty?
    Subtlext::Subtle.spawn( "urxvt -name scratchpad" )
  else
    client = tag.clients.first
    if client.has_tag?( view.name )
      # Hide
      client.tags = [ "scratchpad", "none" ]
    else
      # Move to current
      view.tag( view.name ) unless view.tags.include?( view.name )
      client.tags = [ "scratchpad", view.name ]
    end
  end
end

# Multimedia keys
grab "XF86AudioMute", "amixer set Master toggle"

# Window Info
grab "W-v" do |c|
  out = [ "wm_name:  %s" % c.name,
          "wm_class: %s" % c.instance,
          "wm_role:  %s" % c.role,
          "winid:    %s" % c.win,
          "id:       %s" % c.id,
          "",
          "tags:     %s" % c.tags.join( ", " ),
          "views:    %s" % c.views.map { |v| v.name }.join( ", " ),
          "",
          "geometry: %s" % c.geometry.to_s,
          "gravity:  %s" % c.gravity.to_s,
          "",
          "flags:    %#b" % c.flags,
          "float:    %s" % c.is_float?,
          "stick:    %s" % c.is_stick?,
          "full:     %s" % c.is_full?
        ].join( "\n" )

  xmessage( out )
end

#
# == Tags
#

tag "scratchpad" do
  match :instance => "scratchpad"
  urgent true
  gravity :center33
end

tag "urxvt" do
  match "urxvt"
  exclude :instance => "scratchpad"
  gravity :center
end

tag "browser" do
  match "firefox|navigator|browser"
  gravity :center
end

tag "editor0" do
  match "eclipse|.*Intellij.*"
end

tag "editor1" do
  match "gvim"
  resize true
  gravity :center
end

tag "tuxguitar" do
  match "tuxguitar"
  gravity :center
end

tag "spotify" do
  match "spotify"
  gravity :center
end

tag "other" do
  match "transmission|virtualbox"
  gravity :center
end

tag "xmessage" do
  match "xmessage"
  #float true
  urgent true
  stick true
  gravity :center33
end

tag "mplayer" do
  match "mplayer"
  stick true
  float true
  urgent true
end

tag "sticknfloat" do
  match "display|dialog|subtly|subtle|preferences"
  match :type => :dialog
  match :type => :splash
  float true
  stick true
  urgent true
end

# IM
tag "pidgin", "new_mail_detailed|pidgin"

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
view "im"    , "pidgin.*"
view "terms" , "urxvt"
view "www"   , "browser"
view "dev"   , "editor0"
view "dev2"  , "editor1"
view "other" , "gimp"
view "music" , "spotify|tuxguitar"
view "void"  , "default"

#
# == Launcher
#

begin
  require "/stuff/software/subtle/subtle-contrib/ruby/launcher.rb"

  Subtle::Contrib::Launcher.paths = [ "/usr/bin", "~/bin" ]

  Subtle::Contrib::Launcher.fonts = [
    "xft:DejaVu Sans Mono:pixelsize=80:antialias=true",
    "xft:DejaVu Sans Mono:pixelsize=12:antialias=true"
  ]
rescue LoadError => error
  puts error
end

grab "W-p" do
  Subtle::Contrib::Launcher.run
end

#
# == Other
#

def xmessage( msg, buttons = ["Okay"] )
  Subtlext::Subtle.spawn( "xmessage '#{msg}' -buttons #{buttons.join( "," )}" )
end
