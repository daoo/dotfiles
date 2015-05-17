#!/usr/bin/perl

our $CONFIG = {
  "editor"              => "geany",
  "Linux::DesktopFiles" => {
                             desktop_files_paths     => ["/usr/share/applications"],
                             gtk_rc_filename         => "/home/daniel/.gtkrc-2.0",
                             icon_dirs_first         => undef,
                             icon_dirs_last          => undef,
                             icon_dirs_second        => undef,
                             keep_unknown_categories => 1,
                             skip_entry              => undef,
                             skip_filename_re        => undef,
                             skip_svg_icons          => 0,
                             strict_icon_dirs        => undef,
                             substitutions           => undef,
                             terminalization_format  => "%s -e '%s'",
                             terminalize             => 1,
                             unknown_category_key    => "other",
                           },
  "missing_icon"        => "gtk-missing-image",
  "name_keys"           => ["Name"],
  "terminal"            => "st",
  "VERSION"             => 0.63,
}
