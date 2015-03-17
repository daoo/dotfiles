#!/usr/bin/perl

=for comment

    item:      add an item inside the menu               {item => ["command", "label", "icon"]},
    cat:       add a category inside the menu             {cat => ["name", "label", "icon"]},
    sep:       horizontal line separator                  {sep => undef}, {sep => "label"},
    pipe:      a pipe menu entry                         {pipe => ["command", "label", "icon"]},
    raw:       any valid Openbox XML string               {raw => q(xml string)},
    begin_cat: begin of a category                  {begin_cat => ["name", "icon"]},
    end_cat:   end of a category                      {end_cat => undef},
    obgenmenu: generic menu settings                {obgenmenu => ["label", "icon"]},
    exit:      default "Exit" action                     {exit => ["label", "icon"]},

=cut

# NOTE:
#    * Keys and values are case sensitive. Keep all keys lowercase.
#    * ICON can be a either a direct path to an icon or a valid icon name
#    * Category names are case insensitive. (X-XFCE and x_xfce are equivalent)

require "$ENV{HOME}/.config/obmenu-generator/config.pl";

my $editor = $CONFIG->{editor};

our $SCHEMA = [
  {item => ['xdg-open .',        'File Manager',      'file-manager']},
  {item => ['termite',           'Terminal',          'terminal']},
  {item => ['xdg-open http://',  'Web Browser',       'web-browser']},
  {item => ['gmrun',             'Run command',       'system-run']},

  {sep => 'Applications'},

  {cat => ['utility',     'Accessories', 'applications-utilities']},
  {cat => ['development', 'Development', 'applications-development']},
  {cat => ['education',   'Education',   'applications-science']},
  {cat => ['game',        'Games',       'applications-games']},
  {cat => ['graphics',    'Graphics',    'applications-graphics']},
  {cat => ['audiovideo',  'Multimedia',  'applications-multimedia']},
  {cat => ['network',     'Network',     'applications-internet']},
  {cat => ['office',      'Office',      'applications-office']},
  {cat => ['other',       'Other',       'applications-other']},
  {cat => ['settings',    'Settings',    'applications-accessories']},
  {cat => ['system',      'System',      'applications-system']},

  {item => ['xscreensaver-command -lock', 'Lock', 'lock']},

  {exit => ['Exit', 'exit']},
]
