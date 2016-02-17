#!/usr/bin/perl

require "$ENV{HOME}/.config/obmenu-generator/config.pl";

my $editor = $CONFIG->{editor};

our $SCHEMA = [
  {item => ['xdg-open .',       'File Manager', 'file-manager']},
  {item => ['st',               'Terminal',     'terminal']},
  {item => ['xdg-open http://', 'Web Browser',  'web-browser']},
  {item => ['gmrun',            'Run command',  'system-run']},

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

  {item => ['slock', 'Lock', 'lock']},

  {exit => ['Exit', 'exit']},
]
