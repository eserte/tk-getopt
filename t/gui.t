# -*- perl -*-
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1\n"; }

END {print "not ok 1\n" unless $loaded;}

use Tk;
use Tk::Getopt;
$loaded = 1;

@opttable =
  (#'loading',
   ['adbfile', '=s', undef, 
    {'alias' => ['f'],
     'help' => 'The default database file',
     'longhelp' => "This is an example for a longer help\nYou can use multiple lines\n",
     'subtype' => 'file',
    }],
   ['exportfile', '=s', undef,
    {'choices' => ["/tmp/export.dat", "$ENV{HOME}/export.dat"],
     'subtype' => 'file'}],
   ['dumpfile', '=s', '/tmp/dump', {'subtype' => 'file'}],
   ['autoload', '!', 0,
    {'help' => 'Turns autoloading of the default database file on or off'}],
   
   'x11',
   ['bg', '=s', undef, 
    {'callback' =>
     sub {
	 if ($options->{'bg'}) {
	     $top->optionAdd("*background" => $options->{'bg'}, 'userDefault');
	     $top->optionAdd("*backPageColor" => $options->{'bg'},
			     'userDefault');
	 }
     },
     'help' => 'Background color'}],
   ['fg', '=s', undef,
    {'callback' => 
     sub {
	 $top->optionAdd("*foreground" => $options->{'fg'}, 'userDefault')
	   if $options->{'fg'};
     },
     'help' => 'Foreground color'}],
   ['font', '=s', undef,
    {'callback' =>
     sub {
	 $top->optionAdd("*font" => $options->{'font'}, 'userDefault')
	   if $options->{'font'};
     },
     'help' => 'Default font'}],
   ['i18nfont', '=s', undef,
    {'callback' =>
     sub {
	 if (!$options->{'i18nfont'}) {
	     my(@s) = split(/-/, $top->optionGet('font', 'Font'));
	     if ($#s == 14) {
		 $options->{'i18nfont'} = join('-', @s[0..$#s-2]) . '-%s';
	     }
	 }
     },
     'help' => 'Font used for different encodings'}],

   'appearance',
   ['infowin', '!', 1, {'label' => 'Balloon', 'help' => 'Switches balloons on or off'}] ,
   ['undermouse', '!', 1,
    {'callback' =>
     sub {
	 $top->optionAdd("*popover" => 'cursor', 'userDefault')
	   if $options->{'undermouse'};
     },
     'help' => 'Popup new windows under mouse cursor'}],
   ['fasttemplate', '!', 0,
    {'help' => 'Fast templates without lists of existing objects'}],
   ['shortform', '!', 0,
    {'help' => 'Use a shorter form'}],
   ['editform', '!', 1,
    {'help' => 'Turn editing of forms on or off'}],
   ['statustext', '!', 0,
    {'help' => 'Turn use of a seperate window for status text on or off'}],
   ['debug', '!', 0, {'alias' => ['d']}],
   ['lang', '=s', undef,
    {'choices' => ['en', 'de', 'hr'], 'strict' => 1,
     'label' => 'Language'}],
   ['stderr-extern', '!', 0],

   'extern',
   ['imageviewer', '=s', 'xv %s',
    {'choices' => ['xli %s', 'xloadimage %s', '#NETSCAPE file:%s']}],
   ['internimageviewer', '!', 1,
    {'help' => 'Use intern image viewer if possible'}],
   ['browsercmd', '=s', '#NETSCAPE %s',
    {'choices' => ['#WEB %s', 'mosaic %s', '#XTERM lynx %s']}],
   ['mailcmd', '=s', '#XTERM mail %s', 
    {'choices' => ['#NETSCAPE mailto:%s', '#XTERM elm %s']}],
   ['netscape', '=s', 'netscape',
    {'help' => 'Path to the netscape executable'}],
   ['xterm', '=s', 'xterm -e %s',
    {'choices' => ['color_xterm -e %s', 'rxvt -e %s']}],
   
   'dialing',
   ['devphone', '=s', '/dev/cuaa1',
    {'help' => 'The phone or modem device'}],
   ['dialcmd', '=s', '#DIAL %s',
    {'choices' => ['#XTERM dial %s']}],
   ['hangupcmd', '=s', '#HANGUP'],
   ['dialat', '=s', 'ATD',
    {'choices' => ['ATDT', 'ATDP'],
     'help' => 'Use ATDT for tone and ATDP for pulse dialing'}],
   
   'adr2tex',
   ['adr2tex-cols', '=i', 8, {'range' => [2, 16],
			      'help' => 'Number of columns'}],
   ['adr2tex-font', '=s', 'sf', 
    {'choices' => ['cmr5', 'cmr10', 'cmr17', 'cmss10', 'cmssi10',
		   'cmtt10 scaled 500', 'cmtt10']}],
   ['adr2tex-headline', '=s', 1,
    {'help' => 'Print a headline (default headline: 1)'}],
   ['adr2tex-footer', '=s', 1,
    {'help' => 'Print a footer (default footer: 1)'}],
   ['adr2tex-usecrogersort', '!', 1],
   
  );

$options = {};
$optfilename = "t/opttest";
$opt = new Tk::Getopt(-opttable => \@opttable,
		      -options => $options,
		      -filename => $optfilename);

$opt->set_defaults;
$opt->load_options;
if (!$opt->get_options) {
    die $opt->usage;
}
$top = new MainWindow;
$top->withdraw;

#eval {$opt->process_options};
$opt->process_options;
if ($@) { warn $@ }

my $w;
use Data::Dumper;
$timer = $top->after(60*1000, sub {
			 $t2 = $top->Toplevel(-popover => 'cursor');
			 $t2->Label(-text => "Self-destruction in 5s")->pack;
			 $t2->Popup;
			 $top->after(5*1000, sub { $w->destroy })
		     });

$w = $opt->option_editor($top,
			 -statusbar => 1,
			 -popover => 'cursor',
			 '-wait' => 1);
$timer->cancel;

$w = $opt->option_editor($top);
$w->OnDestroy(sub {$top->destroy});
$top->after(5*1000, sub { $w->destroy });

MainLoop;
#foreach (sort keys %$options) {
#    print "$_ = ", $options->{$_}, "\n";
#}
print "ok 1\n";

