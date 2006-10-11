#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: gui2.t,v 1.5 2006/10/11 20:27:11 eserte Exp $
# Author: Slaven Rezic
#

use strict;

BEGIN {
    if (!eval q{
	use Test::More;
	use Tk;
	use Tk::Dial;
	1;
    }) {
	print "1..0 # skip: no Test::More, Tk and/or Tk::Dial modules\n";
	exit;
    }
}

plan tests => 4;

use Tk::Getopt;

{
    # this is a non-working example, since Dial does not use -textvariable or
    # -variable :-(
    package MyOptions;
    use Tk::Getopt;
    use vars qw(@ISA);
    @ISA = qw(Tk::Getopt);

    sub _number_widget {
	my($self, $frame, $opt) = @_;
	my $v = $self->_varref($opt);
	$frame->Dial
	    (-min => $opt->[3]{'range'}[0],
	     -max => $opt->[3]{'range'}[1],
	     '-format' => '%' . ($opt->[1] =~ /f/ ? 'f' : 'd'),
#            -variable => $v,
	     -value => $$v,
	    );
    }
}

@ARGV = qw(--integer=12 --float=3.141592653);

my $dialbug = q{Note:
If you are using dial widgets, you cannot use undo or share more than one
options editor at one time.};

my @opttable =
  (['integer', '=i', undef, {'range' => [-10, 10],
			     'help' => $dialbug}],
   ['float', '=f', undef, {'range' => [-3, 5],
			   'help' => $dialbug}],
   ['defint', '=i', 50,
    {'range' => [0, 100],
     'widget' => sub { shift->Tk::Getopt::_number_widget(@_)}}]);

my $opt = new MyOptions(-opttable => \@opttable);
isa_ok($opt, "MyOptions");
isa_ok($opt, "Tk::Getopt");
$opt->set_defaults;
if (!$opt->get_options) {
    die $opt->usage;
}

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

my $batch_mode = !!$ENV{BATCH};
my $timerlen = ($batch_mode ? 1000 : 60*1000);

use Tk;
my $top = new MainWindow;
$top->after(100, sub {
		my $e = $opt->option_editor($top);
		$e->after($timerlen, sub { $e->destroy });
		$e->waitWindow;
		ok(1);
		&in_frame;
	    });
MainLoop;

sub in_frame {
    $top->Label(-text => 'Options editor in frame')->pack;
    my $e = $opt->option_editor($top, -toplevel => 'Frame')->pack;
    $e->after($timerlen, sub { $e->destroy });
    $e->waitWindow;
    ok(1);
    $top->destroy;
}
