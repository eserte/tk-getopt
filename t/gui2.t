# -*- perl -*-

BEGIN { $| = 1; print "1..2\n";}
END {print "not ok 1\n" unless $loaded;}

$loaded = 1;
eval { require Tk::Dial };
if ($@) {
    print "# skipping test 1..2\n";
    print "ok 1\nok 2\n";
    exit;
}

# this is a non-working example, since Dial does not use -textvariable or
# -variable :-(
package MyOptions;
use Tk::Getopt;
@ISA = qw(Tk::Getopt);

sub _number_widget {
    my($self, $frame, $opt) = @_;
    my $v = $self->_varref($opt);
    $frame->Dial
      (-min => $opt->[3]{'range'}[0],
       -max => $opt->[3]{'range'}[1],
       '-format' => '%' . ($opt->[1] =~ /f/ ? 'f' : 'd'),
#       -variable => $v,
       -value => $$v,
      );
}

package main;

@ARGV = qw(--integer=12 --float=3.141592653);

$dialbug = q{Note:
If you are using dial widgets, you cannot use undo or share more than one
options editor at one time.};

@opttable =
  (['integer', '=i', undef, {'range' => [-10, 10],
			     'help' => $dialbug}],
   ['float', '=f', undef, {'range' => [-3, 5],
			   'help' => $dialbug}],
   ['defint', '=i', 50,
    {'range' => [0, 100],
     'widget' => sub { shift->Tk::Getopt::_number_widget(@_)}}]);

$opt = new MyOptions(-opttable => \@opttable);
$opt->set_defaults;
if (!$opt->get_options) {
    die $opt->usage;
}

if (!defined $ENV{BATCH}) { $ENV{BATCH} = 1 }

my $batch_mode = !!$ENV{BATCH};
my $timerlen = ($batch_mode ? 1000 : 60*1000);

use Tk;
$top = new MainWindow;
$top->after(100, sub {
		my $e = $opt->option_editor($top);
		$e->after($timerlen, sub { $e->destroy });
		$e->waitWindow;
		print "ok 1\n";
		&in_frame;
	    });
MainLoop;

sub in_frame {
    $top->Label(-text => 'Options editor in frame')->pack;
    my $e = $opt->option_editor($top, -toplevel => 'Frame')->pack;
    $e->after($timerlen, sub { $e->destroy });
    $e->waitWindow;
    print "ok 2\n";
    $top->destroy;
}
