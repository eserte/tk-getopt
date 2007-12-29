use 5.005;
use ExtUtils::MakeMaker;

print STDERR "Checking for Tk...";
eval {
    require Tk;
};
if ($@) {
    print STDERR " failed\n", <<EOF;
$@
The GUI part of Tk::Getopt will not work without Tk. Please install
the Tk distribution (e.g. Tk402.202).

EOF
  sleep 2;
} else {
    print STDERR " ok\n";
}

print STDERR "Checking for Data::Dumper...";
eval {
    require Data::Dumper;
};
if ($@) {
    print STDERR " failed\n", <<EOF;
$@
Data::Dumper is used to store options permanently to a file.

EOF
  sleep 2;
} else {
    print STDERR " ok\n";
}

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile
    (
     'NAME'	=> 'Tk::Getopt',
     'VERSION_FROM' => 'Getopt.pm', # finds $VERSION
     # no PREREQ, because everything is in fact optional...
    );

sub MY::postamble {
    my $postamble = "";

    if (defined $ENV{USER} && $ENV{USER} eq 'eserte' && $^O =~ /bsd/i && -f "../../perl.release.mk") {
	$postamble .= <<'EOF';

.include "../../perl.release.mk"
.include "../../perl.cvs.mk"

EOF
    }

    $postamble;
}
