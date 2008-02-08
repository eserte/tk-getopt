#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: error.t,v 1.1 2008/02/08 22:01:39 eserte Exp $
# Author: Slaven Rezic
#

use strict;

BEGIN {
    if (!eval q{
	use Tk;
	use Test::More;
	use File::Temp qw(tempfile);
	1;
    }) {
	print "1..0 # skip: no Tk, File::Temp and/or Test::More module\n";
	exit;
    }

    if ($^O eq 'MSWin32') {
	print "1..0 # skip: Does not work under MSWin32, probably\n";
	exit;
    }
}

use Tk::Getopt;

plan tests => 3;

my($fh, $file) = tempfile(UNLINK => 1);
chmod 0000, $file;

my %options;
my $opt = Tk::Getopt->new(-opttable => ['test','=s','default'],
			  -options  => \%options,
			  -filename => $file,
			  -useerrordialog => 1,
			 );
eval { $opt->save_options };
my($err) = $@ =~ /^(.*)/;
ok($@, "Found error <$err>");

SKIP: {
    my $mw = eval { tkinit };
    skip("Cannot create MainWindow", 2)
	if !$mw;

    eval { $opt->save_options };
    ok($@, "Called within eval, still no window");

    $mw->after(300, sub {
		   $mw->Walk(sub { $_[0]->isa("Tk::Button") && $_[0]->invoke });
	       });
    $opt->save_options;

    pass("Dialog destroyed, hopefully");
}

__END__
