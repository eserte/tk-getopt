use ExtUtils::MakeMaker;

$| = 1;

print "Checking for Tk...";
eval {
    require Tk;
};
if ($@) {
    print " failed\n", <<EOF;
$@
Tk::Getopt will not work without Tk. Please install the Tk distribution
(e.g. Tk402.202).

EOF
  sleep 2;
} else {
    print " ok\n";
}

print "Checking for Data::Dumper...";
eval {
    require Data::Dumper;
};
if ($@) {
    print " failed\n", <<EOF;
$@
Data::Dumper is used to store options permanently to a file.

EOF
  sleep 2;
} else {
    print " ok\n";
}

print "Checking for Tk::FileDialog...";
eval {
    require Tk::FileDialog;
};
if ($@) {
    print " failed\n", <<EOF;
$@
Tk::FileDialog provides a file dialog for filename options.

EOF
  sleep 2;
} else {
    print " ok\n";
}

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Tk::Getopt',
    'VERSION_FROM' => 'Getopt.pm', # finds $VERSION
    'LIBS'	=> [''],   # e.g., '-lm' 
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING' 
    'INC'	=> '',     # e.g., '-I/usr/include/other' 
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => '.gz',
		    'POSTOP'=>'-$(CHMOD) 644 $(DISTVNAME).tar$(SUFFIX)'},
);
