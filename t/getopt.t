print "1..3\n";

use Tk::Getopt;

@ARGV = qw(--help --file foo --foo --nobar --num=5 -- file);

%getopt = (
   'help'   => \$HELP,
   'file:s' => \$FILE,
   'foo!'   => \$FOO,
   'bar!'   => \$BAR,
   'num:i'  => \$NO,
);

$opt = new Tk::Getopt(-getopt => \%getopt);

if (!$opt->get_options) {
    print "not ";
}
print "ok 1\n";

print "not " unless $HELP && $FOO && !$BAR && $FILE eq 'foo' && $NO == 5;
print "ok 2\n";

print "not " unless "@ARGV" eq "file";
print "ok 3\n";

