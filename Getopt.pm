# -*- perl -*-

#
# $Id: Getopt.pm,v 1.10 1997/03/12 13:07:09 eserte Exp $
# Author: Slaven Rezic
#
# Copyright © 1997 Slaven Rezic. All rights reserved.
# This package is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#
# Mail: <URL:mailto:eserte@cs.tu-berlin.de>
# WWW:  <URL:http://www.cs.tu-berlin.de/~eserte/>
#

package Tk::Getopt;
use strict;
use vars qw($loadoptions $VERSION);

$VERSION = '0.30';

sub new {
    my($pkg, %a) = @_;
    my $self = {};

    $self->{'options'} = delete $a{'-options'} if exists $a{'-options'};

    if (exists $a{'-opttable'}) {
	$self->{'opttable'} = delete $a{'-opttable'};
    } elsif (exists $a{'-getopt'}) {
	# build opttable from -getopt argument
	my @optionlist;
	my $genprefix = "(--|-|\\+)";
	if (ref $a{'-getopt'} eq 'HASH') {
	    # convert hash to array
	    @optionlist
	      = map { ($_, $a{'-getopt'}->{$_}) } keys %{$a{'-getopt'}};
	} else {
	    @optionlist = @{$a{'-getopt'}};
	}
	delete $a{'-getopt'};
	# check if first argument is hash reference
	if (ref $optionlist[0] eq 'HASH') {
	    $self->{'options'} = shift @optionlist;
	}
	while (@optionlist > 0) {
	    my $opt = shift @optionlist;
	    # Strip leading prefix so people can specify "--foo=i"
	    # if they like.
	    $opt = $' if $opt =~ /^($genprefix)+/;

	    if ($opt !~ /^(\w+[-\w|]*)?(!|[=:][infse][@%]?)?$/) {
		warn "Error in option spec: \"", $opt, "\"\n";
		next;
	    }
	    my($o, $c) = ($1, $2);
	    $c = '' unless defined $c;
	    my @aliases;
	    if ($o =~ /\|/) {
		# Handle alias names
		@aliases = split(/\|/, $o);
		$o = shift @aliases;
	    }
	    my $varref;
	    # If no linkage is supplied in the @optionlist, copy it from
	    # the userlinkage ($self->{'options'}) if available.
	    if (defined $self->{'options'} && !ref $optionlist[0]) {
		$varref = (exists $self->{'options'}{$o} ?
			   $self->{'options'}{$o} :
			   \$self->{'options'}{$o});
	    } elsif (ref $optionlist[0]) {
		# link to global variable 
		$varref = shift @optionlist;
	    }
	    my %a;
	    if (defined $varref) {
		$a{'var'} = $varref;
	    }
	    if (defined @aliases) {
		$a{'alias'} = \@aliases;
	    }
	    push(@{$self->{'opttable'}}, [$o, $c, undef, \%a]);
	}
    } else {
	die "No opttable array ref or getopt hash ref";
    }

    $self->{'caller'}   = (caller)[0];
    $self->{'filename'} = delete $a{'-filename'};
    $self->{'nosafe'}   = delete $a{'-nosafe'};

    die "Unknown arguments: " . join(" ", %a) if %a != ();

    bless $self, $pkg;
}

# Returns a list with all option names
sub _opt_array {
    my $self = shift;
    my @res;
    foreach (@{$self->{'opttable'}}) {
	push(@res, $_) if ref $_ eq 'ARRAY';
    }
    @res;
}

# Returns a reference to the option variable given by $opt
sub _varref {
    my($self, $opt) = @_;
    if($opt->[3]{'var'}) {
	$opt->[3]{'var'};
    } elsif ($self->{'options'}) {
	\$self->{'options'}{$opt->[0]};
    } else {
	# Link to global $opt_XXX variable.
	# Make sure a valid perl identifier results.
	my $v;
	($v = $opt->[0]) =~ s/\W/_/g;
	eval q{\$} . $self->{'caller'} . q{::opt_} . $v; # XXX @, %
    }
}

sub set_defaults {
    my $self = shift;
    my $opt;
    foreach $opt ($self->_opt_array) {
	if (defined $opt->[2]) {
	    $ {$self->_varref($opt)} = $opt->[2];
	}
    }
}

sub load_options {
    my($self, $filename) = @_;
    $filename = $self->{'filename'} if !$filename;
    return if !$filename;
    if ($self->{'nosafe'}) {
	require Safe;
	my $c = new Safe;
	$c->share('$loadoptions');
	if (!$c->rdo($filename)) {
	    warn "Can't load $filename";
	    return undef;
	}
    } else {
	eval {do $filename};
	if ($@) {
	    warn $@;
	    return undef;
	}
    }

    my $opt;
    foreach $opt ($self->_opt_array) {
	if (exists $loadoptions->{$opt->[0]}) {
	    $ {$self->_varref($opt)} = $loadoptions->{$opt->[0]};
	}
    }
    1;
}

sub save_options {
    my($self, $filename) = @_;
    $filename = $self->{'filename'} if !$filename;
    die "Saving disabled" if !$filename;
    eval "require Data::Dumper";
    if ($@) {
	warn $@;
	die "No Data::Dumper\n";
    } else {
	if (open(OPT, ">$filename")) {
	    my %saveoptions;
	    my $opt;
	    foreach $opt ($self->_opt_array) {
		$saveoptions{$opt->[0]} = $ {$self->_varref($opt)}
		  if !$opt->[3]{'nosave'};
	    }
	    if (Data::Dumper->can('Dumpxs')) {
		# use faster version of Dump
		print OPT
		  Data::Dumper->Dumpxs([\%saveoptions], ['loadoptions']);
	    } else {
		print OPT
		  Data::Dumper->Dump([\%saveoptions], ['loadoptions']);
	    }
	    close OPT;
	    warn "Options written to $filename";
	    1;
	} else {
	    warn "Can't write to $filename";
	    die "Writing failed\n";
	    undef;
	}
    }
}

sub get_options {
    my $self = shift;
    my %getopt;
    my $opt;
    foreach $opt ($self->_opt_array) {
	$getopt{_getopt_long_string($opt->[0], $opt->[1])} =
	  $self->_varref($opt);
	# process aliases
	foreach (@{$opt->[3]{'alias'}}) {
	    $getopt{_getopt_long_string($_, $opt->[1])} =
	      $self->_varref($opt);
	}
    }
    require Getopt::Long;
    Getopt::Long::GetOptions(%getopt);
}

# Builds a string for Getopt::Long. Arguments are option name and option
# type (e.g. '!' or '=s').
sub _getopt_long_string {
    my($option, $type) = @_;
    $option . (length($option) == 1 && ($type eq '' || $type eq '!')
	       ? '' : $type);
}

# Prints option name with one or two dashes
sub _getopt_long_dash {
    my $option = shift;
    (length($option) == 1 ? '' : '-') . "-$option";
}

sub usage {
    my $self = shift;
    my $usage = "Usage: $0 [options]\n";
    my $opt;
    foreach $opt ($self->_opt_array) {
	# The following prints all options as a comma-seperated list
	# with one or two dashes, depending on the length of the option.
	# Options are sorted by length.
	$usage .= join(', ', 
		       sort { length $a <=> length $b }
		       map { _getopt_long_dash($_) }
		       map { ($opt->[1] eq '!' ? "[no]" : "") . $_ }
		       ($opt->[0], @{$opt->[3]{'alias'}}));
	$usage .= "\t" . $opt->[3]{'help'};
	$usage .= " (default: " . $opt->[2] . ") " if $opt->[2];
	$usage .= "\n";
    }
    $usage;
}

sub process_options {
    my($self, $former) = @_;
    my $options = $self->{'options'};
    foreach ($self->_opt_array) {
	my $opt = $_->[0];
	if (exists $_->[3]{'callback'}) {
	    # execute callback if value changed
	    if (!(defined $former
		  && $ {$self->_varref($_)} eq $former->{$opt})) {
		&{$_->[3]{'callback'}};
	    }
	}
	if ($_->[3]{'strict'}) {
	    # check vor valid values
	    my $v = $ {$self->_varref($_)};
	    if (!grep(/^$v$/, @{$_->[3]{'choices'}})) {
		if (defined $former) {
		    warn "Not allowed: " . $ {$self->_varref($_)}
		    . " for $opt. Using old value $former->{$opt}";
		    $ {$self->_varref($_)} = $former->{$opt};
		} else {
		    die "Not allowed: " . $ {$self->_varref($_)} . " for $opt";
		}
	    }
	}
    }
}

sub _boolean_widget {
    my($self, $frame, $opt) = @_;
    $frame->Checkbutton(-variable => $self->_varref($opt));
}

sub _number_widget {
    my($self, $frame, $opt) = @_;
    $frame->Scale
      (-orient => 'horizontal',
       -from => $opt->[3]{'range'}[0],
       -to => $opt->[3]{'range'}[1],
       -showvalue => 1,
       -resolution => ($opt->[1] =~ /f/ ? 0 : 1),
       -variable => $self->_varref($opt)
      );
}

sub _integer_widget {
    my($self, $frame, $opt) = @_;
    if (exists $opt->[3]{'range'}) {
	$self->_number_widget($frame, $opt);
    } else {
	$self->_string_widget($frame, $opt);
    }
}

sub _float_widget {
    my($self, $frame, $opt) = @_;
    if (exists $opt->[3]{'range'}) {
	$self->_number_widget($frame, $opt);
    } else {
	$self->_string_widget($frame, $opt);
    }
}

sub _list_widget {
    my($self, $frame, $opt) = @_;
    require Tk::BrowseEntry;
    my $w = $frame->BrowseEntry(-variable => $self->_varref($opt));
    my @optlist = @{$opt->[3]{'choices'}};
    unshift(@optlist, $opt->[2]) if defined $opt->[2];
    my $o;
    foreach $o (@optlist) {
	$w->insert("end", $o);
    }
    $w;
}

sub _string_widget {
    my($self, $frame, $opt) = @_;
    if (exists $opt->[3]{'choices'}) {
	$self->_list_widget($frame, $opt);
    } else {
	$frame->Entry(-textvariable => $self->_varref($opt));
    }
}

# Creates one page of the Notebook widget
# Arguments:
#   $optnote: Notebook widget
#   $current_top: title of Notebook page
#   $optlist: list of options for this Notebook page
#   $balloon: Balloon widget
sub _create_page {
    my($self, $optnote, $current_top, $optlist, $balloon) = @_;
    my $current_page = $optnote->{$current_top};
    my $opt;
    my $row = -1;
    foreach $opt (@{$optlist->{$current_top}}) {
	my $f = $current_page;
	my $label;
	my $w;
	if (exists $opt->[3]{'label'}) {
	    $label = $opt->[3]{'label'};
	} else {
	    $label = $opt->[0];
	    if ($label =~ /^(.*)-/ && $1 eq $current_top) {
		$label = $';
	    }
	}
	$row++;
	$f->Label(-text => $label)->grid(-row => $row, -column => 0,
					 -sticky => 'w');
	if (exists $opt->[3]{'widget'}) {
	    # own widget
	    $w = &{$opt->[3]{'widget'}}($self, $f, $opt);
	} elsif ($opt->[1] eq '!' or $opt->[1] eq '') {
	    $w = $self->_boolean_widget($f, $opt);
	} elsif ($opt->[1] =~ /i/) {
	    $w = $self->_integer_widget($f, $opt);
	} elsif ($opt->[1] =~ /f/) {
	    $w = $self->_float_widget($f, $opt);
	} elsif ($opt->[1] =~ /s/) {
	    $w = $self->_string_widget($f, $opt);
	} else {
	    warn "Can't generate options editor entry for $opt->[0]";
	}
	if (defined $w) {
	    $w->grid(-row => $row, -column => 1, -sticky => 'w');
	}
	if (exists $opt->[3]{'help'} && defined $w) {
	    if (defined $balloon) {
		$balloon->attach($w, -msg => $opt->[3]{'help'});
	    }
	}
	if (exists $opt->[3]{'longhelp'}) {
	    $f->Button(-text => '?',
		       -padx => 1,
		       -pady => 1,
		       -command => sub {
			   my $t = $f->Toplevel(-title => $label);
			   $t->Label(-text => $opt->[3]{'longhelp'},
				     -justify => 'left')->pack;
			   $t->Button(-text => 'OK',
				      -command => sub { $t->destroy }
				     )->pack;
		       })->grid(-row => $row, -column => 2, -sticky => 'w');
	}
    }
}

sub option_editor {
    my($self, $top, %a) = @_;
    my $callback  = delete $a{'-callback'};
    my $nosave    = delete $a{'-nosave'};
    my $toplevel  = delete $a{'-toplevel'} || 'Toplevel';
    my $use_statusbar = delete $a{'-statusbar'};
    my $string    = delete $a{'-string'};
    if (!defined $string) {
	$string = {'optedit' => 'Options editor',
		   'undo' => 'Undo',
		   'lastsaved' => 'Last saved',
		   'save' => 'Save',
		   'defaults' => 'Defaults',
		   'ok' => 'OK',
		   'cancel' => 'Cancel'};
    }
    # store old values for undo
    my %undo_options;
    my $opt;
    foreach $opt ($self->_opt_array) {
	$undo_options{$opt->[0]} = $ {$self->_varref($opt)};
    }

    require Tk;
    require Tk::NoteBook;
    require Tk::Balloon;
    my $optedit = eval '$top->' . $toplevel . '(%a)';
    die $@ if $@;
    if ($optedit->can('-title')) {
	$optedit->configure(-title => $string->{optedit});
    }
    my $optnote = $optedit->NoteBook(-ipadx => 6, -ipady => 6);
    my $statusbar;
    if ($use_statusbar) {
	$statusbar = $optedit->Label;
    }
    my $balloon = $optnote->Balloon($use_statusbar 
				    ? (-statusbar => $statusbar)
				    : ());
    my $nopage = 1;
    $optnote->pack(-expand => 1, -fill => 'both');
    my $current_top;
    my $optlist = {};
    foreach $opt (@{$self->{'opttable'}}) {
	if (ref $opt ne 'ARRAY' || $nopage) {
	    undef $nopage if ref $opt ne 'ARRAY';
	    my $label = ($nopage ? $string->{'optedit'} : $opt);
	    $current_top = lc($label);
	    my $c = $current_top;
	    $optlist->{$c} = [];
	    $optnote->add($c,
			  -label => $label,
			  -anchor => 'w',
			  -createcmd =>
			  sub {
			      $self->_create_page($optnote, $c,
						  $optlist, $balloon);
			  });
	    if ($nopage) {
		undef $nopage;
		redo;
	    }
	} else {
	    push(@{$optlist->{$current_top}}, $opt)
	      if !$opt->[3]{'nogui'};
	}
    }

    if (defined $statusbar) {
	$statusbar->pack(-fill => 'x', -anchor => 'w');
    }

    my $f = $optedit->Frame;
    $f->pack(-anchor => 'w');
    $f->Button(-text => $string->{'ok'},
	       -command => sub { $self->process_options(\%undo_options);
				 $optedit->destroy; }
	      )->pack(-side => 'left');
    $f->Button(-text => $string->{'cancel'},
	       -command => sub { $optedit->destroy; }
	      )->pack(-side => 'left');
    $f->Button(-text => $string->{'undo'},
	       -command => sub {
		   my $opt;
		   foreach $opt ($self->_opt_array) {
		       $ {$self->_varref($opt)} = $undo_options{$opt->[0]}
		         if exists $undo_options{$opt->[0]}; 
		   }
	       }
	      )->pack(-side => 'left');
    if ($self->{'filename'}) {
	$f->Button(-text => $string->{'lastsaved'},
		   -command => sub {
		       $top->Busy;
		       $self->load_options;
		       $top->Unbusy;
		   }
		  )->pack(-side => 'left');
	if (!$nosave) {
	    my $sb;
	    $sb = $f->Button(-text => $string->{'save'},
			     -command => sub {
				 $top->Busy;
				 eval { $self->save_options };
				 if ($@ =~ /No Data::Dumper/) {
				     $sb->configure(-state => 'disabled');
				 }
				 $top->Unbusy;
			     }
			    )->pack(-side => 'left');
	}
    }
    $f->Button(-text => $string->{'defaults'},
	       -command => sub {
		   $self->set_defaults;
	       }
	      )->pack(-side => 'left');

    &$callback($self, $optedit) if $callback;

    $optedit;
}

1;

__END__

=head1 NAME

Tk::Getopt - User configuration window for Tk with interface to Getopt::Long

=head1 SYNOPSIS

    use Tk::Getopt;
    @opttable = (['opt1', '=s', 'default'], ['opt2', '!', 1], ...);
    $opt = new Tk::Getopt(-opttable => \@opttable,
                          -options => \%options,
			  -filename => "$ENV{HOME}/.options");
    $opt->load_options;
    $opt->get_options;
    $opt->process_options;
    print $options->{'opt1'}, $options->{'opt2'} ...;
    ...
    $top = new MainWindow;
    $opt->option_editor($top);

or using a F<Getopt::Long>-like interface

    $opt = new Tk::Getopt(-getopt => ['help'   => \$HELP,
				      'file:s' => \$FILE,
				      'foo!'   => \$FOO,
				      'num:i'  => \$NO,
				     ]);

or an alternative F<Getopt::Long> interface

    %optctl = ('foo' => \$foo,
	       'bar' => \$bar);
    $opt = new Tk::Getopt(-getopt => [\%optctl, "foo!", "bar=s"]);

=head1 DESCRIPTION

F<Tk::Getopt> provides an interface to access command line options via
F<Getopt::Long> and editing with a graphical user interface via a Tk window.

Unlike F<Getopt::Long>, this package uses a object oriented interface, so you
have to create a new F<Tk::Getopt> object with B<new>. Unlike other
packages in the Tk hierarchy, this package does not define a Tk widget. The
graphical interface is calles by the method B<option_editor>.

After creating an object with B<new>, you can parse command line options
by calling B<get_options>. This method calls itself
B<Getopt::Long::GetOptions>.

=head1 METHODS

=over 4

=item B<new Tk::Getopt(>I<arg_hash>B<)>

Constructs a new object of the class F<Tk::Getopt>. Arguments are
passed in a hash (just like Tk widgets and methods). There are many variants
to specify the option description. You can use an interface similar to
B<Getopt::Long::GetOptions> by using B<-getopt> or a more powerful
interface by using B<-opttable>. Internally, the option description will
be converted to the B<-opttable> interface. One of the arguments B<-getopt>
or B<-opttable> are mandatory.

The arguments for B<new> are:

=over 8

=item -getopt

B<-getopt> should be a reference to a hash or an array. This hash has the
same format as the argument to the B<Getopt::Long::GetOptions> function.
Look at L<Getopt::Long> for a detailed description. Note
also that not all of B<GetOptions> is implemented, see L<BUGS> for further
information.

Example:

    new Tk::Options(-getopt => [\%options,
                                "opt1=i", "opt2=s" ...]);

=item -opttable

B<-opttable> provides a more powerful interface. The options are stored
in variables named I<$opt_XXX> or in a hash when B<-options> is given (see
below). B<-opttable> should be a reference to an array containing
all options. Elements of this array may
be strings, which indicate the beginning of a new group, or array references
describing the options. The first element of this array is the name of
the option, the second is the type (C<=s> for string, C<=i> for integer,
C<!> for boolean, C<=f> for float etc., see L<Getopt::Long>) for a
detailed list. The third element is optional and contains
the default value (otherwise the default is undefined). The fourth
element is optional too and describes further attributes. It have to be a
reference to a hash with following keys:

=over 12

=item alias

An array of aliases also accepted by F<Getopt::Long>.

=item label

A label to be displayed in the GUI instead of the option name.

=item help

A short help string used by B<usage> and the Balloon help facility in
B<option_editor>.

=item longhelp

A long help string used by B<option_editor>.

=item choices

An array of additional choices for the options editor.

=item range

An array with the beginning and end of a range for an integer or float value.

=item strict

Must be used with B<choices> or B<range>. When set to true, options have to
match either the choices or the range.

=item var

Use variable instead of I<$options->{optname}> or I<$opt_optname>
to store the value.

=item nogui

This option will not have an entry in the GUI.

=item widget

This should be a reference to a subroutine for creating an own widget.
Folowing arguments will be passed to this subroutine:
a reference to the F<Tk::Getopt> object, Frame object, options entry.
The subroutine should create a widget in the frame (packing is not
necessary!) and should return a reference to the created widget.

=item nosafe

Do not use a safe compartment when loading options (see B<load_options>.

=back

Here is an example for this rather complicated argument:

    @opttable = (['debug', # name of the option (--debug)
                  '!',     # type boolean, accept --nodebug
                  0,       # default is 0 (false)
                  {'callback' => sub { $^W = 1 
                                         if $options->{'debug'}; }
                  # additional attribute: callback to be called if
                  # you set or change the value
                  }],
                 ['age',
                  '=i',    # option accepts integer value
                  18,
                  {'strict' => 1, # value must be in range
                   'range' => [0, 100], # allowed range
                   'alias' => ['year', 'years'] # possible aliases
                  }],
                 ['browser',
                  '=s',    # option accepts string value
                  'tkweb',
                  {'choices' => ['mosaic', 'netscape',
                                 'lynx', 'chimera'],
                   # choices for the list widget in the GUI
                   'label' => 'WWW browser program'
                   # label for the GUI instead of 'browser'
                  }],
                 ['foo',
                  '=f',    # option accepts float value
                  undef,   # no default value
                  {'help' => 'This is a short help',
                   # help string for usage() and the help balloon
                   'longhelp' => 'And this is a slightly longer help'
                   # longer help displayed in the GUI's help window
                  }]);
    new Tk::Options(-opttable => \@opttable,
                    -options => \%options);

=item -options

This argument should be a reference to an (empty) hash. Options are set
into this hash. If this argument is missing, options will be stored in
variables named I<$opt_XXX>.

=item -filename

This argument is optional and specified the filename for loading and saving
options.

=back

=item B<set_defaults>

Set default values. This only applies if the B<-opttable> variant is used.

=item B<load_options(>I<filename>B<)>

Loads options from file B<filename>, or, if not specified, from
object's filename as specified in B<new>. The loading is done in a safe
compartment ensure security.The loaded file should have a reference to a hash
named I<$loadoptions>.

=item B<save_options(>I<filename>B<)>

Writes options to file B<filename>, or, if not specified, from
object's filename as specified in B<new>. The saving is done with
F<Data::Dumper>. Since saving may fail, you should call this method inside
of C<eval {}> and check I<$@>. Possible exceptions are C<No Data::Dumper>
(cannot find the F<Data::Dumper> module) and C<Writing failed> (cannot
write to file).

=item B<get_options>

Gets options via B<GetOptions>. Returns the same value as B<GetOptions>, i.e.
0 indicates that the function detected one or more errors.

If you want to process options which does not appear in the GUI, you have
two alternatives:

=over 8

=item *

Use the B<-opttable> variant of C<new> and mark all non-GUI options with
B<nogui>, e.g.

    new Tk::Getopt(-opttable => ['not-in-gui', '!', undef,
                                 {'nogui' => 1}], ...)

=item *

Use I<Getopt::Long::passthrough> and process non-GUI options directly with
B<Getopt::Long::GetOptions>. The remaining args can be passed to
B<get_options>.

Example:

    use Tk::Getopt;
    use Getopt::Long;

    $Getopt::Long::passthrough = 1;
    GetOptions('options!' => \$preloadopt);
    $Getopt::Long::passthrough = 0;

    $opt = new Tk::Getopt( ... );
    $opt->get_options;

=back

=item B<usage>

Generates an usage string from object's opttable. The usage string is
constructed from the option name, default value and help entries.

=item B<process_options(>[I<undo_hash>]B<)>

Checks wheather given values are valid (if B<strict> is set) and calls
any callbacks specified by the B<sub> option. If B<undo_hash> is
given and the new value of an option did not change, no sub is called.

=item B<option_editor(>I<widget>, [I<arguments ...>]B<)>

Pops the option editor up. The editor provides facilitied for editing
options, undoing, restoring to their default valued and saving to the
default options file.

The first argument is the parent widget. Further optional arguments are
passed as a hash:

=over 8

=item -callback

Execute additional callback after creating the option editor. Arguments
passed to this callback are: reference to the F<Tk::Getopt> object and
a reference to the option editor window.

=item -nosave

Disable saving of options.

=item -toplevel

Use another widget instead of B<Toplevel> for embedding the option
editor, e.g. C<Frame> to embed the editor into another toplevel widget (do not
forget to pack the frame!).

=item -statusbar

Use an additional status bar for help messages.

=item -string

Change button labels and title. This argument should be a hash reference
with following keys: C<optedit>, C<undo>, C<lastsaved>, C<save>, C<defaults>,
C<ok>, C<cancel>.

=back

Since the option editor uses the C<NoteBook> widget, options may be
grouped in several pages. Grouping is only possible if using the
C<-opttable> variant of C<new>. Help messages are shown in balloons and,
if specified, in a statusbar.

B<option_editor> returns a reference to the created window.

Note: this method returns immediately to the calling program.

Buttons in the option editor window:

=over 8

=item OK

Accept options and close option editor window.

=item Cancel

Set old values and close option editor window.

=item Undo

Set old values.

=item Last saved

Set last saved options. This button is not displayed if no filename was given
in C<new>.

=item Save

Save options to file. This button is not displayed if no filename was given
in C<new>.

=back

The option types are translated to following widgets:

=over 8

=item Boolean

B<Checkbutton> (B<_boolean_widget>)

=item Integer and Float

B<Scale>, if B<range> is set, otherwise either B<BrowseEntry> or B<Entry>
(B<_integer_widget>, B<_float_widget>).

=item String

B<BrowseEntry> if B<choices> is set, otherwise B<entry> (B<_string_widget>).

=back

=back

=head1 REQUIREMENTS

C<Tk::Getopt> needs C<Data::Dumper> (Version 2.07) for saving options and, of
course, the C<Tk> (Version 400.202) package, if you want to use the graphical
interface. You can found these packages at CPAN.

=head1 BUGS

Not all of Getopt::Long is supported (array and hash options, <>, alias names,
abbrevs).

The Options editor probably should be a real widget.

You should use Tk400.203, since Tk::Balloon has a bug when its parent
window is destroyed.

The API will not be stable until version 1.00.

This manual is confusing. In fact, the whole module is confusing.

=head1 AUTHOR

Slaven Rezic <eserte@cs.tu-berlin.de>

This package is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 SEE ALSO

L<perl>, L<Getopt::Long>, L<Data::Dumper>, L<Tk>, L<Tk::NoteBook>, L<Safe>

=cut
