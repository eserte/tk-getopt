# -*- perl -*-

#
# $Id: Getopt.pm,v 1.7 1997/02/24 17:20:05 eserte Exp $
# Author: Slaven Rezic
#
# Copyright © 1997 Slaven Rezic. All rights reserved.
# This package is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#
# Mail: <URL:mailto:eserte@cs.tu-berlin.de>
# WWW:  <URL:http://www.cs.tu-berlin.de/~eserte/>
#

package Tk::Options;
use strict;
use vars qw($loadoptions $VERSION);

$VERSION = '0.12';

sub new {
    my($pkg, %a) = @_;
    my $self = {};

    if (exists $a{-opttable}) {
	$self->{'opttable'} = $a{'-opttable'};
	die "No options hash ref" if !exists $a{'-options'};
	$self->{'options'} = $a{'-options'};
#    } elsif (exists $a{-getopt}) {
#	# build opttable
#	my %getopt = %{$a{-getopt}};
#	my $optdesc;
#	foreach $optdesc (keys %getopt) {
#	    if ($optdesc !~ /^(\w+[-\w|]*)?(!|[=:][infse][@%]?)?$/) {
#		warn "Error in option spec: \"", $optdesc, "\"\n";
#		next;
#	    }
#	    push(@{$self->{'opttable'}}, [$1, $2, undef]);
#	    $getopt{$optdesc} = $self->{'options'}->{$1};
#	}
    } else {
	die "No opttable array ref or getopt hash ref";
    }

    $self->{'filename'} = $a{'-filename'};
    $self->{'toplevel'} = $a{'-toplevel'} || 'Toplevel';
    bless $self, $pkg;
}

sub set_defaults {
    my $self = shift;
    my $opt;
    foreach $opt (@{$self->{'opttable'}}) {
	if (ref $opt eq 'ARRAY' && defined $opt->[2]) {
	    $self->{'options'}->{$opt->[0]} = $opt->[2];
	}
    }
}

sub load_options {
    my($self, $filename) = @_;
    $filename = $self->{'filename'} if !$filename;
    return if !$filename;
    require Safe;
    my $c = new Safe;
    $c->share('$loadoptions');
    if (!$c->rdo($filename)) {
	warn "Can't load $filename";
	undef;
    } else {
	my $opt;
	foreach $opt (@{$self->{'opttable'}}) {
	    if (ref $opt eq 'ARRAY' && exists $loadoptions->{$opt->[0]}) {
		$self->{'options'}->{$opt->[0]} = $loadoptions->{$opt->[0]};
	    }
	}
	1;
    }
}

sub save_options {
    my($self, $filename) = @_;
    $filename = $self->{'filename'} if !$filename;
    return if !$filename;
    eval "require Data::Dumper";
    if ($@) {
	warn $@;
	undef;
    } else {
	if (open(OPT, ">$filename")) {
	    print OPT
	      Data::Dumper->Dump([$self->{'options'}], ['loadoptions']);
	    close OPT;
	    warn "Options written to $filename";
	    1;
	} else {
	    warn "Can't write to $filename";
	    undef;
	}
    }
}

sub process_options {
    my $self = shift;
    my %getopt;
    my $opt;
    foreach $opt (@{$self->{'opttable'}}) {
	if (ref $opt eq 'ARRAY') {
	    $getopt{_getopt_long_string($opt->[0], $opt->[1])} =
	      \$self->{'options'}->{$opt->[0]};
	    foreach (@{$opt->[3]{'alias'}}) {
		$getopt{_getopt_long_string($_, $opt->[1])} =
		  \$self->{'options'}->{$opt->[0]};
	    }
	}
    }
    require Getopt::Long;
    Getopt::Long::GetOptions(%getopt);
}

sub _getopt_long_string {
    my($option, $type) = @_;
    $option . (length($option) == 1 && $type eq '!'
	       ? '' : $type);
}

sub _getopt_long_dash {
    my $option = shift;
    (length($option) == 1 ? '' : '-') . "-$option";
}

sub usage {
    my $self = shift;
    my $usage = "Usage: $0 [options]\n";
    my $opt;
    foreach $opt (@{$self->{'opttable'}}) {
	if (ref $opt eq 'ARRAY') {
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
    }
    $usage;
}

sub do_options {
    my($self, $undo) = @_;
    my $options = $self->{'options'};
    foreach (@{$self->{'opttable'}}) {
	if (ref $_ eq 'ARRAY') {
	    my $opt = $_->[0];
	    if ($_->[3]{'strict'}) {
		if (!grep(/^$options->{$opt}$/, @{$_->[3]{'choices'}})) {
		    if (defined $undo) {
			warn "Not allowed: $options->{$opt} for $opt. Using old value $undo->{$opt}";
			$options->{$opt} = $undo->{$opt};
		    } else {
			die "Not allowed: $options->{$opt} for $opt";
		    }
		}
	    }
	    if (exists $_->[3]{'sub'}) {
		# nothing changed:
		next if (defined $undo
			 && $options->{$opt} eq $undo->{$opt});
		# callback:
		&{$_->[3]{'sub'}};
	    }
	}
    }
}

sub _create_page {
    my($self, $optnote, $current_top, $optlist) = @_;
    my $current_page = $optnote->{$current_top};
    my $opt;
    my $row = -1;
    foreach $opt (@{$optlist->{$current_top}}) {
	my $f = $current_page;
	my $label;
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
	if ($opt->[1] eq '!' or $opt->[1] eq '') {
	    $f->Checkbutton
	      (-variable => \$self->{'options'}->{$opt->[0]}
	      )->grid(-row => $row, -column => 1, -sticky => 'w');
	} elsif ($opt->[1] =~ /i|f/ 
		 && exists $opt->[3]{'range'}) {
	    $f->Scale
	      (-orient => 'horizontal',
	       -from => $opt->[3]{'range'}[0],
	       -to => $opt->[3]{'range'}[1],
	       -showvalue => 1,
	       -resolution => ($opt->[1] =~ /f/ ? 0 : 1),
	       -variable => \$self->{'options'}->{$opt->[0]}
	      )->grid(-row => $row, -column => 1, -sticky => 'w');
	} elsif ($opt->[1] =~ /(s|i|f)/) {
	    if (exists $opt->[3]{'choices'}) {
		require Tk::BrowseEntry;
		my $w = $f->BrowseEntry
		  (-variable => \$self->{'options'}->{$opt->[0]}
		  )->grid(-row => $row, -column => 1, -sticky => 'w');
		my @optlist = @{$opt->[3]{'choices'}};
		unshift(@optlist, $opt->[2]) if defined $opt->[2];
		my $o;
		foreach $o (@optlist) {
		    $w->insert("end", $o);
		}
	    } else {
		$f->Entry
		  (-textvariable => \$self->{'options'}->{$opt->[0]}
		  )->grid(-row => $row, -column => 1, -sticky => 'w');
	    }
	} else {
	    warn "Can't generate for $opt->[0]";
	}
#	    if (exists $opt->[3]{'help'} && defined $f) {
#		require Tk::Balloon;
#		if (!defined $balloon) {
#		    $balloon = $optedit->Balloon;
#		}
#		$balloon->attach($f, -msg => $opt->[3]{'help'});
#	    }
	if (exists $opt->[3]{'longhelp'}) {
	    $f->Button(-text => '?',
		       -padx => 1,
		       -pady => 1,
		       -command => sub {
			   $f->Toplevel
			     (-title => $label
			     )->Label(-text => $opt->[3]{'longhelp'}
				     )->pack;
		       })->grid(-row => $row, -column => 2, -sticky => 'w');
	}
    }
}

sub options_editor {
    my($self, $top, $string) = @_;
    if (!defined $string) {
	$string = {'optedit' => 'Options editor',
		   'undo' => 'Undo',
		   'lastsaved' => 'Last saved',
		   'save' => 'Save',
		   'defaults' => 'Defaults',
		   'ok' => 'OK',
		   'cancel' => 'Cancel'};
    }
    my %undo_options = %{$self->{'options'}};
    require Tk;
    require Tk::NoteBook;
    my $optedit =
      eval '$top->' . $self->{'toplevel'} . '(-title => $string->{optedit})';
    my $optnote = $optedit->NoteBook(-ipadx => 6, -ipady => 6);
    my $nopage = 1;
    $optnote->pack(-expand => 1, -fill => 'both');
    my $current_top;
    my $optlist = {};
    my $opt;
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
			      $self->_create_page($optnote, $c, $optlist);
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

    my $f = $optedit->Frame;
    $f->pack(-anchor => 'w');
    $f->Button(-text => $string->{'undo'},
	       -command => sub {
		   foreach (keys %undo_options) {
		       $self->{'options'}->{$_} = $undo_options{$_};
		   }
	       }
	      )->pack(-side => 'left');
    $f->Button(-text => $string->{'lastsaved'},
	       -command => sub {
		   $top->Busy;
		   $self->load_options;
		   $top->Unbusy;
	       }
	      )->pack(-side => 'left');
    $f->Button(-text => $string->{'save'},
	       -command => sub {
		   $top->Busy;
		   $self->save_options;
		   $top->Unbusy;
	       }
	      )->pack(-side => 'left');
    $f->Button(-text => $string->{'defaults'},
	       -command => sub {
		   my $opt;
		   foreach $opt (@{$self->{'opttable'}}) {
		       if (ref $opt eq 'ARRAY' && defined $opt->[2]) {
			   $self->{'options'}->{$opt->[0]} = $opt->[2];
		       } 
		   }
	       }
	      )->pack(-side => 'left');
    $f->Button(-text => $string->{'ok'},
	       -command => sub { $self->do_options(\%undo_options);
				 $optedit->destroy; }
	      )->pack(-side => 'left');
    $f->Button(-text => $string->{'cancel'},
	       -command => sub { $optedit->destroy; }
	      )->pack(-side => 'left');

    $optedit->UnderlineAll if $optedit->can('UnderlineAll');
    $optedit->Show if $optedit->can('Show');

    $optedit;
}

1;

__END__

=head1 NAME

Tk::Options - Access to options via Getopt::Long and Tk window interface

=head1 SYNOPSIS

    use Tk::Options;
    ...
    @opttable = (['opt1', '=s', 'default'], ['opt2', '!', 1], ...);
    $opt = new Tk::Options(-opttable => \@opttable,
                           -options => \%options,
			   -filename => "$ENV{HOME}/.options");
    $opt->load_options;
    $opt->process_options;
    $opt->do_options;
    ...
    $opt->options_editor($top);

=head1 DESCRIPTION

C<Tk::Options> provides an interface to access options via Getopt::Long
(command line) and via a Tk window.

The API of this package is likely to change!

=head1 METHODS

=over 4

=item B<new Tk::Options(>I<arg_hash>B<)>

Constructs a new object of the class Tk::Options. Arguments are
delivered in a hash with following keys:

=over 8

=item -opttable

This argument is mandatory and should be a reference to an array
containing all used options. Elements of this array may be strings,
which indicate the beginning of a new group, or array references
describing the options. The first element of this array is the name of
the option, the second is the type (string, integer, boolean etc., see
C<Getopt::Long>) of this option. The third element is optional and is
the default value (otherwise the default is undefined). The fourth
element is optional too and have to be a reference to a hash:

=over 12

=item alias

An array of aliases also accepted by Getopt::Long.

=item label

A label to be displayed in the options editor instead of the option name.

=item help

A short help string used by B<usage> and the Balloon help facility in
B<options_editor>.

=item longhelp

A long help string used by B<options_editor>.

=item choices

An array of additional choices for the options editor.

=item from

The beginning of a range for an integer or float value.

=item to

The end of a range for an integer or float value.

=item strict

Must be used with I<choices> or I<from/to>. When set to true, options have to
match either the choices or the range between I<from> and I<to>.

=back

=item Example:

    @opttable = (['debug', '!', 0,
		  {'sub' => sub { $^W = 1 
				    if $options->{'debug'}; }}],
                 ['age', '=i', 18,
		  {'strict' => 1, 'from' => 0, 'to' => 100,
		   'alias' => ['year', 'years']}],
                 ['browser', '=s', 'tkweb',
		  {'choices' => ['mosaic', 'netscape',
				 'lynx', 'chimera'],
		   'label' => 'WWW browser program'}],
		 ['foo', '=f', undef,
		  {'help' => 'This is a short help',
		   'longhelp' => 'And this is a slightly longer help'}]);

=item -options

This argument is mandatory and should be a reference to a (empty)
hash. Options are set into this hash.

=item -filename

This argument is optional and specified the filename for loading saved
options from a file.

=back

=item B<load_options(>I<filename>B<)>

Loads options from file I<filename>, or, if not specified, from
object's filename as specified in B<new>. The loading is done with a
B<do>-Statement. The loaded file should have a reference to a hash
named B<loadoptions>.

=item B<save_options(>I<filename>B<)>

Writes options to file I<filename>, or, if not specified, from
object's filename as specified in B<new>. The saving is done with
Data::Dumper.

=item B<process_options>

Gets options via B<GetOptions>. Returns the same value as GetOptions, i.e.
0 indicates that the function detected one or more errors.

=item B<usage>

Generates an usage string from object's opttable. The usage string is
constructed from the option name, default value and help entries.

=item B<do_options(>[I<undo_hash>]B<)>

Checks wheather given values are valid (if B<strict> is set) and calls
any subroutines specified by the B<sub> option. If I<undo_hash> is
given and the new value of an option did not change, no sub is called.

=item B<options_editor(>I<widget>, [I<string_hash>]B<)>

Pops the options editor up. The editor provides facilitied for editing
options, undoing, restoring to their default valued and saving to the
default options file.

=back

=head1 BUGS

B<load_options> should be done inside of a B<Safe> compartment.

My English is bad.

=head1 AUTHOR

B<Slaven Rezic> <eserte@cs.tu-berlin.de>

This package is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1), Getopt::Long(3), Data::Dumper(3), Tk(3), Tk::NoteBook(3)

=cut
