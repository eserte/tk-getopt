# -*- perl -*-

#
# $Id: Getopt.pm,v 1.4 1997/02/15 04:30:24 eserte Exp $
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
use Tk;
use Tk::NoteBook;
use Tk::LabEntry;
use Getopt::Long;
use strict;
use vars qw($loadoptions $VERSION);

$VERSION = '0.10';

sub new {
    my($pkg, %a) = @_;
    my $self = {};
    die "No opttable array ref" if !exists $a{'-opttable'};
    $self->{'opttable'} = $a{'-opttable'};
    die "No options hash ref" if !exists $a{'-options'};
    $self->{'options'} = $a{'-options'};
    $self->{'filename'} = $a{'-filename'};
    $self->{'toplevel'} = $a{'-toplevel'} || 'Toplevel';
    bless $self, $pkg;
}

sub load_options {
    my($self, $filename) = @_;
    $filename = $self->{'filename'} if !$filename;
    eval qq{do "$filename"};
    if (!$@) {
	$self->{'loadoptions'} = $loadoptions;
    }
}

sub process_options {
    my $self = shift;
    my %getopt;
    my $opt;
    foreach $opt (@{$self->{'opttable'}}) {
	if (ref $opt eq 'ARRAY') {
	    $self->get_loadoption($opt->[0], $opt->[2]);
	    $getopt{_getopt_long_string($opt->[0], $opt->[1])} =
	      \$self->{'options'}->{$opt->[0]};
	    foreach (@{$opt->[3]{'alias'}}) {
		$getopt{_getopt_long_string($_, $opt->[1])} =
		  \$self->{'options'}->{$opt->[0]};
	    }
	}
    }
    GetOptions(%getopt);
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
			   ($opt->[0], @{$opt->[3]{'alias'}}));
	    $usage .= "\t" . $opt->[3]{'help'};
	    $usage .= " (default: " . $opt->[2] . ") " if $opt->[2];
	    $usage .= "\n";
	}
    }
    $usage;
}

sub get_loadoption {
    my($self, $option, $default) = @_;
    if (exists $self->{'loadoptions'}->{$option}) {
	$self->{'options'}->{$option} = $self->{'loadoptions'}->{$option};
    } elsif (defined $default) {
	$self->{'options'}->{$option} = $default;
    }
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
		&{$_->[3]{'sub'}};
	    }
	}
    }
}

sub options_editor {
    my($self, $top, $string) = @_;
    if (!defined $string) {
	$string = {'optedit' => 'Options editor',
		   'undo' => 'Undo',
		   'save' => 'Save',
		   'defaults' => 'Defaults',
		   'ok' => 'OK',
		   'cancel' => 'Cancel'};
    }
    my %undo_options = %{$self->{'options'}};
    my $optedit =
      eval '$top->' . $self->{'toplevel'} . '(-title => $string->{optedit})';
    my $optnote = $optedit->NoteBook(-ipadx => 6, -ipady => 6);
    $optnote->pack(-expand => 1, -fill => 'both');
    my($current_page, $current_top, $balloon);
    my $opt;
    foreach $opt (@{$self->{'opttable'}}) {
	if (ref $opt ne 'ARRAY') {
	    $current_top = $opt;
	    $current_page = $optnote->add($opt, -label => $opt);
	} else {
	    my $f = $current_page->Frame->pack(-side => 'top', -anchor => 'w',
					       -fill => 'x');
	    my $label;
	    if (exists $opt->[3]{'label'}) {
		$label = $opt->[3]{'label'};
	    } else {
		$label = $opt->[0];
		if ($label =~ /^(.*)-/ && $1 eq $current_top) {
		    $label = $';
		}
	    }
	    if ($opt->[1] eq '!') {
		$f->Checkbutton
		  (-text => $label,
		   -variable => \$self->{'options'}->{$opt->[0]}
		  )->pack(-side => 'left');
	    } elsif ($opt->[1] =~ /i|f/ 
		     && exists $opt->[3]{'range'}) {
		$f->Label(-text => $label)->pack(-side => 'left');
		$f->Scale
		  (-orient => 'horizontal',
		   -from => $opt->[3]{'range'}[0],
		   -to => $opt->[3]{'range'}[1],
		   -showvalue => 1,
		   -variable => \$self->{'options'}->{$opt->[0]}
		  )->pack(-side => 'left');
	    } elsif ($opt->[1] =~ /(s|i|f)/) {
		if (exists $opt->[3]{'choices'}) {
		    require Tk::BrowseEntry;
		    my $w = $f->BrowseEntry
		      (-label => $label,
		       -labelPack => [-side => 'left'],
		       -variable => \$self->{'options'}->{$opt->[0]}
		      )->pack(-side => 'left');
		    my @optlist = @{$opt->[3]{'choices'}};
		    unshift(@optlist, $opt->[2]) if defined $opt->[2];
		    my $o;
		    foreach $o (@optlist) {
			$w->insert("end", $o);
		    }
		} else {
		    $f->LabEntry
		      (-label => $label,
		       -labelPack => [-side => 'left'],
		       -textvariable => \$self->{'options'}->{$opt->[0]}
		      )->pack(-side => 'left');
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
			   })->pack(-side => 'left');
	    }
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
    $f->Button(-text => $string->{'save'},
	       -command => sub {
		   $top->Busy;
		   eval "require Data::Dumper";
		   if ($@) {
		       warn $@;
		   } else {
		       if (open(OPT, ">$self->{'filename'}")) {
			   print OPT
			     Data::Dumper->Dump([$self->{'options'}],
						['loadoptions']),
			     "1;\n";
			   close OPT;
			   warn "Options written to $self->{'filename'}";
		       } else {
			   warn "Can't write to $self->{'filename'}";
		       }
		   }
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
named B<loadoptions>. Processing of the options is done in
B<process_options>.

=item B<process_options>

Does two things: sets the options hash using the loadoptions hash and
gets options via B<GetOptions>.

=item B<usage>

Generates an usage string from object's opttable. The usage string is
constructed from the option name, default value and help entries.

=item B<get_loadoption(>I<option>, I<default>B<)>

Internal method used by process_options. Sets options entry for
I<option> with the loadoptions value or default value I<default>.

=item B<do_options(>[I<undo_hash>]B<)>

Checks wheather given values are valid (if B<strict> is set) and calls
any subroutines specified by the B<sub> option. If I<undo_hash> is
given and the new value of an option did not change, no sub is called.

=item B<options_editor(>I<widget>, [I<string_hash>]B<)>

Pops the options editor up. The editor provides facilitied for editing
options, undoing, restoring to their default valued and saving to the
default options file. The package C<Data::Dumper> is needed for saving.

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
