# -*- perl -*-

#
# $Id: Getopt.pm,v 1.2 1997/02/14 12:29:23 eserte Exp $
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
use Getopt::Long;
use strict;
use vars qw($loadoptions);

sub new {
    my($pkg, %a) = @_;
    my $self = {};
    die "No opttable array ref" if !exists $a{'-opttable'};
    $self->{'opttable'} = $a{'-opttable'};
    die "No options hash ref" if !exists $a{'-options'};
    $self->{'options'} = $a{'-options'};
    $self->{'loadoptions'} = $a{'-loadoptions'};
    $self->{'filename'} = $a{'-filename'};
    $self->{'toplevel'} = $a{'-toplevel'} || 'Toplevel';
    bless $self, $pkg;
}

sub load_options {
    my $self = shift;
    eval qq{do "$self->{'filename'}"};
    if (!$@) {
	$self->{'loadoptions'} = $loadoptions;
    }
}

sub process_options {
    my $self = shift;
    my %getopt;
    foreach (@{$self->{'opttable'}}) {
	if (ref $_ eq 'ARRAY') {
	    $self->get_loadoption($_->[0], $_->[2]);
	    $getopt{$_->[0].$_->[1]} = \$self->{'options'}->{$_->[0]};
	    if (exists $_->[3]{'short'}) {
		$getopt{$_->[3]{short}.($_->[1] ne '!' ? $_->[1] : '')} =
		  \$self->{'options'}->{$_->[0]};
	    }
	}
    }
    GetOptions(%getopt);
}

sub usage {
    my $self = shift;
    my $usage = "Usage: $0 [options]\n";
    foreach (@{$self->{'opttable'}}) {
	if (ref $_ eq 'ARRAY') {
	    $usage .= "-" . $_->[3]{'short'} . ", " if $_->[3]{'short'};
	    $usage .= "--" . $_->[0] . "\t";
	    $usage .= $_->[3]{'help'};
	    $usage .= " (default: " . $_->[2] . ") " if $_->[2];
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
}

1;
