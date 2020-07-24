#!/usr/bin/perl
use strict;
use warnings;
use autodie;
use utf8;
use feature qw(say);
use File::Basename qw(dirname basename);
use List::Util qw(first);
use Carp qw(croak);
use DateTime;
use constant SCALAR => ref \$0;
use constant ARRAY  => ref [];
use constant HASH   => ref {};


our $VERSION = '2.03';
our $LAST    = '2020-07-24';
our $FIRST   = '2017-06-07';


#----------------------------------My::Toolset----------------------------------
sub show_front_matter {
    # """Display the front matter."""
    my $prog_info_href = shift;
    my $sub_name = join('::', (caller(0))[0, 3]);
    croak "The 1st arg of [$sub_name] must be a hash ref!"
        unless ref $prog_info_href eq HASH;

    # Subroutine optional arguments
    my(
        $is_prog,
        $is_auth,
        $is_usage,
        $is_timestamp,
        $is_no_trailing_blkline,
        $is_no_newline,
        $is_copy,
    );
    my $lead_symb = '';
    foreach (@_) {
        $is_prog                = 1  if /prog/i;
        $is_auth                = 1  if /auth/i;
        $is_usage               = 1  if /usage/i;
        $is_timestamp           = 1  if /timestamp/i;
        $is_no_trailing_blkline = 1  if /no_trailing_blkline/i;
        $is_no_newline          = 1  if /no_newline/i;
        $is_copy                = 1  if /copy/i;
        # A single non-alphanumeric character
        $lead_symb              = $_ if /^[^a-zA-Z0-9]$/;
    }
    my $newline = $is_no_newline ? "" : "\n";

    #
    # Fill in the front matter array.
    #
    my @fm;
    my $k = 0;
    my $border_len = $lead_symb ? 69 : 70;
    my %borders = (
        '+' => $lead_symb.('+' x $border_len).$newline,
        '*' => $lead_symb.('*' x $border_len).$newline,
    );

    # Top rule
    if ($is_prog or $is_auth) {
        $fm[$k++] = $borders{'+'};
    }

    # Program info, except the usage
    if ($is_prog) {
        $fm[$k++] = sprintf(
            "%s%s - %s%s",
            ($lead_symb ? $lead_symb.' ' : $lead_symb),
            $prog_info_href->{titl},
            $prog_info_href->{expl},
            $newline,
        );
        $fm[$k++] = sprintf(
            "%s%s v%s (%s)%s",
            ($lead_symb ? $lead_symb.' ' : $lead_symb),
            $prog_info_href->{titl},
            $prog_info_href->{vers},
            $prog_info_href->{date_last},
            $newline,
        );
        $fm[$k++] = sprintf(
            "%sPerl %s%s",
            ($lead_symb ? $lead_symb.' ' : $lead_symb),
            $^V,
            $newline,
        );
    }

    # Timestamp
    if ($is_timestamp) {
        my %datetimes = construct_timestamps('-');
        $fm[$k++] = sprintf(
            "%sCurrent time: %s%s",
            ($lead_symb ? $lead_symb.' ' : $lead_symb),
            $datetimes{ymdhms},
            $newline,
        );
    }

    # Author info
    if ($is_auth) {
        $fm[$k++] = $lead_symb.$newline if $is_prog;
        $fm[$k++] = sprintf(
            "%s%s%s",
            ($lead_symb ? $lead_symb.' ' : $lead_symb),
            $prog_info_href->{auth}{$_},
            $newline,
        ) for (
            'name',
#            'posi',
#            'affi',
            'mail',
        );
    }

    # Bottom rule
    if ($is_prog or $is_auth) {
        $fm[$k++] = $borders{'+'};
    }

    # Program usage: Leading symbols are not used.
    if ($is_usage) {
        $fm[$k++] = $newline if $is_prog or $is_auth;
        $fm[$k++] = $prog_info_href->{usage};
    }

    # Feed a blank line at the end of the front matter.
    if (not $is_no_trailing_blkline) {
        $fm[$k++] = $newline;
    }

    #
    # Print the front matter.
    #
    if ($is_copy) {
        return @fm;
    }
    else {
        print for @fm;
        return;
    }
}


sub validate_argv {
    # """Validate @ARGV against %cmd_opts."""
    my $argv_aref     = shift;
    my $cmd_opts_href = shift;
    my $sub_name = join('::', (caller(0))[0, 3]);
    croak "The 1st arg of [$sub_name] must be an array ref!"
        unless ref $argv_aref eq ARRAY;
    croak "The 2nd arg of [$sub_name] must be a hash ref!"
        unless ref $cmd_opts_href eq HASH;

    # For yn prompts
    my $the_prog = (caller(0))[1];
    my $yn;
    my $yn_msg = "    | Want to see the usage of $the_prog? [y/n]> ";

    #
    # Terminate the program if the number of required arguments passed
    # is not sufficient.
    #
    my $argv_req_num = shift;  # (OPTIONAL) Number of required args
    if (defined $argv_req_num) {
        my $argv_req_num_passed = grep $_ !~ /-/, @$argv_aref;
        if ($argv_req_num_passed < $argv_req_num) {
            printf(
                "\n    | You have input %s nondash args,".
                " but we need %s nondash args.\n",
                $argv_req_num_passed,
                $argv_req_num,
            );
            print $yn_msg;
            while ($yn = <STDIN>) {
                system "perldoc $the_prog" if $yn =~ /\by\b/i;
                exit if $yn =~ /\b[yn]\b/i;
                print $yn_msg;
            }
        }
    }

    #
    # Count the number of correctly passed command-line options.
    #

    # Non-fnames
    my $num_corr_cmd_opts = 0;
    foreach my $arg (@$argv_aref) {
        foreach my $v (values %$cmd_opts_href) {
            if ($arg =~ /$v/i) {
                $num_corr_cmd_opts++;
                next;
            }
        }
    }

    # Fname-likes
    my $num_corr_fnames = 0;
    $num_corr_fnames = grep $_ !~ /^-/, @$argv_aref;
    $num_corr_cmd_opts += $num_corr_fnames;

    # Warn if "no" correct command-line options have been passed.
    if (not $num_corr_cmd_opts) {
        print "\n    | None of the command-line options was correct.\n";
        print $yn_msg;
        while ($yn = <STDIN>) {
            system "perldoc $the_prog" if $yn =~ /\by\b/i;
            exit if $yn =~ /\b[yn]\b/i;
            print $yn_msg;
        }
    }

    return;
}


sub show_elapsed_real_time {
    # """Show the elapsed real time."""
    my @opts = @_ if @_;

    # Parse optional arguments.
    my $is_return_copy = 0;
    my @del;  # Garbage can
    foreach (@opts) {
        if (/copy/i) {
            $is_return_copy = 1;
            # Discard the 'copy' string to exclude it from
            # the optional strings that are to be printed.
            push @del, $_;
        }
    }
    my %dels = map { $_ => 1 } @del;
    @opts = grep !$dels{$_}, @opts;

    # Optional strings printing
    print for @opts;

    # Elapsed real time printing
    my $elapsed_real_time = sprintf("Elapsed real time: [%s s]", time - $^T);

    # Return values
    if ($is_return_copy) {
        return $elapsed_real_time;
    }
    else {
        say $elapsed_real_time;
        return;
    }
}


sub pause_shell {
    # """Pause the shell."""
    my $notif = $_[0] ? $_[0] : "Press enter to exit...";

    print $notif;
    while (<STDIN>) { last; }

    return;
}


sub construct_timestamps {
    # """Construct timestamps."""
    # Optional setting for the date component separator
    my $date_sep  = '';

    # Terminate the program if the argument passed
    # is not allowed to be a delimiter.
    my @delims = ('-', '_');
    if ($_[0]) {
        $date_sep = $_[0];
        my $is_correct_delim = grep $date_sep eq $_, @delims;
        croak "The date delimiter must be one of: [".join(', ', @delims)."]"
            unless $is_correct_delim;
    }

    # Construct and return a datetime hash.
    my $dt  = DateTime->now(time_zone => 'local');
    my $ymd = $dt->ymd($date_sep);
    my $hms = $dt->hms($date_sep ? ':' : '');
    (my $hm = $hms) =~ s/[0-9]{2}$//;

    my %datetimes = (
        none   => '',  # Used for timestamp suppressing
        ymd    => $ymd,
        hms    => $hms,
        hm     => $hm,
        ymdhms => sprintf("%s%s%s", $ymd, ($date_sep ? ' ' : '_'), $hms),
        ymdhm  => sprintf("%s%s%s", $ymd, ($date_sep ? ' ' : '_'), $hm),
    );

    return %datetimes;
}


sub rm_duplicates {
    # """Remove duplicate items from an array."""
    my $aref = shift;
    my $sub_name = join('::', (caller(0))[0, 3]);
    croak "The 1st arg of [$sub_name] must be an array ref!"
        unless ref $aref eq ARRAY;

    my(%seen, @uniqued);
    @uniqued = grep !$seen{$_}++, @$aref;
    @$aref = @uniqued;

    return;
}
#-------------------------------------------------------------------------------


sub parse_argv {
    # """@ARGV parser"""
    my(
        $argv_aref,
        $cmd_opts_href,
        $run_opts_href,
    ) = @_;
    my %cmd_opts = %$cmd_opts_href;  # For regexes

    # Parser: Overwrite default run options if requested by the user.
    my $cmd_arg_field_sep = ',';
    foreach (@$argv_aref) {
        # .bib files
        push @{$run_opts_href->{bib_files}}, $_ if (
            /[.]bib$/i
            and -e
            and not -d  # Added not to read in dirs having .bib in their names
        );

        # All .bib files in the current working directory
        if (/$cmd_opts{bib_all}/) {
            # Modified not to read in directories
            # having .bib in their names
            @{$run_opts_href->{bib_files}} = grep { not -d } glob '*.bib';
        }

        # Output filename flag
        if (/$cmd_opts{out_flag}/i) {
            s/$cmd_opts{out_flag}//i;
            $run_opts_href->{out_flag} = $_;
        }

        # Verbose
        if (/$cmd_opts{verbose}/i) {
            $run_opts_href->{is_verbose} = 1;
        }

        # User preferences file
        if (/$cmd_opts{prf}/i) {
            s/$cmd_opts{prf}//i;
            $run_opts_href->{prf} = $_ if -e;
            if (not -e) {
                print "User preferences file [$_] NOT found in the CWD.";
                print " Default preferences will be used.\n\n";
            }
        }

        # Toggles of the modifier routines
        foreach my $toggle (qw/turn_on turn_off/) {
            if (/$cmd_opts{$toggle}/i) {
                s/$cmd_opts{$toggle}//i;
                @{$run_opts_href->{$toggle}} = split /$cmd_arg_field_sep/;
            }
        }

        # The front matter won't be displayed at the beginning of the program.
        if (/$cmd_opts{nofm}/) {
            $run_opts_href->{is_nofm} = 1;
        }

        # The shell won't be paused at the end of the program.
        if (/$cmd_opts{nopause}/) {
            $run_opts_href->{is_nopause} = 1;
        }
    }
    rm_duplicates($run_opts_href->{bib_files});

    return;
}


sub read_in_prf {
    # """Read in the user preferences."""
    my(
        $run_opts_href,
        $old_and_new_href,
    ) = @_;
    my $prf     = $run_opts_href->{prf};
    my $prf_sep = $run_opts_href->{prf_sep};

    open my $prf_fh, '<:encoding(UTF-8)', $prf;
    foreach (<$prf_fh>) {
        chomp;
        next if /^\s*#/;
        next if /^$/;
        next if not /$prf_sep/;

        my($old, $new) = (split /\s*$prf_sep\s*/)[0, 1];
        $new =~ s/\s*#.*//;  # Suppress an inline comment.
        $old_and_new_href->{$old} = $new;
    }
    close $prf_fh;
    print "\n[$prf] read in.\n\n";

    return;
}


sub enclose_with_braces {
    # """Enclose strings with pairs of braces (curly brackets)."""
    my($field_key, $field_val) = @_;

    # Entry field designator
    my @fields_of_int = qw/
        author
        title
        subtitle
    /;
    return unless first { $field_key =~ /\s*\b$_/i } @fields_of_int;

    #
    # 'author' entry field
    # - An institution name is recognized together with the field value braces;
    #   e.g. {International Atomic Energy Agency}
    #   This will then be recognized and enclosed with a pair of braces as
    #   {{International Atomic Energy Agency}}
    #   In the next run, this will not be recognized because of [^{] and [^}]
    #   in the regex, so that duplicate braces will not be given.
    #
    if ($field_key =~ /author/i) {
        my @insts = (
            qr/[{]International Atomic Energy Agency[}]/,
            qr/[{]OECD[\w\s]*[}]/,
            qr/[{]National Academies of Sciences, Engineering, and Medicine[}]/,
            qr/[{]National Research Council(?: [(]US[)])?[}]/,
            qr/[{]National Research Council(?: [(]US[)])? and Institute of Medicine(?: [(]US[)])[}]/,
            qr/[{]Trace Sciences International[}]/,
            qr/[{]Center of Molecular Research[}]/,
            qr/[{]Canadian Light Source[}]/,
            qr/[{]National Nuclear Data Center[}]/,
            qr/[{]Creative Commons[}]/,
            qr/[{]Science Council of Japan[}]/,
        );
        # Institution names
        $field_val =~ s/
            (?<bef>[^{])
            (?<captured>$_)
            (?<aft>[^}])
        /$+{bef}\{$+{captured}\}$+{aft}/x for @insts;
    }

    #
    # 'title' and 'subtitle' entry fields
    #
    if ($field_key =~ /\b(?:sub)?title/i) {
        my $no_dupl_braces = qr/^.*[{].*[}].*$/;
        my @units = (
            qr/[mkMTG]eV/,
            qr/[mkMTG]Bq/,
        );
        my @proper_nouns = (  # Those not captured by as an all-cap string
            # General
            qr/[1-3]D/,

            # Countries and institutions
            qr/U[.]?S[.]?A?/,
            qr/Japan(ese)?/,
            qr/A-FNS/,

            # Name-based
            qr/Compton/,
            qr/Hamilton(?:ian)/,
            qr/Maxwell(?:ian)?/,
            qr/Boltzmann/,

            # Nuclear data
            qr/ENDF\/B-[A-Z]+[.0]*/,
            qr/CIELO-project/,
            qr/TENDL-[0-9]+/,
            qr/JENDL-[0-9.]+/,

            # Phys
            qr/[LSCX]-band/,
            qr/d-Li/,

            # Chem
            qr/Dowex-[0-9]/,
            qr/99Mo\/99mTc-TCM-autosolex/,
            qr/–?Szilard-Chalmers/,
            qr/\b(?:Mo|Tc|Y|Zr|Nb)\b/,
            qr/100MoO3/,

            # CC-BY lincenses
            qr/Unported|International/,
        );
        my @to_be_smallcap = (
            qr/MCNP[X0-9]?/,
            qr/PHITS/,
            qr/EGS[0-9]?/,
            qr/GEANT[0-9]?/,
            qr/TALYS/,
        );
        my @to_be_teletype = (
            qr/phitar|enrimo|actdyn/,
        );

        # Decompose the field value into space-separated strings.
        my %_decomposed;
        ($_decomposed{field_val_bef} = $field_val) =~ s/
            ^(?<bef>\s*[{])
            .*
        /$+{bef}/x;
        ($_decomposed{field_val_aft} = $field_val) =~ s/
            .*
            (?<aft>\s*[}],?)$
        /$+{aft}/x;
        ($_decomposed{field_val_mid} = $field_val) =~ s/
            ^(?<bef>\s*[{])
            (?<mid>.*)
            (?<aft>\s*[}],?)$
        /$+{mid}/x;
        my @strings = split /\s+/, $_decomposed{field_val_mid};
        foreach my $string (@strings) {
            # Skip if the string has already been given braces.
            # The leading ^.* is for an all-cap string like ({IUPAC}
            # The rear .*$ is for an all-cap string like {IUPAC})
            next if $string =~ $no_dupl_braces;

            # Units
            $string =~ s/(?<captured>$_)/\{$+{captured}\}/g
                for @units;

            # Proper nouns
            $string =~ s/(?<captured>$_)/\{$+{captured}\}/g
                for @proper_nouns;

            # \textsc
            # > Note that upper letters are lowercased.
            $string =~ s/(?<captured>$_)/\\textsc\{\L$+{captured}\}/g
                for @to_be_smallcap;

            # \texttt
            $string =~ s/(?<captured>$_)/\\texttt\{$+{captured}\}/g
                for @to_be_teletype;

            # All-cap strings
            # One more use of $no_dupl_braces to prevent
            # {TBq} of @units above from becoming {{TB}q}
            next if $string =~ $no_dupl_braces;
            $string =~ s/(?<all_cap>[A-Z]{2,})/\{$+{all_cap}\}/g;

            # Dash-expressed isotopes
            # - Must be placed after @proper_nouns to prevent:
            #   JEND{L-4}.0, which should be {JENDL-4.0}
            $string =~ s/
                (?<chem_symb>[A-Z]{1}[a-z]{0,1}) # e.g. H, Tc
                (?<dash>-)
                (?<mass_num>[0-9]{1,3}[mg]?)     # m:metastable, g:ground
            /\{$+{chem_symb}$+{dash}$+{mass_num}\}/gx;
        }
        # Reconstruct the field value.
        $_decomposed{field_val_mid} = join(' ', @strings);

        # The first letter after a punctuation (:, -)
        $_decomposed{field_val_mid} =~ s/
            (?<bef>(?:[:]\s* |\s+[\-]\s+))
            (?<first_lett>[A-Z])
        /$+{bef}\{$+{first_lett}\}/gx;

        # (4) Reconstruct the field value.
        my $_reconstructed;
        $_reconstructed .= $_ for @_decomposed {
            'field_val_bef',
            'field_val_mid',  # The one that modified
            'field_val_aft',
        };
        $field_val = $_reconstructed;
    }

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub subscript_molecules {
    # """Subscript the number of constituent atoms of molecules."""
    my($field_key, $field_val) = @_;

    # Entry field designator
    my @fields_of_int = qw/
        title
        subtitle
    /;
    return unless first { $field_key =~ /\s*\b$_/i } @fields_of_int;

    # Generalizable (multication-multianion)
    $field_val =~ s/
      (?<cat>[A-Z]{1}[a-z]{0,1}) # Cation
      (?<num_cat>[0-9]{1,2})     # Number of cations
      (?<ani>[A-Z]{1}[a-z]{0,1}) # Anion
      (?<num_ani>[0-9]{1,2})     # Number of anions
    /\{$+{cat}\$_{$+{num_cat}}\$$+{ani}\$_{$+{num_ani}}\$\}/gx;

    # Nongeneralizable
    my %molecules = (
        'MoO3'     => 'MoO$_{3}$',
        '100MoO3'  => '$^{100}$MoO$_{3}$',
        'MoO4'     => 'MoO$_{4}$',
        '100Mo2C'  => '$^{100}$Mo$_{2}$C',
        'TcO4-'    => 'TcO$_{4}^{-}$',
        '99mTcO4-' => '$^{99\text{m}}$TcO$_{4}^{-}$',
        'TcO4−'    => 'TcO$_{4}^{-}$',
        '99mTcO4−' => '$^{99\text{m}}$TcO$_{4}^{-}$',
        'Na2'      => 'Na$_{2}$',
    );
    $field_val =~ s/$_/\{$molecules{$_}\}/ for keys %molecules;

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub superscript_isotopes {
    # """Superscript the mass numbers of isotopes."""
    my($field_key, $field_val) = @_;

    # Entry field designator
    my @fields_of_int = qw/
        title
        subtitle
    /;
    return unless first { $field_key =~ /\s*\b$_/i } @fields_of_int;

    # Use TeX command for a superscript-expressed isotope.
    # e.g. 99Mo  => $^{99}$Mo
    # e.g. 99mTc => $^{99m}$Tc
    $field_val =~ s/
        (?<mass_num>[0-9]{1,3}[mg]?)     # m:metastable, g:ground
        (?<chem_symb>[A-Z]{1}[a-z]{0,1}) # e.g. H, Tc
    /\{\$^{$+{mass_num}}\$$+{chem_symb}\}/gx;

    # Add \text{..} to prevent mass number modifiers from being italicized.
    # e.g. $^{99m}$Tc => $^{99\text{m}}$Tc
    # e.g. $^{99g}$Tc => $^{99\text{g}}$Tc
    $field_val =~ s/
        (?<bef>\^\{)
        (?<mass_num>[0-9]{1,3})
        (?<mass_num_mod>[mg]{1})
        (?<aft>\})
    /$+{bef}$+{mass_num}\\text{$+{mass_num_mod}}$+{aft}/gx;

    # Correct ^{1}D, ^{2}D, and ^{3}D resulting from the above snippet.
    $field_val =~ s/\$\^\{(?<dim>[1-3])\}\$D/$+{dim}D/;

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub greeks_to_tex_cmds {
    # """Convert Greek letters to TeX commands."""
    my($field_key, $field_val) = @_;

    # Entry field designator
    my @fields_of_int = qw/
        title
        subtitle
    /;
    return unless first { $field_key =~ /\s*\b$_/i } @fields_of_int;

    # To TeX Greek commands
    my %greeks = (
        'α' => '$\alpha$',
        'β' => '$\beta$',
        'γ' => '$\gamma$',
        'δ' => '$\delta$',
        'ε' => '$\epsilon$',
        'ζ' => '$\zeta$',
        'δ' => '$\delta$',
        'η' => '$\eta$',
        'θ' => '$\theta$',
        'ι' => '$\iota$',
        'κ' => '$\kappa$',
        'λ' => '$\lambda$',
        'μ' => '$\mu$',
        'ν' => '$\nu$',
        'ξ' => '$\xi$',
        'ο' => '$\omicron$',
        'π' => '$\pi$',
        'ρ' => '$\rho$',
        'σ' => '$\sigma$',
        'τ' => '$\tau$',
        'υ' => '$\upsilon$',
        'φ' => '$\phi$',
        'χ' => '$\chi$',
        'ψ' => '$\psi$',
        'ω' => '$\omega$',
    );
    $field_val =~ s/$_/$greeks{$_}/g for keys %greeks;

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub accented_to_tex_syntaxes {
    # """Convert accented strings to TeX syntaxes using ligatures."""
    my($field_key, $field_val) = @_;

    # Entry field designator
    my @fields_of_int = qw/
        author
    /;
    return unless first { $field_key =~ /\s*\b$_/i } @fields_of_int;

    # TeX ligatures
    my %accented = (
        # Refer to: https://tex.stackexchange.com/tags/accents/info
        # grave accent
        'è' => "\\`{e}",
        # acute accent
        'á' => "\\'{a}",
        'é' => "\\'{e}",
        'É' => "\\'{E}",
        'ó' => "\\'{o}",
        'ý' => "\\'{y}",
        # circumflex or hat
        'ê' => "\\^{e}",
        # umlaut or dieresis
        'ö' => "\\\"{o}",
        'ü' => "\\\"{u}",
        # tilde or squiggle
        'ã' => "\\~{a}",
        # háček or check
        'ř' => "\\v{r}",
        'š' => "\\v{s}",
        'Š' => "\\v{S}",
        'ž' => "\\v{z}",
        'Ž' => "\\v{Z}",
        # cedilla
        'ç' => "\\c{c}",
    );
    $field_val =~ s/$_/$accented{$_}/g for keys %accented;

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub to_url_underscores {
    # """Convert underscores to TeX-parsable ones for the URL entry field."""
    my($field_key, $field_val) = @_;

    # Entry field designator
    my @fields_of_int = qw/
        url
    /;
    return unless first { $field_key =~ /\s*\b$_/i } @fields_of_int;

    # To TeX-parsable underscore
    $field_val =~ s/(?<bef>[^\\])_/$+{bef}\\_/g;

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub rm_periods_from_journal {
    # """Remove periods from the journal and shortjournal fields."""
    my($field_key, $field_val) = @_;

    # Entry field designator
    my @fields_of_int = qw/
        journal
        shortjournal
    /;
    return unless first { $field_key =~ /\s*\b$_/i } @fields_of_int;

    # Remove periods.
    $field_val =~ s/[.]//g;

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub to_tex_symbs {
    # """Convert symbols to TeX commands."""
    my($field_key, $field_val) = @_;

    # To TeX commands
    my %symbs = (
        '’' => "'",
        '“' => '"',
        '−' => '{\textendash}',  # endash 1
        '–' => '{\textendash}',  # endash 2
        '—' => '{\textemdash}',
        '™' => '{\texttrademark}',
        '®' => '{\textregistered}',
    );
    $field_val =~ s/$_/$symbs{$_}/g for keys %symbs;

    # Assign the modified field value.
    $_[1] = $field_val;

    return;
}


sub modify_bib {
    # """Modify .bib fields for BibTeX/BibLaTeX."""
    my $run_opts_href = shift;

    # Exceptions and user preferences
    my %old_and_new = (
        'natC' => '\textsuperscript{nat}C',
    );
    read_in_prf($run_opts_href, \%old_and_new) if -e $run_opts_href->{prf};

    # Notification
    if (not @{$run_opts_href->{bib_files}}) {
        print "No .bib file found.\n\n";
        return;
    }
    printf(
        "The following .bib file%s will be converted:\n",
        $run_opts_href->{bib_files}[1] ? 's' : '',
    );
    say "-" x 70;
    say "[$_]" for @{$run_opts_href->{bib_files}};
    say "-" x 70;

    # Predefined modifier routines
    my %modifier_routines_opt = (
        enclose_with_braces => {
            cref   => \&enclose_with_braces,
            toggle => 1,
        },
        subscript_molecules => {
            cref   => \&subscript_molecules,
            toggle => 1,
        },
        superscript_isotopes => {
            cref   => \&superscript_isotopes,
            toggle => 1,
        },
        greeks_to_tex_cmds => {
            cref   => \&greeks_to_tex_cmds,
            toggle => 1,
        },
        accented_to_tex_syntaxes => {
            cref   => \&accented_to_tex_syntaxes,
            toggle => 1,
        },
        to_url_underscores => {
            cref   => \&to_url_underscores,
            toggle => 0,  # Prone to induce BibTeX/BibLaTeX errors
        },
        rm_periods_from_journal => {
            cref   => \&rm_periods_from_journal,
            toggle => 0,
        },
        to_tex_symbs => {  # Must be placed here (at the end)
            cref   => \&to_tex_symbs,
            toggle => 1,
        },
    );
    # The order matters: "to_tex_symbs" must be placed at the end.
    my @modifier_routines = qw/
        enclose_with_braces
        subscript_molecules
        superscript_isotopes
        greeks_to_tex_cmds
        accented_to_tex_syntaxes
        to_url_underscores
        rm_periods_from_journal
        to_tex_symbs
    /;
    foreach (@{$run_opts_href->{turn_on}}) {
        $modifier_routines_opt{$_}{toggle} = 1
            if exists $modifier_routines_opt{$_};
    }
    foreach (@{$run_opts_href->{turn_off}}) {
        $modifier_routines_opt{$_}{toggle} = 0
            if exists $modifier_routines_opt{$_};
    }

    # Work on the .bib files.
    my $yn     = undef;
    my $yn_msg = "Run modifications? (y/n)> ";
    foreach my $bib (@{$run_opts_href->{bib_files}}) {
        my @the_modified = ();
        open my $bib_fh, '<:encoding(UTF-8)', $bib;
        foreach my $line (<$bib_fh>) {
            chomp($line);
            if ($line =~ /=/) {  # Works only on entry fields
                # (1) Decompose an entry field into its key and value.
                # - regex is used instead of split because some entry fields
                #   like url contain the equals sign (=).
                my %decomposed = (field_sep => '=');
                ($decomposed{field_key} = $line) =~ s/
                    ^(?<field_key>.*?) # Nongreedy quantifier
                    $decomposed{field_sep}
                    (?<field_val>.*)$
                /$+{field_key}/x;
                ($decomposed{field_val} = $line) =~ s/
                    ^(?<field_key>.*?)
                    $decomposed{field_sep}
                    (?<field_val>.*)$
                /$+{field_val}/x;

                # (2) Primary modifications
                # - Intended for specific modifications
                # - To which the user preferences belong
                # - Takes precedence over the secondary modifications below
                $decomposed{field_val} =~ s/$_/$old_and_new{$_}/g
                    for keys %old_and_new;

                # (3) Secondary modifications
                # - Intended for general modifications
                # - Predefined modifier routines
                foreach (@modifier_routines) {
                    next if not $modifier_routines_opt{$_}{toggle};
                    my $cref = $modifier_routines_opt{$_}{cref};
                    $cref->($decomposed{field_key}, $decomposed{field_val});
                }

                # (4) Reconstruct the entry field.
                my $reconstructed;
                $reconstructed .= $_ for @decomposed {
                    'field_key',
                    'field_sep',
                    'field_val',  # The one that modified
                };
                $line = $reconstructed;
            };

            # The modified line
            push @the_modified, $line;
        }
        close $bib_fh;

        # Verbose option
        if ($run_opts_href->{is_verbose}) {
            say "-" x 70;
            say "\nThe new lines will be:\n";
            say "-" x 70;
            say for @the_modified;
            say "-" x 70;
        }

        # Timestamping on the output .bib file
        my %datetimes = construct_timestamps('-');
        my $tstamp = sprintf(
            "\@comment\{Modified by %s at %s\}",
            basename($0),
            $datetimes{ymdhms},
        );
        if ($the_modified[0] =~ /\@comment/) {
            $the_modified[0] = $tstamp;  # Overwriting
        }

        # yn prompt
        if (not $yn) {
            print $yn_msg;
            while (chomp($yn = <STDIN>)) {
                last   if $yn =~ /\by\b/i;
                return if $yn =~ /\bn\b/i;
                print $yn_msg;
            }
        }

        # Write the new lines to an output file.
        my $dname = dirname($bib);
        my $patt = qr/(.*)[.]([a-zA-Z]+$)/;
        (my $bname = basename($bib)) =~ s/$patt/$1/;
        (my $ext = basename($bib)) =~ s/$patt/$2/;
        my $bib_out = sprintf(
            "%s/%s%s.%s",
            $dname,
            $bname,
            $run_opts_href->{out_flag},
            $ext,
        );
        open my $bib_out_fh, '>:encoding(UTF-8)', $bib_out;
        if (not $the_modified[0] =~ /\@comment/) {
            say $bib_out_fh "$tstamp\n";
        }
        say $bib_out_fh $_ for @the_modified;
        close $bib_out_fh;
        say "[$bib_out] generated.";
    }

    return;
}


sub bibmod {
    # """bibmod main routine"""
    if (@ARGV) {
        my %prog_info = (
            titl       => basename($0, '.pl'),
            expl       => 'Modify .bib fields for BibTeX/BibLaTeX',
            vers       => $VERSION,
            date_last  => $LAST,
            date_first => $FIRST,
            auth       => {
                name => 'Jaewoong Jang',
#                posi => '',
#                affi => '',
                mail => 'jangj@korea.ac.kr',
            },
        );
        my %cmd_opts = (  # Command-line opts
            bib_all  => qr/-?-a(?:ll)?\b/i,
            prf      => qr/-?-prf\s*=\s*/i,
            out_flag => qr/-?-(?:out_)?flag\s*=\s*/i,
            verbose  => qr/-?-v(?:erbose)?\b/i,
            turn_on  => qr/-?-(?:turn_)?on\s*=\s*/i,
            turn_off => qr/-?-(?:turn_)?off\s*=\s*/i,
            nofm     => qr/-?-nofm\b/i,
            nopause  => qr/-?-nopause\b/i,
        );
        my %run_opts = (  # Program run opts
            bib_files  => [],
            prf        => '',
            out_flag   => '',
            prf_sep    => '=>',
            turn_on    => [],
            turn_off   => [],
            is_nofm    => 0,
            is_nopause => 0,
        );

        # ARGV validation and parsing
        validate_argv(\@ARGV, \%cmd_opts);
        parse_argv(\@ARGV, \%cmd_opts, \%run_opts);

        # Notification - beginning
        show_front_matter(\%prog_info, 'prog', 'auth')
            unless $run_opts{is_nofm};

        # Main
        modify_bib(\%run_opts);

        # Notification - end
        show_elapsed_real_time();
        pause_shell()
            unless $run_opts{is_nopause};
    }

    system("perldoc \"$0\"") if not @ARGV;

    return;
}


bibmod();
__END__

=head1 NAME

bibmod - Modify .bib fields for BibTeX/BibLaTeX

=head1 SYNOPSIS

    perl bibmod.pl [bib_file ...] [--all] [--prf=prf_file]
                   [--out_flag=flag] [--verbose]
                   [--turn_on=routine ...] [--turn_off=routine ...]
                   [--nofm] [--nopause]

=head1 DESCRIPTION

    Designated .bib files are modified according to the predefined
    modifier routines and/or user preferences.
    Although any .bib file can be modified, this program can be
    particularly useful for EndNote-exported .bib files,
    where many strings need to be converted to LaTeX commands.

=head1 OPTIONS

    bib_file ...
        .bib files to be modified.

    --all (short form: -a)
        All .bib files in the current working directory will be modified.

    --prf=prf_file
        A user preferences file for string modifications.
        String pairs contained in this file take precedence
        over the predefined modifier routines.
        Refer to the sample file 'sample.prf' for the syntax.

    --out_flag=flag (short form: --flag, default: empty)
        A string to be appended to the names of modified .bib files.

    --verbose (short form: -v)
        Display the new lines of a bib file before its modifications.

    --turn_on=routine ... (short form: -off)
    --turn_off=routine ... (short form: -off)
        The following modifier routines can be toggled on or off.
            enclose_with_braces      (default: on)
            subscript_molecules      (default: on)
            superscript_isotopes     (default: on)
            greeks_to_tex_cmds       (default: on)
            accented_to_tex_syntaxes (default: on)
            to_url_underscores       (default: off)
            rm_periods_from_journal  (default: off)
            to_tex_symbs             (default: on)

    --nofm
        Do not show the front matter at the beginning of the program.

    --nopause
        Do not pause the shell at the end of the program.

=head1 EXAMPLES

    perl bibmod.pl molytech.bib --v
    perl bibmod.pl molytech.bib murine.bib --prf=molytech.prf
    perl bibmod.pl --all --nofm --on=rm_periods_from_journal

=head1 REQUIREMENTS

Perl 5

=head1 CAVEATS

    Please make sure that your .bib files are encoded in UTF-8;
    non-7-bit characters will be corrupted otherwise.

=head1 SEE ALSO

L<bibmod on GitHub|https://github.com/jangcom/bibmod>

=head1 AUTHOR

Jaewoong Jang <jangj@korea.ac.kr>

=head1 COPYRIGHT

Copyright (c) 2017-2020 Jaewoong Jang

=head1 LICENSE

This software is available under the MIT license;
the license information is found in 'LICENSE'.

=cut
