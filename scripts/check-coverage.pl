#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(all);

# Exit 0 when every file meets both thresholds, 1 when a report was produced but
# a file is below one, 2 when no report could be produced at all.
my $EXIT_CANNOT_RUN = 2;

my @COLUMNS = qw(file line-covered line-total branch-covered branch-total
                 uncovered-lines branch-locations);
my $EXCLUSIONS = 'scripts/coverage-exclusions.sexp';
my $SOURCE_ROOT = 'src';
my %ENVIRONMENT = (
  input        => 'COVERAGE_SUMMARY',
  'min-line'   => 'CL_MARKDOWN_COVERAGE_MIN_LINE',
  'min-branch' => 'CL_MARKDOWN_COVERAGE_MIN_BRANCH',
);
my %DEFAULT = (
  input        => '/tmp/cl-markdown-kit-coverage/per-file.tsv',
  'min-line'   => 100,
  'min-branch' => 100,
);
# Each pair names a kind and the option holding its minimum. The branch counts
# are adjusted only for source locations covered by a declared exclusion.
my @KINDS = (['line', 'min-line'], ['branch', 'min-branch']);

sub fail {
  my ($message) = @_;
  print STDERR "$0: $message\n";
  exit $EXIT_CANNOT_RUN;
}

sub usage {
  print STDERR <<"USAGE";
usage: $0 [--input TSV] [--min-line PERCENT] [--min-branch PERCENT]
             [--source-root DIRECTORY] [--exclusions FILE]

  --input TSV          per-file coverage summary (default: \$COVERAGE_SUMMARY,
                       else /tmp/cl-markdown-kit-coverage/per-file.tsv)
  --min-line PERCENT   required line coverage (default:
                       \$CL_MARKDOWN_COVERAGE_MIN_LINE, else 100)
  --min-branch PERCENT required branch coverage (default:
                       \$CL_MARKDOWN_COVERAGE_MIN_BRANCH, else 100)
  --source-root DIRECTORY source files used to classify exclusions (default: src)
  --exclusions FILE    exclusion data (default: scripts/coverage-exclusions.sexp)

exit 0 when every file meets both thresholds, 1 when a file is below one, and 2
when the settings are invalid or the summary cannot be read.
USAGE
  exit $EXIT_CANNOT_RUN;
}

sub fail_usage {
  my ($message) = @_;
  print STDERR "$0: $message\n\n";
  usage();
}

sub strip_lisp_strings_and_comments {
  my ($line) = @_;
  $line =~ s/"(?:\\.|[^"\\])*"//g;
  $line =~ s/#\\(?:[A-Za-z][A-Za-z0-9-]*|.)//g;
  $line =~ s/;.*//;
  return $line;
}

# A set-but-empty environment variable is how a shell spells "unset", while an
# empty option value is a mistake and stays visible to the validator.
sub resolve {
  my ($setting, $given) = @_;
  my $environment = $ENV{$ENVIRONMENT{$setting}};
  return ($given, 'option') if defined $given;
  return ($environment, $ENVIRONMENT{$setting})
    if defined $environment && length $environment;
  return ($DEFAULT{$setting}, 'default');
}

sub resolve_percent {
  my ($setting, $given) = @_;
  my ($raw, $origin) = resolve($setting, $given);
  my $number = $raw =~ /\A(?:\d+(?:\.\d+)?|\.\d+)\z/ ? $raw : undef;
  fail("--$setting ($origin) must be a percentage in 0..100, got '$raw'")
    if !defined $number || $number > 100;
  return 0 + $number;
}

sub parse_arguments {
  my %given;
  while (@ARGV) {
    my $argument = shift @ARGV;
    my ($setting) = $argument =~ /\A--([a-z][a-z-]*)\z/;
    fail_usage("unexpected argument '$argument'") unless defined $setting;
    fail_usage("unknown option --$setting")
      unless exists $DEFAULT{$setting}
          || $setting eq 'source-root' || $setting eq 'exclusions';
    fail_usage("option --$setting requires a value") unless @ARGV;
    $given{$setting} = shift @ARGV;
  }
  return %given;
}

sub exclusion_rules {
  my ($path) = @_;
  open my $handle, '<', $path or fail("cannot read exclusions $path: $!");
  my @rules;
  while (my $line = <$handle>) {
    next if $line =~ /\A\s*(?:;|\z)/;
    my ($kind, $head, $reproduction)
      = $line =~ /:kind\s+([^\s()]+).*?:head\s+"([^"]+)".*?:reproduction\s+"([^"]+)"/;
    my ($name) = $line =~ /:name\s+"([^"]+)"/;
    fail("$path: expected :kind and :head in '$line'")
      unless defined $kind && defined $head && defined $reproduction;
    fail("$path: missing reproduction $reproduction") unless -f $reproduction;
    push @rules, { kind => $kind, head => $head, name => $name,
                   reproduction => $reproduction };
  }
  close $handle or fail("cannot read exclusions $path: $!");
  fail("$path has no exclusion rules") unless @rules;
  return \@rules;
}

sub source_exclusion_lines {
  my ($path, $rules) = @_;
  open my $handle, '<', $path or fail("cannot read source $path: $!");
  my @lines = <$handle>;
  close $handle or fail("cannot read source $path: $!");
  my @excluded;
  my $depth = 0;
  my $start;
  for my $index (0 .. $#lines) {
    my $clean = strip_lisp_strings_and_comments($lines[$index]);
    my $opens = () = $clean =~ /\(/g;
    my $closes = () = $clean =~ /\)/g;
    if ($depth == 0 && $clean =~ /\(\s*([A-Za-z0-9*+!?_-]+)/) {
      $start = $index + 1;
      my $head = $1;
      my ($form_name) = $clean =~ /\(\s*\Q$head\E\s+([^\s()]+)/;
      my ($rule) = grep {
        $_->{head} eq $head
          && (!defined $_->{name}
              || (defined $form_name && $_->{name} eq $form_name))
      } @$rules;
      if ($rule && $rule->{kind} eq 'defun-header') {
        my $lambda_list_start;
        if ($clean =~ /\(\s*\Q$head\E\s+\Q$form_name\E\s*(\()/) {
          $lambda_list_start = $-[1];
        }
        if (defined $lambda_list_start) {
          my $balance = 0;
          my $header_end;
          for my $j ($index .. $#lines) {
            my $part = strip_lisp_strings_and_comments($lines[$j]);
            $part = substr($part, $lambda_list_start) if $j == $index;
            $balance += (() = $part =~ /\(/g);
            $balance -= (() = $part =~ /\)/g);
            if ($balance == 0) {
              $header_end = $j + 1;
              last;
            }
          }
          push @excluded, [$start, $header_end, $head]
            if defined $header_end && $start <= $header_end;
        }
      } elsif ($rule) {
        my $balance = 0;
        my $end = $index;
        for my $j ($index .. $#lines) {
          my $part = strip_lisp_strings_and_comments($lines[$j]);
          $balance += (() = $part =~ /\(/g);
          $balance -= (() = $part =~ /\)/g);
          if ($balance <= 0) {
            $end = $j;
            last;
          }
        }
        my $excluded_end = $end + 1;
        if ($rule->{kind} eq 'condition-declaration') {
          for my $j ($index .. $end) {
            my $part = strip_lisp_strings_and_comments($lines[$j]);
            if ($part =~ /\(\s*:report\b/) {
              $excluded_end = $j;
              last;
            }
          }
        }
        push @excluded, [$start, $excluded_end, $head]
          if $start <= $excluded_end;
      }
    }
    $depth += $opens - $closes;
  }
  return \@excluded;
}

sub line_is_excluded {
  my ($line, $ranges) = @_;
  return 1 if grep { $line >= $_->[0] && $line <= $_->[1] } @$ranges;
  return 0;
}

sub branch_counts_outside_exclusions {
  my ($row, $ranges) = @_;
  my ($covered, $total) = @{$row->{counts}}[2, 3];
  for my $location (@{$row->{branch_locations}}) {
    next unless line_is_excluded($location->[0], $ranges);
    $covered -= $location->[1];
    $total -= $location->[2];
  }
  return ($covered, $total);
}

sub read_rows {
  my ($path) = @_;
  open my $handle, '<', $path or fail("cannot read $path: $!");
  my (@rows, $number, $header);
  while (my $line = <$handle>) {
    $number++;
    next if $line =~ /\A#/;
    chomp $line;
    my @fields = split /\t/, $line, -1;
    unless ($header) {
      fail("$path line $number: expected header '"
           . join("\t", @COLUMNS) . "', got '$line'")
        unless "@fields" eq "@COLUMNS";
      $header = 1;
      next;
    }
    fail("$path line $number: expected " . scalar(@COLUMNS)
         . " tab-separated columns, got " . scalar(@fields))
      if @fields != @COLUMNS;
    fail("$path line $number: file must not be empty")
      if $fields[0] =~ /\A\s*\z/;
    my @counts;
    for my $column (1 .. 4) {
      fail("$path line $number: $COLUMNS[$column] must be a non-negative"
           . " integer, got '$fields[$column]'")
        unless $fields[$column] =~ /\A\d+\z/;
      push @counts, 0 + $fields[$column];
    }
    fail("$path line $number: line-covered cannot exceed line-total")
      if $fields[1] > $fields[2];
    fail("$path line $number: branch-covered cannot exceed branch-total")
      if $fields[3] > $fields[4];
    my $uncovered = $fields[5];
    fail("$path line $number: invalid uncovered-lines syntax '$uncovered'")
      unless $uncovered eq '-'
          || $uncovered =~ /\A\d+(?:-\d+)?(?:,\d+(?:-\d+)?)*\z/;
    if ($uncovered ne '-') {
      for my $range (split /,/, $uncovered) {
        my ($start, $end) = split /-/, $range;
        $end = $start unless defined $end;
        fail("$path line $number: uncovered-lines range '$range' is backwards")
          if $start < 1 || $end < $start;
      }
    }
    my @branch_locations;
    my ($location_covered, $location_total) = (0, 0);
    if ($fields[6] ne '-') {
      my %seen_lines;
      for my $location (split /,/, $fields[6]) {
        my ($line, $covered, $total)
          = $location =~ /\A(\d+):(\d+)\/(\d+)\z/;
        fail("$path line $number: invalid branch-locations entry '$location'")
          unless defined $total && $line > 0 && $total > 0
              && $covered <= $total;
        fail("$path line $number: duplicate branch-locations line $line")
          if $seen_lines{$line}++;
        push @branch_locations, [0 + $line, 0 + $covered, 0 + $total];
        $location_covered += $covered;
        $location_total += $total;
      }
    }
    fail("$path line $number: branch-locations do not match branch counts")
      if $location_covered != $fields[3] || $location_total != $fields[4];
    push @rows, { file => $fields[0], counts => \@counts,
                  uncovered => $uncovered,
                  branch_locations => \@branch_locations };
  }
  close $handle or fail("cannot read $path: $!");
  fail("$path is empty") unless $number;
  fail("$path has no header row") unless $header;
  fail("$path has no data rows") unless @rows;
  return \@rows;
}

sub percentage {
  my ($covered, $total) = @_;
  return $total ? 100 * $covered / $total : undef;
}

sub percent_text {
  my ($value) = @_;
  return 'n/a' unless defined $value;
  return sprintf '%.1f%%', $value;
}

my %given = parse_arguments();
my ($input) = resolve('input', $given{input});
my $source_root = $given{'source-root'} // $SOURCE_ROOT;
my $exclusions = $given{exclusions} // $EXCLUSIONS;
my @minimum
  = map { resolve_percent($KINDS[$_][1], $given{ $KINDS[$_][1] }) } 0 .. $#KINDS;
my $rows = read_rows($input);
my $rules = exclusion_rules($exclusions);

my (@failures, @totals);
for my $row (@$rows) {
  my $ranges = [];
  my $source = "$source_root/$row->{file}";
  $ranges = source_exclusion_lines($source, $rules) if -f $source;
  $row->{excluded} = $ranges;
  my ($branch_covered, $branch_total)
    = branch_counts_outside_exclusions($row, $ranges);
  my @values;
  for my $index (0 .. $#KINDS) {
    my ($covered, $total) = $index == 1
      ? ($branch_covered, $branch_total)
      : @{$row->{counts}}[2 * $index, 2 * $index + 1];
    push @values, percentage($covered, $total);
    $totals[2 * $index] += $covered;
    $totals[2 * $index + 1] += $total;
  }
  $row->{values} = \@values;
  my @reasons;
  for my $index (0 .. $#KINDS) {
    next unless defined $values[$index];
    next if $values[$index] >= $minimum[$index];
    push @reasons, sprintf '%s %s, required %s', $KINDS[$index][0],
      percent_text($values[$index]), percent_text($minimum[$index]);
  }
  if (@reasons) {
    my $line_failed = defined $row->{values}[0]
      && $row->{values}[0] < $minimum[0];
    my $branch_failed = defined $row->{values}[1]
      && $row->{values}[1] < $minimum[1];
    my $uncovered = $row->{uncovered};
    my $line_failure_is_excluded = $uncovered eq '-'
      ? @$ranges
      : all { my $line = $_; line_is_excluded($line, $ranges) }
            map { my ($start, $end) = split /-/, $_; $end //= $start;
                  ($start .. $end) } split /,/, $uncovered;
    push @failures, { file => $row->{file}, reasons => \@reasons }
      if $branch_failed || ($line_failed && !$line_failure_is_excluded);
  }
}

print "| file | line | branch | uncovered lines |\n";
print "|---|---:|---:|---|\n";
for my $row (@$rows) {
  print sprintf "| %s | %s | %s | %s |\n", $row->{file},
    percent_text($row->{values}[0]), percent_text($row->{values}[1]),
    $row->{uncovered};
}
print sprintf "| total | %s | %s |  |\n",
  percent_text(percentage($totals[0], $totals[1])),
  percent_text(percentage($totals[2], $totals[3]));

if (@failures) {
  print "\n";
  for my $failure (@failures) {
    print "  $failure->{file}: ", join('; ', @{$failure->{reasons}}), "\n";
  }
  printf "coverage gate: FAIL (%d of %d files below threshold)\n",
    scalar @failures, scalar @$rows;
  exit 1;
}
printf "coverage gate: PASS (all %d files at or above threshold)\n",
  scalar @$rows;
exit 0;
