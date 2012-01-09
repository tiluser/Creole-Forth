# Demo Perl program showing Creole interfacing.
# Creole activates script, which takes a seed
# number and number of random numbers to produce.
# Output is written to stacks.txt, which is uploaded
# back to Creole's data stack.
#
# Note : Perl for Windows is available at http://www.activestate.com
#
#!C:/perl/bin/perl.exe
$seed=$ARGV[0];
$num_to_rand=$ARGV[1];

open(OUT,">stack.txt");
srand($seed);

for ($i=0; $i < $num_to_rand; $i++)
{
  $ordarray[$i]=$i;
}

for ($i=1; $i <=$num_to_rand; $i++)
{
  $randarray[$i]=$ordarray[rand(@ordarray)];
  print OUT "$randarray[$i]  \n";
}

close(OUT);