use strict;
use warnings;

use Test::More;
use FindBin;
use Ogg::Vorbis::Header::PurePerl;

my @test_files = qw/test.ogg/;

chdir $FindBin::Bin;

for my $fn (@test_files) {

    ok( my $ogg = Ogg::Vorbis::Header::PurePerl->new($fn)->load_comments,
        "Loaded test file" );

    my $info = $ogg->info();
    ok( $info->{sample_rate} == 48000, "Sample rate matched" );
    ok( int($info->{length}) == 16,    "Track length matched" );

    my @tags = $ogg->comment_tags();
    ok( $tags[3] eq 'genre', "Tag order matched" );
    for my $t (@tags) {
        print "$t\n";
        print "\t$_\n" for ($ogg->comment($t));
    }
    ok( ($ogg->comment('performer')) eq 'Chiara Bertoglio', "Performer matched" );

}

done_testing();
exit;
