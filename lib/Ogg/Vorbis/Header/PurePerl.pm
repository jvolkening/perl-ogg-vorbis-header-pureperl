package Ogg::Vorbis::Header::PurePerl 2.000;

use 5.012;
use strict;
use warnings;

use Carp;
use Digest::CRC;
use List::Util qw/sum min/;
use POSIX qw/floor/;
use Scalar::Util qw/blessed/;
use Cwd qw/abs_path/;
use File::Copy qw/move/;

use constant OGG_CAPTURE       => 'OggS';
use constant HEADER_IDENT      => 1;
use constant HEADER_COMMENT    => 3;
use constant HEADER_SETUP      => 5;
use constant VORBIS_PREFIX_LEN => 7;
use constant IDENT_PAGE_LEN    => 58;
use constant MAX_SEG_LEN       => 255;
use constant MAX_SEGS_PER_PAGE => 255;

sub new {

    my ($class, $fn) = @_;

    my $self = bless {}, $class;

    croak "Error reading $fn" if (! -r $fn);
    $self->{fn} = abs_path($fn);

    return $self;

}

#----------------------------------------------------------------------------#
# Public methods
#----------------------------------------------------------------------------#

sub load {

    my ($self, $fn) = @_;

    # called as instance method
    if (blessed($self) eq 'Ogg::Vorbis::Header::PurePerl') {
        carp "Filename is ignored when Ogg::Vorbis::Header::PurePerl::load"
            . " is called as instance method" if (defined $fn);
    }
    else {
        $self = Ogg::Vorbis::Header::PurePerl->new($fn);
    }

    return $self->load_comments();

}

sub load_comments {

    my ($self) = @_;

    open my $fh, '<:raw', $self->{fn}
        or croak "Error opening $self->{fn} for reading";
    $self->{fh} = $fh;
    
    $self->{curr_offset} = 0;
    $self->{curr_page}   = -1;
    $self->{file_size}   = -s $self->{fh};
    $self->{crc} = 1; # should start defined but not initialized

    $self->_parse_headers();

    close $self->{fh};
    $self->{fh} = undef;

    return $self;

}

sub info {

    my ($self, $key) = @_;

	# if the user supplied a key, return associated value
	# otherwise, return entire info hash
	return  (defined $key) ? $self->{info}->{lc $key} : $self->{info};

}

sub comment_tags {

    my ($self) = @_;

    # return an array of key values (including duplicates)
    return sort map {
        ($_) x scalar(@{ $self->{comments}->{$_} })
    } keys %{ $self->{comments} };

}

sub comment {

    my ($self, $key) = @_;

    # return comment(s) associated with given key
    $key = lc $key;

	return undef unless(defined $self->{comments}->{$key});
	return wantarray 
		? sort @{$self->{'comments'}->{$key}}
		: (sort @{$self->{'comments'}->{$key}})[0];

}

sub add_comments {

    my ($self, @comments) = @_;

    # add one or more key/value pairs to comments
    croak "Odd number of arguments" if (scalar(@comments) % 2);
    my @keys = @comments[ map {$_*2}   0..int($#comments/2) ];
    my @vals = @comments[ map {$_*2+1} 0..int($#comments/2) ];

    for (0..$#keys) {
        
        croak "invalid key name"   if (! _is_valid_key($keys[$_]) );
        croak "invalid value name" if (! _is_valid_value($vals[$_]) );
        utf8::encode($vals[$_])    if ( utf8::is_utf8($vals[$_]) );
        #$self->{comments}->{$keys[$_]} = []
            #if (! defined $self->{comments}->{$keys[$_]} );
        push @{ $self->{comments}->{$keys[$_]} }, $vals[$_];

    }

}

sub edit_comment {

    my ($self, $key, $value, $idx)  = @_;
    $idx = $idx // 0;

    croak "invalid value" if (! _is_valid_value($value));
    utf8::encode($value)  if ( utf8::is_utf8($value) );

    return undef if (! defined $self->{comments}->{$key});
    my @vals = sort @{ $self->{comments}->{$key}};
    return undef if ( $#vals < $idx );

    my $old = splice @vals, $idx, 1, $value;
    $self->{comments}->{$key} = [@vals];

    return $old;

}

sub delete_comment {

    my ($self, $key, $idx)  = @_;
    $idx = $idx // 0;

    return undef if (! defined $self->{comments}->{$key});
    my @vals = sort @{ $self->{comments}->{$key}};
    return undef if ( $#vals < $idx );
    my $old = splice @vals, $idx, 1;
    if (scalar(@vals) < 1) {
        delete $self->{comments}->{$key};
    }
    else {
        $self->{comments}->{$key} = [@vals];
    }

    return $old;

}

sub clear_comments {

    my ($self, @keys) = @_;

    if (scalar @keys) {
        for (@keys) {
            return undef if (! defined $self->{comments}->{$_});
            delete $self->{comments}->{$_};
        }
    }
    else {
        $self->{comments} = {};
    }

    return 1;

}


sub path {

    my ($self) = @_;
    return $self->{fn};

}

#----------------------------------------------------------------------------#
# Private methods
#----------------------------------------------------------------------------#

sub _parse_vorbis_prefix {

    my ($self) = @_;

    # From Section 4.2.1;
    #  Each header packet begins with the same header fields.
    #  
    #  1    1) [packet_type] : 8 bit value
    #  2    2) 0x76, 0x6f, 0x72, 0x62, 0x69, 0x73: the characters ’v’,’o’,’r’,’b’,’i’,’s’ as six octets

    my ($type, $magic) = unpack 'CA*', $self->_ogg_read(VORBIS_PREFIX_LEN);
    croak "Malformed vorbis packet" if ($magic ne 'vorbis');
    
    return $type;

}

sub _parse_headers {

    my ($self) = @_;

    seek $self->{fh}, 0, 0;

    # parse identification and comment headers, and determine offsets and
    # lengths of all three header packets

    # initialize first packet
    $self->{packet_lens} = [0];

    # parse initial Ogg page header
    $self->_load_page();
    # Now we should be at start of ident header

    #-------------------------------------------------------------------------
    # Identfication header 
    #-------------------------------------------------------------------------

    $self->{headers}->{ident}->{file_offset} = tell $self->{fh};
    $self->{headers}->{ident}->{page_offset} = $self->{page_offset};
    $self->{headers}->{ident}->{page_len}    = $self->{page_len};

    # parse vorbis prefix and advance
    my $type = $self->_parse_vorbis_prefix();
    croak "Not an identification header" if ($type ne HEADER_IDENT);

    # unpack header data
    my @values = unpack 'VCVVVVAC', $self->_ogg_read(23);
    my @keys = qw/ version channels sample_rate bitrate_max bitrate_nom
        bitrate_min blocksize_0 blocksize_1 /;

    $values[7] = vec($values[7],0,1); # framing bit is first bit of last byte

    croak "Parse error: framing flag not set" if (pop @values != 1);

    # split and recalculate blocksizes ( two four bit exponents );
    my @blocksizes = map { 2 ** vec($values[6],$_,4) } 0..1;
    splice @values, 6, 1, @blocksizes;

    # generate the hash
    $self->{info} = { map {$keys[$_] => $values[$_]} 0..$#keys };

    # Identification header should always cover a single page (as per spec)
    croak "Identification packet does not end at page boundary"
        if ($self->{page_offset} != 0);

    #-------------------------------------------------------------------------
    # Comments header 
    #-------------------------------------------------------------------------

    $self->{headers}->{comment}->{file_offset} = tell $self->{fh};
    $self->{headers}->{comment}->{page_offset} = $self->{page_offset};
    $self->{headers}->{comment}->{page_len}    = $self->{page_len};

    # parse vorbis prefix and advance
    $type = $self->_parse_vorbis_prefix();
    croak "Not a comment header" if ($type ne HEADER_COMMENT);

    # unpack header data
    my $vendor_len       = unpack 'V',  $self->_ogg_read(4);
    $self->{vendor_name} = unpack "A*", $self->_ogg_read($vendor_len);
    my $comment_count    = unpack 'V',  $self->_ogg_read(4);
    
    # iterate through comments
    for (1..$comment_count) {
        my $comment_len = unpack 'V', $self->_ogg_read(4);

        # this is awkward, since key is always single byte chars but value
        # can have wide chars (so we assume the presence of wide)
        my $t = tell $self->{fh};
        my $comment
            = join '', (map {chr($_)} (unpack 'U*', $self->_ogg_read($comment_len)));
        my $diff = tell($self->{fh}) - $t;
        my ($key,$value,@other) = split '=', $comment; 
        croak "invalid comment" if (scalar(@other) > 0);
        croak "invalid field name $key" if ($key =~ /[^\x{20}-\x{7d}]/);
        $key = lc($key);
        $self->{comments}->{$key} = []
            if (! defined $self->{comments}->{$key});
        push @{ $self->{comments}->{$key} }, $value;
    }

    # verify correct framing bit
    my $framing_bit = vec( $self->_ogg_read(1) ,0 ,1);
    croak "bad framing bit at end of comment packet\n"
        if (! $framing_bit);

    #-------------------------------------------------------------------------
    # Setup header 
    #-------------------------------------------------------------------------

    $self->{headers}->{setup}->{file_offset} = tell $self->{fh};
    $self->{headers}->{setup}->{page_offset} = $self->{page_offset};
    $self->{headers}->{setup}->{page_len}    = $self->{page_len};

    # We don't actually parse the setup header (run away!) but we need to know
    # the length in order to re-write the Vorbis file if asked. This is done
    # by loading additional pages until at least four packet lengths are in
    # memory - the first three then should be complete and represent the
    # header packets

    while (scalar @{$self->{packet_lens}} < 4) {
        my $left_in_page = $self->{page_len} - $self->{page_offset};
        seek $self->{fh}, $left_in_page, 1;
        $self->_load_page();
    }
    
    $self->{last_header_page} = $self->{curr_page};

    $self->{headers}->{ident}->{length}   = $self->{packet_lens}->[0];
    $self->{headers}->{comment}->{length} = $self->{packet_lens}->[1];
    $self->{headers}->{setup}->{length}   = $self->{packet_lens}->[2];
    delete $self->{packet_lens};
    delete $self->{crc};

    $self->_calc_track_len();
    return;

}

sub _calc_track_len {

    my ($self) = @_;

    my $backset = min(
        $self->{info}->{blocksize_1}*2,
        $self->{file_size}
    );

    seek $self->{fh}, -$backset, 2;
    read($self->{fh}, my $chunk, $backset);

    my $gran = 0;
    while ($chunk =~ /OggS/g) {
        
        seek $self->{fh}, pos($chunk) - $backset - 4, 2;
        my ($flags, $g) = $self->_load_page();
            if ($flags & 0x04) {
                $gran = $g;
            }
            else {
                croak "EOF but not last page - corrupt file?\n";
            }
    }

    $self->{info}->{length} = $self->{info}->{sample_rate}
        ? $gran/$self->{info}->{sample_rate}
        : 0;

}


sub _load_page {

    my ($self) = @_;

    # check previous CRC if present
    if (ref($self->{crc})) {
        my $calc = $self->{crc}->digest;
        croak "CRC32 mismatch" if ($calc != $self->{crc_given});

    }

    # handle EOF situations
    if (eof $self->{fh}) {
        $self->{page_len} = 0;
        $self->{page_offset} = 0;
        $self->{crc} = undef;
        return;
    }

    if (defined $self->{crc}) {
        $self->{crc} = Digest::CRC->new(
            width  => 32,
            poly   => 0x04c11db7,
            init   => 0,
            xorout => 0,
            refin  => 0,
            refout => 0,
        );
    }

    my $t_start = tell $self->{fh};

    read $self->{fh}, my $buf, 27;
    my ($capture_pattern,
        $stream_struct_vers,
        $header_type_flag,
        $abs_gran_pos,
        $stream_SN,
        $page_num,
        $crc32,
        $num_segments
    )  = unpack 'A4CCQ<VVVC', $buf;

    croak "Invalid Ogg header"
        if ($capture_pattern ne OGG_CAPTURE);
    
    $self->{sn} = $stream_SN if (! defined $self->{sn});
    $self->{crc_given} = $crc32;

    # zero out CRC for digest check
    substr $buf, 22, 4, pack('C4', 0);
    $self->{crc}->add( $buf )
        if (ref($self->{crc}));

    read $self->{fh}, $buf, $num_segments;
    $self->{crc}->add( $buf )
        if (ref($self->{crc}));
    my @segment_sizes = unpack 'C*', $buf;
    $self->{page_len} = 0;
    for (0..$#segment_sizes) {
        my $l = $segment_sizes[$_];
        $self->{page_len}          += $l;
        $self->{packet_lens}->[-1] += $l
            if (defined $self->{packet_lens});

        # Vorbis spec says that segment lengths of 255 indicate that packet
        # continues in next segment. Anything < 255 indicates the last segment
        # of the current packet. If a packet happens to end with a segment of
        # exactly 255 bytes, an empty segment of length 0 is added to conform
        # to the specification

        push @{ $self->{packet_lens} }, 0 if ($l < 255);
    }

    $self->{page_header_len} = tell($self->{fh}) - $t_start;

    $self->{page_offset} = 0;
    #croak "pages out of order" if ($page_num != $self->{curr_page} + 1);
    $self->{curr_page} = $page_num;
    return ($header_type_flag, $abs_gran_pos);

}

sub _ogg_read {

    # reads a given number of bytes, checking return values and "turning"
    # Ogg pages as necessary

    my ($self, $bytes) = @_;

    my $remaining = $bytes;
    my $to_return = '';
    my $buf = '';

    while ($remaining > 0) {

        my $left_on_page = $self->{page_len} - $self->{page_offset};

        if ($remaining < $left_on_page) {
            my $r = read $self->{fh}, $buf, $remaining;
            $remaining -= $r;
            $self->{page_offset} += $r;
            $self->{crc}->add($buf)
                if (ref($self->{crc}));
            $to_return .= $buf;
        }
        # else rotate pages during read
        else {
            my $r = read $self->{fh}, $buf, $left_on_page;
            $remaining -= $r;
            $self->{page_offset} += $r;
            $to_return .= $buf;
            $self->{crc}->add($buf)
                if (ref($self->{crc}));
            $self->_load_page() if ($self->{page_offset} == $self->{page_len});
        }

    }

    return $to_return;

}

sub write_vorbis {

    my ($self, $fn) = @_;

    my $overwrite = 0;
    my $fh_out;

    if (! defined $fn) {
        my ($tmp_fh, $tmp_fn) = tempfile();
        binmode $tmp_fh;
        $fn     = $tmp_fn;
        $fh_out = $tmp_fh;
        $overwrite = 1;
    }

    else {
        open $fh_out, '>:raw', $fn;
    }

    # Steps for writing:

    # 1. Write the identification header as-is

    # 2. Repack the second and third headers, tracking page counts. The end of
    # the third header packet should be the end of the page.

    # 3. Output the rest of the file (audio packets), updating the page
    # numbering if necessary


    #-------------------------------------------------------------------------
    # Write ident header 
    #-------------------------------------------------------------------------

    # The identification header packet, per spec, takes up exactly one 58 bit
    # page. We'll just forward it verbatim as is since it's already been
    # validated earlier.

    open my $fh, '<:raw', $self->{fn};
    $self->{fh} = $fh;

    seek $self->{fh}, 0, 0;
    read $self->{fh}, my $head, IDENT_PAGE_LEN;
    print {$fh_out} $head;

    #-------------------------------------------------------------------------
    # Write comment and setup header 
    #-------------------------------------------------------------------------
    
    # The second and third headers are written together, as they are allowed
    # to span multiple pages. Combined, they must begin and end on page
    # boundaries.

    # In order to pack, we first generate and hold the actual header contents
    # in memory. Then we wrap them in Ogg pages and print.

    my $comment_packet = '';

    # pack prefix
    $comment_packet = pack 'CA*', 3, 'vorbis';

    # pack vendor name
    my $vendor_packed = pack "A*", $self->{vendor_name};
    $comment_packet .= pack "VA*",
        length($self->{vendor_name}), $self->{vendor_name};

    # pack comment count
    my $comment_count  = scalar map {@{ $self->{comments}->{$_} }} keys %{ $self->{comments} };
    $comment_packet .= pack 'V', $comment_count;

    # pack comments
    my @s = sort {$a cmp $b} keys %{ $self->{comments} };
    for my $key (@s) {
        my @sv = sort {$a cmp $b} @{ $self->{comments}->{$key}};
        for my $value (@sv) {
            $comment_packet .= pack 'V', length("$key=$value");
            $comment_packet .= "$key=$value";
        }
    }

    # pack framing bit
    $comment_packet .= pack 'C', 1;

    my $setup_packet = '';
    

    # Now read in setup header and add verbatim

    my $header_meta      = $self->{headers}->{setup};
    $self->{page_offset} = $header_meta->{page_offset};
    $self->{page_len}    = $header_meta->{page_len};
    seek $self->{fh}, $header_meta->{file_offset}, 0;

    $setup_packet .= $self->_ogg_read( $header_meta->{length} );

    seek $self->{fh}, -$self->{page_header_len}, 1;
    # Note that we should now be at the start of the audio stream

    # Now re-wrap the whole thing in Ogg page(s)

    my $curr_page = 1; # comment header always starts on page 1 (0-based)
    my $paginated = '';

    my $c_l = length($comment_packet);
    my $s_l = length($setup_packet);
    my @c_seg_lens = ( (MAX_SEG_LEN) x floor($c_l/MAX_SEG_LEN), $c_l % MAX_SEG_LEN);
    my @s_seg_lens = ( (MAX_SEG_LEN) x floor($s_l/MAX_SEG_LEN), $s_l % MAX_SEG_LEN);
    my @seg_lens = ( @c_seg_lens, @s_seg_lens );
    my $combined_packets = $comment_packet . $setup_packet;
    
    my $data_added = 0;
    my $flag = 0;
    while (scalar(@seg_lens) > 0) {
        my $segs_this_page = min( scalar(@seg_lens), MAX_SEGS_PER_PAGE );
        my @seg_lens_this_page = splice @seg_lens, 0, $segs_this_page;
        my $page = pack 'A4CCQ<VVVC*',
            'OggS',
            0,
            $flag,
            0,
            $self->{sn},
            $curr_page++,
            0,
            $segs_this_page,
            @seg_lens_this_page;

        my $data_len = sum @seg_lens_this_page;
        $page .= substr $combined_packets, 0, $data_len, '';
        $data_added += $data_len;
        $flag = $flag | 0x01
            if ($data_added != $self->{headers}->{comment}->{length});

        # added as much data as we will to this page

        # update CRC32 field
        my $d = Digest::CRC->new(
            width  => 32,
            poly   => 0x04c11db7,
            init   => 0,
            xorout => 0,
            refin  => 0,
            refout => 0,
        );
        my $crc32 = $d->add($page)->digest;
        substr $page, 22, 4, pack('V', $crc32);

        print {$fh_out} $page;

        
    }

    # now write audio data

    # if re-wrapping hasn't changed page count, just dump the rest of the file
    # verbatim
    if ($curr_page - 1 == $self->{last_header_page}) {
        my $buf = '';
        print {$fh_out} $buf while (read $self->{fh}, $buf, 4096);
    }
    # otherwise we have to re-write the page count of each page
    else {
        while ($self->_load_page()) {
            my $page = '';
            seek $self->{fh}, -$self->{page_header_len}, 1;
            my $h = '';
            read $self->{fh}, $h, $self->{page_header_len};
            substr $h, 18, 8, pack('VV',$curr_page++,0);
            $page .= $h;
            read $self->{fh}, my $s, $self->{page_len};
            $page .= $s;
            my $d = Digest::CRC->new(
                width  => 32,
                poly   => 0x04c11db7,
                init   => 0,
                xorout => 0,
                refin  => 0,
                refout => 0,
            );
            my $crc32 = $d->add($page)->digest;
            substr $page, 22, 4, pack('V',$crc32);
            print {$fh_out} $page;
        }
    }
    close $fh_out;
    close $self->{fh};
    $self->{fh} = undef;

    if ($overwrite) {
        move $fn => $self->{fn};
    }

}

sub _is_valid_value {

    my ($value) = @_;

    # Vorbis specs allow comment values to contain any UTF-8 character
    # with the exception of the field delimiter 0x3D (equals sign)
    return ($value =~ / \x{3D} /x) ? 0 : 1;

}

sub _is_valid_key {

    my ($key) = @_;

    # Vorbis specs allow comment keys to contain any ASCII character between
    # 0x20 and 0x7D, inclusive, with the exception of the field delimiter 0x3D
    # (equals sign)
    return ($key =~ m/(?: [^\x{20}-\x{7d}] | \x{3D} )/x) ? 0 : 1

}

1;

__END__

=head1 NAME

Ogg::Vorbis::Header::PurePerl - Read and write Ogg Vorbis metadata headers

=head1 SYNOPSIS

    use Ogg::Vorbis::Header::PurePerl

    my $ogg = Ogg::Vorbis::Header::PurePerl->new('song.ogg');


=head1 DESCRIPTION

This module provides read/write access to Ogg Vorbis header fields. It is
intended to be a drop-in pure-Perl replacement for Ogg::Vorbis::Header.

=head1 CONSTRUCTORS

=over 4

=item new ($filename)

Creates a class instance tied to the given filename. The only check that is
currently performed is that the filename given is available for reading.

=back

=head1 METHODS

=over 4

=item load_comments

Reads the comment data from the filenamed tied to the object.

=item load ([filename])

This method is included for backward compatiblity with Ogg::Vorbis::Header,
which allows it to be called as either a constructor or instance method. In
this implementation, when called as an instance method (no arguments allowed)
it is simply an alias for C<load_comments>. When called as a class method, it
both instantiates the object and loads the comment data. In other words,

    my $ogg = Ogg::Vorbis::Header::PurePerl->load('song.ogg')

is the equivalent of
    
    my $ogg = Ogg::Vorbis::Header::PurePerl->new('song.ogg')
    $ogg->load_comments();

and it's usage as a constructor is discouraged in new code due to the somewhat
unexpected syntax. A better one-liner would be

    my $ogg = Ogg::Vorbis::Header::PurePerl->new('song.ogg')->load_comments();

=item info ([$key])

Returns technical information about the Ogg Vorbis file from the information
header. Hash fields are:

    * version

    * channels

    * rate

    * bitrate_upper

    * bitrate_nominal

    * bitrate_lower

    * bitrate_window

    * length

If $key is provided, returns a scalar that value from the information header.
Otherwise returns a hashref containing all data fields. An error is thrown if
$key is invalid (this deviates from the behavior of Ogg::Vorbis::Header).

=back

=head1 DEPENDENCIES

=head1 CAVEATS AND BUGS

Please report bugs to the author.

=head1 AUTHORS

Jeremy Volkening <jdv@base2bio.com>

=head1 COPYRIGHT AND LICENSE

Copyright 2015-2016 Jeremy Volkening

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.

=cut
