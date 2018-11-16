\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    \tempo "Andante" 4 = 80
    
    g'16 d16 e16 b16 r16 d16 e16 b16
    g'16 d16 e16 b16 r16 d16 e16 b16
    g'16 d16 e16 cis16 r16 d16 e16 a,16
    g'16 d16 e16 cis16 r16 d16 e16 a,16
    fis'16 ais,16 e'16 cis16 d16 b16 e16 ais,16
    fis'16 ais,16 e'16 cis16 d16 b16 e16 ais,16
    a16 fis'16 b,16 fis'16 g16 e16 b16 g16
    a16 fis'16 b,16 fis'16 g16 e16 b16 g16
    gis16 a16 b16 f'16 e16 d16 bes16 a16
    gis16 a16 b16 f'16 e16 d16 bes16 a16
    aes16 cis16 g16 d'16 gis,16 dis'16 g,16 e'16
    aes,16 cis16 g16 d'16 gis,16 dis'16 g,16 e'16
    r16 e16 dis16 e16 f16 b,16 c16 d16
    r16 e16 dis16 e16 f16 b,16 c16 d16
    e16 bes16 e8 e4
    e16 bes16 e8 e4
    
    }
}

% Export melody to pdf and midi files

\score{
    \melody
    \layout {
        \context {
        \Score
        \omit BarNumber }
    indent = #0 }
    \midi {}
    }