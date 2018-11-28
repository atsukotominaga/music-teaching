\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    \tempo "Andante" 4 = 80
    
    c16\< d16 e16 f16\! g16\> f16 e16 d16\!
    c16\< d16 e16 f16\! g16\> f16 e16 d16\!
    c16\< d16 e16 f16\! g16\> f16 e16 d16\!
    c4 g'4
    c,16\< d16 e16 f16\! g16\> f16 e16 d16\!
    c16\< d16 e16 f16\! g16\> f16 e16 d16\!
    c16\< d16 e16 f16\! g16\> f16 e16 d16\!
    c2

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