\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    
    c16\f d16 e16 f16 g16 f16 e16 d16
    c16\p d16 e16 f16 g16 f16 e16 d16
    c16\f d16 e16 f16 g16 f16 e16 d16
    c16\p d16 e16 f16 g16 f16 e16 d16
    c1
    g'16\p f16 e16 d16 c16 d16 e16 f16
    g16\f f16 e16 d16 c16 d16 e16 f16
    g16\p f16 e16 d16 c16 d16 e16 f16
    g16\f f16 e16 d16 c16 d16 e16 f16
    g2 c,2 \bar "|."

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
    % Remove % if you need a midi file
    % \midi {}
    }