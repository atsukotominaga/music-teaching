\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    
    g''8 fis8 g8 e8 c g8 g8 g8
    g'8 fis8 g8 e8 c8 e8 g8 c8
    b8 a8 a8 a8 b8 a8 c8 a8
    fis8 g8 g8 g8 g4 r4
    f8 f8 e8 e8 d8 d8 d8 d8
    g8 g8 f8 f8 e8 e8 e8 e8
    a8 f8 e8 d8 e8 g8 d8 b8
    c8 c8 d8 d8 e8 e8 e8 e8
    f,8 f8 e8 e8 d8 d8 d8 d8
    g8 g8 b8 b8 c8 c8 cis8 cis8
    d8 d8 f8 f8 e8 e8 d8 d8
    c4 c'4 c4 r4
    \bar "|."

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