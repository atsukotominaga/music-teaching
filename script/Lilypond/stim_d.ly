\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    
    g''8\f fis8 g8 e8 c8 g8-.\p g8-. g8-.
    g'8\f fis8 g8 e8 c8 e8 g8 c8
    b8 a8-.\p a8-. a8-. b8\f a8 c8 a8
    fis8 g8 g4 g4 r4
    f8-.\p f8-. e8-. e8-. d4 r4
    g4 f4 e8-. e8-. e8-. e8-.
    a8\f f8 e8 d8 e8 g8 d8 b8
    c8-.\p c8-. d8-. d8-. e4 r4
    f,8-.\p f8-. e8-. e8-. d4 r4
    g8-. g8-. b8-. b8-. c8-. c8-. cis8-. cis8-.
    d4\f f4 e4 d4
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