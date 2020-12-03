\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    
    g''8-5 (fis8-4 g8-5 e8-3 c8-1) g8-3-. g8-2-. g8-1-.
    g'8-5 (fis8-4 g8-5 e8-3 c8-1 e8-2 g8-3 c8-5
    b8-4) a8-3-. a8-2-. a8-1-. b8-2 (a8-1 c8-2 a8-1
    fis8-2) g8-3 g4-2 g4-1 r4
    f8-4-. f8-3-. e8-2-. e8-1-. d4-3 r4
    g4-2 f4-1 e8-4-. e8-3-. e8-2-. e8-1-.
    a8-5 (f8-3 e8-2 d8-1 e8-3 g8-5 d8-2 b8-1)
    c8-2-. c8-1-. d8-3-. d8-1-. e4-3 r4
    f,8-4-. f8-3-. e8-2-. e8-1-. d4-3 r4
    g8-3-. g8-1-. b8-3-. b8-1-. c8-3-. c8-1-. cis8-3-. cis8-1-.
    d4-2 f4-4 e4-3 d4-2
    c4-1 c'4-5 c4-5 r4
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