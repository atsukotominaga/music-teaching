\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    
    g''4. (e8 c8) g8-. g8-. g8-.
    g'4. (e8 c16 d16 e16 f16 g16 a16 b16 c16
    b8) a8-. a8-. a8-. b16 (a16 gis16 a16 c8-.) a8-. 
    fis8 (g8-.) g8-. g8-. g4 r4
    f8-. f8-. e8-. e8-. d4 r4
    g8-. g8-. f8-. f8-. e4 r4
    a16 (f16 e16 d16 a'16 f16 e16 d16
    e16 g16 e16 c16 d16 f16 d16 b16
    c8-.) c8-. d8-. d8-. e4 r4
    f,8-. f8-. e8-. e8-. d4 r4
    g8-. g8-. b8-. b8-. c8-. c8-. cis8-. cis8-.
    d8-. d8-. f8-. f8-. e8-. e8-. d8-. d8-.
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