\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    
    \override TextSpanner.bound-details.left.text = "accel."
    c16 \startTextSpan d16 e16 f16
    g16 f16 e16 d16 \stopTextSpan
    \override TextSpanner.bound-details.left.text = "rit."
    c16 \startTextSpan d16 e16 f16
    g16 f16 e16 d16 \stopTextSpan
    \override TextSpanner.bound-details.left.text = "accel."
    c16 \startTextSpan d16 e16 f16 \stopTextSpan
    \override TextSpanner.bound-details.left.text = "rit."
    g16 \startTextSpan f16 e16 d16 \stopTextSpan
    c4 g'4
    \override TextSpanner.bound-details.left.text = "accel."
    c,16 \startTextSpan d16 e16 f16
    g16 f16 e16 d16 \stopTextSpan
    \override TextSpanner.bound-details.left.text = "rit."
    c16 \startTextSpan d16 e16 f16
    g16 f16 e16 d16 \stopTextSpan
    \override TextSpanner.bound-details.left.text = "accel."
    c16 \startTextSpan d16 e16 f16 \stopTextSpan
    \override TextSpanner.bound-details.left.text = "rit."
    g16 \startTextSpan f16 e16 d16 \stopTextSpan
    c2 \bar "|."

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