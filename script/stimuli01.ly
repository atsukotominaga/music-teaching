\version "2.18.2"

melody = {
    \relative c' {
    \clef treble
    \time 4/4
    \tempo "Andente" 4 = 80
    
    \tuplet 3/2 { fis'8 a,8 a8 }
    \tuplet 3/2 { e'8 g,8 g8 }
    \tuplet 3/2 { fis'8 a,8 a8 }
    \tuplet 3/2 { e'8 g,8 g8 }
    \tuplet 3/2 { a8 fis8 fis8  }
    \tuplet 3/2 { b8 a8 b8 }
    \tuplet 3/2 { a8 fis8 fis8  }
    \tuplet 3/2 { b8 a8 b8 }
    }\break
    
    \relative c' {
    \tuplet 3/2 { cis'8 bes8 bes8 }
    \tuplet 3/2 { d8 a8 a8 }
    \tuplet 3/2 { cis8 bes8 bes8 }
    \tuplet 3/2 { d8 a8 a8 }
    \tuplet 3/2 { e'8 gis,8 gis8 }
    fis'4
    \tuplet 3/2 { e8 gis,8 gis8 }
    fis'4
    }\break
    
    \relative c' {
    \tuplet 3/2 { e'8 fis8 e8 }
    \tuplet 3/2 { c8 a8 a8 }
    \tuplet 3/2 { e'8 fis8 e8 }
    \tuplet 3/2 { c8 a8 a8 }
    \tuplet 3/2 { c8 aes8 aes8 }
    \tuplet 3/2 { c8 g8 g8 }
    \tuplet 3/2 { c8 aes8 aes8 }
    \tuplet 3/2 { c8 g8 g8 }
    }\break
    
    \relative c' {
    \tuplet 3/2 { fis8 g8 a8 }
    \tuplet 3/2 { b8 fis8 fis'8 }
    \tuplet 3/2 { fis,8 g8 a8 }
    \tuplet 3/2 { b8 fis8 fis'8 }
    \tuplet 3/2 { e8 e8 e8}
    e4
    \tuplet 3/2 { e8 e8 e8 }
    e4
    }\break
    }

\score{
    \new Staff \melody
    \layout {}
    \midi {}
    }
