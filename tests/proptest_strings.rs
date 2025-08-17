use proptest::prelude::*;
use sentience_tokenize::{tokenize, TokenKind};

proptest! {
    // Generiši stringove bez navodnika i backslash-a, pa proveri round-trip.
    #[test]
    fn string_round_trip_without_escapes(s in "[[:print:][:space:]]{0,64}") {
        let clean: String = s.chars().filter(|&ch| ch != '\"' && ch != '\\').collect();
        let src = format!("\"{}\"", clean);
        let toks = tokenize(&src).unwrap();
        prop_assert!(matches!(toks[0].kind, TokenKind::String(ref v) if v == &clean));
    }
}
