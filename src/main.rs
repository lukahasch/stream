use stream::parser::{complex, spanned::Spanned};

fn main() {
    dbg!(complex(Spanned::new("test", "\"test\".len(10 + 10)")));
}
