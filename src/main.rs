use ariadne::sources;
use chumsky::prelude::*;
use std::sync::Arc;
use stream::parser::{Origin, parser, stream};

fn main() {
    let mut origin = Origin::new();
    let result = parser().parse(origin.load("example.stream").unwrap());
    match result {
        Ok(node) => println!("{:?}", node),
        Err(err) => {
            for e in err.iter() {
                e.report().eprint(origin.cache()).unwrap();
            }
        }
    }
}
