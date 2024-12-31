use ariadne::sources;
use chumsky::prelude::*;
use std::sync::Arc;
use stream::parser::{parser, stream};

fn main() {
    let code = r#"if 0 then () else []"#;
    let origin = Arc::from("main");
    let result = parser().parse(stream(&origin, code.to_string()));
    let mut sources = sources(vec![(origin, code)]);
    match result {
        Ok(node) => println!("{:?}", node),
        Err(err) => {
            for e in err.iter() {
                e.report().eprint(&mut sources).unwrap();
            }
        }
    }
}
