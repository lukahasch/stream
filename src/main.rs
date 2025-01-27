use stream::parser::lexer::Loader;
use stream::parser::parse;

fn main() {
    let mut loader = Loader::new();
    let file_name = "example.stream";
    let content = loader.load(file_name).unwrap();
    let parsed = parse(&content, file_name.into());
    match parsed {
        Ok(tokens) => {
            println!("{:#?}", tokens);
        }
        Err(errors) => {
            for error in errors {
                error.report().eprint(loader.cache()).unwrap()
            }
        }
    }
}
