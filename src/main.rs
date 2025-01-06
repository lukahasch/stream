use stream::parser::lexer::lex;
use stream::parser::parse;

fn main() {
    let file_name = "example.stream";
    let content = std::fs::read_to_string(file_name).unwrap();
    let parsed = parse(&content, file_name.into()).unwrap();
    println!("{:#?}", parsed);
}
