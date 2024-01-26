use libdisplay::*;

pub fn main() {
    const IDLE_EXPRESSION: EpsilonExpressionSet =
        EpsilonExpressionSet::from_sections(parse_bmp!("src/test-std.bmp").split_to_sections());
    println!("{:#?}", IDLE_EXPRESSION);
    println!("{}", IDLE_EXPRESSION);
}
