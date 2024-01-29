use libdisplay::*;

const EPSILON_SECTIONS: usize = 8;
pub type EpsilonExpressionSet = ExpressionSet<EPSILON_SECTIONS>;
pub type EpsilonExpression = Expression<EPSILON_SECTIONS>;

pub fn main() {
    const IDLE_EXPRESSION_SET: EpsilonExpressionSet =
        EpsilonExpressionSet::from_sections(parse_bmp!("src/test-std.bmp").split_to_sections());

    const IDLE_EXPRESSION: EpsilonExpression =
        EpsilonExpression::from_defined(&use_expr!("src/test-std.bmp"));

    println!("{:#?}", IDLE_EXPRESSION);
    println!("{}", IDLE_EXPRESSION);
    println!("{:#?}", IDLE_EXPRESSION_SET);
    println!("{}", IDLE_EXPRESSION_SET);
}
