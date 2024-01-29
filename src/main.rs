use libdisplay::*;

const EPSILON_SECTIONS: usize = 8;
type EpsilonExpression = Expression<EPSILON_SECTIONS>;

pub fn main() {
    const IDLE_EXPRESSION: EpsilonExpression =
        EpsilonExpression::from_defined(&use_expr!("src/test-std.bmp"));

    println!("{:#?}", IDLE_EXPRESSION);
    println!("{}", IDLE_EXPRESSION);
}
