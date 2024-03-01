#![allow(dead_code)]
#![allow(unused_imports)]

use display::*;

const EPSILON_SECTIONS: usize = 8;
// type EpsilonExpression = Expression<EPSILON_SECTIONS>;

pub fn main() {
    const X: DisplayImage<64, 32> = parse_bmp!("/workspaces/simple_display/src/test-std.bmp");
    let modification: ShiftDiff = ShiftDiff {
        shift: 3,
        wrapping: false,
    };
    dbg!(std::mem::size_of_val(&modification));
    let joined: Expression<ShiftDiff, 64, 32> = Expression {
        image: &X,
        modification: Some(modification),
    };
    dbg!(std::mem::size_of_val(&joined));
}
