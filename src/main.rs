#![feature(const_mut_refs)] // for Color and Pixel impls
#![feature(stmt_expr_attributes)] // for include lints
#![feature(generic_arg_infer)] // for proc-macro-ish bmp parsing
#![allow(dead_code, unused)]
#![feature(const_option)] // for const pixel diffs
#![feature(const_trait_impl)] // for const ToDisplayDiff impls

use core::fmt;
use include_bmp::get_bmp;
use std::mem;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl Color {
    pub const fn new() -> Self {
        Self { r: 0u8, g: 0u8, b: 0u8 }
    }

    pub const fn from_hex(hex: u32) -> Self {
        Self {
            r: (hex >> 16) as u8,
            g: (hex >> 8) as u8,
            b: hex as u8,
        }
    }

    pub const fn as_hex(&self) -> u32 {
        let output: u32 = (self.r as u32) << 16 | (self.g as u32) << 8 | self.b as u32;
        debug_assert!(output <= (2u32.pow(24) - 1));
        output
    }

    pub const fn set(&mut self, c2: Color) {
        *self = c2;
    }

    pub const fn set_hex(&mut self, c2: u32) {
        *self = Self::from_hex(c2);
    }

    pub const fn into_pixel(self) -> Pixel {
        Pixel(self)
    }

    pub const fn channel_deltas(&self, c2: &Self) -> Color {
        // where r is delta r, etc.
        let r: u8 = c2.r.abs_diff(self.r);
        let g: u8 = c2.g.abs_diff(self.g);
        let b: u8 = c2.b.abs_diff(self.b);
        Color { r, g, b }
    }

    pub const fn distance_to(&self, c2: &Self) -> u32 {
        let deltas: Color = self.channel_deltas(c2);
        let dr = (deltas.r as u32).pow(2);
        let dg = (deltas.g as u32).pow(2);
        let db = (deltas.b as u32).pow(2);
        let distance: u32 = dr + dg + db;
        if distance > ((u8::MAX as u32).pow(2) * 3) {
            panic!("got a value larger than should be possible");
        } else {
            distance
        }
    }

    pub const fn closest_terminal_color(&self) -> u8 {
        let mut i: usize = 0;
        let mut best: (u32, u8) = (u32::MAX, 0u8); // distance, color
                                                   // using [u32::MAX] here is a good placeholder since any value
                                                   // above `257^2 * 3` *shouldn't* be possible

        while i < TERMINAL_COLORS.len() {
            let c2: Color = Color::from_hex(TERMINAL_COLORS[i]);
            let diff: u32 = self.distance_to(&c2);
            if diff < best.0 {
                best.0 = diff;
                best.1 = i as u8;
            }
            i += 1;
        }
        best.1
    }

    pub const fn clamp(&mut self) {
        *self = Self::from_hex(TERMINAL_COLORS[self.closest_terminal_color() as usize]);
    }
}

impl core::fmt::UpperHex for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:08X}", self.as_hex())
    }
}

impl core::fmt::Debug for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Pixel {{ {:08X} }}", self)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pixel(Color);

impl Pixel {
    const BLANK_PIXEL_HEX: u32 = 0u32;

    pub const fn new() -> Self {
        Self(Color::from_hex(Self::BLANK_PIXEL_HEX))
    }

    pub const fn set_hex(&mut self, new_color: u32) {
        *self = Color::from_hex(new_color).into_pixel();
    }

    pub const fn clear(&mut self) {
        *self = Color::from_hex(Self::BLANK_PIXEL_HEX).into_pixel();
    }

    pub const fn value_hex(&self) -> u32 {
        self.0.as_hex()
    }

    pub const fn value_color(&self) -> Color {
        self.0
    }

    pub const fn from_hex(hex: u32) -> Self {
        Self(Color::from_hex(hex))
    }

    pub const fn from_color(color: Color) -> Self {
        Self(color)
    }

    pub const fn set_color(&mut self, new: Color) {
        self.0 = new;
    }

    const fn diff(&mut self, diff: SDiff) {
        match diff {
            SDiff::Change(x) => self.set_color(x.new),
            SDiff::Shift(x) => {
                let cshft = x.color_diff;
                match x.direction {
                    true => {
                        self.0.r = u8::saturating_add(self.0.r, cshft.r);
                        self.0.g = u8::saturating_add(self.0.g, cshft.g);
                        self.0.b = u8::saturating_sub(self.0.b, cshft.b);
                    }
                    false => {
                        self.0.r = u8::saturating_sub(self.0.r, cshft.r);
                        self.0.g = u8::saturating_sub(self.0.g, cshft.g);
                        self.0.b = u8::saturating_sub(self.0.b, cshft.b);
                    }
                }
            }
        }
    }
}

impl From<[u8; 3]> for Pixel {
    fn from(value: [u8; 3]) -> Self {
        Self(Color {
            r: value[0],
            g: value[1],
            b: value[2],
        })
    }
}

impl From<u32> for Pixel {
    fn from(value: u32) -> Self {
        Self::from_hex(value)
    }
}

impl Into<u32> for Pixel {
    fn into(self) -> u32 {
        self.value_hex()
    }
}

const PIXEL_WIDTH: usize = 3;

#[cfg(not(feature = "24bpp"))]
impl fmt::Display for Pixel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\x1b[48;5;{}m{}\x1b[49m",
            self.0.closest_terminal_color(),
            " ".repeat(PIXEL_WIDTH)
        )
    }
}

#[cfg(feature = "24bpp")]
impl fmt::Display for Pixel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (r, g, b) = (self.0.r, self.0.g, self.0.b);
        write!(
            f,
            "\x1b[48;2;{:};{:};{:}m{}\x1b[49m",
            r,
            g,
            b,
            " ".repeat(PIXEL_WIDTH)
        )
    }
}

impl core::fmt::UpperHex for Pixel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:08X}", self.value_hex())
    }
}

impl core::fmt::Debug for Pixel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Pixel {{ {:08X} }}", self)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct DisplayRow<const W: usize>([Pixel; W]);

impl<const W: usize> DisplayRow<W> {
    pub const fn new() -> DisplayRow<W> {
        DisplayRow([Pixel::new(); W])
    }
}

impl<const W: usize> fmt::Display for DisplayRow<W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in &self.0 {
            // TODO: uhh... why twice?
            write!(f, "{}", i)?;
            write!(f, "{}", i)?;
        }
        Ok(())
    }
}

impl<const W: usize> core::fmt::Debug for DisplayRow<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for x in self.0 {
            write!(f, "{:08X}", x)?
        }
        Ok(())
    }
}

pub struct DisplayImage<const W: usize, const H: usize>([DisplayRow<W>; H]);

impl<const W: usize, const H: usize> DisplayImage<W, H> {
    pub const fn get_pixel(&mut self, pos: PixelPos) -> &mut Pixel {
        let (x, y) = pos;
        let x = x as usize;
        let y = y as usize;

        //debug_assert!(x < W, "attempted out-of-array access! tried to get x idx {:} of array of len {:}", x, W);
        //debug_assert!(y < H, "attempted out-of-array access! tried to get y idx {:} of array of len {:}", y, H);
        #[cfg(not(debug_assertions))]
        {
            if (x >= W as u32) || (y >= H as u32) {
                unreachable!()
            }
        }

        &mut self.0[y].0[x]
        //todo!()
    }

    pub const fn set_color(&mut self, pos: PixelPos, new: Color) {
        let x: &mut Pixel = self.get_pixel(pos);
        x.set_color(new);
    }

    pub const fn parse_diff(&mut self, diff: DisplayDiff) {
        match diff {
            DisplayDiff::Spot(spot) => self.get_pixel(spot.pos).diff(spot.kind),
            DisplayDiff::All(all) => {
                let mut a: usize = 0;
                let mut b: usize = 0;
                let mut c: usize = 0;

                while a < self.0.len() {
                    let row = &mut self.0[a];
                    while b < row.0.len() {
                        let pixel = &mut row.0[b];
                        pixel.diff(all.kind.to_sdiff());
                        b += 1;
                    }
                    a += 1;
                }
            }
        }
    }
}

impl<const W: usize, const H: usize> fmt::Display for DisplayImage<W, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in self.0 {
            for pixel in row.0 {
                write!(f, "{}", pixel)?
            }
            writeln!(f, "")?
        }
        Ok(())
    }
}

macro_rules! parse_bmp {
    ($path:literal) => {
        #[allow(dead_code)]
        {
            type DI<const W: usize, const H: usize> = DisplayImage<W, H>;
            type P = Pixel;
            type C = Color;
            type DR<const W: usize> = DisplayRow<W>;
            const fn dr<const W: usize>(input: [Pixel; W]) -> DisplayRow<W> {
                DisplayRow(input)
            }

            const fn ph(input: u32) -> Pixel { // essentially a type alias
                Pixel::from_hex(input)
            }

            const fn di<const W: usize, const H: usize>(input: [DisplayRow<W>; H]) -> DisplayImage<W, H> {
                DisplayImage(input)
            }

            Box::new(get_bmp!($path))
            // TODO: move to built-in parsing (with a proc macro?) to not rely on python script
            // probably use std::IO (since proc macro can use host features) to build the struct
            // would still be an unhygenic expansion and increase compile time, but this is likely
            // the most reasonable way to get it done (the only downside is at compile time afaict)
            // could also just use include_bytes!()? will need to see how this can be done at compile time
        }
    }
}

type DisplayAxisUnit = u32;
type PixelPos = (DisplayAxisUnit, DisplayAxisUnit);

trait ToDisplayDiff {
    fn to_fqsdiff(&self, pos: PixelPos) -> DisplayDiff;
    fn to_fqadiff(&self) -> DisplayDiff;
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Change {
    new: Color,
}

impl const ToDisplayDiff for Change {
    fn to_fqsdiff(&self, pos: PixelPos) -> DisplayDiff {
        DisplayDiff::Spot(SpotDiff{kind: SDiff::Change(*self), pos})
    }

    fn to_fqadiff(&self) -> DisplayDiff {
        DisplayDiff::All(AllDiff{kind: ADiff::Change(*self)})
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Shift {
    color_diff: Color,
    direction: bool,
}

impl const ToDisplayDiff for Shift {
    fn to_fqsdiff(&self, pos: PixelPos) -> DisplayDiff {
        DisplayDiff::Spot(SpotDiff{kind: SDiff::Shift(*self), pos})
    }

    fn to_fqadiff(&self) -> DisplayDiff {
        DisplayDiff::All(AllDiff{kind: ADiff::Shift(*self)})
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SDiff {
    Change(Change),
    Shift(Shift),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ADiff {
    Change(Change),
    Shift(Shift),
}

impl ADiff {
    pub const fn to_sdiff(&self) -> SDiff {
        match self {
            Self::Change(x) => SDiff::Change(*x),
            Self::Shift(x) => SDiff::Shift(*x),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SpotDiff {
    kind: SDiff,
    pos: PixelPos,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct AllDiff {
    kind: ADiff,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DisplayDiff {
    Spot(SpotDiff),
    All(AllDiff),
}

impl DisplayDiff {
    pub const fn to_spot(&self, pos: PixelPos) -> Self {
        match self {
            Self::Spot(SpotDiff { kind: x, pos: _ }) => Self::Spot(SpotDiff { kind: *x, pos }),
            Self::All((AllDiff { kind: x })) => Self::Spot(SpotDiff {
                kind: x.to_sdiff(),
                pos,
            }),
        }
    }
    pub const fn target(&self) -> Option<&PixelPos> {
        match self {
            Self::Spot(SpotDiff { kind: _, pos }) => Some(pos),
            Self::All(_) => None,
        }
    }
}

impl fmt::Debug for DisplayDiff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::All(x) => {
                write!(f, "entire-display ")?;
                match x.kind {
                    ADiff::Change(c) => writeln!(f, "change to {:X}", c.new)?,
                    ADiff::Shift(s) => {
                        let dir: &str = match s.direction {
                            true => "+",
                            false => "-",
                        };

                        writeln!(f, "shift by {}{:X}", dir, s.color_diff)?
                    }
                }
            }

            Self::Spot(x) => {
                write!(f, "pixel ({:}, {:}) ", x.pos.0, x.pos.1)?;
                match x.kind {
                    SDiff::Change(c) => writeln!(f, "change to {:X}", c.new)?,
                    SDiff::Shift(s) => {
                        let dir: &str = match s.direction {
                            true => "+",
                            false => "-",
                        };

                        writeln!(f, "shift by {}{:X}", dir, s.color_diff)?
                    }
                }
            }
        }
        Ok(())
    }
}


macro_rules! diff { 
    ($kind:expr) => {
        $kind.to_fqadiff()
    };
    ($kind:expr, $pos:expr) => {
        $kind.to_fqsdiff($pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_HEX: u32 = 0x1eb3ab;
    const TEST_COLOR: Color = Color::from_hex(TEST_HEX);
    //const TEST_PIXEL: Pixel = Pixel::from_hex(TEST_HEX);
    const BLACK_HEX: u32 = 0u32;
    const BLACK_COLOR: Color = Color::from_hex(BLACK_HEX);
    //const BLACK_PIXEL: Pixel = Pixel(BLACK_COLOR);

    #[test]
    fn color_hex_conv() {
        assert_eq!(TEST_COLOR.as_hex(), TEST_HEX);
        assert_eq!(BLACK_COLOR.as_hex(), BLACK_HEX);
        assert_eq!((TEST_COLOR.r, TEST_COLOR.g, TEST_COLOR.b), (30, 179, 171));
    }

    #[test]
    fn color_clamping() {
        let mut terminal_pixels: DisplayRow<256> = DisplayRow([Pixel::new(); 256]);
        for i in 0..TERMINAL_COLORS.len() {
            terminal_pixels.0[i] = Pixel::from_hex(TERMINAL_COLORS[i])
        }
        let mut terminal_pixels_clamped: DisplayRow<256> = terminal_pixels;
        for i in 0..terminal_pixels_clamped.0.len() {
            terminal_pixels_clamped.0[i].0.clamp();
        }
        assert_eq!(terminal_pixels, terminal_pixels_clamped);
    }

    #[test]
    fn diff_macro() {
        let sdiff1: DisplayDiff = DisplayDiff::Spot(SpotDiff{kind: SDiff::Change(Change{new: Color::new()}), pos: (0, 0)});
        let sdiff2: DisplayDiff = diff!(Change{new: Color::new()}, (0, 0));
        let sdiff3: DisplayDiff = diff!(Change{new: Color::new()}, (0, 1));
        let adiff1: DisplayDiff = DisplayDiff::All(AllDiff{kind: ADiff::Change(Change{new: Color::new()})});
        let adiff2: DisplayDiff = diff!(Change{new: Color::new()});
        let adiff3: DisplayDiff = diff!(Change{new: Color::from_hex(0xFFu32)});
        assert_eq!(sdiff1, sdiff2);
        assert_ne!(sdiff1, sdiff3);
        assert_eq!(adiff1, adiff2);
        assert_ne!(adiff1, adiff3);
    }
}

const TERMINAL_COLORS: [u32; 256] = [
    0x000000, 0x800000, 0x008000, 0x808000, 0x000080, 0x800080, 0x008080, 0xc0c0c0, 0x808080,
    0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff, 0x00ffff, 0xffffff, 0x000000, 0x00005f,
    0x000087, 0x0000af, 0x0000d7, 0x0000ff, 0x005f00, 0x005f5f, 0x005f87, 0x005faf, 0x005fd7,
    0x005fff, 0x008700, 0x00875f, 0x008787, 0x0087af, 0x0087d7, 0x0087ff, 0x00af00, 0x00af5f,
    0x00af87, 0x00afaf, 0x00afd7, 0x00afff, 0x00d700, 0x00d75f, 0x00d787, 0x00d7af, 0x00d7d7,
    0x00d7ff, 0x00ff00, 0x00ff5f, 0x00ff87, 0x00ffaf, 0x00ffd7, 0x00ffff, 0x5f0000, 0x5f005f,
    0x5f0087, 0x5f00af, 0x5f00d7, 0x5f00ff, 0x5f5f00, 0x5f5f5f, 0x5f5f87, 0x5f5faf, 0x5f5fd7,
    0x5f5fff, 0x5f8700, 0x5f875f, 0x5f8787, 0x5f87af, 0x5f87d7, 0x5f87ff, 0x5faf00, 0x5faf5f,
    0x5faf87, 0x5fafaf, 0x5fafd7, 0x5fafff, 0x5fd700, 0x5fd75f, 0x5fd787, 0x5fd7af, 0x5fd7d7,
    0x5fd7ff, 0x5fff00, 0x5fff5f, 0x5fff87, 0x5fffaf, 0x5fffd7, 0x5fffff, 0x870000, 0x87005f,
    0x870087, 0x8700af, 0x8700d7, 0x8700ff, 0x875f00, 0x875f5f, 0x875f87, 0x875faf, 0x875fd7,
    0x875fff, 0x878700, 0x87875f, 0x878787, 0x8787af, 0x8787d7, 0x8787ff, 0x87af00, 0x87af5f,
    0x87af87, 0x87afaf, 0x87afd7, 0x87afff, 0x87d700, 0x87d75f, 0x87d787, 0x87d7af, 0x87d7d7,
    0x87d7ff, 0x87ff00, 0x87ff5f, 0x87ff87, 0x87ffaf, 0x87ffd7, 0x87ffff, 0xaf0000, 0xaf005f,
    0xaf0087, 0xaf00af, 0xaf00d7, 0xaf00ff, 0xaf5f00, 0xaf5f5f, 0xaf5f87, 0xaf5faf, 0xaf5fd7,
    0xaf5fff, 0xaf8700, 0xaf875f, 0xaf8787, 0xaf87af, 0xaf87d7, 0xaf87ff, 0xafaf00, 0xafaf5f,
    0xafaf87, 0xafafaf, 0xafafd7, 0xafafff, 0xafd700, 0xafd75f, 0xafd787, 0xafd7af, 0xafd7d7,
    0xafd7ff, 0xafff00, 0xafff5f, 0xafff87, 0xafffaf, 0xafffd7, 0xafffff, 0xd70000, 0xd7005f,
    0xd70087, 0xd700af, 0xd700d7, 0xd700ff, 0xd75f00, 0xd75f5f, 0xd75f87, 0xd75faf, 0xd75fd7,
    0xd75fff, 0xd78700, 0xd7875f, 0xd78787, 0xd787af, 0xd787d7, 0xd787ff, 0xd7af00, 0xd7af5f,
    0xd7af87, 0xd7afaf, 0xd7afd7, 0xd7afff, 0xd7d700, 0xd7d75f, 0xd7d787, 0xd7d7af, 0xd7d7d7,
    0xd7d7ff, 0xd7ff00, 0xd7ff5f, 0xd7ff87, 0xd7ffaf, 0xd7ffd7, 0xd7ffff, 0xff0000, 0xff005f,
    0xff0087, 0xff00af, 0xff00d7, 0xff00ff, 0xff5f00, 0xff5f5f, 0xff5f87, 0xff5faf, 0xff5fd7,
    0xff5fff, 0xff8700, 0xff875f, 0xff8787, 0xff87af, 0xff87d7, 0xff87ff, 0xffaf00, 0xffaf5f,
    0xffaf87, 0xffafaf, 0xffafd7, 0xffafff, 0xffd700, 0xffd75f, 0xffd787, 0xffd7af, 0xffd7d7,
    0xffd7ff, 0xffff00, 0xffff5f, 0xffff87, 0xffffaf, 0xffffd7, 0xffffff, 0x080808, 0x121212,
    0x1c1c1c, 0x262626, 0x303030, 0x3a3a3a, 0x444444, 0x4e4e4e, 0x585858, 0x606060, 0x666666,
    0x767676, 0x808080, 0x8a8a8a, 0x949494, 0x9e9e9e, 0xa8a8a8, 0xb2b2b2, 0xbcbcbc, 0xc6c6c6,
    0xd0d0d0, 0xdadada, 0xe4e4e4, 0xeeeeee,
];

pub fn main() {
    let mut q = parse_bmp!("src/image.bmp");
    let x = q.get_pixel((1, 1));
    x.clear();
    println!("{}", q);
    q.set_color((2, 2), Pixel::new().value_color());
    println!("{}", q);

    dbg!(mem::size_of::<DisplayDiff>());
    enum G {
        A,
        B,
    }
    let testdiff: DisplayDiff = DisplayDiff::Spot(SpotDiff{kind: SDiff::Change(Change{new: Color::new()}), pos: (0, 0)});
    let testdiff2: DisplayDiff = diff!(Change{new: Color::new()}, (1, 0));
    dbg!(&testdiff);
    dbg!(&testdiff2);
    q.parse_diff(testdiff);
    q.parse_diff(testdiff2);
    println!("{}", q);
    
}
