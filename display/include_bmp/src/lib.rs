extern crate proc_macro;
use proc_macro::TokenStream;
use rustbitmap::BitMap;
use syn::{parse_macro_input, LitStr};

#[proc_macro]
pub fn get_bmp(input: TokenStream) -> TokenStream {
    let path = parse_macro_input!(input as LitStr);
    let bmp = Box::new(BitMap::read(&path.value()).unwrap());
    let (width, height) = (bmp.get_width() as usize, bmp.get_height() as usize);
    let mut pixel_vector: Vec<Vec<u32>> = Vec::<Vec<u32>>::with_capacity(height);
    let mut place_count: usize = 0;
    let mut row_count: usize = 0;
    pixel_vector.push(Vec::<u32>::new());

    for pixel in bmp.get_pixels() {
        let (r, g, b) = (pixel.get_red(), pixel.get_green(), pixel.get_blue());
        let color: u32 = (r as u32) << 16 | (g as u32) << 8 | b as u32;
        if place_count == width {
            place_count = 0;
            row_count += 1;
            pixel_vector.push(Vec::<u32>::new());
        }
        pixel_vector[row_count].push(color);
        //dbg!(place_count, color);
        place_count += 1;
    }

    debug_assert_eq!(
        pixel_vector.len(),
        height,
        "pixel vector wrong size! expected {:} rows, got {:} rows\ncontents: {:?}",
        height,
        pixel_vector.len(),
        pixel_vector
    );
    for row in &pixel_vector {
        debug_assert_eq!(
            row.len(),
            width,
            "pixel vector wrong size! expected {:} columns, got {:} columns\ncontents: {:?}",
            width,
            row.len(),
            row
        );
    }

    pixel_vector.reverse(); // since bitmaps are flipped
    let prefix = "di(";
    let mut row: String = "[".to_string();
    for line in &pixel_vector {
        row += "dr([";
        for hex in line {
            row += &format!("ph(0x{:X}), ", hex);
        }
        row += "]), ";
    }
    for _ in 0..2 {
        row.pop();
    }
    row += "])";

    let output_string: String = prefix.to_owned() + &row;
    output_string.parse().unwrap()
}
