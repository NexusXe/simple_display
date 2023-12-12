# http://www.ue.eti.pg.gda.pl/fpgalab/zadania.spartan3/zad_vga_struktura_pliku_bmp_en.html
from PIL import Image
import numpy as np

bitmap = np.array(Image.open("src\image_converter\\test2.bmp", "r"))
print(bitmap.shape)
width, height, bytes_per_pixel = bitmap.shape
assert bytes_per_pixel == 3

title: str = "bean"

prefix: str = f"const {title.upper()}: DisplayImage<{width}, {height}> = DisplayImage("

row: str = "["
for line in bitmap:
    row += "["
    for r, g, b in line:
        row += f"Pixel::from_color(Color{{r: {r}, g: {g}, b: {b}}}), "
    row += "], "
row = row[:-2]
row += "]);"

print(prefix + row)

#

#print("let image: DisplayImage<{width}, {height}> = DisplayImage::<{width}, {height}>::from_bmp()"
# file_size = bitmap.read(4)
# reserved = bitmap.read(4)
# data_offset = bitmap.read(4)
# print(bitmap.read(4))
# print((40).to_bytes(4, 'little'))