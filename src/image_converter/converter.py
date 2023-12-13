# http://www.ue.eti.pg.gda.pl/fpgalab/zadania.spartan3/zad_vga_struktura_pliku_bmp_en.html
from PIL import Image
import numpy as np

bitmap = np.array(Image.open("/workspaces/simple_display/src/image_converter/image.bmp", "r"))
#print(bitmap.shape)
width, height, bytes_per_pixel = bitmap.shape
assert bytes_per_pixel == 3

title: str = "bean"

prefix: str = "DisplayImage("

row: str = "["
for line in bitmap:
    row += "["
    for r, g, b in line:
        row += f"pf(({r}, {g},{b})), "
    row += "], "
row = row[:-2]
row += "])"

print(prefix + row)

#

#print("let image: DisplayImage<{width}, {height}> = DisplayImage::<{width}, {height}>::from_bmp()"
# file_size = bitmap.read(4)
# reserved = bitmap.read(4)
# data_offset = bitmap.read(4)
# print(bitmap.read(4))
# print((40).to_bytes(4, 'little'))