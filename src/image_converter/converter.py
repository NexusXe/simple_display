# http://www.ue.eti.pg.gda.pl/fpgalab/zadania.spartan3/zad_vga_struktura_pliku_bmp_en.html
from PIL import Image
import numpy as np

bitmap = np.array(Image.open("/workspaces/simple_display/src/image_converter/image.bmp", "r"))
print(bitmap.shape)
# file_size = bitmap.read(4)
# reserved = bitmap.read(4)
# data_offset = bitmap.read(4)
# print(bitmap.read(4))
# print((40).to_bytes(4, 'little'))